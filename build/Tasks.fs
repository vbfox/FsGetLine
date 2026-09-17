module BlackFox.FsGetLine.Build.Tasks

open System.Xml.Linq

open Fake.Api
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.Tools
open BlackFox.Fake
open BlackFox.CommandLine

let rootDir = __SOURCE_DIRECTORY__ </> ".." |> Path.getFullName
let srcDir = rootDir </> "src"
let artifactsDir = rootDir </> "artifacts"
let solution = rootDir </> "FsGetLine.sln"
let libraryProject = srcDir </> "BlackFox.FsGetLine" </> "BlackFox.FsGetLine.fsproj"
let testsProject = srcDir </> "BlackFox.FsGetLine.Tests" </> "BlackFox.FsGetLine.Tests.fsproj"

let configuration = DotNet.BuildConfiguration.Release

/// The profile where the project is posted
let gitOwner = "vbfox"
let gitHome = "https://github.com/" + gitOwner

/// The name of the project on GitHub
let gitName = "FsGetLine"

let private getUnionCaseName (x: 'a) =
    match Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name

let createAndGetDefault () =
    // A release tag build (the "Publish" workflow, triggered by a version tag push) must use the
    // plain version from Release Notes.md, not a CI-suffixed prerelease version, since it has to
    // match the pushed tag and be the version actually published.
    let isReleaseTagBuild = Environment.environVarOrNone "GITHUB_REF_TYPE" = Some "tag"

    let release =
        let fromFile = ReleaseNotes.load (rootDir </> "Release Notes.md")

        if BuildServer.buildServer <> BuildServer.LocalBuild && not isReleaseTagBuild then
            let buildServerName = (getUnionCaseName BuildServer.buildServer).ToLowerInvariant()
            let nugetVer = sprintf "%s-%s.%s" fromFile.NugetVersion buildServerName BuildServer.buildVersion
            ReleaseNotes.ReleaseNotes.New(fromFile.AssemblyVersion, nugetVer, fromFile.Date, fromFile.Notes)
        else
            fromFile

    Trace.setBuildNumber release.NugetVersion

    let nupkgFile = artifactsDir </> sprintf "BlackFox.FsGetLine.%s.nupkg" release.NugetVersion

    /// Written to artifacts/Version.props and imported by Directory.Build.props so that every
    /// project in the solution is built and packed with the version from Release Notes.md.
    let writeVersionProps () =
        let doc =
            XDocument(
                XElement(
                    XName.Get "Project",
                    XElement(
                        XName.Get "PropertyGroup",
                        XElement(XName.Get "Version", release.NugetVersion),
                        XElement(XName.Get "PackageReleaseNotes", String.toLines release.Notes)
                    )
                )
            )

        System.IO.File.WriteAllText(artifactsDir </> "Version.props", doc.ToString())

    let init = BuildTask.create "Init" [] {
        Directory.create artifactsDir
    }

    let clean = BuildTask.create "Clean" [init] {
        Shell.cleanDir artifactsDir
    }

    let generateVersionInfo = BuildTask.create "GenerateVersionInfo" [init; clean.IfNeeded] {
        writeVersionProps ()
    }

    let build = BuildTask.create "Build" [generateVersionInfo; clean.IfNeeded] {
        DotNet.build (fun o -> { o with Configuration = configuration }) solution
    }

    let runTests = BuildTask.create "RunTests" [build] {
        DotNet.test (fun o -> { o with Configuration = configuration; NoBuild = true }) testsProject
    }

    let pack = BuildTask.create "Pack" [build; runTests.IfNeeded] {
        DotNet.pack
            (fun o -> { o with Configuration = configuration; OutputPath = Some artifactsDir })
            libraryProject

        Trace.publish ImportData.BuildArtifact nupkgFile
    }

    /// Validate that it's safe to cut a release, then tag and push. Pushing the tag is what triggers
    /// the "Publish" GitHub Actions workflow, which does the actual build/test/pack/publish.
    let tagRelease = BuildTask.create "TagRelease" [init] {
        Git.CommandHelper.directRunGitCommandAndFail "" "fetch origin main --tags"

        if Git.Information.getBranchName "" <> "main" then
            failwith "Releases must be created from the 'main' branch."

        let localSha = Git.Branches.getSHA1 "" "HEAD"
        let remoteSha = Git.Branches.getSHA1 "" "origin/main"

        if localSha <> remoteSha then
            failwithf
                "Local 'main' (%s) is not in sync with 'origin/main' (%s). Pull or push before releasing."
                localSha
                remoteSha

        if not (Git.Information.isCleanWorkingCopy "") then
            failwith "Working copy has uncommitted changes."

        let tagExistsLocally =
            Git.CommandHelper.getGitResult "" "tag --list" |> Seq.contains release.NugetVersion

        let tagExistsOnRemote =
            Git.CommandHelper.getGitResult "" "ls-remote --tags origin"
            |> Seq.exists (fun (line: string) -> line.EndsWith("refs/tags/" + release.NugetVersion))

        if tagExistsLocally || tagExistsOnRemote then
            failwithf "Tag %s already exists, nothing to release." release.NugetVersion

        let remote =
            Git.CommandHelper.getGitResult "" "remote -v"
            |> Seq.filter (fun (s: string) -> s.EndsWith "(push)")
            |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + gitName))
            |> function
                | None -> gitHome + "/" + gitName
                | Some(s: string) -> s.Split().[0]

        Trace.log (sprintf "About to release %s to %s:" release.NugetVersion remote)
        Trace.log (String.toLines release.Notes)

        let answer =
            UserInput.getUserInput (
                sprintf "Push tag %s? This triggers the publish workflow. [y/N] " release.NugetVersion
            )

        if answer.Trim().ToLowerInvariant() <> "y" then
            failwith "Aborted."

        Git.Branches.tag "" release.NugetVersion
        Git.Branches.pushTag "" remote release.NugetVersion
    }

    /// Guard against a tag that doesn't match Release Notes.md (the published version comes from
    /// the file, so a mismatch would publish something other than what was tagged).
    let checkReleaseTag = BuildTask.create "CheckReleaseTag" [init] {
        match Environment.environVarOrNone "GITHUB_REF_NAME" with
        | Some tag when isReleaseTagBuild && tag <> release.NugetVersion ->
            failwithf "Tag '%s' doesn't match the version in Release Notes.md ('%s')." tag release.NugetVersion
        | _ -> ()
    }

    let ciPublishNuget = BuildTask.create "CIPublishNuget" [checkReleaseTag; pack] {
        let key =
            match Environment.environVarOrNone "nuget-key" with
            | Some key -> key
            | None -> UserInput.getUserPassword "NuGet key: "

        let args =
            CmdLine.empty
            |> CmdLine.append "push"
            |> CmdLine.append nupkgFile
            |> CmdLine.appendPrefix "--api-key" key
            |> CmdLine.appendPrefix "--source" "https://api.nuget.org/v3/index.json"
            |> CmdLine.toString

        let result = DotNet.exec id "nuget" args

        if not result.OK then
            failwithf "dotnet nuget push failed with code %i:\n%s" result.ExitCode (String.concat "\n" result.Errors)
    }

    let ciPublishGitHubRelease = BuildTask.create "CIPublishGitHubRelease" [checkReleaseTag; pack] {
        let client =
            match Environment.environVarOrNone "GITHUB_TOKEN" with
            | Some token -> GitHub.createClientWithToken token
            | None ->
                // This path is never taken by CI but kept for running the task by hand
                let user =
                    match Environment.environVarOrNone "github-user" with
                    | Some s -> s
                    | None -> UserInput.getUserInput "GitHub Username: "

                let pw =
                    match Environment.environVarOrNone "github-pw" with
                    | Some s -> s
                    | None -> UserInput.getUserPassword "GitHub Password or Token: "

                GitHub.createClient user pw

        client
        |> GitHub.draftNewRelease gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes
        |> GitHub.uploadFile nupkgFile
        |> GitHub.publishDraft
        |> Async.RunSynchronously
    }

    /// Run locally to cut a release: validates preconditions then tags and pushes, which triggers
    /// the `Publish` GitHub Actions workflow (see .github/workflows/publish.yml).
    let _release = BuildTask.createEmpty "Release" [tagRelease]

    /// Invoked by the `Publish` GitHub Actions workflow after a release tag is pushed: builds,
    /// tests, packs, creates the GitHub Release and publishes the NuGet package.
    let _ciPublishRelease =
        BuildTask.createEmpty "CIPublishRelease" [clean; runTests; pack; ciPublishGitHubRelease; ciPublishNuget]

    let _ci = BuildTask.createEmpty "CI" [clean; build; runTests; pack]

    BuildTask.createEmpty "Default" [build; runTests]
