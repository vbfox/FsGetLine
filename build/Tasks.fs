module BlackFox.FsGetLine.Build.Tasks

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open BlackFox.Fake
open BlackFox.CommandLine

let rootDir = __SOURCE_DIRECTORY__ </> ".." |> Path.getFullName
let srcDir = rootDir </> "src"
let artifactsDir = rootDir </> "artifacts"
let solution = rootDir </> "FsGetLine.sln"
let libraryProject = srcDir </> "BlackFox.FsGetLine" </> "BlackFox.FsGetLine.fsproj"
let testsProject = srcDir </> "BlackFox.FsGetLine.Tests" </> "BlackFox.FsGetLine.Tests.fsproj"

let configuration = DotNet.BuildConfiguration.Release

let release = ReleaseNotes.load (rootDir </> "Release Notes.md")

let createAndGetDefault () =
    let clean = BuildTask.create "Clean" [] {
        Shell.cleanDir artifactsDir
    }

    let build = BuildTask.create "Build" [clean.IfNeeded] {
        DotNet.build (fun o -> { o with Configuration = configuration }) solution
    }

    let runTests = BuildTask.create "RunTests" [build] {
        DotNet.test (fun o -> { o with Configuration = configuration; NoBuild = true }) testsProject
    }

    let pack = BuildTask.create "Pack" [build] {
        DotNet.pack
            (fun o ->
                { o with
                    Configuration = configuration
                    OutputPath = Some artifactsDir
                    MSBuildParams =
                        { o.MSBuildParams with
                            Properties = ("Version", release.NugetVersion) :: o.MSBuildParams.Properties } })
            libraryProject
    }

    let publish = BuildTask.create "Publish" [pack] {
        let apiKey =
            match Environment.environVarOrNone "NUGET_KEY" with
            | Some key -> key
            | None -> UserInput.getUserPassword "NuGet API key: "

        let nupkg = artifactsDir </> sprintf "BlackFox.FsGetLine.%s.nupkg" release.NugetVersion

        let args =
            CmdLine.empty
            |> CmdLine.append "push"
            |> CmdLine.append nupkg
            |> CmdLine.appendPrefix "--api-key" apiKey
            |> CmdLine.appendPrefix "--source" "https://api.nuget.org/v3/index.json"
            |> CmdLine.toString

        let result = DotNet.exec id "nuget" args
        if not result.OK then
            failwithf "dotnet nuget push failed with code %i" result.ExitCode
    }

    let ci = BuildTask.createEmpty "CI" [clean; build; runTests; pack]

    BuildTask.createEmpty "Default" [build; runTests]
