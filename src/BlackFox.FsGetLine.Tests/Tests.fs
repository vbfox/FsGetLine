module BlackFox.FsGetLine.Tests.LineEditorTests

open Expecto
open BlackFox.FsGetLine

let config = { FsCheckConfig.defaultConfig with maxTest = 10000 }

[<Tests>]
let commonPrefixLengthTests =
    testList "commonPrefixLength" [
        testCase "empty" <| fun _ ->
            let result = commonPrefixLength [| "" |]
            Expect.equal result None "no common prefix on empty"

        testCase "stops advancing once a divergent character is found" <| fun _ ->
            let result = commonPrefixLength [| "food"; "fork"; "foo" |]
            Expect.equal result (Some 1) "matches up to 'fo'"

        testCase "does not crash when a later candidate is a strict prefix of the first (regression)" <| fun _ ->
            let result = commonPrefixLength [| "abcdef"; "ab" |]
            Expect.equal result (Some 1) "matches up to 'ab'"

        testCase "char then empty" <| fun _ ->
            let result = commonPrefixLength [| "a"; "" |]
            Expect.equal result None "no common prefix"

        testCase "simple" <| fun _ ->
            let result = commonPrefixLength [| "Build.Foo"; "Build.Bar" |]
            Expect.equal result (Some 5) "matches up to 'Build.'"

        testProperty "prefix is in bound" <|
              fun (xs: string array) ->
                let result = commonPrefixLength xs
                match result with
                | Some result ->
                    Expect.isGreaterThanOrEqual result -1 "Result is -1 or more"
                    let maxLen = if xs.Length > 0 then xs |> Array.map (fun x -> x.Length) |> Array.max else -1
                    Expect.isLessThan result maxLen "Result is less than max element length"
                | None ->
                    ()

    ]
