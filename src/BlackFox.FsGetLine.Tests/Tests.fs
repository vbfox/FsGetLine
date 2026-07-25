module BlackFox.FsGetLine.Tests.LineEditorTests

open Expecto
open BlackFox.FsGetLine

[<Tests>]
let commonPrefixLengthTests =
    testList "commonPrefixLength" [
        testCase "stops advancing once a divergent character is found (regression, would previously hang)" <| fun _ ->
            // The inner scan of the other candidates never advanced its index, so once the first
            // candidate ("food") diverged from another ("fork") past position 0, the loop spun
            // forever instead of detecting the mismatch and returning.
            let result = commonPrefixLength [| "food"; "fork"; "foo" |]
            Expect.equal result 1 "matches up to 'fo'"
    ]
