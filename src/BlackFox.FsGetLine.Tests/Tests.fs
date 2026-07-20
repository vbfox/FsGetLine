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

[<Tests>]
let csharpDotHeuristicTests =
    testList "csharpDotHeuristicTriggersCompletion" [
        testCase "triggers completion after a '.' following an identifier" <| fun _ ->
            let text = "foo."
            Expect.isTrue (csharpDotHeuristicTriggersCompletion text text.Length) "identifier member access should complete"

        testCase "does not trigger for a '.' inside a numeric literal" <| fun _ ->
            let text = "1."
            Expect.isFalse (csharpDotHeuristicTriggersCompletion text text.Length) "decimal point in a number literal"

        testCase "does not trigger for a '.' in a multi-digit numeric literal" <| fun _ ->
            let text = "123."
            Expect.isFalse (csharpDotHeuristicTriggersCompletion text text.Length) "decimal point in a multi-digit number"

        testCase "triggers when the digits are part of an underscore-prefixed identifier" <| fun _ ->
            let text = "x_123."
            Expect.isTrue (csharpDotHeuristicTriggersCompletion text text.Length) "identifier containing digits, not a numeric literal"

        testCase "triggers when the digits follow a letter (identifier, not a numeric literal)" <| fun _ ->
            let text = "x123."
            Expect.isTrue (csharpDotHeuristicTriggersCompletion text text.Length) "identifier containing digits"
    ]

[<Tests>]
let completionStateTests =
    let withCompletions items (cs: CompletionState.CompletionState) = { cs with Completions = items }

    testList "CompletionState" [
        testCase "create rejects a negative column" <| fun _ ->
            Expect.throwsT<System.ArgumentException> (fun () -> CompletionState.create -1 0 10 5 |> ignore) "col < 0"

        testCase "create rejects a negative row" <| fun _ ->
            Expect.throwsT<System.ArgumentException> (fun () -> CompletionState.create 0 -1 10 5 |> ignore) "row < 0"

        testCase "create rejects a width below one" <| fun _ ->
            Expect.throwsT<System.ArgumentException> (fun () -> CompletionState.create 0 0 0 5 |> ignore) "width < 1"

        testCase "create rejects a height below one" <| fun _ ->
            Expect.throwsT<System.ArgumentException> (fun () -> CompletionState.create 0 0 10 0 |> ignore) "height < 1"

        testCase "create accepts valid bounds" <| fun _ ->
            let cs = CompletionState.create 0 0 10 5
            Expect.equal cs.SelectedItem 0 "starts on the first item"
            Expect.equal cs.TopItem 0 "starts scrolled to the top"

        testCase "nextSelection advances the selection without scrolling while inside the window" <| fun _ ->
            let cs = CompletionState.create 0 0 10 3 |> withCompletions [| "a"; "b"; "c"; "d"; "e" |]
            match CompletionState.nextSelection cs with
            | Some (selected, top) ->
                Expect.equal selected 1 "moves to the next item"
                Expect.equal top 0 "does not scroll yet, still inside the 3-row window"
            | None -> failwith "expected Some"

        testCase "nextSelection must not scroll while the new selection is still inside the window (642b064 fix)" <| fun _ ->
            // Window height 3 currently showing items [1,2,3] (TopItem=1). Moving to item 2 is
            // still inside that window. The pre-fix upstream formula
            // (selected_item + top_item >= Height, i.e. 2+1=3>=3) would have scrolled here
            // incorrectly; the fixed formula (selected_item - top_item >= Height, 2-1=1>=3) does not.
            let cs =
                { CompletionState.create 0 0 10 3 with SelectedItem = 1; TopItem = 1 }
                |> withCompletions [| "a"; "b"; "c"; "d"; "e" |]
            match CompletionState.nextSelection cs with
            | Some (selected, top) ->
                Expect.equal selected 2 "moves to the next item"
                Expect.equal top 1 "item 2 is still visible in the current window, must not scroll"
            | None -> failwith "expected Some"

        testCase "nextSelection scrolls the window by one once the selection passes the last visible row" <| fun _ ->
            let cs =
                { CompletionState.create 0 0 10 3 with SelectedItem = 3; TopItem = 1 }
                |> withCompletions [| "a"; "b"; "c"; "d"; "e" |]
            match CompletionState.nextSelection cs with
            | Some (selected, top) ->
                Expect.equal selected 4 "moves to the next item"
                Expect.equal top 2 "scrolls by exactly one row"
            | None -> failwith "expected Some"

        testCase "nextSelection returns None on the last item" <| fun _ ->
            let cs =
                { CompletionState.create 0 0 10 3 with SelectedItem = 2; TopItem = 0 }
                |> withCompletions [| "a"; "b"; "c" |]
            Expect.isNone (CompletionState.nextSelection cs) "already on the last item"

        testCase "previousSelection retreats the selection without scrolling while inside the window" <| fun _ ->
            let cs =
                { CompletionState.create 0 0 10 3 with SelectedItem = 3; TopItem = 1 }
                |> withCompletions [| "a"; "b"; "c"; "d"; "e" |]
            match CompletionState.previousSelection cs with
            | Some (selected, top) ->
                Expect.equal selected 2 "moves to the previous item"
                Expect.equal top 1 "item 2 is still within the current window, no scroll needed"
            | None -> failwith "expected Some"

        testCase "previousSelection scrolls up once the selection reaches the top row" <| fun _ ->
            let cs =
                { CompletionState.create 0 0 10 3 with SelectedItem = 1; TopItem = 1 }
                |> withCompletions [| "a"; "b"; "c"; "d"; "e" |]
            match CompletionState.previousSelection cs with
            | Some (selected, top) ->
                Expect.equal selected 0 "moves to the first item"
                Expect.equal top 0 "scrolls up to keep the selection visible"
            | None -> failwith "expected Some"

        testCase "previousSelection returns None on the first item" <| fun _ ->
            let cs =
                { CompletionState.create 0 0 10 3 with SelectedItem = 0; TopItem = 0 }
                |> withCompletions [| "a"; "b"; "c" |]
            Expect.isNone (CompletionState.previousSelection cs) "already on the first item"

        testCase "current returns the selected completion" <| fun _ ->
            let cs =
                { CompletionState.create 0 0 10 3 with SelectedItem = 1 }
                |> withCompletions [| "a"; "b"; "c" |]
            Expect.equal (CompletionState.current cs) "b" "index 1 is 'b'"
    ]
