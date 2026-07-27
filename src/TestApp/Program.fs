namespace BlackFox

    module GetLineTestApp =
        open System
        open ColoredString

        [<EntryPoint>]
        let main argv =
            coloredWriteLine "Welcome to the ^[Cyan]F#^[Reset] GetLine shell demo"
            coloredWriteLine "^[DarkGray]Use Ctrl+D to quit, Tab to complete, or type '.' for a popup"
            Console.WriteLine ()

            let autoComplete (str:String) cursor =
                if str.StartsWith("test") && cursor = 4 then
                    { FsGetLine.Completion.Result = ["42"]; FsGetLine.Completion.Prefix = "" }
                else if str.StartsWith("x") && cursor = 1 then
                    { FsGetLine.Completion.Result = ["y";"x";"MarkTheSpot"]; FsGetLine.Completion.Prefix = "x" }
                else if cursor > 0 && str.[cursor - 1] = '.' then
                    // Shown automatically (no Tab needed) whenever HeuristicsMode triggers on '.'
                    { FsGetLine.Completion.Result = ["One";"Two";"Three";"Four";"Five";"Six";"Seven";"Eight";"Nine";"Ten"]; FsGetLine.Completion.Prefix = "" }
                else
                    { Result = []; Prefix = "" }
            let mutable editor = FsGetLine.create (fun s ->
                {
                    s with
                        AppName = Some("FsGetLineSample")
                        AutoCompleteEvent = Some(autoComplete)
                        HeuristicsMode = FsGetLine.HeuristicsMode.CSharp
                })
            let mutable s = Some("")
            while s.IsSome do
                let (newEditor, line) = editor |> FsGetLine.get (ColoredString "^[Cyan]F# ^[Yellow]Shell ^[DarkGray]> ") ""
                editor <- newEditor
                s <- line
                
                match s with
                | Some(s) -> printf "%s\r\n\r\n" s
                | None -> ()
            0