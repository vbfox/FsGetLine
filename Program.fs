namespace BlackFox

    module GetLineTestApp =
        open System
        open FsGetLine.ColoredString

        [<EntryPoint>]
        let main argv = 
            coloredWriteLine "Welcome to the ^[Cyan]F#^[Reset] GetLine shell demo"
            coloredWriteLine "^[DarkGray]Use Ctrl+D to quit"
            Console.WriteLine ()
            
            let autoComplete (str:String) cursor =
                if str.StartsWith("test") && cursor = 4 then
                    { FsGetLine.Completion.Result = ["42"]; FsGetLine.Completion.Prefix = "" }
                else if str.StartsWith("x") && cursor = 1 then
                    { FsGetLine.Completion.Result = ["y";"x";"MarkTheSpot"]; FsGetLine.Completion.Prefix = "x" }
                else
                    { Result = []; Prefix = "" }
            let mutable editor = FsGetLine.create (fun s ->
                {
                    s with
                        AppName = Some("FsGetLineSample")
                        AutoCompleteEvent = Some(autoComplete)
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