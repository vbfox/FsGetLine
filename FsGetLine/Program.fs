namespace BlackFox

    module GetLineTestApp =
        open System

        [<EntryPoint>]
        let main argv = 
            let mutable editor = FsGetLine.create (fun s -> { s with AppName = Some("foo") })
            let mutable s = Some("")
            while s.IsSome do
                let (newEditor, line) = editor |> FsGetLine.get "F#" ""
                editor <- newEditor
                s <- line
                
                match s with
                | Some(s) -> printf "%s\r\n\r\n" s
                | None -> ()
            0