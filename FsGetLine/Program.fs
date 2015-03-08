// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

namespace Mono.Terminal

    module GetLineTestApp =

        open System
        open Mono.Terminal.GetLine

        [<EntryPoint>]
        let main argv = 
            let le = new LineEditor()
            let mutable editorState = makeGlobalState (Some("foo")) 100
            let mutable s = Some("")
            while s.IsSome do
                let (newState, line) = le.Edit "shell> " "" editorState
                editorState <- newState
                s <- line

                match s with
                | Some(s) -> Console.WriteLine ("----> [{0}]", s);
                | None -> ()

            0