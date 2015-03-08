// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

namespace Mono.Terminal

    module GetLineTestApp =

        open System
        open Mono.Terminal

        [<EntryPoint>]
        let main argv = 
            let mutable editor = GetLine.create (fun s -> { s with AppName = Some("foo") })
            let mutable s = Some("")
            while s.IsSome do
                let (newEditor, line) = editor |> GetLine.get "F#" ""
                editor <- newEditor
                s <- line
                
                match s with
                | Some(s) -> Console.WriteLine ("----> [{0}]", s);
                | None -> ()

            0