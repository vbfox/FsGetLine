// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

namespace Mono.Terminal

    module GetLineTestApp =

        open System
        open Mono.Terminal.GetLine

        [<EntryPoint>]
        let main argv = 
            let le = new LineEditor(Some("foo"))
            let mutable s = Some("")
            while s.IsSome do
                s <- le.Edit "shell> " ""

                match s with
                | Some(s) -> Console.WriteLine ("----> [{0}]", s);
                | None -> ()

            0