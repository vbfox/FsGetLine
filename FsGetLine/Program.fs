// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

namespace Mono.Terminal

    module GetLineTestApp =

        open System
        open Mono.Terminal.GetLine

        [<EntryPoint>]
        let main argv = 
            let le = new LineEditor("foo")
            let mutable s = ""
            while s <> null do
                s <- le.Edit "shell> " ""
                if s <> null then Console.WriteLine ("----> [{0}]", s);

            0