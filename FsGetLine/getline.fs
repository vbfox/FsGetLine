//
// getline.fs: A command line editor
//
// Authors:
//   Miguel de Icaza (miguel@novell.com)
//   Julien Roncaglia (julien@roncaglia.fr)
//
// Copyright 2008 Novell, Inc.
// Copyright 2015 Julien Roncaglia <julien@roncaglia.fr>
//
// Dual-licensed under the terms of the MIT X11 license or the
// Apache License 2.0
//
// TODO:
//    Enter an error (a = 1);  Notice how the prompt is in the wrong line
//        This is caused by Stderr not being tracked by System.Console.
//    Completion support
//    Why is Thread.Interrupt not working?   Currently I resort to Abort which is too much.
//
// Limitations in System.Console:
//    Console needs SIGWINCH support of some sort
//    Console needs a way of updating its position after things have been written
//    behind its back (P/Invoke puts for example).
//    System.Console needs to get the DELETE character, and report accordingly.
//
namespace Mono.Terminal

    module GetLine =
        open System
        open System.Text
        open System.IO
        open System.Threading
        open System.Reflection

        type Completion = { Result : string list; Prefix : string }
        type AutoCompleteHandler = string -> int -> Completion
        type KeyHandler = unit -> unit

        type Command =
            | Done = 1
            | Home = 2
            | End = 3
            | Left = 4
            | Right = 5
            | HistoryPrev = 6
            | HistoryNext = 7
            | Backspace = 8
            | TabOrComplete = 9
            | Yank = 10
            | DeleteChar = 11
            | Refresh = 12
            | ReverseSearch = 13
            | BackwardWord = 14
            | ForwardWord = 15
            | DeleteWord = 16
            | DeleteBackword = 17
            | Quote = 18
            | CmdKillToEOF = 19

        type SearchDirection =
            | Forward = 1
            | Backward = 2

        type Handler(cmd : Command, keyInfo : ConsoleKeyInfo, h : KeyHandler) =
            member val HandledCommand = cmd
            member val KeyInfo = keyInfo
            member val KeyHandler = h

            new(cmd : Command, key, h : KeyHandler) = Handler(cmd, new ConsoleKeyInfo((char) 0, key, false, false, false), h)
            new(cmd : Command, c, h : KeyHandler) = Handler(cmd, new ConsoleKeyInfo (c, ConsoleKey.Zoom, false, false, false), h)
            
            static member Alt cmd c k h = Handler (cmd, new ConsoleKeyInfo (c, k, false, true, false), h)
            static member Control cmd (c : char) h = Handler (cmd, (char) ((int)c - (int)'A' + 1), h)

        /// Emulates the bash-like behavior, where edits done to the
        /// history are recorded
        module History =
            type History =
                {
                    Lines : string array
                    Head : int
                    Tail : int
                    Cursor : int
                    Count : int
                    App : string option
                }
                member this.Length with get() = this.Lines.Length

            let empty app size = { Lines = Array.zeroCreate size; Head = 0; Tail = 0; Cursor = 0; Count = 0; App = app}

            let append line history =
                let newLines = history.Lines |> Array.copy
                newLines.[history.Head] <- line
                let newHead = (history.Head+1) % history.Length
                let newTail = if newHead = history.Tail then (history.Tail+1 % history.Length) else history.Tail
                let newCount = if history.Count <> history.Length then history.Count + 1 else history.Count

                { history with Lines = newLines; Head = newHead; Tail = newTail; Count = newCount}

            /// Updates the current cursor location with the string,
            /// to support editing of history items.   For the current
            /// line to participate, an Append must be done before.
            let update s history =
                let newLines = history.Lines |> Array.copy
                newLines.[history.Cursor] <- s
                { history with Lines = newLines }

            let removeLast history =
                {
                    history with
                        Head = if history.Head = 0 then history.Length - 1 else history.Head - 1
                }
            
            let accept s history = 
                let newLines = history.Lines |> Array.copy
                let t = if history.Head - 1 >= 0 then history.Head - 1 else history.Length - 1
                newLines.[t] <- s;
                { history with Lines = newLines }

            let previousAvailable history =
                if history.Count = 0 then
                    false
                else
                    let next = if history.Cursor-1 >= 0 then history.Cursor-1 else history.Count-1
                    not (next = history.Head)

            let nextAvailable history =
                if history.Count = 0 then
                    false
                else
                    let next = (history.Cursor + 1) % history.Length
                    not (next = history.Head)

            /// Returns: a string with the previous line contents, or
            /// nul if there is no data in the history to move to.
            let previous history =
                if not (previousAvailable history) then
                    (history, None)
                else
                    let newCursor = if history.Cursor = 0 then history.Length - 1 else history.Cursor - 1
                    ({ history with Cursor = newCursor }, Some(history.Lines.[newCursor]))

            let next history =
                if not (nextAvailable history) then
                    (history, None)
                else
                    let newCursor = (history.Cursor + 1) % history.Length
                    ({ history with Cursor = newCursor }, Some(history.Lines.[newCursor]))

            let cursorToEnd history =
                if history.Head <> history.Tail then
                    { history with Cursor = history.Head }
                else
                    history

            let dump history =
                printf "Head=%i Tail=%i Cursor=%i count=%i" history.Head history.Tail history.Cursor history.Count
                for i = 0 to history.Length - 1 do
                    let cursorIndicator = if i = history.Cursor then "==>" else "   "
                    printf " %s %i: %s" cursorIndicator i history.Lines.[i]

            let searchBackward (term:string) history =
                let mutable i = 0
                let mutable found = false
                let mutable newCursor = history.Cursor
                while (i < history.Count && not found) do
                    let mutable slot = history.Cursor-i-1;
                    if slot < 0 then
                        slot <- history.Length+slot;
                    if slot >= history.Length then
                        slot <- 0
                    if (history.Lines.[slot] <> null && history.Lines.[slot].IndexOf (term) <> -1) then
                        newCursor <- slot;
                        found <- true

                    i <- i + 1

                if found then
                    ({history with Cursor = newCursor}, Some(history.Lines.[newCursor]))
                else
                    (history, None)

            let private getFile app = 
                match app with
                | Some(app) ->
                    let dir = Environment.GetFolderPath (Environment.SpecialFolder.Personal)
                    let path = Path.Combine (dir, "." + app + "-history")
                    Some(path)
                | None ->
                    None

            let load app size =
                let histfile = getFile app

                match histfile with
                | Some(histfile) ->
                    if File.Exists histfile then
                        let rec loadNextLine (reader:StreamReader) history =
                            let line = reader.ReadLine ()
                            match line with
                            | null -> history
                            | _ -> history |> append line |> loadNextLine reader

                        use sr = File.OpenText histfile
                        loadNextLine sr (empty app size)
                    else
                        empty app size
                | None -> empty app size               

            let save history =
                let histfile = getFile history.App
                match histfile with
                | Some(histfile) ->
                    use sw = File.CreateText histfile
                    let start = if history.Count = history.Length then history.Head else history.Tail
                    for i = start to start + history.Count - 1 do
                        let p = i % history.Length
                        sw.WriteLine history.Lines.[p]
                | None -> ()

        type LineEditorGlobalState =
            {
                /// Our object that tracks history
                History : History.History

                /// The contents of the kill buffer (cut/paste in Emacs parlance)
                KillBuffer : string

                /// Invoked when the user requests auto-completion using the tab character
                AutoCompleteEvent : AutoCompleteHandler option
            }

        let makeGlobalState (name : string option) (histsize : int) =
            {
                History = History.empty name histsize
                KillBuffer = ""
                AutoCompleteEvent = None
            }

        type SearchState =
            {
                /// Current search direction
                Direction: SearchDirection

                /// The position where we found the match
                MatchAt: int

                /// The string being searched for
                Term: string
            }

        type LineEditorState =
            {
                /// The text being edited.
                Text : string

                /// The text as it is rendered (replaces (char)1 with ^A on display for example).
                RenderedText : string

                /// The prompt specified
                SpecifiedPrompt : string

                /// The prompt shown to the user.
                ShownPrompt : string

                // The current cursor position, indexes into "text", for an index
                // into st.RenderedText, use TextToRenderPos
                Cursor : int

                // The row where we started displaying data.
                HomeRow : int

                // The maximum length that has been displayed on the screen
                MaxRendered : int

                // If we are done editing, this breaks the interactive loop
                DoneEditing : bool

                // The thread where the Editing started taking place
                EditThread : Thread

                /// Our object that tracks history
                History : History.History

                /// The contents of the kill buffer (cut/paste in Emacs parlance)
                KillBuffer : string

                /// Invoked when the user requests auto-completion using the tab character
                AutoCompleteEvent : AutoCompleteHandler option

                SearchState : SearchState option
                PreviousSearch : string option
        
                // Used to implement the Kill semantics (multiple Alt-Ds accumulate)
                LastCommand : Command option
            }

        let makeDefaultLineEditorState (name : string option) (histsize : int) =
            {
                Text = ""
                RenderedText = ""
                SpecifiedPrompt = ""
                ShownPrompt = ""
                Cursor = 0
                HomeRow = 0
                MaxRendered = 0
                DoneEditing = false
                EditThread = null
                History = History.empty name histsize
                KillBuffer = ""
                AutoCompleteEvent = None
                SearchState = None
                PreviousSearch = None
                LastCommand = None
            }

        type LineEditor (name : string option, histsize : int) as x =

            let mutable st = makeDefaultLineEditorState name histsize

            [<DefaultValue>]
            val mutable public xx_sharp_is_annoying : string

            let mutable handlers : Handler array = Array.zeroCreate 0

            new(name) = LineEditor(name, 10)

            do 
                handlers <-
                    [|
                        new Handler (Command.Done, ConsoleKey.Enter,      x.CmdDone)
                        new Handler (Command.Home,ConsoleKey.Home,       x.CmdHome)
                        new Handler (Command.End,ConsoleKey.End,        x.CmdEnd)
                        new Handler (Command.Left,ConsoleKey.LeftArrow,  x.CmdLeft)
                        new Handler (Command.Right,ConsoleKey.RightArrow, x.CmdRight)
                        new Handler (Command.HistoryPrev,ConsoleKey.UpArrow,    x.CmdHistoryPrev)
                        new Handler (Command.HistoryNext,ConsoleKey.DownArrow,  x.CmdHistoryNext)
                        new Handler (Command.Backspace,ConsoleKey.Backspace,  x.CmdBackspace)
                        new Handler (Command.DeleteChar,ConsoleKey.Delete,     x.CmdDeleteChar)
                        new Handler (Command.TabOrComplete,ConsoleKey.Tab,        x.CmdTabOrComplete)
                
                        // Emacs keys
                        Handler.Control Command.Home 'A' (x.CmdHome)
                        Handler.Control Command.End 'E' (x.CmdEnd)
                        Handler.Control Command.Left 'B' (x.CmdLeft)
                        Handler.Control Command.Right 'F' (x.CmdRight)
                        Handler.Control Command.HistoryPrev 'P' (x.CmdHistoryPrev)
                        Handler.Control Command.HistoryNext 'N' (x.CmdHistoryNext)
                        Handler.Control Command.CmdKillToEOF 'K' (x.CmdKillToEOF)
                        Handler.Control Command.Yank 'Y' (x.CmdYank)
                        Handler.Control Command.DeleteChar 'D' (x.CmdDeleteChar)
                        Handler.Control Command.Refresh 'L' (x.CmdRefresh)
                        Handler.Control Command.ReverseSearch 'R' (x.CmdReverseSearch)
                        
                        Handler.Alt Command.BackwardWord 'B' ConsoleKey.B (x.CmdBackwardWord)
                        Handler.Alt Command.ForwardWord 'F' ConsoleKey.F (x.CmdForwardWord)
                
                        Handler.Alt Command.DeleteWord 'D' ConsoleKey.D (x.CmdDeleteWord)
                        Handler.Alt Command.DeleteBackword ((char)8) ConsoleKey.Backspace (x.CmdDeleteBackword)
                
                        // DEBUG
                        //Handler.Control ('T', CmdDebug),

                        // quote
                        Handler.Control Command.Quote 'Q' (fun () -> x.HandleChar ((Console.ReadKey (true)).KeyChar))
                    |]

            member private x.CmdDebug () =
                st.History |> History.dump
                Console.WriteLine ()
                x.Render ()

            member private x.Render () =
                Console.Write st.ShownPrompt
                Console.Write st.RenderedText

                let max = System.Math.Max (st.RenderedText.Length + st.ShownPrompt.Length, st.MaxRendered);
            
                for i = st.RenderedText.Length + st.ShownPrompt.Length to st.MaxRendered - 1 do
                    Console.Write (' ');
                st <- { st with MaxRendered = st.ShownPrompt.Length + st.RenderedText.Length }

                // Write one more to ensure that we always wrap around properly if we are at the
                // end of a line.
                Console.Write ' '

                x.UpdateHomeRow max

            member private x.UpdateHomeRow screenpos = 
                let lines = 1 + (screenpos / Console.WindowWidth);

                st <- { st with HomeRow = System.Math.Max (0, Console.CursorTop - (lines - 1)) }       

            member private x.RenderFrom pos =
                let rpos = x.TextToRenderPos (pos)
                let mutable i = rpos;
            
                while i < st.RenderedText.Length do
                    Console.Write (st.RenderedText.[i])
                    i <- i + 1

                if (st.ShownPrompt.Length + st.RenderedText.Length) > st.MaxRendered then
                    st <- { st with MaxRendered = st.ShownPrompt.Length + st.RenderedText.Length }
                else
                    let max_extra = st.MaxRendered - st.ShownPrompt.Length
                    while i < max_extra do
                        Console.Write (' ')
                        i <- i + 1

            let render (text:string) =                 
                let renderedText = new StringBuilder()

                for i = 0 to text.Length - 1 do
                    let c = (int) text.[i];
                    if c < 26 then
                        if c = (int)'\t' then
                            renderedText.Append ("    ") |> ignore
                        else
                            renderedText.Append ('^') |> ignore
                            renderedText.Append ((char) (c + (int) 'A' - 1)) |> ignore
                        
                    else
                       renderedText.Append ((char)c) |> ignore

                renderedText.ToString()

            member private x.TextToRenderPos pos =
                let mutable p = 0;

                for i = 0 to pos - 1 do
                    let c = (int) st.Text.[i];
                
                    if c < 26 then
                        if c = 9 then
                            p <- p + 4;
                        else
                            p <- p + 2;
                    else
                        p <- p + 1

                p
            
            member private x.TextToScreenPos pos = st.ShownPrompt.Length + x.TextToRenderPos (pos)

            member private x.LineCount with get () = (st.ShownPrompt.Length + st.RenderedText.Length)/Console.WindowWidth

            member private x.ForceCursor newpos = 
                st <- { st with Cursor = newpos }

                let actual_pos = st.ShownPrompt.Length + x.TextToRenderPos (st.Cursor)
                let row = st.HomeRow + (actual_pos/Console.WindowWidth)
                let row = if row < Console.BufferHeight then row else Console.BufferHeight-1
                let col = actual_pos % Console.WindowWidth

                Console.SetCursorPosition (col, row);

            member private x.UpdateCursor newpos =
                if st.Cursor <> newpos then
                    x.ForceCursor newpos

            member private x.InsertChar (c:char) =
                let prev_lines = x.LineCount
                let newText = st.Text.Insert (st.Cursor, (string)c)
                st <- { st with Text = newText; RenderedText = render newText }
                
                if prev_lines <> x.LineCount then
                    Console.SetCursorPosition (0, st.HomeRow)
                    x.Render ()
                    st <- { st with Cursor = st.Cursor + 1 }
                    x.ForceCursor st.Cursor
                else 
                    x.RenderFrom st.Cursor
                    st <- { st with Cursor = st.Cursor + 1 }
                    x.ForceCursor st.Cursor
                    x.UpdateHomeRow (x.TextToScreenPos (st.Cursor));

            member private x.CmdDone () =
                st <- { st with DoneEditing = true}
            
            member val TabAtStartCompletes : bool = false

            member private x.CmdTabOrComplete () =
                let mutable complete = false;

                if st.AutoCompleteEvent.IsSome then
                    if x.TabAtStartCompletes then
                        complete <- true
                    else 
                        let mutable i = 0
                        while i < st.Cursor && not complete do
                            if not (Char.IsWhiteSpace (st.Text.[i])) then
                                complete <- true

                    if complete then
                        let completion = st.AutoCompleteEvent.Value st.Text st.Cursor
                        let completions = completion.Result
                        if completions.Length <> 0 then
                            let ncompletions = completions.Length
                    
                            if completions.Length = 1 then
                                x.InsertTextAtCursor (completions.[0])
                            else
                                let mutable last = -1
                                let mutable p = 0
                                let mutable mismatch = false
                                while p < completions.[0].Length && not mismatch do
                                    let c = completions.[0].[p]
                                    let mutable i = 1
                                    while i < ncompletions && not mismatch do
                                        if completions.[i].Length < p then mismatch <- true
                                        if completions.[i].[p] <> c then mismatch <- true
                                    
                                    if not mismatch then
                                        last <- p;
                                        p <- p + 1

                                if last <> -1 then
                                    x.InsertTextAtCursor (completions.[0].Substring (0, last+1))
                                
                                Console.WriteLine ()
                                for s in completions do
                                    Console.Write (completion.Prefix)
                                    Console.Write (s)
                                    Console.Write (' ')
                                
                                Console.WriteLine ()
                                x.Render ()
                                x.ForceCursor st.Cursor
                            
                    else
                        x.HandleChar ('\t')
                else
                    x.HandleChar ('t')
            
        
            member private x.CmdHome () = x.UpdateCursor (0)
            member private x.CmdEnd () = x.UpdateCursor (st.Text.Length)
            member private x.CmdLeft () = if st.Cursor <> 0 then x.UpdateCursor (st.Cursor-1)

            member private x.CmdBackwardWord () =
                let p = x.WordBackward st.Cursor
                if p <> -1 then x.UpdateCursor (p)

            member private x.CmdForwardWord () =
                let p = x.WordForward st.Cursor
                if p <> -1 then x.UpdateCursor (p);

            member private x.CmdRight () =
                if (st.Cursor <> st.Text.Length) then x.UpdateCursor (st.Cursor+1);


            member private x.RenderAfter p =
                x.ForceCursor p
                x.RenderFrom p
                x.ForceCursor st.Cursor
        
            member private x.CmdBackspace () =
                if st.Cursor <> 0 then
                    let newCursor = st.Cursor - 1
                    let newText = st.Text.Remove (newCursor, 1)
                    st <- { st with Cursor = newCursor; Text = newText; RenderedText = render newText}
                    x.RenderAfter st.Cursor

            member private x.CmdDeleteChar () =
                // If there is no input, this behaves like EOF
                if st.Text.Length = 0 then
                    st <- { st with DoneEditing = true; Text = null }
                    Console.WriteLine ()
                else if (st.Cursor <> st.Text.Length) then
                    let newText = st.Text.Remove (st.Cursor, 1)
                    st <- { st with Text = newText; RenderedText = render newText }
                    x.RenderAfter st.Cursor

            member private x.WordForward p =
                let text = st.Text

                if (p >= text.Length) then
                    -1
                else
                    let mutable i = p;
                    if Char.IsPunctuation (text.[p]) || Char.IsSymbol (text.[p]) || Char.IsWhiteSpace (text.[p]) then
                        while (i < text.Length && not (Char.IsLetterOrDigit (text.[i]))) do i <- i + 1
                        while (i < text.Length && Char.IsLetterOrDigit (text.[i])) do i <- i + 1
                    else
                        while (i < text.Length && Char.IsLetterOrDigit (text.[i])) do i <- i + 1
                    
                    if i <> p then i else -1
            

            member private x.WordBackward p =
                let text = st.Text

                if p = 0 then
                    -1
                else if p = 1 then
                    0
                else
                    let mutable i = p-1;
            
                    if Char.IsPunctuation (text.[i]) || Char.IsSymbol (text.[i]) || Char.IsWhiteSpace (text.[i]) then
                        while (i >= 0 && not (Char.IsLetterOrDigit (text.[i]))) do i <- i - 1
                        while (i >= 0 && Char.IsLetterOrDigit (text.[i])) do i <- i - 1
                    else
                        while (i >= 0 && Char.IsLetterOrDigit (text.[i])) do i <- i - 1
                    
                    i <- i + 1
            
                    if i <> p then i else -1
            
       
            member private x.CmdDeleteWord () =
                let pos = x.WordForward st.Cursor

                if pos <> -1 then
                    let k = st.Text.Substring (st.Cursor, pos-st.Cursor)
           
                    let newKillBuffer = if st.LastCommand = Some(Command.DeleteWord) then st.KillBuffer + k else k
                    let newText = st.Text.Remove (st.Cursor, pos-st.Cursor)
                    st <- { st with KillBuffer = newKillBuffer; Text = newText; RenderedText = render newText }

                    x.RenderAfter st.Cursor
        
            member private x.CmdDeleteBackword () =
                let pos = x.WordBackward st.Cursor
                if pos <> -1 then
                    let k = st.Text.Substring (pos, st.Cursor-pos)
            
                    let newKillBuffer = if st.LastCommand = Some(Command.DeleteBackword) then st.KillBuffer + k else k
                    let newText = st.Text.Remove (pos, st.Cursor-pos)
                    st <- { st with KillBuffer = newKillBuffer; Text = newText; RenderedText = render newText }

                    x.RenderAfter (pos)
        
            //
            // Adds the current line to the history if needed
            //
            member private x.HistoryUpdateLine () =
                let newHistory = st.History |> History.update st.Text
                st <- { st with History = newHistory }
        
            member private x.CmdHistoryPrev () =
                if History.previousAvailable st.History then
                    x.HistoryUpdateLine ()
                    let (newHistory, text) = History.previous st.History
                    st <- { st with History = newHistory }
                    x.SetText (text)

            member private x.CmdHistoryNext () =
                if History.nextAvailable st.History then
                    let (newHistory, text) = st.History |> History.update st.Text |> History.next 
                    st <- { st with History = newHistory }
                    x.SetText (text)

            member private x.CmdKillToEOF () =
                let newKillBuffer = st.Text.Substring (st.Cursor, st.Text.Length-st.Cursor)
                let newText = st.Text.Substring(0, st.Cursor)
                st <- { st with KillBuffer = newKillBuffer; Text = newText; RenderedText = render newText }
                
                x.RenderAfter st.Cursor

            member private x.CmdYank () =
                x.InsertTextAtCursor st.KillBuffer


            member private x.InsertTextAtCursor str =
                let prev_lines = x.LineCount;
                let newText = st.Text.Insert (st.Cursor, str)
                st <- { st with Text = newText; RenderedText = render newText}
                if prev_lines <> x.LineCount then
                    Console.SetCursorPosition (0, st.HomeRow)
                    x.Render ()
                    st <- { st with Cursor = st.Cursor + str.Length }
                    x.ForceCursor st.Cursor
                else
                    x.RenderFrom st.Cursor
                    st <- { st with Cursor = st.Cursor + str.Length }
                    x.ForceCursor st.Cursor
                    x.UpdateHomeRow (x.TextToScreenPos st.Cursor)
        
            member private x.SetSearchPrompt s =
                x.SetPrompt ("(reverse-i-search)`" + s + "': ")

            member private x.ReverseSearch () =
                match st.SearchState with
                | Some(search) ->
                    let mutable search_backward = true

                    if st.Cursor = st.Text.Length then
                        // The cursor is at the end of the string
                        let p = st.Text.LastIndexOf (search.Term)
                        if p <> -1 then
                            st <- { st with SearchState = Some({ search with MatchAt = p }); Cursor = p }
                            x.ForceCursor st.Cursor
                            search_backward <- false
                    else
                        // The cursor is somewhere in the middle of the string
                        let start = if st.Cursor = search.MatchAt then st.Cursor - 1 else st.Cursor
                        if start <> -1 then
                            let p = st.Text.LastIndexOf (search.Term, start)
                            if p <> -1 then
                                st <- { st with SearchState = Some({ search with MatchAt = p }); Cursor = p }
                                x.ForceCursor st.Cursor
                                search_backward <- false

                    if search_backward then
                        // Need to search backwards in history
                        x.HistoryUpdateLine ()
                        let (newHistory, searchResult) = History.searchBackward search.Term st.History
                        st <- { st with History = newHistory }
                        match searchResult with
                        | Some(_) ->
                            st <- { st with SearchState = Some({ search with MatchAt = -1 }) }
                            x.SetText searchResult
                            x.ReverseSearch ()
                        | None -> ()
                | None ->
                    failwith "No search in progress"

            member private x.CmdReverseSearch () =
                match st.SearchState with
                | None ->
                    st <- { st with SearchState = Some { MatchAt = -1; Term = ""; Direction = SearchDirection.Backward } }
                    x.SetSearchPrompt ("")
                | Some(search) ->
                    if search.Term = "" then
                        match st.PreviousSearch with
                        | None | Some("") ->
                            ()
                        | Some(previousTerm) -> 
                            st <- { st with SearchState = Some { search with Term = previousTerm } }
                            x.SetSearchPrompt previousTerm
                            x.ReverseSearch ()
                    else
                        x.ReverseSearch ()


            member private x.SearchAppend (c:char) search =
                let newTerm = search.Term + (string)c
                st <- { st with SearchState = Some { search with Term = newTerm } }
                x.SetSearchPrompt newTerm

                //
                // If the new typed data still matches the current text, stay here
                //
                let mutable still_matches = false
                if st.Cursor < st.Text.Length then
                    let r = st.Text.Substring (st.Cursor, st.Text.Length - st.Cursor)
                    if r.StartsWith newTerm then still_matches <- true
                
                if not still_matches then
                    x.ReverseSearch ()
       
            member private x.CmdRefresh () =
                Console.Clear ()
                st <- { st with MaxRendered = 0 }
                x.Render ()
                x.ForceCursor st.Cursor

            member private x.InterruptEdit (sender:obj) (a:ConsoleCancelEventArgs) =
                // Do not abort our program:
                a.Cancel <- true;

                // Interrupt the editor
                if st.EditThread <> null then
                    st.EditThread.Abort ()

            member private x.HandleChar c =
                match st.SearchState with
                | Some(search) -> x.SearchAppend c search
                | None -> x.InsertChar (c)

            let readKeyWithEscMeaningAlt () =
                let key = Console.ReadKey (true)
                if key.Key = ConsoleKey.Escape then
                    (Console.ReadKey (true), ConsoleModifiers.Alt)
                else
                    (key, key.Modifiers)

            member private x.EditLoop () =
                while not st.DoneEditing do
                    let (newInput, modifier) = readKeyWithEscMeaningAlt ()
               
                    let mutable handler_index = 0
                    let mutable command : Command option = None
                    while handler_index < handlers.Length && command.IsNone do
                        let handler = handlers.[handler_index]
                        let handlerKeyInfo = handler.KeyInfo;

                        if handlerKeyInfo.Key = newInput.Key && handlerKeyInfo.Modifiers = modifier then
                            handler.KeyHandler ()
                            command <- Some(handler.HandledCommand)
                        else if handlerKeyInfo.KeyChar = newInput.KeyChar && handlerKeyInfo.Key = ConsoleKey.Zoom then
                            handler.KeyHandler ()
                            command <- Some(handler.HandledCommand)

                        handler_index <- handler_index + 1

                    if command.IsSome then
                        st <- { st with LastCommand = command }
                        match (st.SearchState, command) with
                        | ( _, Some(Command.ReverseSearch)) -> ()
                        | (Some(search), _) ->
                            st <- { st with PreviousSearch = Some(search.Term); SearchState = None}
                            x.SetPrompt st.SpecifiedPrompt
                        | _ -> ()
                   
                    else if (newInput.KeyChar <> (char) 0) then
                        x.HandleChar (newInput.KeyChar)
                 
            member private x.InitText (initial:string option) =
                let newText = match initial with | Some(initial) -> initial | None -> ""
                st <- { st with Cursor = newText.Length; Text = newText; RenderedText = render newText }
                x.Render ()
                x.ForceCursor st.Cursor

            member private x.SetText newtext =
                Console.SetCursorPosition (0, st.HomeRow)
                x.InitText (newtext)

            member private x.SetPrompt newprompt =
                st <- { st with ShownPrompt = newprompt }
                Console.SetCursorPosition (0, st.HomeRow)
                x.Render ()
                x.ForceCursor st.Cursor
       
            member public x.Edit prompt initial =
                st <-
                    {
                        st with
                            EditThread = Thread.CurrentThread
                            SearchState = None
                            DoneEditing = false
                            History = st.History |> History.cursorToEnd |> History.append initial
                            MaxRendered = 0
                            SpecifiedPrompt = prompt
                            ShownPrompt = prompt
                    }

                let cancelHandler = new ConsoleCancelEventHandler(x.InterruptEdit)
                Console.CancelKeyPress.AddHandler cancelHandler

                x.InitText (Some(initial))

                while not st.DoneEditing do
                    try
                        x.EditLoop ()
                    with
                    | :? ThreadAbortException ->
                        st <- { st with SearchState = None }
                        Thread.ResetAbort ()
                        Console.WriteLine ()
                        x.SetPrompt (prompt)
                        x.SetText (Some(""))
                
                Console.WriteLine ();
            
                Console.CancelKeyPress.RemoveHandler cancelHandler

                if st.Text <> "" then
                    st <- { st with History = History.accept st.Text st.History }
                else
                    st <- { st with History = History.removeLast st.History }

                Some(st.Text)
        
            member public x. SaveHistory () =
                st.History |> History.save
                