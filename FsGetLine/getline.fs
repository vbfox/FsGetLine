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
        type AutoCompleteHandler = delegate of string * int -> Completion
        type KeyHandler = delegate of unit -> unit

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

        type Handler(cmd : Command, cki : ConsoleKeyInfo, h : KeyHandler) =
            member val HandledCommand = cmd
            member val CKI = cki
            member val KeyHandler = h

            new(cmd : Command, key, h : KeyHandler) = Handler(cmd, new ConsoleKeyInfo((char) 0, key, false, false, false), h)
            new(cmd : Command, c, h : KeyHandler) = Handler(cmd, new ConsoleKeyInfo (c, ConsoleKey.Zoom, false, false, false), h)
            
            static member Alt cmd c k h = Handler (cmd, new ConsoleKeyInfo (c, k, false, true, false), h)
            static member Control cmd (c : char) h = Handler (cmd, (char) ((int)c - (int)'A' + 1), h)

        /// Emulates the bash-like behavior, where edits done to the
        /// history are recorded
        type History(app : string, size : int) as x =
            let mutable history : string array = Array.zeroCreate size
            let mutable histfile = null
            let mutable head = 0
            let mutable tail = 0
            let mutable cursor = 0
            let mutable count = 0

            do
                if app <> null then
                    let dir = Environment.GetFolderPath (Environment.SpecialFolder.Personal)
                    histfile <- Path.Combine (dir, ".mal-history");

                if File.Exists histfile then
                    use sr = File.OpenText histfile
                    let mutable line = ""

                    let mutable cont = true
                    while cont do
                        line <- sr.ReadLine ()
                        if line <> null then
                            if line <> "" then x.Append line
                        else
                            cont <- false
            
            member x.Close () =
                if histfile <> null then
                    use sw = File.CreateText histfile
                    let start = if count = history.Length then head else tail
                    for i = start to start + count - 1 do
                        let p = i % history.Length
                        sw.WriteLine history.[p]

            /// Appends a value to the history
            member x.Append s =
                history.[head] <- s
                head <- (head+1) % history.Length
                if head = tail then
                    tail <- (tail+1 % history.Length)
                if count <> history.Length then
                    count <- count + 1



            /// Updates the current cursor location with the string,
            /// to support editing of history items.   For the current
            /// line to participate, an Append must be done before.
            member x.Update s =
                history.[cursor] <- s

            member x.RemoveLast () =
                head <- head-1;
                if head < 0 then
                    head <- history.Length - 1
            
            member x.Accept s = 
                let t = if head-1 >= 0 then head-1 else history.Length - 1
                history.[t] <- s;
            
            member x.PreviousAvailable () =
                if count = 0 then
                    false
                else
                    let next = if cursor-1 >= 0 then cursor-1 else count-1
                    not (next = head)
            
            member x.NextAvailable () =
                if count = 0 then
                    false
                else
                    let next = (cursor + 1) % history.Length
                    not (next = head)
            
            /// Returns: a string with the previous line contents, or
            /// nul if there is no data in the history to move to.
            member x.Previous () =
                if not (x.PreviousAvailable()) then
                    null
                else
                    cursor <- cursor - 1
                    if cursor < 0 then
                        cursor <- history.Length - 1

                    history.[cursor]

            member x.Next () =
                if not (x.NextAvailable()) then
                    null
                else

                cursor <- (cursor + 1) % history.Length
                history.[cursor]

            member x.CursorToEnd () =
                if head <> tail then
                    cursor <- head

            member x.Dump () =
                Console.WriteLine ("Head={0} Tail={1} Cursor={2} count={3}", head, tail, cursor, count);
                for i = 0 to history.Length - 1 do
                    let arrow = if i = cursor then "==>" else "   "
                    Console.WriteLine (" {0} {1}: {2}", arrow, i, history.[i]);

            member x.SearchBackward (term:string) =
                let mutable i = 0
                let mutable found = false
                while (i < count && not found) do
                    let mutable slot = cursor-i-1;
                    if slot < 0 then
                        slot <- history.Length+slot;
                    if slot >= history.Length then
                        slot <- 0
                    if (history.[slot] <> null && history.[slot].IndexOf (term) <> -1) then
                        cursor <- slot;
                        found <- true

                    i <- i + 1

                if found then history.[cursor] else null

        type LineEditor (name : string, histsize : int) as x =
            // The text being edited.
            let mutable text = new StringBuilder ()

            // The text as it is rendered (replaces (char)1 with ^A on display for example).
            let rendered_text = new StringBuilder ()

            // The prompt specified, and the prompt shown to the user.
            let mutable specified_prompt : string = null

            let mutable shown_prompt : string = null
        
            // The current cursor position, indexes into "text", for an index
            // into rendered_text, use TextToRenderPos
            let mutable cursor : int = 0

            // The row where we started displaying data.
            let mutable home_row : int = 0

            // The maximum length that has been displayed on the screen
            let mutable max_rendered: int = 0

            // If we are done editing, this breaks the interactive loop
            let mutable done_editing = false

            // The thread where the Editing started taking place
            let mutable edit_thread : Thread = null

            // Our object that tracks history
            let history = new History (name, histsize)

            // The contents of the kill buffer (cut/paste in Emacs parlance)
            let mutable kill_buffer = "";

            // The string being searched for
            let mutable search : string = null
            let mutable last_search : string = null

            // whether we are searching (-1= reverse; 0 = no; 1 = forward)
            let mutable searching : int = 0

            // The position where we found the match.
            let mutable match_at : int = 0
        
            // Used to implement the Kill semantics (multiple Alt-Ds accumulate)
            let mutable last_command : Command = new Command()

            /// Invoked when the user requests auto-completion using the tab character
            [<DefaultValue>]
            val mutable public AutoCompleteEvent : AutoCompleteHandler

            let mutable handlers : Handler array = Array.zeroCreate 0

            new(name) = LineEditor(name, 10)

            do 
                handlers <-
                    [|
                        new Handler (Command.Done, ConsoleKey.Enter,      new KeyHandler(x.CmdDone))
                        new Handler (Command.Home,ConsoleKey.Home,       new KeyHandler(x.CmdHome))
                        new Handler (Command.End,ConsoleKey.End,        new KeyHandler(x.CmdEnd))
                        new Handler (Command.Left,ConsoleKey.LeftArrow,  new KeyHandler(x.CmdLeft))
                        new Handler (Command.Right,ConsoleKey.RightArrow, new KeyHandler(x.CmdRight))
                        new Handler (Command.HistoryPrev,ConsoleKey.UpArrow,    new KeyHandler(x.CmdHistoryPrev))
                        new Handler (Command.HistoryNext,ConsoleKey.DownArrow,  new KeyHandler(x.CmdHistoryNext))
                        new Handler (Command.Backspace,ConsoleKey.Backspace,  new KeyHandler(x.CmdBackspace))
                        new Handler (Command.DeleteChar,ConsoleKey.Delete,     new KeyHandler(x.CmdDeleteChar))
                        new Handler (Command.TabOrComplete,ConsoleKey.Tab,        new KeyHandler(x.CmdTabOrComplete))
                
                        // Emacs keys
                        Handler.Control Command.Home 'A' (new KeyHandler(x.CmdHome))
                        Handler.Control Command.End 'E' (new KeyHandler(x.CmdEnd))
                        Handler.Control Command.Left 'B' (new KeyHandler(x.CmdLeft))
                        Handler.Control Command.Right 'F' (new KeyHandler(x.CmdRight))
                        Handler.Control Command.HistoryPrev 'P' (new KeyHandler(x.CmdHistoryPrev))
                        Handler.Control Command.HistoryNext 'N' (new KeyHandler(x.CmdHistoryNext))
                        Handler.Control Command.CmdKillToEOF 'K' (new KeyHandler(x.CmdKillToEOF))
                        Handler.Control Command.Yank 'Y' (new KeyHandler(x.CmdYank))
                        Handler.Control Command.DeleteChar 'D' (new KeyHandler(x.CmdDeleteChar))
                        Handler.Control Command.Refresh 'L' (new KeyHandler(x.CmdRefresh))
                        Handler.Control Command.ReverseSearch 'R' (new KeyHandler(x.CmdReverseSearch))
                        
                        Handler.Alt Command.BackwardWord 'B' ConsoleKey.B (new KeyHandler(x.CmdBackwardWord))
                        Handler.Alt Command.ForwardWord 'F' ConsoleKey.F (new KeyHandler(x.CmdForwardWord))
                
                        Handler.Alt Command.DeleteWord 'D' ConsoleKey.D (new KeyHandler(x.CmdDeleteWord))
                        Handler.Alt Command.DeleteBackword ((char)8) ConsoleKey.Backspace (new KeyHandler(x.CmdDeleteBackword))
                
                        // DEBUG
                        //Handler.Control ('T', CmdDebug),

                        // quote
                        Handler.Control Command.Quote 'Q' (new KeyHandler (fun () -> x.HandleChar ((Console.ReadKey (true)).KeyChar)))
                        
                    |]

            member private x.CmdDebug () =
                history.Dump ()
                Console.WriteLine ()
                x.Render ()

            member private x.Render () =
                Console.Write (shown_prompt);
                Console.Write (rendered_text);

                let max = System.Math.Max (rendered_text.Length + shown_prompt.Length, max_rendered);
            
                for i = rendered_text.Length + shown_prompt.Length to max_rendered - 1 do
                    Console.Write (' ');
                max_rendered <- shown_prompt.Length + rendered_text.Length

                // Write one more to ensure that we always wrap around properly if we are at the
                // end of a line.
                Console.Write (' ')

                x.UpdateHomeRow (max)

            member private x.UpdateHomeRow screenpos = 
                let lines = 1 + (screenpos / Console.WindowWidth);

                home_row <- Console.CursorTop - (lines - 1);
                if home_row < 0 then
                    home_row <- 0
        

            member private x.RenderFrom pos =
                let rpos = x.TextToRenderPos (pos)
                let mutable i = rpos;
            
                while i < rendered_text.Length do
                    Console.Write (rendered_text.[i])
                    i <- i + 1

                if (shown_prompt.Length + rendered_text.Length) > max_rendered then
                    max_rendered <- shown_prompt.Length + rendered_text.Length
                else
                    let max_extra = max_rendered - shown_prompt.Length
                    while i < max_extra do
                        Console.Write (' ')
                        i <- i + 1
                

            member private x.ComputeRendered () = 
                rendered_text.Length <- 0;

                for i = 0 to text.Length - 1 do
                    let c = (int) text.[i];
                    if c < 26 then
                        if c = (int)'\t' then
                            rendered_text.Append ("    ") |> ignore
                        else
                            rendered_text.Append ('^') |> ignore
                            rendered_text.Append ((char) (c + (int) 'A' - 1)) |> ignore
                        
                    else
                        rendered_text.Append ((char)c) |> ignore


            member private x.TextToRenderPos pos =
                let mutable p = 0;

                for i = 0 to pos - 1 do
                    let c = (int) text.[i];
                
                    if c < 26 then
                        if c = 9 then
                            p <- p + 4;
                        else
                            p <- p + 2;
                    else
                        p <- p + 1

                p
            
            member private x.TextToScreenPos pos = shown_prompt.Length + x.TextToRenderPos (pos)

            member private x.LineCount with get () = (shown_prompt.Length + rendered_text.Length)/Console.WindowWidth

            member private x.ForceCursor newpos = 
                cursor <- newpos;

                let actual_pos = shown_prompt.Length + x.TextToRenderPos (cursor)
                let row = home_row + (actual_pos/Console.WindowWidth)
                let row = if row < Console.BufferHeight then row else Console.BufferHeight-1
                let col = actual_pos % Console.WindowWidth

                Console.SetCursorPosition (col, row);

            member private x.UpdateCursor newpos =
                if cursor <> newpos then
                    x.ForceCursor newpos

            member private x.InsertChar (c:char) =
                let prev_lines = x.LineCount
                text.Insert (cursor, c) |> ignore
                x.ComputeRendered ()
                if prev_lines <> x.LineCount then
                    Console.SetCursorPosition (0, home_row)
                    x.Render ()
                    cursor <- cursor + 1
                    x.ForceCursor cursor
                else 
                    x.RenderFrom (cursor);
                    cursor <- cursor + 1
                    x.ForceCursor cursor
                    x.UpdateHomeRow (x.TextToScreenPos (cursor));

            member private x.CmdDone () =
                done_editing <- true
            
            member val TabAtStartCompletes : bool = false

            member private x.CmdTabOrComplete () =
                let mutable complete = false;

                if x.AutoCompleteEvent <> null then
                    if x.TabAtStartCompletes then
                        complete <- true
                    else 
                        let mutable i = 0
                        while i < cursor && not complete do
                            if not (Char.IsWhiteSpace (text.[i])) then
                                complete <- true

                    if complete then
                        let completion = x.AutoCompleteEvent.Invoke (text.ToString (), cursor)
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
                                x.ForceCursor (cursor)
                            
                    else
                        x.HandleChar ('\t')
                else
                    x.HandleChar ('t')
            
        
            member private x.CmdHome () = x.UpdateCursor (0)
            member private x.CmdEnd () = x.UpdateCursor (text.Length)
            member private x.CmdLeft () = if cursor <> 0 then x.UpdateCursor (cursor-1)

            member private x.CmdBackwardWord () =
                let p = x.WordBackward (cursor)
                if p <> -1 then x.UpdateCursor (p)

            member private x.CmdForwardWord () =
                let p = x.WordForward (cursor)
                if p <> -1 then x.UpdateCursor (p);

            member private x.CmdRight () =
                if (cursor <> text.Length) then x.UpdateCursor (cursor+1);


            member private x.RenderAfter p =
                x.ForceCursor (p)
                x.RenderFrom (p)
                x.ForceCursor (cursor)
        
            member private x.CmdBackspace () =
                if (cursor <> 0) then
                    cursor <- cursor - 1
                    text.Remove (cursor, 1) |> ignore
                    x.ComputeRendered ()
                    x.RenderAfter (cursor)

            member private x.CmdDeleteChar () =
                // If there is no input, this behaves like EOF
                if text.Length = 0 then
                    done_editing <- true
                    text <- null
                    Console.WriteLine ()
                else if (cursor <> text.Length) then
                    text.Remove (cursor, 1) |> ignore
                    x.ComputeRendered ()
                    x.RenderAfter (cursor)

            member private x.WordForward p =
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
                let pos = x.WordForward (cursor)

                if pos <> -1 then
                    let k = text.ToString (cursor, pos-cursor)
            
                    if last_command = Command.DeleteWord then
                        kill_buffer <- kill_buffer + k;
                    else
                        kill_buffer <- k;
            
                    text.Remove (cursor, pos-cursor) |> ignore
                    x.ComputeRendered ()
                    x.RenderAfter (cursor)
        
            member private x.CmdDeleteBackword () =
                let pos = x.WordBackward (cursor);
                if pos <> -1 then
                    let k = text.ToString (pos, cursor-pos)
            
                    if last_command = Command.DeleteBackword then
                        kill_buffer <- k + kill_buffer;
                    else
                        kill_buffer <- k;
            
                    text.Remove (pos, cursor-pos) |> ignore
                    x.ComputeRendered ()
                    x.RenderAfter (pos)
            

        
            //
            // Adds the current line to the history if needed
            //
            member private x.HistoryUpdateLine () =
                history.Update (text.ToString ())
        
            member private x.CmdHistoryPrev () =
                if history.PreviousAvailable () then
                    x.HistoryUpdateLine ()
                    x.SetText (history.Previous ())

            member private x.CmdHistoryNext () =
                if history.NextAvailable() then
                    history.Update (text.ToString ())
                    x.SetText (history.Next ());


            member private x.CmdKillToEOF () =
                kill_buffer <- text.ToString (cursor, text.Length-cursor);
                text.Length <- cursor;
                x.ComputeRendered ();
                x.RenderAfter (cursor);

            member private x.CmdYank () =
                x.InsertTextAtCursor (kill_buffer)


            member private x.InsertTextAtCursor str =
                let prev_lines = x.LineCount;
                text.Insert (cursor, str) |> ignore
                x.ComputeRendered ()
                if prev_lines <> x.LineCount then
                    Console.SetCursorPosition (0, home_row)
                    x.Render ()
                    cursor <- cursor + str.Length
                    x.ForceCursor (cursor)
                else
                    x.RenderFrom (cursor)
                    cursor <- cursor + str.Length
                    x.ForceCursor (cursor)
                    x.UpdateHomeRow (x.TextToScreenPos (cursor))


        
            member private x.SetSearchPrompt s =
                x.SetPrompt ("(reverse-i-search)`" + s + "': ")

            member private x.ReverseSearch () =
                let mutable search_backward = true

                if cursor = text.Length then
                    // The cursor is at the end of the string
                
                    let p = (text.ToString()).LastIndexOf (search)
                    if p <> -1 then
                        match_at <- p
                        cursor <- p
                        x.ForceCursor (cursor)
                        search_backward <- false
                else
                    // The cursor is somewhere in the middle of the string
                    let start = if cursor = match_at then cursor - 1 else cursor
                    if start <> -1 then
                        let p = (text.ToString()).LastIndexOf (search, start)
                        if p <> -1 then
                            match_at <- p
                            cursor <- p
                            x.ForceCursor (cursor)
                            search_backward <- false

                if search_backward then
                    // Need to search backwards in history
                    x.HistoryUpdateLine ()
                    let s = history.SearchBackward (search)
                    if s <> null then
                        match_at <- -1;
                        x.SetText (s);
                        x.ReverseSearch ();


        
            member private x.CmdReverseSearch () =
                if searching = 0 then
                    match_at <- -1
                    last_search <- search
                    searching <- -1
                    search <- ""
                    x.SetSearchPrompt ("")
                else
                    if search = "" then
                        if last_search <> "" && last_search <> null then
                            search <- last_search
                            x.SetSearchPrompt (search)

                            x.ReverseSearch ()
                    else
                        x.ReverseSearch ()


            member private x.SearchAppend (c:char) =
                search <- search + (string)c
                x.SetSearchPrompt (search)

                //
                // If the new typed data still matches the current text, stay here
                //
                let mutable still_matches = false
                if cursor < text.Length then
                    let r = text.ToString (cursor, text.Length - cursor)
                    if r.StartsWith (search) then still_matches <- true
                
                if not still_matches then
                    x.ReverseSearch ();
       
            member private x.CmdRefresh () =
                Console.Clear ();
                max_rendered <- 0;
                x.Render ();
                x.ForceCursor (cursor);

            member private x.InterruptEdit (sender:obj) (a:ConsoleCancelEventArgs) =
                // Do not abort our program:
                a.Cancel <- true;

                // Interrupt the editor
                edit_thread.Abort();

            member private x.HandleChar c =
                if searching <> 0 then
                    x.SearchAppend (c)
                else
                    x.InsertChar (c)


            member private x.EditLoop () =
                let mutable cki : ConsoleKeyInfo = new ConsoleKeyInfo()

                while not done_editing do
                    let mutable modifier : ConsoleModifiers = new ConsoleModifiers()
                
                    cki <- Console.ReadKey (true)
                    if cki.Key = ConsoleKey.Escape then
                        cki <- Console.ReadKey (true);

                        modifier <- ConsoleModifiers.Alt
                    else
                        modifier <- cki.Modifiers
                
                    let mutable handled = false;
                    
                    let mutable handler_index = 0
                    while handler_index < handlers.Length && not handled do
                        let handler = handlers.[handler_index]
                        let t = handler.CKI;

                        if t.Key = cki.Key && t.Modifiers = modifier then
                            handled <- true
                            handler.KeyHandler.Invoke ()
                            last_command <- handler.HandledCommand
                        else if t.KeyChar = cki.KeyChar && t.Key = ConsoleKey.Zoom then
                            handled <- true
                            handler.KeyHandler.Invoke()
                            last_command <- handler.HandledCommand

                        handler_index <- handler_index + 1

                    if handled then
                        if searching <> 0 then
                            if last_command <> Command.ReverseSearch then
                                searching <- 0
                                x.SetPrompt (specified_prompt)
                    
                    else if (cki.KeyChar <> (char) 0) then
                        x.HandleChar (cki.KeyChar)
                 
            member private x.InitText (initial:string) =
                text <- new StringBuilder (initial)
                x.ComputeRendered ()
                cursor <- text.Length
                x.Render ()
                x.ForceCursor (cursor)

            member private x.SetText newtext =
                Console.SetCursorPosition (0, home_row)
                x.InitText (newtext)

            member private x.SetPrompt newprompt =
                shown_prompt <- newprompt
                Console.SetCursorPosition (0, home_row)
                x.Render ()
                x.ForceCursor (cursor)

       
            member public x.Edit prompt initial =
                edit_thread <- Thread.CurrentThread;
                searching <- 0;
                let cancelHandler = new ConsoleCancelEventHandler(x.InterruptEdit)
                Console.CancelKeyPress.AddHandler cancelHandler
            
                done_editing <- false
                history.CursorToEnd ()
                max_rendered <- 0
            
                specified_prompt <- prompt
                shown_prompt <- prompt;
                x.InitText (initial);
                history.Append (initial);

                while not done_editing do
                    try
                        x.EditLoop ()
                    with
                    | :? ThreadAbortException ->
                        searching <- 0;
                        Thread.ResetAbort ()
                        Console.WriteLine ()
                        x.SetPrompt (prompt)
                        x.SetText ("")
                
                Console.WriteLine ();
            
                Console.CancelKeyPress.RemoveHandler cancelHandler

                if text = null then
                    history.Close ()
                    null
                else
                    let result = text.ToString ()
                    if result <> "" then
                        history.Accept (result)
                    else
                        history.RemoveLast ()

                    result
        
            member public x. SaveHistory () =
                history.Close ()
                