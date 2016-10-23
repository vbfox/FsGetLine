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
namespace BlackFox

    module FsGetLine =
        open System
        open System.Text
        open System.IO
        open System.Threading

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
                    next <> history.Head

            let nextAvailable history =
                if history.Count = 0 then
                    false
                else
                    let next = (history.Cursor + 1) % history.Length
                    next <> history.Head

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
                    if (not (isNull(history.Lines.[slot])) && history.Lines.[slot].IndexOf (term) <> -1) then
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
                    let dir = Environment.GetFolderPath (Environment.SpecialFolder.ApplicationData)
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

        /// Handle the display of ANSI C0 control characters by prefixing them with "^"
        module private AnsiControlCodes =
            let public (|CanEscape|NoEscape|) (c : char) = if (int)c < 31 then CanEscape else NoEscape
            let public toDisplayableChar c = (char) ((int)c + (int) 'A' - 1)

            let public fold (onNormal:'st -> char -> 'st) (onEscape:'st -> char -> 'st) (st:'st) (s:string) =
                let foldFunc st c =
                    match c with
                    | CanEscape -> onEscape st c
                    | _ -> onNormal st c

                s |> Seq.fold foldFunc st

            let public escapeChar c = 
                match c with
                | CanEscape -> "^" + (string)(toDisplayableChar c)
                | c -> (string)c

        type Completion = { Result : string list; Prefix : string }
        type AutoCompleteHandler = string -> int -> Completion

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

        type GetLineSettings =
            {
                AppName : string option

                HistorySize : int

                /// Invoked when the user requests auto-completion using the tab character
                AutoCompleteEvent : AutoCompleteHandler option

                TabAtStartCompletes : bool
            }

        let private defaultSettings = 
            {
                AppName = None
                HistorySize = 10
                AutoCompleteEvent = None
                TabAtStartCompletes = true
            }

        type GetLine =
            {
                /// Our object that tracks history
                History : History.History

                /// The contents of the kill buffer (cut/paste in Emacs parlance)
                KillBuffer : string

                Settings : GetLineSettings
            }

        let create (getSettings : GetLineSettings -> GetLineSettings) =
            let settings = getSettings defaultSettings
            {
                History = History.empty settings.AppName settings.HistorySize
                KillBuffer = ""
                Settings = settings
            }

        type private SearchState =
            {
                /// Current search direction
                Direction: SearchDirection

                /// The position where we found the match
                MatchAt: int

                /// The string being searched for
                Term: string
            }

        open ColoredString

        type private LineEditorState =
            {
                /// The text being edited.
                Text : string

                /// The text as it is rendered (replaces (char)1 with ^A on display for example).
                RenderedText : ColoredString

                /// The prompt specified
                SpecifiedPrompt : ColoredString

                /// The prompt shown to the user.
                ShownPrompt : ColoredString

                /// The current cursor position, indexes into "text", for an index
                /// into st.RenderedText, use TextToRenderPos
                Cursor : int

                /// The row where we started displaying data.
                HomeRow : int

                /// The maximum length that has been displayed on the screen
                MaxRendered : int

                /// If we are done editing, this breaks the interactive loop
                DoneEditing : bool

                /// Signal to the caller that the user cancelled either via Ctrl+C or by using Ctrl+D on an empty buffer
                SignalExit : bool

                /// The thread where the Editing started taking place
                EditThread : Thread

                /// Our object that tracks history
                History : History.History

                /// The contents of the kill buffer (cut/paste in Emacs parlance)
                KillBuffer : string

                SearchState : SearchState option
                PreviousSearch : string option
        
                /// Used to implement the Kill semantics (multiple Alt-Ds accumulate)
                LastCommand : Command option

                Settings : GetLineSettings
            }

        let private makeDefaultLineEditorState (globalState : GetLine) =
            {
                Text = ""
                RenderedText = ColoredString ""
                SpecifiedPrompt = ColoredString ""
                ShownPrompt = ColoredString ""
                Cursor = 0
                HomeRow = 0
                MaxRendered = 0
                DoneEditing = false
                SignalExit = false
                EditThread = null
                History = globalState.History
                KillBuffer = globalState.KillBuffer
                SearchState = None
                PreviousSearch = None
                LastCommand = None
                Settings = globalState.Settings
            }

        type private KeyHandler = LineEditorState -> LineEditorState

        type private Handler(cmd : Command, keyInfo : ConsoleKeyInfo, h : KeyHandler) =
            member val HandledCommand = cmd
            member val KeyInfo = keyInfo
            member val KeyHandler = h

            new(cmd : Command, key, h : KeyHandler) = Handler(cmd, new ConsoleKeyInfo((char) 0, key, false, false, false), h)
            new(cmd : Command, c, h : KeyHandler) = Handler(cmd, new ConsoleKeyInfo (c, ConsoleKey.Zoom, false, false, false), h)
            
            static member Alt cmd c k h = Handler (cmd, new ConsoleKeyInfo (c, k, false, true, false), h)
            static member Control cmd (c : char) h = Handler (cmd, (char) ((int)c - (int)'A' + 1), h)

        let private cmdDone st = { st with DoneEditing = true}

        let private (|IsTab|IsNotTab|) c = if c = '\t' then IsTab else IsNotTab

        let private textToRenderPos pos (text:string) =
            text.Substring(0, pos)
                |> AnsiControlCodes.fold
                    (fun st c -> st + (if c = '\t' then 4 else 1))
                    (fun st c -> st + 2)
                    0
            
        let private textToScreenPos pos st = st.ShownPrompt.Length + (textToRenderPos pos st.Text)

        let private lineCount st = (st.ShownPrompt.Length + st.RenderedText.Length) / Console.WindowWidth

        let private forceCursor newpos st = 
            let actualPos = st.ShownPrompt.Length + (textToRenderPos newpos st.Text)
            let row = st.HomeRow + (actualPos/Console.WindowWidth)
            let row = if row < Console.BufferHeight then row else Console.BufferHeight-1
            let col = actualPos % Console.WindowWidth

            Console.SetCursorPosition (col, row);

            { st with Cursor = newpos }

        let private updateCursor newpos st =
            if st.Cursor <> newpos then
                st |> forceCursor newpos
            else
                st

        let private renderText (text:string) =
            let builder = text |> AnsiControlCodes.fold
                            (fun (b:StringBuilder) c ->
                                if c = '\t' then
                                    b.Append("    ")
                                else
                                    b.Append(ColoredString.EscapeToString((string)c)))
                            (fun b c -> b.Append(sprintf "^[DarkGreen]^^^[Green]%c^[Reset]" (AnsiControlCodes.toDisplayableChar c)))
                            (new StringBuilder())

            ColoredString(builder.ToString())

        let private updateHomeRow screenpos st = 
            let lines = 1 + (screenpos / Console.WindowWidth);

            { st with HomeRow = System.Math.Max (0, Console.CursorTop - (lines - 1)) }

        let private render st =
            st.ShownPrompt.WriteToConsole ()
            st.RenderedText.WriteToConsole()

            let max = System.Math.Max (st.RenderedText.Length + st.ShownPrompt.Length, st.MaxRendered);
            
            for i = st.RenderedText.Length + st.ShownPrompt.Length to st.MaxRendered - 1 do
                Console.Write (' ');

            // Write one more to ensure that we always wrap around properly if we are at the
            // end of a line.
            Console.Write ' '

            { st with MaxRendered = st.ShownPrompt.Length + st.RenderedText.Length } |> updateHomeRow max

        let private cmdDebug st =
            st.History |> History.dump
            Console.WriteLine ()
            st |> render

        let private renderFrom pos st =
            let rpos = textToRenderPos pos st.Text

            st.RenderedText.WriteToConsoleFrom rpos
            let mutable i = st.RenderedText.Length

            if (st.ShownPrompt.Length + st.RenderedText.Length) > st.MaxRendered then
                { st with MaxRendered = st.ShownPrompt.Length + st.RenderedText.Length }
            else
                let maxExtra = st.MaxRendered - st.ShownPrompt.Length
                while i < maxExtra do
                    Console.Write (' ')
                    i <- i + 1
                st

        let private insertChar (c:char) st =
            let prevLines = lineCount st
            let newText = st.Text.Insert (st.Cursor, (string)c)
            
            let st = { st with Text = newText; RenderedText = renderText newText }
                
            if prevLines <> (lineCount st) then
                Console.SetCursorPosition (0, st.HomeRow)
                let newCursor = st.Cursor + 1
                st
                    |> render
                    |> forceCursor newCursor
            else 
                let newCursor = st.Cursor + 1
                st
                    |> renderFrom st.Cursor
                    |> forceCursor newCursor
                    |> updateHomeRow (textToScreenPos newCursor st)

        let private insertTextAtCursor str st =
            let prevLines = st |> lineCount;
            let newText = st.Text.Insert (st.Cursor, str)
            let st = { st with Text = newText; RenderedText = renderText newText}
            if prevLines <> (lineCount st) then
                Console.SetCursorPosition (0, st.HomeRow)
                st |> render |> forceCursor (st.Cursor + str.Length)
            else
                let st = st |> renderFrom st.Cursor |> forceCursor (st.Cursor + str.Length)
                st |> updateHomeRow (textToScreenPos st.Cursor st)

        let private initText (initial:string option) st =
            let newText = match initial with | Some(null) | None -> "" | Some(initial) -> initial
            { st with Cursor = newText.Length; Text = newText; RenderedText = renderText newText }
                |> render 
                |> forceCursor newText.Length

        let private setText newtext st =
            Console.SetCursorPosition (0, st.HomeRow)
            st |> initText (newtext)

        let private setPromptCore newprompt st =
            Console.SetCursorPosition (0, st.HomeRow)
            { st with ShownPrompt = newprompt }
                |> render
                |> forceCursor st.Cursor

        let private setPrompt newprompt st =
            st |> setPromptCore newprompt

        let private setSearchPrompt s st =
            let promptRaw = sprintf "^[DarkGray](^[Cyan]reverse-i-search^[DarkGray])`^[Reset]%s^[DarkGray]': " s
            st |> setPromptCore (ColoredString promptRaw)

        //
        // Adds the current line to the history if needed
        //
        let private historyUpdateLine st =
            let newHistory = st.History |> History.update st.Text
            { st with History = newHistory }

        let rec private reverseSearch st =
            match st.SearchState with
            | Some(search) ->
                let mutable p = -1
                if st.Cursor = st.Text.Length then
                    // The cursor is at the end of the string
                    p <- st.Text.LastIndexOf (search.Term)
                else
                    // The cursor is somewhere in the middle of the string
                    let start = if st.Cursor = search.MatchAt then st.Cursor - 1 else st.Cursor
                    if start <> -1 then p <- st.Text.LastIndexOf (search.Term, start)

                if p <> -1 then
                    { st with SearchState = Some({ search with MatchAt = p })} |> forceCursor p
                else
                    // Need to search backwards in history
                    let st = st |> historyUpdateLine
                    let (newHistory, searchResult) = History.searchBackward search.Term st.History
                    let st = { st with History = newHistory }
                    match searchResult with
                    | Some(_) ->
                        { st with SearchState = Some({ search with MatchAt = -1 }) }
                            |> setText searchResult
                            |> reverseSearch 
                    | None -> st
            | None ->
                failwith "No search in progress"

        let private searchAppend (c:char) search st =
            let newTerm = search.Term + (string)c
            let st =
                { st with SearchState = Some { search with Term = newTerm } }
                |> setSearchPrompt newTerm

            //
            // If the new typed data still matches the current text, stay here
            //
            let mutable stillMatches = false
            if st.Cursor < st.Text.Length then
                let r = st.Text.Substring (st.Cursor, st.Text.Length - st.Cursor)
                if r.StartsWith newTerm then stillMatches <- true
                
            if not stillMatches then
                st |> reverseSearch
            else
                st

        let private handleChar c st =
            match st.SearchState with
            | Some(search) -> st |> searchAppend c search
            | None -> st |> insertChar (c)

        let private cmdTabOrComplete st =
            let mutable complete = false;

            if st.Settings.AutoCompleteEvent.IsSome then
                if st.Settings.TabAtStartCompletes then
                    complete <- true
                else 
                    let mutable i = 0
                    while i < st.Cursor && not complete do
                        if not (Char.IsWhiteSpace (st.Text.[i])) then
                            complete <- true

                if complete then
                    let completion = st.Settings.AutoCompleteEvent.Value st.Text st.Cursor
                    let completions = completion.Result
                    if completions.Length <> 0 then
                        let ncompletions = completions.Length
                    
                        if completions.Length = 1 then
                            st |> insertTextAtCursor (completions.[0])
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

                            let st = 
                                if last <> -1 then
                                    st |> insertTextAtCursor (completions.[0].Substring (0, last+1))
                                else
                                    st
                                
                            Console.WriteLine ()
                            for s in completions do
                                Console.Write (completion.Prefix)
                                Console.Write (s)
                                Console.Write (' ')
                                
                            Console.WriteLine ()
                            st |> render |> forceCursor st.Cursor
                    else
                        st
                else
                    st |> handleChar ('\t')
            else
                st |> handleChar ('t')

        let private cmdHome st = st |> updateCursor 0
        let private cmdEnd st = st |> updateCursor st.Text.Length
        let private cmdLeft st = if st.Cursor <> 0 then st |> updateCursor (st.Cursor-1) else st

        let private wordForward p st =
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
            

        let private wordBackward p st =
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

        let private cmdBackwardWord st =
            let p = wordBackward st.Cursor st
            if p <> -1 then st |> updateCursor p else st

        let private cmdForwardWord st =
            let p = wordForward st.Cursor st
            if p <> -1 then st |> updateCursor p else st

        let private cmdRight st =
            if (st.Cursor <> st.Text.Length) then st |> updateCursor (st.Cursor+1) else st

        let private renderAfter p st =
            let st = st |> forceCursor p |> renderFrom p
            st |> forceCursor st.Cursor
        
        let private cmdBackspace st =
            if st.Cursor <> 0 then
                let newCursor = st.Cursor - 1
                let newText = st.Text.Remove (newCursor, 1)
                let st = { st with Cursor = newCursor; Text = newText; RenderedText = renderText newText}
                st |> renderAfter newCursor
            else
                st

        let private cmdDeleteChar st =
            if st.Text.Length = 0 then
                Console.WriteLine ()
                // If there is no input, this behaves like EOF
                { st with DoneEditing = true; SignalExit = true }
            else if (st.Cursor <> st.Text.Length) then
                let newText = st.Text.Remove (st.Cursor, 1)
                let st = { st with Text = newText; RenderedText = renderText newText }
                st |> renderAfter st.Cursor
            else
                st
            
        let private cmdDeleteWord st =
            let pos = st |> wordForward st.Cursor

            if pos <> -1 then
                let k = st.Text.Substring (st.Cursor, pos-st.Cursor)
           
                let newKillBuffer = if st.LastCommand = Some(Command.DeleteWord) then st.KillBuffer + k else k
                let newText = st.Text.Remove (st.Cursor, pos-st.Cursor)

                { st with KillBuffer = newKillBuffer; Text = newText; RenderedText = renderText newText }
                    |> renderAfter st.Cursor
            else
                st
        
        let private cmdDeleteBackword st =
            let pos = st |> wordBackward st.Cursor
            if pos <> -1 then
                let k = st.Text.Substring (pos, st.Cursor-pos)
            
                let newKillBuffer = if st.LastCommand = Some(Command.DeleteBackword) then st.KillBuffer + k else k
                let newText = st.Text.Remove (pos, st.Cursor-pos)
                { st with KillBuffer = newKillBuffer; Text = newText; RenderedText = renderText newText }
                    |> renderAfter pos
            else
                st
      
        let private cmdHistoryPrev st =
            if History.previousAvailable st.History then
                let (newHistory, text) = st.History |> History.update st.Text |> History.previous
                { st with History = newHistory } |> setText text
            else
                st

        let private cmdHistoryNext st =
            if History.nextAvailable st.History then
                let (newHistory, text) = st.History |> History.update st.Text |> History.next 
                { st with History = newHistory } |> setText text
            else
                st

        let private cmdKillToEOF st =
            let newKillBuffer = st.Text.Substring (st.Cursor, st.Text.Length-st.Cursor)
            let newText = st.Text.Substring(0, st.Cursor)
            { st with KillBuffer = newKillBuffer; Text = newText; RenderedText = renderText newText }
                |> renderAfter st.Cursor

        let private cmdYank st =
            st |> insertTextAtCursor st.KillBuffer

        let private cmdReverseSearch st =
            match st.SearchState with
            | None ->
                { st with SearchState = Some { MatchAt = -1; Term = ""; Direction = SearchDirection.Backward } }
                    |> setSearchPrompt ("")
            | Some(search) ->
                if search.Term = "" then
                    match st.PreviousSearch with
                    | None | Some("") ->
                        st
                    | Some(previousTerm) -> 
                        { st with SearchState = Some { search with Term = previousTerm } }
                            |> setSearchPrompt previousTerm
                            |> reverseSearch 
                else
                    st |> reverseSearch

        let private cmdRefresh st =
            Console.Clear ()
            { st with MaxRendered = 0 }
                |> render
                |> forceCursor st.Cursor

        let private handlers =
            [|
                new Handler (Command.Done, ConsoleKey.Enter,      cmdDone)
                new Handler (Command.Home,ConsoleKey.Home,       cmdHome)
                new Handler (Command.End,ConsoleKey.End,        cmdEnd)
                new Handler (Command.Left,ConsoleKey.LeftArrow,  cmdLeft)
                new Handler (Command.Right,ConsoleKey.RightArrow, cmdRight)
                new Handler (Command.HistoryPrev,ConsoleKey.UpArrow,    cmdHistoryPrev)
                new Handler (Command.HistoryNext,ConsoleKey.DownArrow,  cmdHistoryNext)
                new Handler (Command.Backspace,ConsoleKey.Backspace,  cmdBackspace)
                new Handler (Command.DeleteChar,ConsoleKey.Delete,     cmdDeleteChar)
                new Handler (Command.TabOrComplete,ConsoleKey.Tab,        cmdTabOrComplete)
                
                // Emacs keys
                Handler.Control Command.Home 'A' (cmdHome)
                Handler.Control Command.End 'E' (cmdEnd)
                Handler.Control Command.Left 'B' (cmdLeft)
                Handler.Control Command.Right 'F' (cmdRight)
                Handler.Control Command.HistoryPrev 'P' (cmdHistoryPrev)
                Handler.Control Command.HistoryNext 'N' (cmdHistoryNext)
                Handler.Control Command.CmdKillToEOF 'K' (cmdKillToEOF)
                Handler.Control Command.Yank 'Y' (cmdYank)
                Handler.Control Command.DeleteChar 'D' (cmdDeleteChar)
                Handler.Control Command.Refresh 'L' (cmdRefresh)
                Handler.Control Command.ReverseSearch 'R' (cmdReverseSearch)
                        
                Handler.Alt Command.BackwardWord 'B' ConsoleKey.B (cmdBackwardWord)
                Handler.Alt Command.ForwardWord 'F' ConsoleKey.F (cmdForwardWord)
                
                Handler.Alt Command.DeleteWord 'D' ConsoleKey.D (cmdDeleteWord)
                Handler.Alt Command.DeleteBackword ((char)8) ConsoleKey.Backspace (cmdDeleteBackword)
                
                // DEBUG
                //Handler.Control ('T', CmdDebug),

                // quote
                Handler.Control Command.Quote 'Q' (fun st -> st |> handleChar ((Console.ReadKey (true)).KeyChar))
            |]

        let private interruptEdit (thread:Thread) (sender:obj) (a:ConsoleCancelEventArgs) =
            // Do not abort our program:
            a.Cancel <- true;

            // Interrupt the editor
            thread.Abort ()
            
        let private readKeyWithEscMeaningAlt () =
            let key = Console.ReadKey (true)
            if key.Key = ConsoleKey.Escape then
                (Console.ReadKey (true), ConsoleModifiers.Alt)
            else
                (key, key.Modifiers)

        let private tryFindHandler (input:ConsoleKeyInfo) modifier =
            handlers |> Array.tryFind (fun handler ->
                    let handlerKeyInfo = handler.KeyInfo;

                    (handlerKeyInfo.Key = input.Key && handlerKeyInfo.Modifiers = modifier)
                        || (handlerKeyInfo.KeyChar = input.KeyChar && handlerKeyInfo.Key = ConsoleKey.Zoom)
                )

        let private readOneInput st =
            let (newInput, modifier) = readKeyWithEscMeaningAlt ()
               
            let inputHander = tryFindHandler newInput modifier

            match inputHander with
            | Some(handler) -> 
                let st = handler.KeyHandler st
                let st = { st with LastCommand = Some(handler.HandledCommand) }
                match (st.SearchState, handler.HandledCommand) with
                | ( _, Command.ReverseSearch) -> st
                | (Some(search), _) -> { st with PreviousSearch = Some(search.Term); SearchState = None} |> setPrompt st.SpecifiedPrompt
                | _ -> st
            | None -> st |> handleChar (newInput.KeyChar)

        let rec private readInputUntilDoneEditing = function
            | st when st.DoneEditing -> st
            | st -> st |> readOneInput |> readInputUntilDoneEditing
       
        let private updateEditor editor st =
            { editor with GetLine.History = st.History; KillBuffer = st.KillBuffer }

        let get prompt initial editor =
            let mutable st = makeDefaultLineEditorState editor
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

            let cancelHandler = new ConsoleCancelEventHandler(interruptEdit Thread.CurrentThread)
            Console.CancelKeyPress.AddHandler cancelHandler

            st <- st |> initText (Some(initial))

            while not st.DoneEditing do
                try
                    st <- readInputUntilDoneEditing st
                with
                | :? ThreadAbortException ->
                    Thread.ResetAbort ()
                    match st.SearchState with
                    | Some(_) ->
                        st <- { st with SearchState = None }
                        Console.WriteLine ()
                        st <- st |> setPrompt (prompt)
                        st <- st |> setText (Some(""))
                    | None ->
                        st <- { st with DoneEditing = true; SignalExit = true}
                
            Console.WriteLine ();
            
            Console.CancelKeyPress.RemoveHandler cancelHandler

            if st.SignalExit then
                st.History |> History.save
                (st |> updateEditor editor, None)
            else
                if st.Text <> "" then
                    st <- { st with History = History.accept st.Text st.History }
                else
                    st <- { st with History = History.removeLast st.History }

                (st |> updateEditor editor, Some(st.Text))