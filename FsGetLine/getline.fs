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

        type PromptDisplay =
            {
                Show : string -> unit
                ExtraChars : int
            }

        type GetLineSettings =
            {
                AppName : string option

                HistorySize : int

                /// Invoked when the user requests auto-completion using the tab character
                AutoCompleteEvent : AutoCompleteHandler option

                TabAtStartCompletes : bool

                PromptDisplay : PromptDisplay

                SearchPromptDisplay : PromptDisplay
            }

        let private defaultPromptDisplay (s:string) =
            let previousColor = Console.ForegroundColor
            Console.ForegroundColor <- ConsoleColor.Cyan
            Console.Write(s)
            Console.ForegroundColor <- ConsoleColor.DarkGray
            Console.Write(" > " )
            Console.ForegroundColor <- previousColor

        let private defaultSearchPromptDisplay (s:string) =
            let previousColor = Console.ForegroundColor
            Console.ForegroundColor <- ConsoleColor.DarkGray
            Console.Write("(")
            Console.ForegroundColor <- ConsoleColor.Cyan
            Console.Write("reverse-i-search")
            Console.ForegroundColor <- ConsoleColor.DarkGray
            Console.Write(")`")
            Console.ForegroundColor <- previousColor
            Console.Write(s)
            Console.ForegroundColor <- ConsoleColor.DarkGray
            Console.Write("': ")
            Console.ForegroundColor <- previousColor

        let private defaultSettings = 
            {
                AppName = None
                HistorySize = 10
                AutoCompleteEvent = None
                TabAtStartCompletes = true
                PromptDisplay = { Show = defaultPromptDisplay; ExtraChars = " > ".Length }
                SearchPromptDisplay = { Show = defaultSearchPromptDisplay; ExtraChars = "(reverse-i-search)`': ".Length }
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

        type private LineEditorState =
            {
                /// The text being edited.
                Text : string

                /// The text as it is rendered (replaces (char)1 with ^A on display for example).
                RenderedText : string

                /// The prompt specified
                SpecifiedPrompt : string

                /// The prompt shown to the user.
                ShownPrompt : string
                ShownPromptDisplay : PromptDisplay

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
                RenderedText = ""
                SpecifiedPrompt = ""
                ShownPrompt = ""
                ShownPromptDisplay = globalState.Settings.PromptDisplay
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

        let private CmdDone st = { st with DoneEditing = true}

        let private TextToRenderPos pos (text:string) =
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
            
        let private promptLen st = st.ShownPrompt.Length + st.ShownPromptDisplay.ExtraChars

        let private TextToScreenPos pos st = (promptLen st) + (TextToRenderPos pos st.Text)

        let private LineCount st = ((promptLen st) + st.RenderedText.Length)/Console.WindowWidth

        let private ForceCursor newpos st = 
            let actual_pos = (promptLen st) + (TextToRenderPos newpos st.Text)
            let row = st.HomeRow + (actual_pos/Console.WindowWidth)
            let row = if row < Console.BufferHeight then row else Console.BufferHeight-1
            let col = actual_pos % Console.WindowWidth

            Console.SetCursorPosition (col, row);

            { st with Cursor = newpos }

        let private UpdateCursor newpos st =
            if st.Cursor <> newpos then
                st |> ForceCursor newpos
            else
                st

        let private render (text:string) =                 
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

        let private UpdateHomeRow screenpos st = 
            let lines = 1 + (screenpos / Console.WindowWidth);

            { st with HomeRow = System.Math.Max (0, Console.CursorTop - (lines - 1)) }



        let private Render st =
            st.ShownPromptDisplay.Show st.ShownPrompt
            let promptLen = st |> promptLen
            Console.Write st.RenderedText

            let max = System.Math.Max (st.RenderedText.Length + promptLen, st.MaxRendered);
            
            for i = st.RenderedText.Length + promptLen to st.MaxRendered - 1 do
                Console.Write (' ');
            let st = { st with MaxRendered = promptLen + st.RenderedText.Length }

            // Write one more to ensure that we always wrap around properly if we are at the
            // end of a line.
            Console.Write ' '

            st |> UpdateHomeRow max

        let private CmdDebug st =
            st.History |> History.dump
            Console.WriteLine ()
            st |> Render

        let private RenderFrom pos st =
            let rpos = TextToRenderPos pos st.Text
            let mutable i = rpos;
            
            while i < st.RenderedText.Length do
                Console.Write (st.RenderedText.[i])
                i <- i + 1

            let promptLen = st |> promptLen

            if (promptLen + st.RenderedText.Length) > st.MaxRendered then
                { st with MaxRendered = promptLen + st.RenderedText.Length }
            else
                let max_extra = st.MaxRendered - promptLen
                while i < max_extra do
                    Console.Write (' ')
                    i <- i + 1
                st

        let private InsertChar (c:char) st =
            let prev_lines = LineCount st
            let newText = st.Text.Insert (st.Cursor, (string)c)
            
            let st = { st with Text = newText; RenderedText = render newText }
                
            if prev_lines <> (LineCount st) then
                Console.SetCursorPosition (0, st.HomeRow)
                let newCursor = st.Cursor + 1
                st
                    |> Render
                    |> ForceCursor newCursor
            else 
                let newCursor = st.Cursor + 1
                st
                    |> RenderFrom st.Cursor
                    |> ForceCursor newCursor
                    |> UpdateHomeRow (TextToScreenPos newCursor st)

        let private InsertTextAtCursor str st =
            let prev_lines = st |> LineCount;
            let newText = st.Text.Insert (st.Cursor, str)
            let st = { st with Text = newText; RenderedText = render newText}
            if prev_lines <> (LineCount st) then
                Console.SetCursorPosition (0, st.HomeRow)
                st |> Render |> ForceCursor (st.Cursor + str.Length)
            else
                let st = st |> RenderFrom st.Cursor |> ForceCursor (st.Cursor + str.Length)
                st |> UpdateHomeRow (TextToScreenPos st.Cursor st)

        let private InitText (initial:string option) st =
            let newText = match initial with | Some(null) | None -> "" | Some(initial) -> initial
            { st with Cursor = newText.Length; Text = newText; RenderedText = render newText }
                |> Render 
                |> ForceCursor newText.Length

        let private SetText newtext st =
            Console.SetCursorPosition (0, st.HomeRow)
            st |> InitText (newtext)

        let private SetPromptCore newprompt display st =
            Console.SetCursorPosition (0, st.HomeRow)
            { st with ShownPrompt = newprompt; ShownPromptDisplay = display }
                |> Render
                |> ForceCursor st.Cursor

        let private SetPrompt newprompt st =
            st |> SetPromptCore newprompt st.Settings.PromptDisplay

        let private SetSearchPrompt s st =
           st |> SetPromptCore s st.Settings.SearchPromptDisplay

        //
        // Adds the current line to the history if needed
        //
        let private HistoryUpdateLine st =
            let newHistory = st.History |> History.update st.Text
            { st with History = newHistory }

        let rec private ReverseSearch st =
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
                    { st with SearchState = Some({ search with MatchAt = p })} |> ForceCursor p
                else
                    // Need to search backwards in history
                    let st = st |> HistoryUpdateLine
                    let (newHistory, searchResult) = History.searchBackward search.Term st.History
                    let st = { st with History = newHistory }
                    match searchResult with
                    | Some(_) ->
                        { st with SearchState = Some({ search with MatchAt = -1 }) }
                            |> SetText searchResult
                            |> ReverseSearch 
                    | None -> st
            | None ->
                failwith "No search in progress"

        let private SearchAppend (c:char) search st =
            let newTerm = search.Term + (string)c
            let st =
                { st with SearchState = Some { search with Term = newTerm } }
                |> SetSearchPrompt newTerm

            //
            // If the new typed data still matches the current text, stay here
            //
            let mutable still_matches = false
            if st.Cursor < st.Text.Length then
                let r = st.Text.Substring (st.Cursor, st.Text.Length - st.Cursor)
                if r.StartsWith newTerm then still_matches <- true
                
            if not still_matches then
                st |> ReverseSearch
            else
                st

        let private HandleChar c st =
            match st.SearchState with
            | Some(search) -> st |> SearchAppend c search
            | None -> st |> InsertChar (c)

        let private CmdTabOrComplete st =
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
                            st |> InsertTextAtCursor (completions.[0])
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
                                    st |> InsertTextAtCursor (completions.[0].Substring (0, last+1))
                                else
                                    st
                                
                            Console.WriteLine ()
                            for s in completions do
                                Console.Write (completion.Prefix)
                                Console.Write (s)
                                Console.Write (' ')
                                
                            Console.WriteLine ()
                            st |> Render |> ForceCursor st.Cursor
                    else
                        st
                else
                    st |> HandleChar ('\t')
            else
                st |> HandleChar ('t')

        let private CmdHome st = st |> UpdateCursor 0
        let private CmdEnd st = st |> UpdateCursor st.Text.Length
        let private CmdLeft st = if st.Cursor <> 0 then st |> UpdateCursor (st.Cursor-1) else st

        let private WordForward p st =
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
            

        let private WordBackward p st =
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

        let private CmdBackwardWord st =
            let p = WordBackward st.Cursor st
            if p <> -1 then st |> UpdateCursor p else st

        let private CmdForwardWord st =
            let p = WordForward st.Cursor st
            if p <> -1 then st |> UpdateCursor p else st

        let private CmdRight st =
            if (st.Cursor <> st.Text.Length) then st |> UpdateCursor (st.Cursor+1) else st

        let private RenderAfter p st =
            let st = st |> ForceCursor p |> RenderFrom p
            st |> ForceCursor st.Cursor
        
        let private CmdBackspace st =
            if st.Cursor <> 0 then
                let newCursor = st.Cursor - 1
                let newText = st.Text.Remove (newCursor, 1)
                let st = { st with Cursor = newCursor; Text = newText; RenderedText = render newText}
                st |> RenderAfter newCursor
            else
                st

        let private CmdDeleteChar st =
            if st.Text.Length = 0 then
                Console.WriteLine ()
                // If there is no input, this behaves like EOF
                { st with DoneEditing = true; SignalExit = true }
            else if (st.Cursor <> st.Text.Length) then
                let newText = st.Text.Remove (st.Cursor, 1)
                let st = { st with Text = newText; RenderedText = render newText }
                st |> RenderAfter st.Cursor
            else
                st
            
        let private CmdDeleteWord st =
            let pos = st |> WordForward st.Cursor

            if pos <> -1 then
                let k = st.Text.Substring (st.Cursor, pos-st.Cursor)
           
                let newKillBuffer = if st.LastCommand = Some(Command.DeleteWord) then st.KillBuffer + k else k
                let newText = st.Text.Remove (st.Cursor, pos-st.Cursor)

                { st with KillBuffer = newKillBuffer; Text = newText; RenderedText = render newText }
                    |> RenderAfter st.Cursor
            else
                st
        
        let private CmdDeleteBackword st =
            let pos = st |> WordBackward st.Cursor
            if pos <> -1 then
                let k = st.Text.Substring (pos, st.Cursor-pos)
            
                let newKillBuffer = if st.LastCommand = Some(Command.DeleteBackword) then st.KillBuffer + k else k
                let newText = st.Text.Remove (pos, st.Cursor-pos)
                { st with KillBuffer = newKillBuffer; Text = newText; RenderedText = render newText }
                    |> RenderAfter pos
            else
                st
      
        let private CmdHistoryPrev st =
            if History.previousAvailable st.History then
                let (newHistory, text) = st.History |> History.update st.Text |> History.previous
                { st with History = newHistory } |> SetText text
            else
                st

        let private CmdHistoryNext st =
            if History.nextAvailable st.History then
                let (newHistory, text) = st.History |> History.update st.Text |> History.next 
                { st with History = newHistory } |> SetText text
            else
                st

        let private CmdKillToEOF st =
            let newKillBuffer = st.Text.Substring (st.Cursor, st.Text.Length-st.Cursor)
            let newText = st.Text.Substring(0, st.Cursor)
            { st with KillBuffer = newKillBuffer; Text = newText; RenderedText = render newText }
                |> RenderAfter st.Cursor

        let private CmdYank st =
            st |> InsertTextAtCursor st.KillBuffer

        let private CmdReverseSearch st =
            match st.SearchState with
            | None ->
                { st with SearchState = Some { MatchAt = -1; Term = ""; Direction = SearchDirection.Backward } }
                    |> SetSearchPrompt ("")
            | Some(search) ->
                if search.Term = "" then
                    match st.PreviousSearch with
                    | None | Some("") ->
                        st
                    | Some(previousTerm) -> 
                        { st with SearchState = Some { search with Term = previousTerm } }
                            |> SetSearchPrompt previousTerm
                            |> ReverseSearch 
                else
                    st |> ReverseSearch

        let private CmdRefresh st =
            Console.Clear ()
            { st with MaxRendered = 0 }
                |> Render
                |> ForceCursor st.Cursor

        let private handlers =
            [|
                new Handler (Command.Done, ConsoleKey.Enter,      CmdDone)
                new Handler (Command.Home,ConsoleKey.Home,       CmdHome)
                new Handler (Command.End,ConsoleKey.End,        CmdEnd)
                new Handler (Command.Left,ConsoleKey.LeftArrow,  CmdLeft)
                new Handler (Command.Right,ConsoleKey.RightArrow, CmdRight)
                new Handler (Command.HistoryPrev,ConsoleKey.UpArrow,    CmdHistoryPrev)
                new Handler (Command.HistoryNext,ConsoleKey.DownArrow,  CmdHistoryNext)
                new Handler (Command.Backspace,ConsoleKey.Backspace,  CmdBackspace)
                new Handler (Command.DeleteChar,ConsoleKey.Delete,     CmdDeleteChar)
                new Handler (Command.TabOrComplete,ConsoleKey.Tab,        CmdTabOrComplete)
                
                // Emacs keys
                Handler.Control Command.Home 'A' (CmdHome)
                Handler.Control Command.End 'E' (CmdEnd)
                Handler.Control Command.Left 'B' (CmdLeft)
                Handler.Control Command.Right 'F' (CmdRight)
                Handler.Control Command.HistoryPrev 'P' (CmdHistoryPrev)
                Handler.Control Command.HistoryNext 'N' (CmdHistoryNext)
                Handler.Control Command.CmdKillToEOF 'K' (CmdKillToEOF)
                Handler.Control Command.Yank 'Y' (CmdYank)
                Handler.Control Command.DeleteChar 'D' (CmdDeleteChar)
                Handler.Control Command.Refresh 'L' (CmdRefresh)
                Handler.Control Command.ReverseSearch 'R' (CmdReverseSearch)
                        
                Handler.Alt Command.BackwardWord 'B' ConsoleKey.B (CmdBackwardWord)
                Handler.Alt Command.ForwardWord 'F' ConsoleKey.F (CmdForwardWord)
                
                Handler.Alt Command.DeleteWord 'D' ConsoleKey.D (CmdDeleteWord)
                Handler.Alt Command.DeleteBackword ((char)8) ConsoleKey.Backspace (CmdDeleteBackword)
                
                // DEBUG
                //Handler.Control ('T', CmdDebug),

                // quote
                Handler.Control Command.Quote 'Q' (fun st -> st |> HandleChar ((Console.ReadKey (true)).KeyChar))
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
                | (Some(search), _) -> { st with PreviousSearch = Some(search.Term); SearchState = None} |> SetPrompt st.SpecifiedPrompt
                | _ -> st
            | None -> st |> HandleChar (newInput.KeyChar)

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
                        ShownPromptDisplay = editor.Settings.PromptDisplay
                }

            let cancelHandler = new ConsoleCancelEventHandler(interruptEdit Thread.CurrentThread)
            Console.CancelKeyPress.AddHandler cancelHandler

            st <- st |> InitText (Some(initial))

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
                        st <- st |> SetPrompt (prompt)
                        st <- st |> SetText (Some(""))
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