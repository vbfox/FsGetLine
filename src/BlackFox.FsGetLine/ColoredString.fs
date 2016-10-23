module ColoredString
open System
open System.Text

type private ColorIdentifier =
    | NoColor
    | Reset
    | Color of color : ConsoleColor

type private OnCharParsed<'st> = 'st -> char -> 'st
type private OnColorParsed<'st> = 'st -> ColorIdentifier * ColorIdentifier -> 'st

let private tryParseColor (s:string) =
    if s.ToLower () = "reset" then
        Reset
    else
        try
            Color(Enum.Parse(typedefof<ConsoleColor>, s) :?> ConsoleColor)
        with
        | :? ArgumentException -> NoColor
        | :? OverflowException -> NoColor

let private parseColorCodes (s:string) =
    let split = s.Split(';')

    let foreground = if split.Length >= 1 then tryParseColor split.[0] else NoColor
    let background = if split.Length >= 2 then tryParseColor split.[1] else NoColor

    (foreground, background)

/// Support color markers in strings like ^[Red] or ^[Red;Blue] where the first color is
/// the foreground and the second the background.
/// Color names are the string values of ConsoleColors and the special value 'Reset' set
/// the corresponding color to the one that was present when the display started.
type ColoredString(raw : string) =
    member val Raw = raw

    static member public EscapeToString (s:string) =
        s.Replace("^", "^^")

    static member public Escape (s:string) =
        new ColoredString(ColoredString.EscapeToString(s))

    member private __.Fold (onChar: OnCharParsed<'st>) (onColor: OnColorParsed<'st>) (st:'st) = 
        let foldFunc (st, escCount, content) c =
            match escCount with
            | 0 ->
                match c with
                | '^' -> (st, 1, "")
                | _ -> (onChar st c, 0, "")
            | 1 ->
                match c with
                | '[' -> (st, 2, "")
                | _ -> (onChar st c, 0, "")
            | 2 ->
                match c with
                | ']' -> (onColor st (parseColorCodes content), 0, "")
                | _ -> (st, 2, content + (string)c)
            | _ ->
                failwith("Impossible escape count")

        let (newSt, _, _) = raw |> Seq.fold foldFunc (st, 0, "")
        newSt

    member x.Length
        with get () = x.Fold (fun x _ -> x + 1) (fun x _ -> x) 0

    override x.ToString () =
        let builder = x.Fold (fun (b:StringBuilder) c -> b.Append c) (fun b _ -> b) (new StringBuilder())
        builder.ToString()

    member private __.WriteCore (inner: (ColorIdentifier -> ColorIdentifier -> unit) -> unit) =
        let initalForeground = Console.ForegroundColor
        let initialBackground = Console.BackgroundColor

        let setForeground = function
            | NoColor -> ()
            | Reset -> Console.ForegroundColor <- initalForeground
            | Color color -> Console.ForegroundColor <-  color

        let setBackground = function
            | NoColor -> ()
            | Reset -> Console.BackgroundColor <- initialBackground
            | Color color -> Console.BackgroundColor <-  color

        let setColors foreground background =
            setForeground foreground
            setBackground background
                        
        inner setColors

        setColors Reset Reset

    member x.WriteToConsole () =
        x.WriteCore (fun setColors ->
            x.Fold (fun _ c -> Console.Write(c)) (fun _ (foreground, background) -> setColors foreground background) ()
            )

    member x.WriteToConsoleFrom from =
        x.WriteCore (fun setColors ->
            let writeChar i (c:char) =
                if i < from then
                    i + 1
                else
                    Console.Write c
                    i + 1
            let setColors' i (foreground, background) =
                setColors foreground background
                i
            x.Fold writeChar setColors' 0 |> ignore
            )

let coloredWrite s =
    (ColoredString s).WriteToConsole ()

let coloredWriteLine s =
    coloredWrite s
    Console.WriteLine()