namespace Site

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html5

[<JavaScript>]
module AudioHolder =
    let mutable currentPlaying : MediaStreamAudioSourceNode option = None
    let mutable jsNode : ScriptProcessorNode option = None

    let StopCurrent() =
        currentPlaying
        |> Option.iter (fun e -> e.Disconnect())

        jsNode
        |> Option.iter (fun e-> e.Disconnect())

    let SetCurrent src jsnode =
        currentPlaying <- Some src
        jsNode <- Some jsnode