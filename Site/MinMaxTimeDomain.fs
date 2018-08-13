namespace Site

open WebSharper
open WebSharper.JavaScript
open WebSharper.JQuery
open WebSharper.UI
open WebSharper.UI.Notation
open WebSharper.UI.Html
open WebSharper.UI.Client

[<JavaScript>]
module MinMaxTimeDomain =
    let context = new AudioContext ()
    let javascriptNode = context.CreateScriptProcessor (1024, 1, 1)

    let ToList (from : Uint8Array) =
        let rec helper n res =
            if n <= -1 then res
            else helper (n - 1) (from.Get(n)::res)

        helper (from.Length - 1) []

    let CanvasEl = Elt.canvas [attr.width "800"; attr.height "256"; attr.style "background-color: black;"] []

    let mutable column = 0.

    let DrawTimeDomain (ctx : CanvasRenderingContext2D) (array : Uint8Array) =
        let c = JQuery.Of(CanvasEl.Dom)
        let width = float <| c.Width()
        let height = float <| c.Height()

        let minValue = ref 9999999.
        let maxValue = ref 0.

        ToList array
        |> List.iteri (fun i a -> 
                            let value = (float a) / 256.
                            if value > !maxValue then
                                maxValue := value
                            if value < !minValue then
                                minValue := value
                      )

        let max = height - (height * !maxValue) - 1.
        let min = height - (height * !minValue) - 1.

        ctx.FillStyle <- "#ffffff"                                            
        ctx.FillRect(column, min, 1. ,max - min)

        column <- column + 1.
        if column >= width then
            column <- 0.
            ctx.ClearRect(0., 0., width, height);

    let Analyser (stream : MediaStream) =
        let analyser = context.CreateAnalyser ()
        analyser.SmoothingTimeConstant <- 0.
        analyser.FftSize <- 1024

        //Workaroud for a bug in Chrome which makes the GC destroy 
        //the ScriptProcessorNode if it's not in global scope
        JS.Global?sourceNode <- javascriptNode

        javascriptNode.Connect(context.Destination)
        javascriptNode.Onaudioprocess <- fun e ->
                                            let array = new Uint8Array(int(analyser.FrequencyBinCount))
                                            analyser.GetByteTimeDomainData array
                                            let ctx = (As<CanvasElement> CanvasEl.Dom).GetContext("2d")
                                            DrawTimeDomain ctx array

        let streamNode = context.CreateMediaStreamSource(stream)
        streamNode.Connect(analyser)
        streamNode.Connect(context.Destination)

        AudioHolder.SetCurrent streamNode javascriptNode

        analyser.Connect(javascriptNode)

    let um = new UserMedia()

    let LoadSound (onNotSupported : MediaStreamError -> unit) =
        um.GetUserMedia(MediaStreamConstraints(false, true), 
                               (fun stream ->
                                    Analyser stream
                               ), fun e -> onNotSupported e)

    let Main (elem : Dom.Element) =
        AudioHolder.StopCurrent ()

        let error = Var.Create ""
        error.View.Doc(function
            | "" -> CanvasEl :> Doc
            | err -> div [] [text err]
        )
        |> Doc.Run elem

        if not (As um.GetUserMedia) then
            error := "Your browser does not support this feature!"                                                           
        else
            LoadSound <| fun e -> error := "No microphone was found!"

    let Sample =
        Samples.Build()
            .Id("MinMaxTimeDomain")
            .FileName(__SOURCE_FILE__)
            .Keywords(["webrtc"; "visualizer"; "time domain"; "microphone"])
            .Render(Main)
            .Create()
 