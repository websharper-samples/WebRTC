namespace Site

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.JavaScript
open IntelliFactory.WebSharper.JQuery
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.Html5

open System.Text

[<JavaScript>]
module ASCIICamera =
    
    type Video = {
        Video : HTMLVideoElement
        Handle : Handle option
        Playing : bool
    }

    //Some inline JavaScript for functions that are not yet implemented in WebSharper    
    [<Direct @"window.requestAnimFrame = (function(){
                    return  window.requestAnimationFrame       ||
                    window.webkitRequestAnimationFrame ||
                    window.mozRequestAnimationFrame    ||
                    function( callback ){
                        window.setTimeout(callback, 1000 / 60);
                    };
               })();
               
               window.cancelAnimFrame =(function(){
                    return window.cancelAnimationFrame ||
                    window.mozCancelAnimationFrame ||                    
                    window.clearTimeout
              })();">]
    let SetupRequestAnimFrame () = X<unit>

    [<Inline "window.requestAnimFrame($callback)">]
    let RequestAnimFrame (callback : unit -> unit) = X<Handle>

    [<Inline "window.cancelAnimFrame($id)">]
    let CancelAnimFrame (id : Handle) = X<unit>

    [<Inline "(window.URL && window.URL.createObjectURL($stream)) || $stream">]
    let CreateObjectURL (stream : MediaStream) = X<string>


    //Global references for the sze of the otuput
    let private UserMedia = new UserMedia()

    let width = 120
    let height = 90


    //The module responsible for rendering the canvas input to ASCII characters
    module Ascii =
        type Color = { Red : int; Green : int; Blue : int; Alpha : int }

        let private characters = " .,:;i1tfLCG08@"

        let GetColorAtOffset (data : int []) (offset : int) =
            {
                Red = data.[offset]
                Green = data.[offset + 1]
                Blue = data.[offset + 2]
                Alpha = data.[offset + 3]
            }

        let Bound value minn maxx =
            max minn (min maxx value)

        let AsciiFromCanvas (canvas : CanvasElement) =
            let ctx = canvas.GetContext("2d")
            let width = canvas.Width
            let height = canvas.Height

            let contrast = 128.
            let contrastFactor = (259. * (contrast + 255.)) / (255. * (259. - contrast))

            let image = ctx.GetImageData(0., 0., float width, float height)

            let mutable x = 0
            let mutable y = 0
            let mutable result = ""

            while y < height do
                x <- 0
                while x < width do
                    let offset = (y * width + x) * 4
                    let color = GetColorAtOffset (As image.Data) offset
                    let contrastedColor =
                        {
                            Red = Bound (int (floor (float (color.Red - 128) * contrastFactor) + 128.)) 0 255
                            Green = Bound (int (floor (float (color.Green - 128) * contrastFactor) + 128.)) 0 255
                            Blue = Bound (int (floor (float (color.Blue - 128) * contrastFactor) + 128.)) 0 255
                            Alpha = color.Alpha
                        }

                    let brightness =
                        (0.299 * float contrastedColor.Red + 0.587 * float contrastedColor.Green + 0.114 * float contrastedColor.Blue) / 255.

                    result <- result + string characters.[(characters.Length - 1) - int (round (brightness * float (characters.Length - 1)))]
                    x <- x + 1
                result <- result + "\n"
                y <- y + 2
            
            result

    //The mpdule responsible for accessing the camera        
    module Camera =
        let rec FrameFun (video : Video) (draw : Video -> unit) () =
            draw video
            RequestAnimFrame(FrameFun video draw) |> ignore

        let InitVideoStream (video : HTMLVideoElement) (onNotSupprted : MediaStreamError -> unit) =

            UserMedia.GetUserMedia(MediaStreamConstraints(true, false),
                    (fun stream ->
                            video.Src <- CreateObjectURL stream
                    ), onNotSupprted)

            {
                Video = video
                Handle = None
                Playing = false
            }

        let Start (draw : Video -> unit) (video : Video) =
            video.Video.Play()
            { video with
                Handle = FrameFun video draw |> RequestAnimFrame |> Some
                Playing = true
            }

        let Pause (video : Video) =
            video.Handle
            |> Option.iter CancelAnimFrame
            video.Video.Pause()
            { video with
                Handle = None
                Playing = false
            }

        let Stop (video : Video) =
            Pause video
            |> (fun v -> 
                    v.Video.Src <- ""
                    v
               )

    module Application =
        let private video  = HTML5.Tags.Video [ Attr.Width <| string width; Attr.Height <| string height ]
        let private canvas = HTML5.Tags.Canvas [ Attr.Width <| string width; Attr.Height <| string height ]

        let private asciiContainer = Pre []
        let private error = Div []

        let Draw (v : Video) =
            let video = v.Video
            let context = (As<CanvasElement> canvas.Body).GetContext("2d");
            context.DrawImage(As video, 0., 0., float video.Width, float video.Height)
            asciiContainer.Html <- Ascii.AsciiFromCanvas(As canvas.Body)

        let Main (elem : Dom.Element) =
            AudioHolder.StopCurrent()
            
            SetupRequestAnimFrame ()
            JQuery.Of(elem).Append(asciiContainer.Dom).Ignore

            if not (As UserMedia.GetUserMedia) then
                error.Text <- "Your browser does not support this feature!"                                                           
                asciiContainer.SetCss("display", "none")
                JQuery.Of(elem).Append(error.Dom).Ignore
            else
                Camera.InitVideoStream <| As (video.Body) <| (fun e -> 
                                                                asciiContainer.SetCss("display", "none")
                                                                error.Text <- "No camera was found!"                                                           
                                                                JQuery.Of(elem).Append(error.Dom).Ignore
                                                             )
                |> Camera.Start Draw
                |> ignore
            
        let Sample =
            Samples.Build()
                .Id("ASCIICamera")
                .FileName(__SOURCE_FILE__)
                .Keywords(["webrtc"; "camera"; "video"; "effects"])
                .Render(Main)
                .Create()
     
