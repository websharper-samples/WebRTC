namespace Site

open WebSharper

[<JavaScript>]
module Client =
    let All =
        let ( !+ ) x = Samples.Set.Singleton(x)

        Samples.Set.Create [
            !+ MinMaxTimeDomain.Sample
            !+ ASCIICamera.Application.Sample
        ]

    [<SPAEntryPoint>]
    let Main() =
        All.Show()