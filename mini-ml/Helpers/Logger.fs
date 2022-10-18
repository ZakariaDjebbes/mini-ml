module Helpers.Logger

open System

type Logger() =
    member val Line = true with get, set

    member this.log (msg: string) (color: ConsoleColor) =
        Console.ForegroundColor <- color

        if this.Line then
            printfn $"%s{msg}"
        else
            printf $"%s{msg}"

        Console.ResetColor()

    member this.logDefault(msg: string) =
        Console.ResetColor()

        if this.Line then
            printfn $"%s{msg}"
        else
            printf $"%s{msg}"

    member this.logInfo(msg: string) = this.log msg ConsoleColor.Blue
    member this.logError(msg: string) = this.log msg ConsoleColor.Red
    member this.logSuccess(msg: string) = this.log msg ConsoleColor.Green
    member this.logWarning(msg: string) = this.log msg ConsoleColor.Yellow
    member this.logFatal(msg: string) = this.log msg ConsoleColor.Magenta
