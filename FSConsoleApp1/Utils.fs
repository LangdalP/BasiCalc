module Utils

let parseFloat (s : string) =
    match System.Double.TryParse(s) with
    | true, v -> Some v
    | false, _ -> None

