
let basePath = "C:/Users/DavWi/OneDrive/Desktop/V-ProjektDateien/ProjektRessourcen/ProjectLogs"
//let relativeFilePath = "AxymReaderConnection/apache2_2024-05-09/acc.log"
let relativeFilePath = "AxymReader/apache2_2024-05-04/apachelogs_access_log"
//let relativeOutputFilePath = "AxymReader/apache2/ip_address_counts.txt"

let recurringUserThreshold = 5

let logFilePath = basePath + "/" + relativeFilePath

type RequestMethod = GET | POST | PUT | DELETE | PATCH | HEAD | OPTIONS | TRACE | CONNECT | UNKNOWN_METHOD
type RequestProtocol = HTTP | HTTPS | UNKNOWN_PROTOCOL

type LogLine = {
    IpAddress: string
    Date: string
    Time: string
    Method: RequestMethod
    Path: string
    QueryString: string
    Protocol: RequestProtocol
    ProtocolVersion: string
    StatusCode: int
    Size: int
}

let requestMethodFromString = function
    | "GET" -> GET
    | "POST" -> POST
    | "PUT" -> PUT
    | "DELETE" -> DELETE
    | "PATCH" -> PATCH
    | "HEAD" -> HEAD
    | "OPTIONS" -> OPTIONS
    | "TRACE" -> TRACE
    | "CONNECT" -> CONNECT
    | _ -> UNKNOWN_METHOD

let protocolFromString = function
    | "HTTP" -> HTTP
    | "HTTPS" -> HTTPS
    | _ -> UNKNOWN_PROTOCOL

let automaticRequestIdentifiers = ["_blazor"; "_framework"; "_content"; "_css"; "_js"; "_lib"; "_wasm"; "sitemap.txt"; "robots.txt"; "favicon.ico"; "apple-touch"; "wp-"; ".js"; ".xml"; ".well-known"; ".php"; ".css"; ".jpg"; ".png"; "templates"; "sites"; "admin"; "vendor"; ".git"]
let validRequestMethods = [GET; POST]
let nonUserIps = ["-"; "127.0.0.1"; "77.25.7.52"; "69.162.124.235"; "85.215.79.188"]

// Example line: 
// 95.225.7.52 - - [18/Apr/2024:17:28:02 +0000] "POST /_blazor?id=hMLNLqDYAnNeBLvccz2Nrg HTTP/1.1" 200 195

let readLogFile (path: string) =
    System.IO.File.ReadAllLines(path)

let parseLogFile (lines: string[]) =
    lines
    |> Array.map (fun line -> line.Split(' '))
    |> Array.filter (fun parts -> parts.Length >= 8)
    |> Array.map (fun parts -> 
        {
            IpAddress = parts.[0]
            Date = parts.[3].TrimStart('[').Split(':').[0]
            Time = parts.[3].Substring(parts.[3].IndexOf(':') + 1)
            Method = requestMethodFromString (parts.[5].Trim('"'))
            Path = if parts[6].Contains('?') then parts.[6].Split('?').[0] else parts.[6]
            QueryString = if parts[6].Contains('?') then parts.[6].Split('?').[1] else ""
            Protocol = protocolFromString (parts.[7].Split('/')[0])
            ProtocolVersion = try parts.[7].Split('/')[1] with _ -> ""
            StatusCode = try int parts.[8] with _ -> 200
            Size = try int parts.[9] with _ -> 200
        }
    )

let groupBy (selector : 'T -> string) (items: 'T[]) =
    items
    |> Array.groupBy selector
    |> Array.map (fun (key, values) -> (key, values.Length))
    |> Array.append([|("Total", items.Length)|])

// Define a function to write the status code counts to a file
let writeGroupResultToFile (path: string) (groupResults: (string * int)[]) =
    System.IO.File.WriteAllLines(path, groupResults |> Array.map (fun (status, count) -> sprintf "%s: %d" status count))

let isNonUserRequest (line: LogLine) =
    automaticRequestIdentifiers
    |> Seq.exists (fun identifier -> line.Path.Contains(identifier)) || not (validRequestMethods |> List.contains line.Method) || (nonUserIps |> List.contains line.IpAddress)


let printGroupResult = function
    | (key, count) -> printfn "%s: %d" key count

let filteredEntries = 
    logFilePath
    |> readLogFile
    |> parseLogFile
    |> Array.where (fun line -> not (isNonUserRequest line))

// dispay the total number of gruped results
filteredEntries
|> groupBy (fun line -> line.IpAddress + " @ " + line.Date + " - " + line.Time)
|> Array.length
|> printfn "Requests: %d"

// display the number of requests per user
filteredEntries
|> groupBy (fun line -> line.IpAddress)
|> Array.length
|> printfn "Visitors: %d"

// measure of users - condition: more than recurringUserThreshold requests on diffrerent days
filteredEntries
|> groupBy (fun line -> line.IpAddress + "@" + line.Date)
|> Array.filter (fun (_, count) -> count >= recurringUserThreshold)
|> Array.sortByDescending (fun (_, count) -> count)
|> Array.length
|> printfn "Recurring Users: %d"

filteredEntries
|> groupBy (fun line -> line.Time.Substring(0, line.Time.IndexOf(":")))
|> Array.sortBy (fun (key, _) -> key)
|> Array.iter printGroupResult