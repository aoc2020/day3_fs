// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

type Location = (int * int)
type Forest = Set<Location>

let readLines (filePath: String) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let treeXs (line:String): int[] =
    let isTree (_, ground) = ground = '#'
    let pos (p,_) = p
    line |> Seq.indexed |> Seq.filter isTree |> Seq.map pos |> Seq.toArray

let toForest (lines: String[]) : Set<Location> =
    let toLocations (y,xs) = xs |> Seq.map (fun x -> (x,y)) |> Seq.toArray
    lines |> Seq.map treeXs |> Seq.indexed |> Seq.map toLocations |> Seq.concat |> Set.ofSeq
    
let countTrees (f:Forest) depth width dx dy =
    let posAtStep n = ((n*dx) % width, n*dy)
    let hasTree (x,y) =
        // printfn "checking (%d,%d)" x y
        f |> Set.contains (x,y) 
    let coordinates = seq { 1 .. depth } |> Seq.map posAtStep
    let trees = coordinates |> Seq.filter hasTree |> Seq.toArray
    printfn "%A" trees
    trees |> Seq.length
    
let solve1 forest depth width = countTrees forest depth width 3 1

let solve2 forest depth width =
    let check = countTrees forest depth width 
    let c11 = check 1 1 |> uint64
    let c31 = check 3 1 |> uint64
    let c51 = check 5 1 |> uint64
    let c71 = check 7 1 |> uint64
    let c12 = check 1 2 |> uint64
    c11 * c31 * c51 * c71 * c12
    
[<EntryPoint>]
let main argv =
    let inputFile = "/Users/xeno/projects/aoc2020/day3_fs/day3_fs/input.txt"
    let lines = readLines(inputFile) |> Seq.toArray
    let width = lines.[0].Length
    let depth = lines.Length
    let forest = toForest lines 
    // printfn "Forest: %A" forest
    // let answer1 = countTrees forest depth width 3 1
    let answer1 = solve1 forest depth width
    let answer2 = solve2 forest depth width
    printfn "Answer 1: %d" answer1
    printfn "Answer 2: %d" answer2
    0 // return an integer exit code