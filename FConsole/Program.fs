open System

let listFunc() =  
    let testList = [1;2;3]
    let testList2 = List.map(fun x -> x*x*2) testList
    printfn "%A" testList2
    [8;9;12;16;19] |> List.filter(fun v -> v % 2 = 0) |> List.map(fun x -> x) |> printfn "%A"


let square x = x*x
let mult x = x*2
let add x = x+2

let mult_add = mult >> add
let add_mult = mult << add

let squareSum n = [1..n] |> List.map(fun x -> x * x)|> List.sum //Summe der Quadratzahlen von 1 bis n
let sum n = [1..n] |> List.sum  //Summe von 1 bis n

let rec fak n = if n = 1 then 1 else n*fak (n-1)    //Fakultät
let rec fib n = if n <= 2 then 1 else fib (n-1) + fib (n-2) //Fibonacci Zahlen
let fibSeq n = [1..n] |> List.map fib   //erste n Fibonacci Zahlen
let checkLeapYear n = if n % 400 = 0 then true 
                      else if n % 100 = 0 then false 
                      else if n % 4 = 0 then true 
                      else false
let checkLeapYearWrapper n = if checkLeapYear n = true then n else 0


let ABC = [|0..25|] |> Array.map char |> Array.map (fun x -> x + 'A')
let abc = ABC|> Array.map (fun x -> x + char 32)

type Car = { HP : int; Brand : string }

let testCar = { HP = 200; Brand = "BMW" }
let tunedTestCar = { testCar with HP = 500 }


[<EntryPoint>]
let main argv =
    let num = 2
    printfn "%i"(square(num))
    listFunc()
    printfn "%i" (mult_add 5)
    printfn "%i" (add_mult 5)
    printfn "%i" (squareSum 100)
    printfn "%i" (sum 100)
    printfn "%i" (fak 5)
    printfn "%A" (fibSeq 10)
    printfn "%A" ([1990..2020] |> List.map checkLeapYearWrapper |> List.filter(fun x -> x > 0))
    printfn "%A" (Array.map(fun x-> x*x) [|1..25|]) //quadratzaheln bis 25
    for a in [1..10] do
        printfn "*"
    printfn "Fertig"
    let mutable ar = [|for i in 1..10 -> 2*i|]
    printfn "%A" ar
    printfn ""
    for i in [1..10] do
        ar <- [|i..i..10*i|]
        printfn "%A" ar

    ar <- [|1..10|]
    printf "\n\n%i\n" (ar |> Array.sum)

    printfn "%A" abc
    printfn "%A" ABC
    printfn "combined: %A" (Array.zip ABC abc)
    printfn ""
    printfn ""
    let pos = [|1..fib 6|]
    let neg = pos|> Array.map (fun x -> -x)
    printfn "%A" pos
    printfn "%A" neg
    printfn "combined: %A" (Array.zip pos neg) 

    printfn "%i" ((fibSeq >> List.reduce(fun x ac -> x*ac)) 10)




    0 // return an integer exit code