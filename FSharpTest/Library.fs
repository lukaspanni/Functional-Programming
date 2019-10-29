namespace FSharpTest

module ListFunctions = 
    let squareList list = List.map(fun x -> x * x) list
    let sumList list = List.reduce(fun acc x -> acc + x) list
    let prodList list = List.reduce(fun acc x -> acc * x) list
