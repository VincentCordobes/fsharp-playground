let rec fib n =
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> fib (n-1) + fib (n-2)

let toto = fib 10
printfn "%A" toto

let rec merge a b =
 match (a, b) with
 | (x, []) -> x
 | ([], y) -> y
 | (x::tx, y::ty) -> if x < y then x::merge tx (y::ty)
                     else y::merge ty (x::tx)

let rec mergeSort a =
 match a with
 | [] -> []
 | [x] -> [x]
 | _ -> let (l1, l2) = List.splitAt (List.length a / 2) a
        in merge (mergeSort l1) (mergeSort l2)

mergeSort [4; 5; 7; 2; 3; 4] |> printfn "%A"

let rec sum a =
  match a with
  | [] -> 0
  | h::tail -> h + sum tail

sum [1; 2; 4] |> printfn "%A"


let div a b =
  match b with
  | 0 -> None
  | _ -> Some (a/b)



let a = div 10 0
match a with
| None -> printfn "rien"
| Some x -> printfn "%d" x


let printDiv () =
  match div 10 2 with
  | None -> printfn "Nothing to print" 
  | Some a -> printfn "%A" a


type ComputeResult = 
  | Positif of int
  | Negatif of int
  | Noop
let computeSomething () =
  let randGenerator = System.Random()
  let n = randGenerator.Next(-100, 100)
  match n with 
  | x when x < 0 -> Negatif x
  | y when y > 0 -> Positif  y 
  | _ -> Noop


let handleSomething s =
  match s with
  | Positif x -> printfn "The generated number is positif (%d)" x 
  | Negatif x -> printfn "The generated number is negatif (%d)" x
  | Noop -> printfn "The generated number is invalid"

computeSomething () |> handleSomething

let max2 a b =
  if a > b then a else b;

let rec max a =
  match a with 
  | [] -> 0
  | [x] -> x
  | y -> 
    let (left, right) = List.splitAt (List.length a/2) y in
    max2 (max left) (max right)


max [4; 5; 7; 2; 3; 4] |> printfn "%A"

// 1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy)

let rec last = function
  | [] -> None
  | [x] -> Some x
  | h::tail -> last tail

last ["a" ; "b" ; "c" ; "d"] |> printfn "%A"
// - : string option = Some "d"

last []  |> printfn "%A"
// - : 'a option = None

let rec lastTwo = function
  | [] -> None
  | [x;y] -> Some (x, y) 
  | _::tail -> lastTwo tail

lastTwo [ "a" ; "b" ; "c" ; "d" ] |> printfn "%A"
// - : (string * string) option = Some ("c", "d")
lastTwo [ "a" ] |> printfn "%A"
// - : (string * string) option = None

let rec at n = function
  | [] -> None
  | h::t -> if n = 0 then Some h else at (n-1) t

at 3 [ "a" ; "b"; "c"; "d"; "e" ] |> printfn "%A"
// - : string option = Some "c"
at 3 [ "a" ] |> printfn "%A"
// - : string option = None


// tail recursive impl.
let length list = 
  let rec aux i = function
    | [] -> i 
    | h::t -> aux (i+1) t
  aux 0 list


length [ "a" ; "b" ; "c"] |> printfn "%A"
// - : int = 3
length [] |> printfn "%A"
// - : int = 0

let rev list =
  let rec aux acc = function
  | [] -> acc 
  | h::tail -> aux (h::acc) tail
  aux [] list

rev ["a" ; "b" ; "c"] |> printfn "%A"
// - : string list = ["c"; "b"; "a"]


let traverse list =
  let rec aux acc = function
  | [] -> acc
  | h::tail -> aux (h::acc) tail
  (aux [] >> List.rev) list


// traverse [1; 2; 3; 4; 5]


type  Tree<'a> = 
  | Leaf 
  | Node of value:'a * left: Tree<'a> * right: Tree<'a>

let generateTree () =
  Node (1, 
        Node (2, Leaf, Leaf),
        Node (3,
              Node (4, Leaf, Leaf),
              Leaf
        )
  )

let traverseTree tree =
  let rec aux tree =
    match tree with
    | Leaf -> []
    | Node (v, left, right) -> v::List.concat [aux left; aux right]
  aux tree

// generateTree () |> traverseTree 

type  Tree1<'a> = 
  | Leaf1
  | Node1 of value: 'a *  children: List<Tree1<'a>>

let generateTree1 (): Tree1<int> =
  Node1 (1, [Node1 (2, [Leaf1; Leaf1]); Node1 (3, [Node1 (4, [Leaf1; Leaf1]); Leaf1])])

let traverseTree1 tree =
  let rec aux tree =
    match tree with
    | Leaf1 -> []
    | Node1 (v, children) -> v::List.collect aux children
  aux tree

generateTree1 () |> traverseTree1 |> printfn "%A"


let isPalindrome list =
  list  = List.rev list

isPalindrome [ "x" ; "a" ; "m" ; "a" ; "x" ] |> ignore
isPalindrome [ "a" ; "b"; "c" ] |> ignore

type 'a Node =
  | One of 'a
  | Many of 'a Node list

let rec flatten (node: 'a Node) =
  match node with
  | One e -> [e]
  | Many nodes -> nodes |> List.collect flatten
  
flatten (Many [One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ]) |> ignore


let rec compress list =
  match list with
  | [] -> []
  | [a] -> [a]
  | [a; b] -> if a = b then [a] else [a;b]
  | a::b::tail -> if a = b then compress (b::tail) else a::compress (b::tail)

compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] |> ignore


let pack list =
  let rec aux current acc list =
    match list with 
    | [] -> []
    | [a] -> (a::current)::acc
    | a::(b::_ as tail) -> 
      if a = b then
        aux (a::current) acc tail
      else
        aux [] ((a::current)::acc) tail 
  List.rev (aux [] [] list)


pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"] |>

let tata = Some 5

Option.map (fun x -> (x+1)) tata |> printfn "%A"

type Fruit = 
| Pomme of string
| Banane 
| Kiwi 

let myFruit = Pomme "grannit"

let toString myFruit = 
  match myFruit with
  | Pomme name -> "Pomme"
  | Banane -> "Banage"
  | Kiwi -> "Kiwi"

