// Some fun with trying to replace some standard types like int and bool
// with functions. This is inspired by a talk "Practical Lambda Calculus"
// given by Jonas Winje and Einar Høst @LambdaDays2017, Kraków

// implementation is in F#, no dependencies required, should be okay to just
// put it through F# interpreter

// first let's try to avoid having bools and define
// true and false as functions
let tr a b = a
let fl a b = b

// testing
let testBool x = x 1 0
tr |> testBool // should be "1"
fl |> testBool // should be "0"

// for "not" we'll just swap arguments
// if x=tr then x returns first argument
// then (not tr) a b = tr b a = b, so it's 
// similarly goes for (not fl)
let notB x a b = x b a
notB tr |> testBool // should be "0"

// note we use some conventions for naming
// x,y,z.. - bools
// a,b - arguments of the result function
let orB x y a b  = x a (y a b)
orB tr fl |> testBool // should be "1"
orB fl fl |> testBool // should be "0"
orB tr tr |> testBool // should be "1"

// lets implement "and"
let andB x y a b = x (y a b) b
andB tr fl |> testBool // "0"
andB fl fl |> testBool // "0"
andB tr tr |> testBool // "1"

// so far so good, lets implement if then else :)
let ifThenElse condition resIfTrue resIfFalse =
    condition resIfTrue resIfFalse

// for me this is "aha" moment :P
// our bools are in fact functions that act similarly to if then else
// let's try it:
ifThenElse (notB (andB tr fl)) "!(true && false) = true" "!(true && false) = false"
//             /\ condition          /\ resultIfTrue         /\ resultIfFalse

// I don't know what else fun I can do with bools so let's move to ints
// let the lambda int be a function that given some initial value "i" and a function "f"
// will return f applied on i n times :P

type Integer<'a> = 'a -> ('a -> 'a) -> 'a

let one : Integer<'a> = 
    fun i f -> f i // just applied once
let zero : Integer<'a> = 
    fun i f -> i // applied twice

let test num = num 0 (fun i -> i + 1)

zero |> test // "0"
one |> test // "1"

// adding two integers
// first we use "y" on initial value and
// then we apply "x" on the result
let add x y i f =
    x (y i f) f

add one one |> test // "2"

// I don't know how not to specialize it to be Integer<int> 
// and leave it fully generic :(
let two : Integer<int> = (add one one)
let three : Integer<int> = (add two one)

add two three |> test // "5"

// let's try to to multiplication!
// inead of passing f to the x, we will
// pass a custom funciton that will apply f y-times
// and how to apply f y-times? y will do exactly that!
let mul x y i f = x i (fun el -> y el f)

mul two three |> test // '6"

// let's see how for a given integer x we can generate a list
// or x elements being [x, x-1, ..., 1]

// first let's build a function that given a list will append a new element
let addNextEl lst = 
    match lst with
    | h::t -> (add h one) :: lst
    | [] -> [one]

// example
addNextEl [two; one] |> List.map test // [3;2;1]

// okay! so having that we can use this function as "f"
// for example:
(mul (add one one)  (add one one)) [] addNextEl |> List.map test // [4;3;2;1]

// this way we can build a factorial!
// just multiply all elements in a list using fold
let factorial x = 
    List.fold mul one (x [] addNextEl) 

// testing, note that corner cases are handled
factorial (add one one) |> test // 2! = 2
factorial zero |> test // 0! = 1
factorial one |> test // 1! = 1
factorial (mul (add one one)  (add one one)) |> test // 4! = 24

// lets try implementing 2^n
// this is ugly that we build a list of all powers
// but I could not make it work in a different way
let addNextMultipliedBy2 lst = 
    match lst with
    | h::t -> (mul (add one one) h) :: lst
    | [] -> [add one one]

let allPowersOfTwoUpTo x =
    x [] addNextMultipliedBy2

allPowersOfTwoUpTo (mul (add one one) (add one one)) |> List.map test // [16, 8, 4, 2]

let pow2 x = match allPowersOfTwoUpTo x with
    | h::t -> h
    | [] -> one

pow2 (mul (add one one) (add one one)) |> test // 2^4 = 16

// further things that might be doable:
// - equality check on ints returning a lambda bool
//   this can be possibly done using lists and pattern matching
