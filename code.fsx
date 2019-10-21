(*
Alright, so the idea is to build some simple stuff to make sure I understand
the concept of binding, two-way-railtrack, function composition, and some
other stuff. Yes, I did have this idea while washing dishes.
*)

// So, the idea is to represent some real-life scenarios to try and explain
// (primarily to myself, but also to whomever may be interested) the concepts
// of map, bind, et cetera with dish washing.
// Disclaimer: this is not the best way to do things in real life IMO. What
// I'm writing here is for educational purposes only.

// Say we have a Dish type,
type Dish =
  // which can be represented by a clean, dry dish,
  | CleanDish
  // a washed, but still wet dish,
  | WashedDish
  // and a dirty dish.
  | DirtyDish

// And we have some handy functions to deal with them, say,
let washDish (_: Dish): Dish =
  WashedDish

let cleanDish (_: Dish): Dish =
  CleanDish

// Nice, now we can have some shiny dishes.

// With these, we can wash and clean one dish, or use map to wash and clean
// several dishes at once.

let dishes = [DirtyDish; DirtyDish; WashedDish; DirtyDish; CleanDish]

// We wash them all
dishes |> List.map washDish
  // and clean them
  |> List.map cleanDish
  // and finally, check the results
  |> printfn "map washDish -> map cleanDish:\t%A"

// Mmm... But that doesn't really feel right... I mean, we're iterating the list
// twice. Maybe we don't _need_ to do that. What if, for every dish, we washed
// AND cleaned it right afterwards? We can do that with composition.

let washAndCleanDish = washDish >> cleanDish
// ↑ this means, the output of `washDish` will be the input of `cleanDish`.
// Pretty much the same as the pipe (|>) operator, but without the arguments.
// The same could be written with the pipe operator as follows:
let washAndCleanDish2 dish = washDish dish |> cleanDish
// or
let washAndCleanDish3 = cleanDish << washDish
// or
let washAndCleanDish4 dish = cleanDish <| washDish dish
// Remember: the direction of the arrow tells the way; in all of these,
// `washDish` will run first.

// Now we can use one, and only one call to map to wash and clean dishes:
dishes
  |> List.map washAndCleanDish
  // And display the results:
  |> printfn "map washAndClean:\t\t%A"
