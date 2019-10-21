// Great! But there's one thing that's been bugging me.
// Did you notice we're washing and cleaning dishes that
// may already be washed or cleaned?
// Doesn't feel right, eh? Why would we waste water and
// dish soap to wash and clean everything again?
// That's why we will ~choose~ which dishes we should wash,
// and which dishes we should clean.

type Dish =
  | CleanDish
  | WashedDish
  | DirtyDish


// Also, we have to make sure our functions understand that
// we're not supposed to wash washed dishes, or clean cleaned
// dishes. Let's create an exception type and raise it!
exception DishException of string


let washDish (dish: Dish): Dish =
  match dish with
  | DirtyDish -> WashedDish
  | _         -> raise <| DishException "no need to wash"

let cleanDish (dish: Dish): Dish =
  match dish with
  | WashedDish  -> CleanDish
  | DirtyDish   -> raise <| DishException "must wash first"
  | CleanDish   -> raise <| DishException "no need to clean"


let dishes = [DirtyDish; DirtyDish; WashedDish; DirtyDish; CleanDish]
let washAndCleanDish = washDish >> cleanDish

// Now if we try to map washAndCleanDish to dishes, we'll get and exception:
try
  List.map washAndCleanDish dishes
with
| DishException msg -> printfn "DishException: %s" msg; [] // -> we need this so the
// compiler won't complain about type mismatches

// To achieve our initial result, we're choosing specific dishes to wash and clean.
// List.choose works as a combined filter and map: you return *Some value* based
// on a condition; *Nothing* if the condition isn't met.
let dishesToWash = List.choose (function DirtyDish as d -> Some d | _ -> None) dishes
// Also, remember 'function' is just a shortcut to 'match' with the last argument.
let dishesToClean = List.choose (fun d -> match d with WashedDish -> Some d | _ -> None) dishes

// Now we wash our dirty dishes:
let washedDishes = List.map washDish dishesToWash
printfn "%A" washedDishes

// And clean it all
let cleanedDishes = List.map cleanDish <| dishesToClean @ washedDishes
printfn "%A" cleanedDishes

// Finally, we have all our dishes washed and cleaned. Shiny dishes :)
let allDishesCleaned =
  dishes
  |> List.choose (function CleanDish as d -> Some d | _ -> None)
  |> (@) <| cleanedDishes
  // Understanding what's happening here is an exercise to the reader.

printfn "%A" allDishesCleaned
