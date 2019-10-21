// Mm... I'm not happy with our last step.
// Creating and raising exceptions is soooo object-oriented-ish.
// What if we create a type that serves as different errors, and
// return it in case something goes wrong? Yeah, let's do that.

type Dish =
  | CleanDish
  | WashedDish
  | DirtyDish

type DishError =
  | AlreadyWashed   // if washing a non-dirty dish
  | AlreadyCleaned  // if cleaning a cleaned dish
  | MustWashFirst   // if cleaning a dirty dish

// Looks pretty ok to me, except for one thing:
// we can't make our functions return a Dish or a DishError
// depending on how things go. But we need to be able to
// return either one or another. In that case, we can say
// that our functions will return a result, which can be
// either a washed or cleaned dish, or a dish error.
type DishResult =
  | Ok of Dish
  | Error of DishError

// And adapt our functions.
let washDish (dish: Dish): DishResult =
  match dish with
  | DirtyDish   -> Ok WashedDish
  | WashedDish  -> Error AlreadyWashed
  | CleanDish   -> Error AlreadyCleaned

let cleanDish (dish: Dish): DishResult =
  match dish with
  | DirtyDish   -> Error MustWashFirst
  | WashedDish  -> Ok CleanDish
  | CleanDish   -> Error AlreadyCleaned

// Good and cool! But now we have another problem: composition doesn't work
// anymore because we can't pass a DishResult as a Dish.
// One way to solve it is to define washAndCleanDish by hand:
let uglyWashAndCleanDish dish =
  match washDish dish with
  | Ok d          -> cleanDish d
  | Error _ as e  -> e

// This works... but... ugh.
// Maybe we can generalize it in such a way that will let us
// just compose them as we did previously? Let's see...
// Here we're taking a function that maps Dish to DishResult,
// then either (a) extracting the Dish from this DishResult and
// passing it along, or (b) just returning the DishResult containing
// a DishError.
// That is, (Dish -> DishResult) (Dish -> DishResult) -> DishResult
// Unfortunately, we can't use generic types because we still need
// to match the cases of the DishResults.
let (>>=) (f: Dish -> DishResult) (g: Dish -> DishResult) (dish: Dish): DishResult =
  match f dish with
  | Ok d          -> g d
  | Error _ as e  -> e

// Now we can happily compose our functions just like we did before,
// and taking full advantage of currying and partial application:
let washAndCleanDish =
  washDish >>= cleanDish // Note that we're not passing the third argument here.

let dishes = [DirtyDish; DirtyDish; WashedDish; DirtyDish; CleanDish]

// And of course, applying washAndCleanDish to our dishes should return the errors
// and successes, as can be seen running the code below:
List.map washAndCleanDish dishes
  |> printfn "%A"

// Fair enough. Finally, let us compose yet another function that will take care
// of our error cases:
let onlyCleanDishes (r: DishResult): DishResult =
  match r with
  | Ok CleanDish  | Error AlreadyCleaned  -> Ok CleanDish
  | Ok WashedDish | Error AlreadyWashed   -> cleanDish WashedDish
  | Ok DirtyDish  | Error MustWashFirst   -> washAndCleanDish DirtyDish

let makeEverythingShine =
  washAndCleanDish >> onlyCleanDishes

List.map makeEverythingShine dishes
  |> printfn "%A"
