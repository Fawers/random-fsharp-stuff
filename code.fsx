// I'm just playing with some more types in this step.
// Nothing reeeally new.

type DishType =
  | Plate
  | Pot
  | FryingPan
  // this can go on almost infinitely

type Dish =
  | CleanDish of DishType
  | WashedDish of DishType
  | DirtyDish of DishType

type DishError =
  | AlreadyWashed of DishType
  | AlreadyCleaned of DishType
  | MustWashFirst of DishType

type DishResult =
  | Ok of Dish
  | Error of DishError

let washDish (dish: Dish): DishResult =
  match dish with
  | DirtyDish dt  -> Ok <| WashedDish dt
  | WashedDish dt -> Error <| AlreadyWashed dt
  | CleanDish dt  -> Error <| AlreadyCleaned dt

let cleanDish (dish: Dish): DishResult =
  match dish with
  | DirtyDish dt  -> Error <| MustWashFirst dt
  | WashedDish dt -> Ok <| CleanDish dt
  | CleanDish dt  -> Error <| AlreadyCleaned dt


let (>>=) (f: Dish -> DishResult) (g: Dish -> DishResult) (dish: Dish): DishResult =
  match f dish with
  | Ok d          -> g d
  | Error _ as e  -> e

let washAndCleanDish =
  washDish >>= cleanDish

let dishes = [DirtyDish Pot; DirtyDish FryingPan; WashedDish Plate; DirtyDish Plate; CleanDish FryingPan]

List.map washAndCleanDish dishes
  |> printfn "%A"

let onlyCleanDishes (r: DishResult): DishResult =
  match r with
  | Ok (CleanDish dt)  | Error (AlreadyCleaned dt)  -> Ok <| CleanDish dt
  | Ok (WashedDish dt) | Error (AlreadyWashed dt)   -> cleanDish <| WashedDish dt
  | Ok (DirtyDish dt)  | Error (MustWashFirst dt)   -> washAndCleanDish <| DirtyDish dt

let makeEverythingShine =
  washAndCleanDish >> onlyCleanDishes

List.map makeEverythingShine dishes
  |> printfn "%A"
