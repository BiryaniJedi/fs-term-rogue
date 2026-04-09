module Program

open Types

let clampStat min max value =
    match value with
    | x when x < min -> min
    | x when x > max -> max
    | _ -> value

let clampHp = clampStat 0 100

let describeHealth curHp maxHp =
    match curHp with
    | x when x = 0 -> "dead"
    | x when x = maxHp -> "full"
    | x when x <= maxHp / 2 -> "critical"
    | x when x > maxHp / 2 -> "injured"
    | _ -> ""

let applyAndDescribe curHp = describeHealth (clampHp curHp) 100

printfn "clamped: %d" (clampStat 0 10 100)
printfn "clamped Hp -100: %d" (clampHp -100)
printfn "clamped Hp 101: %d" (clampHp 101)
printfn "clamped Hp 99: %d" (clampHp 99)
printfn "Apply and describe Hp -100: %s" (applyAndDescribe -100)

let player1 = newPlayer "Sanay"
printfn "%A" player1

let greatSword = Item.Weapon(name = "Greatsword", damage = 10, kind = Sword)

player1 |> Entity.equipWeapon greatSword |> printfn "%A"
player1 |> Entity.prepToInventory (Item.Potion 15) |> printfn "%A"
player1 |> Entity.takeDamage 15 |> printfn "%A"
