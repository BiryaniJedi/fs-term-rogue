module Types

type EntityKind =
    | Player
    | Goblin
    | Orc
    | Skeleton
    | Ghoul

type WeaponKind =
    | Sword
    | Dagger
    | Axe
    | Staff
    | Bow

module WeaponKind =
    let toString (kind: WeaponKind) : string =
        match kind with
        | Sword -> "Sword"
        | Dagger -> "Dagger"
        | Axe -> "Axe"
        | Staff -> "Staff"
        | Bow -> "Bow"

type Item =
    | Weapon of name: string * damage: int * kind: WeaponKind
    | Armor of name: string * defense: int
    | Potion of healAmount: int
    | Gold of amount: int

module Item =
    let itemName (item: Item) : string =
        match item with
        | Weapon(name, _, _) -> name
        | Armor(name, _) -> name
        | Potion amount -> sprintf "Potion (%d hp)" amount
        | Gold amount -> sprintf "Gold (%d)" amount

    let itemValue (item: Item) : int =
        match item with
        | Weapon(_, damage, _) -> damage * 3
        | Armor(_, defense) -> defense * 4
        | Potion amount -> amount * 2
        | Gold amount -> amount

    let isWeapon (item: Item) : bool =
        match item with
        | Weapon _ -> true
        | _ -> false

    let getWeight (item: Item) : int =
        match item with
        | Weapon _ -> 5
        | Armor _ -> 8
        | Potion _ -> 1
        | Gold _ -> 0

type Stats =
    { Strength: int
      Dexterity: int
      MaxHp: int
      CurHp: int }

module Stats =
    let defaultStats (maxHp: int) (strength: int) : Stats =
        { Strength = strength
          Dexterity = 5
          MaxHp = maxHp
          CurHp = maxHp }

type Entity =
    { Name: string
      Kind: EntityKind
      Stats: Stats
      Inventory: Item list
      EquippedWeapon: Item option
      EquippedArmor: Item option }

module Entity =
    let equipWeapon (weapon: Item) (entity: Entity) : Entity =
        { entity with
            EquippedWeapon = Some weapon }

    let prepToInventory (item: Item) (entity: Entity) : Entity =
        { entity with
            Inventory = item :: entity.Inventory }

    let takeDamage (amount: int) (entity: Entity) : Entity =
        let damagedStats =
            { entity.Stats with
                CurHp = entity.Stats.CurHp - amount }

        { entity with Stats = damagedStats }

    let (|Dead|Alive|) (entity: Entity) =
        match entity.Stats with
        | { CurHp = hp } when hp > 0 -> Alive
        | _ -> Dead

    let statusDescription (entity: Entity) : string =
        match entity.Stats with
        | { CurHp = 0 } -> "dead"
        | { CurHp = hp; MaxHp = max } when hp < max / 4 -> "critical"
        | { CurHp = hp; MaxHp = max } when hp < max * 3 / 4 -> "injured"
        | _ -> "healthy"

    let (|HasWeapon|Unarmed|) (entity: Entity) =
        match entity.EquippedWeapon with
        | Some(Weapon(_, damage, _)) -> HasWeapon damage
        | _ -> Unarmed

    let combatReady (entity: Entity) : string =
        match entity with
        | Alive & HasWeapon _ -> "ready"
        | Alive & Unarmed -> "unarmed"
        | Dead -> "dead"

    let inventoryValue (entity: Entity) : int =
        entity.Inventory |> List.sumBy Item.itemValue

    let setStrength (newStrength: int) (entity: Entity) : Entity =
        { entity with
            Stats =
                { entity.Stats with
                    Strength = newStrength } }

    let equippedDamage (entity: Entity) : int =
        entity.EquippedWeapon
        |> Option.bind (function
            | Weapon(_, damage, _) -> Some damage
            | _ -> None)
        |> Option.defaultValue 0

    let equippedDefense (entity: Entity) : int =
        entity.EquippedArmor
        |> Option.bind (function
            | Armor(_, defense) -> Some defense
            | _ -> None)
        |> Option.defaultValue 0

    let equippedWeapon (entity: Entity) : int =
        entity.EquippedWeapon
        |> Option.bind (function
            | Weapon(_, damage, _) -> Some damage
            | _ -> None)
        |> Option.defaultValue 0


let newPlayer (name: string) : Entity =
    { Name = name
      Kind = Player
      Stats = Stats.defaultStats 30 7
      Inventory = []
      EquippedWeapon = None
      EquippedArmor = None }

let newGoblin (name: string) : Entity =
    { Name = name
      Kind = Goblin
      Stats = Stats.defaultStats 8 3
      Inventory = [ Gold 2 ]
      EquippedWeapon = Some(Weapon("Goblin's dagger", 2, Dagger))
      EquippedArmor = Some(Armor("Goblin's rags", 7)) }

// Inventory queries
let countPotions (xs: Item list) : int =
    xs
    |> List.map (function
        | Potion _ -> 1
        | _ -> 0)
    |> List.fold (+) 0

let inline maxBy (fn: 'a -> 'b) (xs: 'a list) : 'a option when 'b: comparison =
    if List.length xs = 0 then
        None
    else
        let maxVal =
            xs
            |> List.fold
                (fun acc item ->
                    match acc, item with
                    | accItem, curItem when fn curItem > fn accItem -> item
                    | _ -> acc)
                (List.head xs)

        Some maxVal

let bestWeapon (xs: Item list) : Item option =
    xs
    |> List.filter (function
        | Weapon _ -> true
        | _ -> false)
    |> maxBy (function
        | Weapon(_, damage, _) -> damage
        | _ -> -1)

let totalCarryWeight (xs: Item list) : int =
    xs |> List.map Item.getWeight |> List.fold (+) 0

let removePotions (xs: Item list) : Item list =
    xs
    |> List.filter (function
        | Potion _ -> false
        | _ -> true)

// Entity queries
let livingEntities (xs: Entity list) : Entity list =
    xs
    |> List.filter (fun e ->
        match e with
        | Entity.Alive -> true
        | Entity.Dead -> false)

let deadEntities (xs: Entity list) : Entity list =
    xs
    |> List.filter (function
        | Entity.Alive -> false
        | Entity.Dead -> true)

let getEnemies (xs: Entity list) : Entity list =
    xs
    |> List.filter (function
        | { Kind = Player } -> false
        | _ -> true)

let strongestEnemy (xs: Entity list) : Entity option =
    xs |> getEnemies |> maxBy (fun e -> e.Stats.Strength)

let totalThreat (xs: Entity list) : int =
    xs |> getEnemies |> List.sumBy (fun e -> e.Stats.Strength)

let groupByKind (xs: Entity list) : Map<EntityKind, Entity list> =
    xs |> List.groupBy (fun e -> e.Kind) |> Map.ofList

let tryFindItemByName name items =
    items
    |> List.filter (function
        | Weapon _ -> true
        | Armor _ -> true
        | _ -> false)
    |> List.tryFind (function
        | Weapon(curName, _, _) -> curName = name
        | Armor(curName, _) -> curName = name
        | _ -> false)

// Combat?
type CombatError =
    | TargetAlreadyDead
    | AttackerAlreadyDead

let attack
    (attacker: Entity)
    (defender: Entity)
    : Result<Entity * Entity, CombatError list> =
    let pushErrorGiven
        (e: Entity)
        (fn: Entity -> bool)
        (err: CombatError)
        (xs: CombatError list)
        : CombatError list =
        if fn e then err :: xs else xs

    let errors =
        []
        |> pushErrorGiven
            attacker
            (fun e -> e.Stats.CurHp <= 0)
            AttackerAlreadyDead
        |> pushErrorGiven
            defender
            (fun e -> e.Stats.CurHp <= 0)
            TargetAlreadyDead


    if not (List.isEmpty errors) then
        Error errors
    else
        let attackDmg = attacker |> Entity.equippedDamage
        let defense = defender |> Entity.equippedDefense
        let netDmg = max 0 attackDmg - defense
        Ok(attacker, Entity.takeDamage netDmg defender)

let simulateCombatRound
    (attacker: Entity)
    (defender: Entity)
    : Result<Entity * Entity, CombatError list> =
    attack attacker defender
    |> Result.bind (fun res -> attack (snd res) (fst res))
