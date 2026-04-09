module Tests

open Xunit
open Types

// ── helpers ─────────────────────────────────────────────────────────────────

/// Entity with MaxHp=20 and the given CurHp; no inventory, no weapon.
let private makeEntity curHp =
    { Name = "Test"
      Kind = Goblin
      Stats =
        { Strength = 5
          Dexterity = 5
          MaxHp = 20
          CurHp = curHp }
      Inventory = []
      EquippedWeapon = None
      EquippedArmor = None }

let private withWeapon item e = { e with EquippedWeapon = Some item }
let private withArmor item e = { e with EquippedArmor = Some item }
let private withInventory items e = { e with Inventory = items }

// ── Item.itemName ────────────────────────────────────────────────────────────

[<Fact>]
let ``itemName: Weapon returns its name`` () =
    Assert.Equal("Iron Sword", Item.itemName (Weapon("Iron Sword", 5, Sword)))

[<Fact>]
let ``itemName: Armor returns its name`` () =
    Assert.Equal("Leather Vest", Item.itemName (Armor("Leather Vest", 3)))

[<Fact>]
let ``itemName: Potion includes heal amount`` () =
    Assert.Equal("Potion (10 hp)", Item.itemName (Potion 10))

[<Fact>]
let ``itemName: Gold includes amount`` () =
    Assert.Equal("Gold (50)", Item.itemName (Gold 50))

// ── Item.itemValue ───────────────────────────────────────────────────────────

[<Fact>]
let ``itemValue: Weapon is damage * 3`` () =
    Assert.Equal(15, Item.itemValue (Weapon("Sword", 5, Sword)))

[<Fact>]
let ``itemValue: Armor is defense * 4`` () =
    Assert.Equal(12, Item.itemValue (Armor("Shield", 3)))

[<Fact>]
let ``itemValue: Potion is healAmount * 2`` () =
    Assert.Equal(20, Item.itemValue (Potion 10))

[<Fact>]
let ``itemValue: Gold is face value`` () =
    Assert.Equal(7, Item.itemValue (Gold 7))

[<Fact>]
let ``itemValue: zero-damage weapon is worth 0`` () =
    Assert.Equal(0, Item.itemValue (Weapon("Fists", 0, Sword)))

// ── Item.isWeapon ────────────────────────────────────────────────────────────

[<Fact>]
let ``isWeapon: true for Weapon`` () =
    Assert.True(Item.isWeapon (Weapon("Sword", 5, Sword)))

[<Fact>]
let ``isWeapon: false for Armor`` () =
    Assert.False(Item.isWeapon (Armor("Shield", 3)))

[<Fact>]
let ``isWeapon: false for Potion`` () = Assert.False(Item.isWeapon (Potion 5))

[<Fact>]
let ``isWeapon: false for Gold`` () = Assert.False(Item.isWeapon (Gold 10))

// ── Entity.statusDescription ─────────────────────────────────────────────────

// MaxHp = 20. Thresholds: dead=0, critical<25%(5), injured<75%(15), healthy>=75%.

[<Fact>]
let ``statusDescription: dead at 0 HP`` () =
    Assert.Equal("dead", Entity.statusDescription (makeEntity 0))

[<Fact>]
let ``statusDescription: critical below 25 percent`` () =
    // MaxHp=20, 25% threshold = 20/4 = 5. CurHp=4 is below that.
    Assert.Equal("critical", Entity.statusDescription (makeEntity 4))

[<Fact>]
let ``statusDescription: critical at 1 HP`` () =
    Assert.Equal("critical", Entity.statusDescription (makeEntity 1))

[<Fact>]
let ``statusDescription: injured between 25 and 75 percent`` () =
    // MaxHp=20, CurHp=10 = 50% — should be "injured"
    Assert.Equal("injured", Entity.statusDescription (makeEntity 10))

[<Fact>]
let ``statusDescription: healthy at full HP`` () =
    Assert.Equal("healthy", Entity.statusDescription (makeEntity 20))

[<Fact>]
let ``statusDescription: healthy well above 75 percent`` () =
    // MaxHp=20, CurHp=16 = 80%
    Assert.Equal("healthy", Entity.statusDescription (makeEntity 16))

// ── Entity.combatReady ───────────────────────────────────────────────────────

[<Fact>]
let ``combatReady: dead when HP is 0`` () =
    Assert.Equal("dead", Entity.combatReady (makeEntity 0))

[<Fact>]
let ``combatReady: dead when HP is negative`` () =
    Assert.Equal("dead", Entity.combatReady (makeEntity -3))

[<Fact>]
let ``combatReady: unarmed when alive with no weapon`` () =
    Assert.Equal("unarmed", Entity.combatReady (makeEntity 10))

[<Fact>]
let ``combatReady: unarmed when alive with non-weapon equipped`` () =
    let e = makeEntity 10 |> withWeapon (Armor("Plate", 5))
    Assert.Equal("unarmed", Entity.combatReady e)

[<Fact>]
let ``combatReady: ready when alive and weapon equipped`` () =
    let e = makeEntity 10 |> withWeapon (Weapon("Axe", 5, Axe))
    Assert.Equal("ready", Entity.combatReady e)

[<Fact>]
let ``combatReady: dead takes precedence over having a weapon`` () =
    // Dead entity with a weapon should still be "dead", not "ready"
    let e = makeEntity 0 |> withWeapon (Weapon("Sword", 5, Sword))
    Assert.Equal("dead", Entity.combatReady e)

// ── Entity.inventoryValue ────────────────────────────────────────────────────

[<Fact>]
let ``inventoryValue: 0 for empty inventory`` () =
    Assert.Equal(0, Entity.inventoryValue (makeEntity 10))

[<Fact>]
let ``inventoryValue: sums all item values`` () =
    // Weapon(5dmg)=15, Gold(7)=7, Potion(3)=6 → total=28
    let e =
        makeEntity 10
        |> withInventory [ Weapon("Sword", 5, Sword); Gold 7; Potion 3 ]

    Assert.Equal(28, Entity.inventoryValue e)

[<Fact>]
let ``inventoryValue: does not include equipped weapon`` () =
    // Equipped weapon lives in EquippedWeapon, not Inventory
    let sword = Weapon("Sword", 5, Sword)
    let e = makeEntity 10 |> withWeapon sword |> withInventory [ Gold 10 ]
    Assert.Equal(10, Entity.inventoryValue e)

[<Fact>]
let ``inventoryValue: single gold coin`` () =
    let e = makeEntity 10 |> withInventory [ Gold 1 ]
    Assert.Equal(1, Entity.inventoryValue e)


// ========== Types.countPotions
[<Fact>]
let ``countPotions: 0 for empty item list`` () =
    let xs: Item list = []
    Assert.Equal(0, countPotions xs)

[<Fact>]
let ``countPotions: 0 for item list with no potions`` () =
    let xs =
        [ Weapon("Test", 0, Sword)
          Armor("Test", 1000)
          Gold 10
          Gold 17
          Gold 67
          Weapon("Test2", 0, Axe) ]

    Assert.Equal(0, countPotions xs)

[<Fact>]
let ``countPotions: 1 for item list with only 1 potion`` () =
    let xs = [ Potion 10 ]

    Assert.Equal(1, countPotions xs)

[<Fact>]
let ``countPotions: 1 for item list with 1 potion and other items`` () =
    let xs =
        [ Potion 10; Weapon("Test", 0, Sword); Armor("Test", 10); Gold 69420 ]

    Assert.Equal(1, countPotions xs)

// Types.maxBy

[<Fact>]
let ``maxBy: Correct for list of weapons`` () =
    let maxWeapon =
        [ Weapon("Test Sword", 10, Sword)
          Weapon("Test Axe", 11, Axe)
          Weapon("Test Dagger", 69420, Dagger) ]
        |> maxBy (function
            | Weapon(_, damage, _) -> damage
            | _ -> -1)
        |> Option.get

    Assert.Equal(Weapon("Test Dagger", 69420, Dagger), maxWeapon)

[<Fact>]
let ``maxBy: None for empty list`` () =
    let maxWeapon =
        []
        |> maxBy (function
            | Weapon(_, damage, _) -> damage
            | _ -> -1)

    Assert.Equal(None, maxWeapon)

// Types.bestWeapon
[<Fact>]
let ``bestWeapon: Correct for list of weapons and other items`` () =
    let maxWeapon =
        [ Weapon("Test Sword", 10, Sword)
          Weapon("Test Axe", 11, Axe)
          Weapon("Test Dagger", 69420, Dagger)
          Potion 15
          Gold 69420
          Armor("Yo Test", 49)
          Weapon("Test Bow", 6942067, Bow) ]
        |> bestWeapon

    Assert.Equal(Some(Weapon("Test Bow", 6942067, Bow)), maxWeapon)

[<Fact>]
let ``bestWeapon: None for empty list`` () =
    let maxWeapon = [] |> bestWeapon

    Assert.Equal(None, maxWeapon)

// Types.totalCarryWeight
[<Fact>]
let ``totalCarryWeight: 0 for empty list`` () =
    let carryWeight = [] |> totalCarryWeight

    Assert.Equal(0, carryWeight)

[<Fact>]
let ``totalCarryWeight: Correct for list of items`` () =
    let carryWeight =
        [ Weapon("Test", 10, Sword)
          Armor("Test", 16)
          Gold 10
          Gold 11
          Potion 12 ]
        |> totalCarryWeight

    Assert.Equal(14, carryWeight)

// Types.removePotions
[<Fact>]
let ``removePotions: returns empty list on empty list`` () =
    let noPotions = [] |> removePotions

    Assert.Empty noPotions

[<Fact>]
let ``removePotions: returns empty list on list with just potions`` () =
    let noPotions = [ Potion 12; Potion 13; Potion -1 ] |> removePotions

    Assert.Empty noPotions

[<Fact>]
let ``removePotions: returned list contains no potions, but isn't empty`` () =
    let noPotions =
        [ Potion 12
          Potion 13
          Potion -1
          Weapon("Test", 15, Sword)
          Gold 15
          Armor("Test", 14) ]
        |> removePotions

    Assert.All(
        noPotions,
        fun item ->
            Assert.True(
                match item with
                | Potion _ -> false
                | _ -> true
            )
    )

    Assert.NotEmpty noPotions

// Types.livingEntities and Types.deadEntities
let deadPlayer () =
    let player = newPlayer "Test"

    { player with
        Stats = { player.Stats with CurHp = 0 } }

let deadGoblin () =
    let goblin = newGoblin "Test"

    { goblin with
        Stats = { goblin.Stats with CurHp = 0 } }

[<Fact>]
let ``livingEntities: returns an empty list when given an empty list`` () =
    let living = [] |> livingEntities

    Assert.Empty living

[<Fact>]
let ``livingEntities: returns an empty list when given a list of dead players and goblins``
    ()
    =
    let living =
        [ deadPlayer ()
          deadPlayer ()
          deadGoblin ()
          deadPlayer ()
          deadGoblin () ]
        |> livingEntities

    Assert.Empty living

[<Fact>]
let ``livingEntities: returns list of only living entities`` () =
    let living =
        [ newPlayer "Test"; deadPlayer (); deadGoblin (); newGoblin "Test" ]
        |> livingEntities

    Assert.Equal(2, List.length living)

    Assert.All(
        living,
        fun e ->
            Assert.True(
                match e with
                | Entity.Alive -> true
                | Entity.Dead -> false
            )
    )

[<Fact>]
let ``deadEntities: returns an empty list when given an empty list`` () =
    let dead = [] |> deadEntities

    Assert.Empty dead

[<Fact>]
let ``deadEntities: returns an empty list when given a list of living players and goblins``
    ()
    =
    let dead =
        [ newPlayer "Test"
          newPlayer "Test"
          newGoblin "Test"
          newGoblin "Test"
          newPlayer "Test" ]
        |> deadEntities

    Assert.Empty dead

[<Fact>]
let ``deadEntities: returns list of only dead entities`` () =
    let dead =
        [ newPlayer "Test"; deadPlayer (); deadGoblin (); newGoblin "Test" ]
        |> deadEntities

    Assert.Equal(2, List.length dead)

    Assert.All(
        dead,
        fun e ->
            Assert.True(
                match e with
                | Entity.Alive -> false
                | Entity.Dead -> true
            )
    )

// Types.strongestEnemy
[<Fact>]
let ``strongestEnemy: returns None on empty list`` () =
    let strongest = [] |> strongestEnemy

    Assert.Equal(None, strongest)

[<Fact>]
let ``strongestEnemy: returns None on list with only players`` () =
    let strongest =
        [ newPlayer "Test"; newPlayer "Test2"; newPlayer "Test3" ]
        |> strongestEnemy

    Assert.Equal(None, strongest)

[<Fact>]
let ``strongestEnemy: proper output regular case`` () =
    let strongest =
        [ newPlayer "Test"
          newGoblin "Test2"
          (newGoblin >> Entity.setStrength 100) "Test2"
          newPlayer "Test3" ]
        |> strongestEnemy
        |> Option.get

    Assert.Equal("Test2", strongest.Name)
    Assert.Equal(100, strongest.Stats.Strength)

// Types.getEnemies
[<Fact>]
let ``getEnemies: empty on empty list`` () =
    let enemies = [] |> getEnemies

    Assert.Empty enemies

[<Fact>]
let ``getEnemies: empty on list of only players`` () =
    let enemies = [ newPlayer "Test"; deadPlayer () ] |> getEnemies

    Assert.Empty enemies

[<Fact>]
let ``getEnemies: only returns enemies from mixed entity list`` () =
    let enemies =
        [ newPlayer "Test"; newGoblin "Test"; deadGoblin () ] |> getEnemies

    Assert.Equal(2, List.length enemies)

    Assert.All(
        enemies,
        fun e ->
            Assert.True(
                match e.Kind with
                | Player -> false
                | _ -> true
            )
    )

// Types.totalThreat
[<Fact>]
let ``totalThreat: returns 0 on empty list`` () =
    let total = [] |> totalThreat

    Assert.Equal(0, total)

[<Fact>]
let ``totalThreat: returns 0 on list of only players`` () =
    let total =
        [ newPlayer "Test"
          newPlayer "Test"
          newPlayer "Test"
          newPlayer "You thought I was gonna write test again didn't you" ]
        |> totalThreat

    Assert.Equal(0, total)

[<Fact>]
let ``totalThreat: proper output regular case`` () =
    let total =
        [ newPlayer "Test"
          newPlayer "Test"
          newPlayer "Test"
          newPlayer "You thought I was gonna write test again didn't you"
          newGoblin "Test"
          (newGoblin >> Entity.setStrength 100) "Test"
          (newGoblin >> Entity.setStrength 200) "Test" ]
        |> totalThreat

    Assert.Equal(303, total)

// Types.groupByKind
[<Fact>]
let ``groupByKind: empty on empty input`` () =
    let grouped = [] |> groupByKind

    Assert.Empty grouped

let allKindsMatch (kind: EntityKind) (xs: Entity list) : bool =
    xs |> List.forall (fun e -> e.Kind = kind)

[<Fact>]
let ``groupByKind: proper amount of keys and types match on only players`` () =
    let grouped = [ deadPlayer (); deadPlayer () ] |> groupByKind

    Assert.Equal(1, grouped.Count)
    Assert.True(grouped.ContainsKey EntityKind.Player)

    Assert.Equal(true, allKindsMatch Player grouped[Player])

[<Fact>]
let ``groupByKind: proper amount of keys and types match on mixed`` () =
    let grouped =
        [ deadPlayer (); deadPlayer (); deadGoblin (); deadGoblin () ]
        |> groupByKind

    Assert.Equal(2, grouped.Count)
    Assert.True(grouped.ContainsKey Player)
    Assert.True(grouped.ContainsKey Goblin)

    Assert.Equal(true, allKindsMatch Player grouped[Player])
    Assert.Equal(true, allKindsMatch Goblin grouped[Goblin])

    Assert.Equal(2, List.length grouped[Player])
    Assert.Equal(2, List.length grouped[Goblin])

// Types.tryFindItemByName

[<Fact>]
let ``tryFindItemByName: returns None on empty input`` () =
    let found = [] |> tryFindItemByName "name"

    Assert.Equal(None, found)

[<Fact>]
let ``tryFindItemByName: returns None on no match with proper input`` () =
    let found =
        [ Weapon("Test", 10, Sword); Gold 10; Potion 67; Armor("Not name", 12) ]
        |> tryFindItemByName "name"

    Assert.Equal(None, found)

[<Fact>]
let ``tryFindItemByName: returns proper match with proper input`` () =
    let found =
        [ Weapon("name", 10, Sword); Gold 10; Potion 67; Armor("Not name", 12) ]
        |> tryFindItemByName "name"

    Assert.Equal(Some(Weapon("name", 10, Sword)), found)

// Types.attack
[<Fact>]
let ``attack: Correct error on dead attacker`` () =
    let player, goblin = deadPlayer (), newGoblin "Test"

    match attack player goblin with
    | Ok(p, g) -> Assert.Fail $"Expected {AttackerAlreadyDead}, got ({p}, {g})"
    | Error errors ->
        match errors with
        | [ error ] ->
            match error with
            | err when err <> AttackerAlreadyDead ->
                Assert.Fail $"Got {err} Expected {AttackerAlreadyDead}"
            | _ -> ()
        | _ ->
            Assert.Fail
                $"Got the wrong amount of errors, expected = 1 got={List.length errors}"

[<Fact>]
let ``attack: Correct error on dead target`` () =
    let player, goblin = newPlayer "Test", deadGoblin ()

    match attack player goblin with
    | Ok(p, g) -> Assert.Fail $"Expected {TargetAlreadyDead}, got ({p}, {g})"
    | Error errors ->
        match errors with
        | [ error ] ->
            match error with
            | err when err <> TargetAlreadyDead ->
                Assert.Fail $"Got {err} Expected {TargetAlreadyDead}"
            | _ -> ()
        | _ ->
            Assert.Fail
                $"Got the wrong amount of errors, expected = 1 got={List.length errors}"

[<Fact>]
let ``attack: Correct errors on dead attacker and target`` () =
    let player, goblin = deadPlayer (), deadGoblin ()

    match attack player goblin with
    | Ok(p, g) -> Assert.Fail $"Expected errors, got ({p}, {g})"
    | Error errors ->
        match errors with
        | [ error1; error2 ] ->
            match error1, error2 with
            | err1, err2 when
                err1 <> TargetAlreadyDead && err2 <> AttackerAlreadyDead
                ->
                let message =
                    $"Got ${[ err1; err2 ]} Expected {[ AttackerAlreadyDead; TargetAlreadyDead ]}" in

                Assert.Fail message
            | _ -> ()
        | _ ->
            Assert.Fail
                $"Got the wrong amount of errors, expected = 1 got={List.length errors}"

// Types.simulateCombatRound
//
// NOTE: simulateCombatRound has a tuple-swap bug.
// `attack a b` returns Ok(a, damagedB).
// The bind then does `attack (snd res) (fst res)` = `attack damagedB a`
// which returns Ok(damagedB, damagedA).
// So the final Ok tuple is (originalDefender, originalAttacker) — swapped.
// Tests below document actual behavior. The fix is to swap fst/snd in the bind,
// or return a named record instead of a raw tuple.

let private armedEntity name hp dmg =
    makeEntity hp
    |> fun e -> { e with Name = name }
    |> withWeapon (Weapon("Sword", dmg, Sword))

let private armedEntityWithArmor name hp dmg def =
    armedEntity name hp dmg |> withArmor (Armor("Shield", def))

[<Fact>]
let ``simulateCombatRound: Error when attacker is already dead`` () =
    let dead = makeEntity 0
    let alive = armedEntity "Alive" 20 5
    match simulateCombatRound dead alive with
    | Ok _ -> Assert.Fail "Expected Error, got Ok"
    | Error errors -> Assert.Contains(AttackerAlreadyDead, errors)

[<Fact>]
let ``simulateCombatRound: Error when defender is already dead`` () =
    let alive = armedEntity "Alive" 20 5
    let dead = makeEntity 0
    match simulateCombatRound alive dead with
    | Ok _ -> Assert.Fail "Expected Error, got Ok"
    | Error errors -> Assert.Contains(TargetAlreadyDead, errors)

[<Fact>]
let ``simulateCombatRound: Error when both are already dead`` () =
    let dead1 = makeEntity 0
    let dead2 = makeEntity 0
    match simulateCombatRound dead1 dead2 with
    | Ok _ -> Assert.Fail "Expected Error, got Ok"
    | Error errors ->
        Assert.Contains(AttackerAlreadyDead, errors)
        Assert.Contains(TargetAlreadyDead, errors)

[<Fact>]
let ``simulateCombatRound: Ok when both survive the round`` () =
    // Attacker: 20 HP, 5 dmg. Defender: 20 HP, 3 dmg. No armor.
    // Round 1: defender takes 5 dmg → 15 HP
    // Round 2: damagedDefender (5 HP gone, 3 dmg) attacks attacker → attacker takes 3 dmg → 17 HP
    // Result tuple is SWAPPED: Ok(damagedDefender, damagedAttacker)
    let attacker = armedEntity "Attacker" 20 5
    let defender = armedEntity "Defender" 20 3
    match simulateCombatRound attacker defender with
    | Error e -> Assert.Fail $"Expected Ok, got Error {e}"
    | Ok(first, second) ->
        // first = original defender (now damaged by attacker's hit)
        Assert.Equal("Defender", first.Name)
        Assert.Equal(15, first.Stats.CurHp)
        // second = original attacker (now damaged by defender's counter)
        Assert.Equal("Attacker", second.Name)
        Assert.Equal(17, second.Stats.CurHp)

[<Fact>]
let ``simulateCombatRound: armor reduces damage in both directions`` () =
    // Attacker: 20 HP, 8 dmg. Defender: 20 HP, 6 dmg, 3 defense.
    // Round 1: defender takes max(0, 8-3)=5 dmg → 15 HP
    // Round 2: damagedDefender (6 dmg) attacks attacker (no armor) → attacker takes 6 dmg → 14 HP
    let attacker = armedEntity "Attacker" 20 8
    let defender = armedEntityWithArmor "Defender" 20 6 3
    match simulateCombatRound attacker defender with
    | Error e -> Assert.Fail $"Expected Ok, got Error {e}"
    | Ok(first, second) ->
        Assert.Equal(15, first.Stats.CurHp)  // defender took 5 net
        Assert.Equal(14, second.Stats.CurHp) // attacker took 6 (no armor)

[<Fact>]
let ``simulateCombatRound: Error when attacker kills defender in first hit`` () =
    // Attacker hits for more than defender's HP.
    // Round 1 succeeds: defender CurHp goes <= 0.
    // Round 2: dead defender tries to counter → Error AttackerAlreadyDead.
    // This is the railway tradeoff: the bind short-circuits on the counter's error.
    let attacker = armedEntity "Attacker" 20 25  // 25 dmg, enough to one-shot
    let defender = makeEntity 10                  // 10 HP, no weapon, no armor
    match simulateCombatRound attacker defender with
    | Ok _ -> Assert.Fail "Expected Error (dead defender cannot counter-attack)"
    | Error errors -> Assert.Contains(AttackerAlreadyDead, errors)

[<Fact>]
let ``simulateCombatRound: original entities are not mutated`` () =
    let attacker = armedEntity "Attacker" 20 5
    let defender = armedEntity "Defender" 20 3
    let _ = simulateCombatRound attacker defender
    // F# is immutable so this is guaranteed, but worth asserting explicitly
    Assert.Equal(20, attacker.Stats.CurHp)
    Assert.Equal(20, defender.Stats.CurHp)
