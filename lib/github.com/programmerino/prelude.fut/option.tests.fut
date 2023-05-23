-- | ignore
import "option"

-- ==
-- entry: test_map
-- input { 5 } output { true }
entry test_map x =
    let input = #Some(x)
    let expected = #Some(10i32)
    let actual = Option.map (\x -> x * 2) input
    in
    actual == expected

-- ==
-- entry: test_flatmap
-- input { 5 } output { true }
entry test_flatmap x =
    let input = #Some(x)
    let expected = #Some(10i32)
    let actual = Option.flatmap (\x -> #Some(x * 2)) input
    in
    actual == expected

-- ==
-- entry: test_unwrap
-- input { 5 } output { 5 }
entry test_unwrap x = Option.unwrap 0i32 (#Some(x))

-- ==
-- entry: test_issome
-- input { 5 } output { true }
entry test_issome (x: i32) =
    let input = #Some(x)
    in
    Option.isSome input

-- ==
-- entry: test_isnone
-- input { } output { true }
entry test_isnone =
    let input = #None
    in
    Option.isNone input

-- ==
-- entry: test_defaultValue
-- input { 10 } output { 10 }
entry test_defaultValue (x: i32) =
    let input = #None
    in
    Option.defaultValue x input