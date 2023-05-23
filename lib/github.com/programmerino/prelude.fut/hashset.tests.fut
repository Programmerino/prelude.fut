-- | ignore

import "hashset"
import "array"
import "../../../../lib/github.com/diku-dk/sorts/radix_sort"

module Set = LPSeti64

-- ==
-- entry: test_empty
-- input { true }
-- output { true }
entry test_empty (_: bool) =
    let hs = Set.empty
    let cond1 = Set.length hs == 0
    in
    assert cond1 cond1

-- ==
-- entry: test_add
-- input { true }
-- output { true }
entry test_add (_: bool) =
    let empty = Set.empty
    let x = Set.add 0 (copy empty)
    let xy = Set.add 1 x
    let xyz = Set.add 2 (copy xy)
    let wxyz = Set.add 3 (copy xyz)
    let cond1 = Set.length xy == 2
    let cond2 = Set.length xyz == 3
    let cond3 = Set.length wxyz == 4
    in assert cond1 cond1 && assert cond2 cond2 && assert cond3 cond3

-- ==
-- entry: test_exists
-- input { true }
-- output { true }
entry test_exists (_: bool) =
    let empty = Set.empty
    let odds = Set.multiadd (1 .. 3 ... 11) (copy empty)
    let cond1 = Set.exists 6 odds == false
    let cond2 = Set.exists 7 odds == true
    in assert cond1 cond1 && assert cond2 cond2

-- ==
-- entry: test_length
-- input { true }
-- output { true }
entry test_length (_: bool) =
    let empty = Set.empty
    let one = Set.add 1 (copy empty)
    let multi = Set.multiadd ('a' ... 'z') (copy empty)
    let cond1 = Set.length empty == 0
    let cond2 = Set.length one == 1
    let cond3 = trace (Set.length multi) == 26
    in assert cond1 cond1 && assert cond2 cond2 && assert cond3 cond3


-- ==
-- entry: test_delete
-- input { true }
-- output { true }
entry test_delete (_: bool) =
    let result = Set.delete 42 (copy Set.empty)
    let single = Set.add 100 (copy Set.empty)
    let resulta = Set.delete 100 (copy single)
    let resultb = Set.delete 1 single
    let a = Set.multiadd (1 ... 5) (copy Set.empty)
    let b = Set.delete 3 (copy a)
    let c = Set.delete 3 (copy b)
    let cond1 = Set.length result == 0
    let cond2 = Set.length resulta == 0
    let cond3 = Set.length resultb == 1
    let cond4 = Set.length a == 5
    let cond5 = Set.length b == 4
    let cond6 = Set.length c == 4
    let cond7 = Set.exists 3 c == false
    in assert cond1 cond1 && assert cond2 cond2 && assert cond3 cond3 && assert cond4 cond4 && assert cond5 cond5 && assert cond6 cond6 && assert cond7 cond7
    -- && assert (a2 == b) true

-- ==
-- entry: test_general
-- compiled random input { [65535]i64 [20]u8 }
-- output { true }
entry test_general (elems: [65535]i64) (rmv_list: [20]u8) =
    let ht = Set.multiadd elems (copy Set.empty)
    let sorted_pre_keys = radix_sort_int i64.num_bits i64.get_bit (Set.get_keys ht)
    let pre_length = length sorted_pre_keys
    let sorted_pre_keys = sorted_pre_keys :> [pre_length]i64
    let sorted_pre_input = radix_sort_int i64.num_bits i64.get_bit elems :> [pre_length]i64
    let ht_remov =
        loop ht for i in rmv_list do
            Set.delete elems[i64.u8 i] ht
    let sorted_keys = radix_sort_int i64.num_bits i64.get_bit (Set.get_keys ht_remov)
    let post_length = length sorted_keys
    let sorted_keys = sorted_keys :> [post_length]i64
    let sorted_input = radix_sort_int i64.num_bits i64.get_bit (Array.remove_indices elems (map (i64.u8) rmv_list)) :> [post_length]i64
    in
    assert (Array.equals (==) sorted_input sorted_keys) (Array.equals (==) sorted_input sorted_keys)
    && assert (Array.equals (==) sorted_pre_input sorted_pre_keys) (Array.equals (==) sorted_pre_input sorted_pre_keys)
