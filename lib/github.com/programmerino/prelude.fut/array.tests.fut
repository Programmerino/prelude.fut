-- | ignore
import "array"
import "string"
import "option"
import "iarray"

-- ==
-- entry: test_to_string
-- input { [1u8, 2u8, 3u8, 4u8, 5u8] }
-- output { "1, 2, 3, 4, 5" }
entry test_to_string = Array.to_string (\x -> [String.int_to_digit x |> Option.unwrap '?'])

-- START OF BLIT

-- ==
-- entry: test_blit_i32
-- input { true }
-- output { true }
entry test_blit_i32 (_: bool) =
    let intSrc = (1...10)
    let intDes = replicate 10 0i32
    let intDes = Array.blit intSrc 0 intDes 0 5
    in
    assert (intDes[4] == 5) true
    && assert (intDes[5] == 0) true

-- ==
-- entry: test_blit_string
-- input { true }
-- output { true }
entry test_blit_string (_: bool) =
    let strSrc = "abcdej"
    let strDes = replicate 10 'w'
    let strDes = Array.blit strSrc 1 strDes 2 3
    in
    assert (strDes[3] == 'c') true
    && assert (strDes[4] != 'w') true

let intSrc = (1...10i32)
let intDes = Array.blit intSrc 0 (replicate 10 0i32) 0 5

-- ==
-- entry: test_blit_bounds1
-- input { true }
-- error: src_pos >= 0
entry test_blit_bounds1 (_: bool) =
    Array.blit intSrc (-1) (copy intDes) 1 3

-- ==
-- entry: test_blit_bounds2
-- input { true }
-- error: dst_pos >= 0
entry test_blit_bounds2 (_: bool) =
    Array.blit intSrc 1 (copy intDes) (-1) 3

-- ==
-- entry: test_blit_bounds3
-- input { true }
-- error: len >= 0
entry test_blit_bounds3 (_: bool) =
    Array.blit intSrc 1 (copy intDes) 1 (-3)

-- ==
-- entry: test_blit_bounds4
-- input { true }
-- error: len <= n - src_pos
entry test_blit_bounds4 (_: bool) =
    Array.blit intSrc 1 (copy intDes) 1 300

-- ==
-- entry: test_blit_bounds5
-- input { true }
-- error: len <= m - dst_pos
entry test_blit_bounds5 (_: bool) =
    Array.blit intSrc 1 (copy intDes) 5 8

-- END OF BLIT

-- ==
-- entry: test_blit_all
-- input { [1, 2, 3, 4, 5] 10i64 0 }
-- output { [1, 2, 3, 4, 5, 0, 0, 0, 0, 0] }
entry test_blit_all src n (v: i32) =
    let dst = replicate n v
    in
    Array.blit_all src 0 dst 0

-- ==
-- entry: test_indices_to_values
-- input { [1, 2, 3, 4, 5] [0i64, 2i64, 4i64] }
-- output { [1, 3, 5] }
entry test_indices_to_values (arr: []i32) indices = Array.indices_to_values arr indices

-- ==
-- entry: test_remove_indices
-- random input { [65535]i64 [32767]u16 }
-- auto output
entry test_remove_indices (vs: [65535]i64) (is: [32767]u16) =
    Array.remove_indices vs (map i64.u16 is)

-- ==
-- entry: test_windowed_i32
-- input { 1i64 [1i32, 2i32, 3i32, 4i32, 5i32, 6i32, 7i32, 8i32, 9i32, 10i32] }
-- output { [[1], [2], [3], [4], [5], [6], [7], [8], [9], [10]] }
-- input { 5i64 [1i32, 2i32, 3i32, 4i32, 5i32, 6i32, 7i32, 8i32, 9i32, 10i32] }
-- output { [[1, 2, 3, 4, 5], [2, 3, 4, 5, 6], [3, 4, 5, 6, 7], [4, 5, 6, 7, 8], [5, 6, 7, 8, 9], [6, 7, 8, 9, 10]] }
-- input { 10i64 [1i32, 2i32, 3i32, 4i32, 5i32, 6i32, 7i32, 8i32, 9i32, 10i32] }
-- output { [[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]] }
-- input { 25i64 [1i32, 2i32, 3i32, 4i32, 5i32, 6i32, 7i32, 8i32, 9i32, 10i32] }
-- output { empty([0][25]i32) }
-- input { 2i64 empty([0]i32) }
-- output { empty([0][2]i32) }
-- input { 0i64 [1i32, 2i32, 3i32, 4i32, 5i32, 6i32, 7i32, 8i32, 9i32, 10i32] }
-- error: w > 0
entry test_windowed_i32 [n] (w: i64) (xs: [n]i32) : [][w]i32 = Array.windowed w xs

-- ==
-- entry: test_differences
-- input { [1, 3, 5, 7] }
-- output { [2, 2, 2] }
entry test_differences: []i32 -> []i32 = Array.differences (-)

-- ==
-- entry: test_split_by
-- input { [1, 2, 3, 0, 4, 5, 0, 6, 7] 0i64 }
-- output { [1, 2, 3] }
-- input { [1, 2, 3, 0, 4, 5, 0, 6, 7] 1i64 }
-- output { [4, 5] }
-- input { [1, 2, 3, 0, 4, 5, 0, 6, 7] 2i64 }
-- output { [6, 7] }
entry test_split_by arr i =
    IArray.i (Array.split_by (\x -> x == 0i32) arr) i

-- ==
-- entry: test_iota_pairs
-- input { }
-- output { true }
entry test_iota_pairs =
    let n = 3
    let expected = [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2)]
    let actual = Array.iota_pairs n :> [9](i64, i64)
    in
    Array.equals (==) actual expected

-- ==
-- entry: test_cart_prod
-- input { [1i64, 2i64, 3i64] [4i64, 5i64, 6i64] }
-- output { true }
entry test_cart_prod xs ys =
    let expected = [(1, 4), (1, 5), (1, 6), (2, 4), (2, 5), (2, 6), (3, 4), (3, 5), (3, 6)]
    let actual = Array.cart_prod xs ys :> [9](i64, i64)
    in
    Array.equals (==) actual expected

-- ==
-- entry: test_equals
-- input { [1, 2, 3] [1, 2, 3] }
-- output { true }
entry test_equals: []i32 -> []i32 -> bool = Array.equals (==)

-- ==
-- entry: test_mapi
-- input { [1i32, 2i32, 3i32] }
-- output { true }
entry test_mapi arr =
    let expected = [(0i64, 1i32), (1, 2), (2, 3)]
    let actual = Array.mapi (\i x -> (i, x)) arr
    in
    Array.equals (==) actual expected

-- ==
-- entry: test_indexed
-- input { [1, 2, 3] }
-- output { true }
entry test_indexed arr =
    let expected = [(0, 1), (1, 2), (2, 3i32)]
    let actual = Array.indexed arr
    in
    Array.equals (==) actual expected