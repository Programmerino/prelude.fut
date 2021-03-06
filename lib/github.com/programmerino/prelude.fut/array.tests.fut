-- | ignore
import "array"
import "string"

-- START OF DISTINCT_BY

-- ==
-- entry: test_distinct_by_failfun
-- input { empty([0]i32) }
-- output { empty([0]i32) }
entry test_distinct_by_failfun (xs: []i32): []i32 = Array.distinct_by 0 (\_ _ -> assert false true) xs

-- ==
-- entry: test_distinct_by_id
-- input { [1] }
-- output { [1] }
-- input { [1, 1] }
-- output { [1] }
-- input { [1, 2, 3, 1] }
-- output { [1, 2, 3] }
entry test_distinct_by_id (xs: []i32): []i32 = Array.distinct_by 0 (\x y -> x == y) xs

let distinct_by_proj (xs: []i32) (proj: i32 -> i32): []i32 =
    Array.distinct_by 0 (\x y -> (proj x) == (proj y)) xs

-- ==
-- entry: test_distinct_by_div
-- input { [1, 2] }
-- output { [1] }
entry test_distinct_by_div (xs: []i32): []i32 = distinct_by_proj xs (\x -> x / x)

-- ==
-- entry: test_distinct_by_less
-- input { [1, 2, 3, 4] }
-- output { [1, 2] }
entry test_distinct_by_less (xs: []i32): []i32 = distinct_by_proj xs (\x -> if x < 3 then x else 1)

-- irregularity prevents this
-- -- ==
-- -- entry: test_distinct_by_sum
-- -- input { [[1,2], [1,3], [2,1]] }
-- -- output { [[1, 2], [1, 3]] }

-- entry test_distinct_by_sum (xs: [3][2]i32): [2][2]i32 =
--     let proj x = i32.sum x
--     in
--     Array.distinct_by [] (\x y -> (proj x) == (proj y)) xs

-- END OF DISTINCT_BY

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

-- START OF WINDOWED

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
-- entry: test_windowed_str
-- input { 2i64 [[115u8, 116u8, 114u8, 49u8], [115u8, 116u8, 114u8, 50u8], [115u8, 116u8, 114u8, 51u8], [115u8, 116u8, 114u8, 52u8]] }
-- output { [[[115u8, 116u8, 114u8, 49u8], [115u8, 116u8, 114u8, 50u8]], [[115u8, 116u8, 114u8, 50u8], [115u8, 116u8, 114u8, 51u8]], [[115u8, 116u8, 114u8, 51u8], [115u8, 116u8, 114u8, 52u8]]] }
entry test_windowed_str [n] (w: i64) (xs: []string[n]) = Array.windowed w xs

-- END OF WINDOWED