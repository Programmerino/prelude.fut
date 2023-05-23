-- | ignore

import "iarray"
import "array"

-- ==
-- entry: test_from_array_lengths
-- input { [1,2,3,4,5] [2i64,3i64] } output { true }
entry test_from_array_lengths (xs: []i32) (ys: []i64) =
  let res = IArray.from_array_lengths xs ys
  in
  Array.equals (==) res.data [1,2,3,4,5] && Array.equals (==) res.lengths [2, 3]

-- ==
-- entry: test_from_array_flags
-- input { [1,2,3,4,5] [true, false, true, false, false] } output { true }
entry test_from_array_flags (xs: []i32) (ys: []bool) =
  let res = IArray.from_array_flags xs ys
  in
  Array.equals (==) res.data [1,2,3,4,5] && Array.equals (==) (res.lengths :> [2]i64) [2,3]

-- ==
-- entry: test_singleton
-- input { [1,2,3] } output { true }
entry test_singleton (xs: []i32) =
  let res = IArray.singleton xs
  in
  Array.equals (==) res.data [1,2,3] && Array.equals (==) res.lengths [3]

-- ==
-- entry: test_concat
-- input { [1,2,3] [2i64] [4,5,6] [1i64,2i64] } output { true }
entry test_concat (a: []i32) b c d =
  let res = IArray.concat (IArray.from_array_lengths a b) (IArray.from_array_lengths c d)
  in
  Array.equals (==) (res.data :> [6]i32) [1,2,3,4,5,6] && Array.equals (==) (res.lengths :> [3]i64) [2,1,2]

-- ==
-- entry: test_i
-- input { [1,2,3,4,5] [2i64,3i64] 1i64 } output { [3,4,5] }
entry test_i (xs: []i32) (lens: []i64) i = IArray.i (IArray.from_array_lengths xs lens) i

-- ==
-- entry: test_i_to
-- input { [1,2,3,4,5] [2i64,3i64] 3i64 1i64 } output { [3,4,5] }
entry test_i_to (xs: []i32) (lens: []i64) to i = IArray.i_to (IArray.from_array_lengths xs lens) to i

-- ==
-- entry: test_map_all
-- input { [1,2,3,4,5] [2i64,3i64] 1 } output { true }
entry test_map_all (xs: []i32) (lens: []i64) n =
  let arr = IArray.from_array_lengths xs lens
  let arr = IArray.map_all (\x -> x + n) arr
  in
  arr == {
    data = [2,3,4,5,6],
    lengths = [2, 3]
  }

-- ==
-- entry: test_generate_flagged_segments
-- input { [1,2,3,4,5] [2i64,3i64] } output { true }
entry test_generate_flagged_segments (xs: []i32) (lens: []i64) =
  let arr = IArray.from_array_lengths xs lens
  in
  (IArray.generate_flagged_segments arr) == ([true, false, true, false, false], [1,2,3,4,5])

-- ==
-- entry: test_length
-- input { [1,2,3,4,5] [2i64,3i64] } output { 2i64 }
entry test_length (xs: []i32) (lens: []i64) =
  let arr = IArray.from_array_lengths xs lens
  in
  IArray.length arr

-- ==
-- entry: test_from_array_starts
-- input { [1,2,3,4,5] [0i64,2i64,4i64] } output { true }
entry test_from_array_starts (xs: []i32) (starts: []i64) =
  let arr = IArray.from_array_starts xs starts :> iarray[][3]i32
  in
  arr == {
    data = [1,2,3,4,5],
    lengths = [2,2,1]
  }

-- ==
-- entry: test_segmented_scan
-- input { [true,false,false,true,false,false,true,false,false,false]
--         [1i64,2i64,3i64,4i64,5i64,6i64,7i64,8i64,9i64,10i64] 0i64 }
-- output { [1i64,3i64,6i64] }
-- input { [true,false,false,true,false,false,true,false,false,false]
--         [1i64,2i64,3i64,4i64,5i64,6i64,7i64,8i64,9i64,10i64] 1i64 }
-- output { [4i64,9i64,15i64] }
-- input { [true,false,false,true,false,false,true,false,false,false]
--         [1i64,2i64,3i64,4i64,5i64,6i64,7i64,8i64,9i64,10i64] 2i64 }
-- output { [7i64,15i64,24i64,34i64] }
-- input { [true] [1i64] 0i64 }
-- output { [1i64] }
-- input { empty([0]bool) empty([0]i64) 0i64 }
-- error: out of bounds
entry test_segmented_scan (flags: []bool) (as: []i64) (i: i64) =
  IArray.i (IArray.segmented_scan (+) 0 (IArray.from_array_flags as flags)) i

-- ==
-- entry: test_segmented_reduce
-- input { [true,false,false,true,false,false,true,false,false,false]
--         [1i64,2i64,3i64,4i64,5i64,6i64,7i64,8i64,9i64,10i64] }
-- output { [6i64,15i64,34i64] }
-- input { [true] [1i64] }
-- output { [1i64] }
entry test_segmented_reduce (flags: []bool) (as: []i64) =
  IArray.segmented_reduce (+) 0 (IArray.from_array_flags as flags)

-- ==
-- entry: test_replicated_iota
-- input { [2i64,3i64,1i64] 0i64 } output { [0i64,0i64] }
-- input { [2i64,3i64,1i64] 1i64 } output { [1i64,1i64,1i64] }
-- input { [2i64,3i64,1i64] 2i64 } output { [2i64] }
-- input { [3i64] 0i64 } output { [0i64,0i64,0i64] }
-- input { [2i64,0i64,1i64] 0i64 } output { [0i64,0i64] }
-- input { [2i64,0i64,1i64] 1i64 } output { empty([0]i64) }
-- input { [2i64,0i64,1i64] 2i64 } output { [2i64] }
-- input { empty([0]i64) 0i64 } error: out of bounds
-- input { [0i64] 0i64 } output { empty([0]i64) }
-- input { [0i64,0i64] 0i64 } output { empty([0]i64) }
-- input { [0i64,0i64] 1i64 } output { empty([0]i64) }
entry test_replicated_iota (repl:[]i64) (i: i64): []i64 =
  IArray.i (IArray.replicated_iota repl) i


-- ==
-- entry: test_segmented_iota
-- input { [3i64, 4i64] 0i64 } output { [0i64,1i64,2i64] }
-- input { [3i64, 4i64] 1i64 } output { [0i64,1i64,2i64,3i64] }
-- input { [1i64] 0i64 } output { [0i64] }
-- input { empty([0]i64) 0i64 } error: out of bounds
entry test_segmented_iota (length: []i64) (i: i64) : []i64 =
  IArray.i (IArray.segmented_iota length) i

-- ==
-- entry: test_expand
-- input { [2i64,3i64,1i64] 0i64 } output { [0i64,2i64] }
-- input { [2i64,3i64,1i64] 1i64 } output { [0i64,3i64,6i64] }
-- input { [2i64,3i64,1i64] 2i64 } output { [0i64] }
entry test_expand (arr:[]i64) (i: i64): []i64 =
  IArray.i (IArray.expand (\x -> x) (\x i -> x*i) arr) i

-- ==
-- entry: test_expand_reduce
-- input { [2i64,0i64,3i64,1i64] }
-- output { [2i64,9i64,0i64] }
entry test_expand_reduce (arr:[]i64) : []i64 =
  IArray.expand_reduce (\ x -> x) (\x i -> x*i) (+) 0 arr

-- ==
-- entry: test_expand_outer_reduce
-- input { [2i64,0i64,3i64,1i64] }
-- output { [2i64,0i64,9i64,0i64] }
entry test_expand_outer_reduce (arr:[]i64) : []i64 =
  IArray.expand_outer_reduce (\ x -> x) (\x i -> x*i) (+) 0 arr