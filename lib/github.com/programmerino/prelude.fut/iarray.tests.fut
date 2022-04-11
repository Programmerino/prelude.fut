-- | ignore

import "iarray"

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