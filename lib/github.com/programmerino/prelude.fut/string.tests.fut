-- | ignore

import "string"
import "iarray"

-- ==
-- entry: test_split_on_chars
-- input { [ 44u8, 58u8 ] [102u8, 111u8, 111u8, 44u8, 98u8, 97u8, 114u8, 58u8, 98u8, 97u8, 122u8] 0i64 } output { [ 102u8, 111u8, 111u8 ] }
-- input { [ 44u8, 58u8 ] [102u8, 111u8, 111u8, 44u8, 98u8, 97u8, 114u8, 58u8, 98u8, 97u8, 122u8] 1i64 } output { [ 98u8, 97u8, 114u8 ] }
-- input { [ 44u8, 58u8 ] [102u8, 111u8, 111u8, 44u8, 98u8, 97u8, 114u8, 58u8, 98u8, 97u8, 122u8] 2i64 } output { [ 98u8, 97u8, 122u8 ] }
entry test_split_on_chars [n] (c: []u8) (x: string[n]) i =
    let res = String.split_on_chars c x
    in
    IArray.i_to res 3 i


-- ==
-- entry: bench_split_on_chars
-- random input { [127]u8 [256]u8 }
-- auto output
entry bench_split_on_chars (c: [127]u8) (x: [256]u8) =
    let res = String.split_on_chars c x
    in
    res.data

-- ==
-- entry: bench_concat
-- random input { [2]u8 [5000][20]u8 }
-- auto output
entry bench_concat (c: [2]u8) (x: [5000][20]u8) =
    String.concat c x

-- ==
-- entry: test_length
-- input { empty([0]u8) }
-- output { 0i64 }
-- input { [ 102u8, 111u8, 111u8 ] }
-- output { 3i64 }
entry test_length [n] (x: string[n]) =
    String.length x