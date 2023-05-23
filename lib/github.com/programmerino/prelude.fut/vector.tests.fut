-- | ignore
import "vector"


-- Return the number of elements in the vector
-- val length [n] 't: vector [n] t -> i64
-- ==
-- entry: test_length
-- input { [1,2,3,4,5] }
-- output { 5i64 }
entry test_length (xs: []i32) =
    let vec = Vector.of_array xs
    in Vector.length vec

-- Is the vector empty?
-- val null [n] 't: vector [n] t -> bool
-- ==
-- entry: test_null
-- input { empty([0]i32) }
-- output { true }
entry test_null (xs: []i32) =
    let vec = Vector.of_array xs
    in Vector.null vec

-- `get xs idx` gets the element in `xs` at index `idx`. If `xs` has
-- `len` elements in it, then the valid indexes range from `0` to `len-1`.
-- val get [n] 't: vector [n] t -> i64 -> t
-- ==
-- entry: test_get
-- input { [1,2,3,4,5] 2i64 }
-- output { 3i32 }
entry test_get (xs: []i32) (idx: i64) =
    let vec = Vector.of_array xs
    in Vector.get vec idx

-- `set xs idx x` sets the element in `xs` at index `idx` to `x`. If `xs`
-- has `len` elements in it, then the valid indexes range from `0` to `len-1`.
-- val set [n] 't: *vector [n] t -> i64 -> t -> vector [n] t
-- ==
-- entry: test_set
-- input { [1,2,3,4,5] 2i64 42i32 }
-- output { [1,2,42,4,5] }
entry test_set (xs: *[]i32) (idx: i64) (x: i32) =
    let vec = Vector.of_array xs
    in Vector.to_array (Vector.set vec idx x)

-- Convert vector to array
-- val to_array [n] 't: vector [n] t -> []t
-- ==
-- entry: test_to_array
-- input { [1,2,3,4,5] }
-- output { [1,2,3,4,5] }
entry test_to_array (xs: []i32) =
    let vec = Vector.of_array xs
    in Vector.to_array vec

-- Convert array to vector
-- val of_array [n] 't: [n]t -> vector [n] t
-- ==
-- entry: test_of_array
-- input { [1,2,3,4,5] }
-- output { [1,2,3,4,5] }
entry test_of_array (xs: []i32) =
    let vec = Vector.of_array xs
    in Vector.to_array vec

-- Empty vector
-- val empty 't: vector [0] t
-- ==
-- entry: test_empty
-- input { 0 }
-- output { empty([0]i32) }
entry test_empty (_: i32): []i32 =
    let vec = Vector.empty
    in Vector.to_array vec

-- Push an element to the back of the vector
-- val push_back 't [n] : *vector [n] t -> t -> vector [] t
-- ==
-- entry: test_push_back
-- input { [1,2,3,4,5] 42i32 }
-- output { [1,2,3,4,5,42] }
entry test_push_back (xs: *[]i32) (x: i32) =
    let vec = Vector.of_array xs
    in Vector.to_array (Vector.push_back vec x)

-- Concatenate an array with a vector and return a vector
-- val concat_array 't [n][m] : *vector [n] t -> [m]t -> vector [] t
-- ==
-- entry: test_concat_array
-- input { [1,2,3,4,5] [6,7,8,9,10] }
-- output { [1,2,3,4,5,6,7,8,9,10] }
entry test_concat_array (xs: *[]i32) (ys: []i32) =
    let vec = Vector.of_array xs
    in Vector.to_array (Vector.concat_array vec ys)

-- Concatenation with another vector
-- val concat 't [n][m] : *vector [n] t -> *vector [m] t -> vector [] t
-- ==
-- entry: test_concat
-- input { [1,2,3,4,5] [6,7,8,9,10] }
-- output { [1,2,3,4,5,6,7,8,9,10] }
entry test_concat (xs: *[]i32) (ys: *[]i32) =
    let vec1 = Vector.of_array xs
    let vec2 = Vector.of_array ys
    in Vector.to_array (Vector.concat vec1 vec2)