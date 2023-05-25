-- | A data structure and functions for achieving 2D irregular arrays.

import "util"

-- | An expansion of the concepts discussed on [this comment](https://github.com/diku-dk/futhark/issues/454#issuecomment-355406287)
-- with implementations for scan and reduce adapted from [diku-dk/segmented](https://github.com/diku-dk/segmented).
-- This implementation works by storing a single-dimensional array and an array of associated lengths for each subarray or
-- segment. IArrays can be created with the `from_array_lengths`@IArray, `from_array_flags`@IArray, or `singleton`@IArray functions 
-- and can be accessed with the `i`@IArray function. Eventually, it should support most of the standard prelude functions for arrays.

type iarray [a][n] 't = { data: [a]t, lengths: [n]i64 }

module type IArray = {

    -- | Creates new array segments from the given array given the ordered
    -- lengths that should be constructed
    val from_array_lengths [n][o] 't: [n]t -> [o]i64 -> iarray [n][o] t

    -- | Creates new array segments from the given array and an array of
    -- booleans where true starts a segment and false continues a segment.
    val from_array_flags [n] 't: [n]t -> [n]bool -> iarray [n][] t

    -- | Generates an IArray containing one array segment
    val singleton [n] 't: [n]t -> iarray [n][1] t

    -- | Concatenates two IArrays
    val concat [a][n][b][m] 't: iarray [a][n] t -> iarray [b][m] t -> *iarray [][] t

    -- | Returns the segment at a given index
    val i [a][n] 't: iarray [a][n] t -> i64 -> []t

    -- | Concatenates the segments at the given indices into a new array
    val concat_indices [a][n][i] 't: iarray [a][n] t -> [i]i64 -> []t

    -- | Returns the segment at a given index, forcing a certain length
    val i_to [a][n] 't: iarray [a][n] t -> (b: i64) -> i64 -> [b]t

    -- | Applies operation to values across all segments
    val map_all [a][n] 't 'u: (t -> u) -> iarray [a][n] t -> iarray [a][n] u

    -- | Converts an IArray into a similar data structure represented
    --  as an array of booleans where true indicates the start of a
    -- segment and false otherwise, and the underlying array
    val generate_flagged_segments [a][n] 't: iarray [a][n] t -> ([a]bool, [a]t)

    -- | Segmented scan. Given a binary associative operator ``op`` with
    -- neutral element ``ne``, computes the inclusive prefix scan of the
    -- segments of ``as``.
    val segmented_scan [a][n] 't: (t -> t -> t) -> t -> iarray [a][n] t -> iarray [a][n] t
    
    -- | Segmented reduction. Given a binary associative operator ``op``
    -- with neutral element ``ne``, computes the reduction of the segments
    -- of ``as``.  One value is returned per segment.
    val segmented_reduce [a][n] 't: (t -> t -> t) -> t -> iarray [a][n] t -> *[]t

    val length [a][n] 't: iarray [a][n] t -> i64

    -- | Creates an iarray from an underlying array and an array of start
    -- indices (inclusive)
    val from_array_starts [a][n] 't: [a]t -> [n]i64 -> iarray [a][] t

    -- | Replicated iota. Given a repetition array, the function returns
    -- an array with each index (starting from 0) repeated according to
    -- the repetition array. As an example, replicated_iota [2,3,1]
    -- returns the iarray [[0,0], [1,1,1] ,[2]].
    val replicated_iota [n]: [n]i64 -> iarray [][n] i64

    -- | Segmented iota. Given a lengths array, the function returns an
    -- array of index sequences, each of which is reset according to the
    -- flags array. As an examples, segmented_iota
    -- [3,4] returns the iarray
    -- [[0,1,2], [0,1,2,3]].
    val segmented_iota [n]: [n]i64 -> iarray [][n] i64

    -- | Generic expansion function. The function expands a source array
    -- into a target array given (1) a function that determines, for each
    -- source element, how many target elements it expands to and (2) a
    -- function that computes a particular target element based on a
    -- source element and the target element number associated with the
    -- source. As an example, the expression expand (\x->x) (*) [2,3,1]
    -- returns the iarray [[0,2],[0,3,6],[0]].
    val expand [n] 'a 'b: (a -> i64) -> (a -> i64 -> b) -> [n]a -> iarray [][n] b

    -- | Expand followed by a call to segmented_reduce
    val expand_reduce [n] 'a 'b: (a -> i64) -> (a -> i64 -> b) -> (b -> b -> b) -> b -> [n]a -> []b

    -- | Expansion followed by an ''outer segmented reduce'' that ensures
    -- that each element in the result array corresponds to expanding and
    -- reducing the corresponding element in the source array.
    val expand_outer_reduce [n] 'a 'b: (a -> i64) -> (a -> i64 -> b) -> (b -> b -> b) -> b -> [n]a -> [n]b
}

module IArray: IArray = {

    def from_array_lengths xs lengths =
        {
            data = xs,
            lengths = lengths
        }

    def singleton xs =
        {
            data = xs,
            lengths = [length xs]
        }

    def concat 't (s1: iarray [][] t) (s2: iarray [][] t) =
        { data = concat s1.data s2.data, lengths = concat s1.lengths s2.lengths }

    local def start_index_of 't (s: iarray [][] t) leni =
        s.lengths
        |> take leni
        |> reduce_comm (+) 0

    local def start_indices 't (s: iarray [][] t) =
        (indices s.lengths) |> map (start_index_of s)

    -- Returns the indices of the end of segments, not including empty segments
    local def end_indices 't (s: iarray [][] t) = scan (+) 0 (s.lengths |> filter (\x -> x != 0)) |> map(\x -> x - 1)

    def i s i =
        let start = start_index_of s i
        in
        s.data[start : start + s.lengths[i]]

    def concat_indices s is =
        let length = reduce_comm (+) 0 (is |> map (\i -> s.lengths[i]))
        in
        (loop (acc, offset) = (replicate length s.data[0], 0) for i in is do
            let start = start_index_of s i
            let length = s.lengths[i]
            in (ArrayUtils.blit s.data start acc offset length, offset + length)).0

    def i_to 't s b ind =
        i s ind :> [b]t

    def map_all 't f (s: iarray [][] t) =
        {
            data = (map f s.data),
            lengths = s.lengths
        }

    def generate_flagged_segments [a][n] 't (s: iarray [a][n] t) =
        ((scatter (replicate a false) (start_indices s) (replicate n true)), s.data)

    def segmented_scan op ne arr =
        let (flags, as) = generate_flagged_segments arr
        in
        arr with data = 
        (unzip (scan (\(x_flag,x) (y_flag,y) ->
                        (x_flag || y_flag,
                        if y_flag then y else x `op` y))
                (false, ne)
                (zip flags as))).1

    def segmented_reduce op ne arr =
        let as' = segmented_scan op ne arr
        let arr_ends = end_indices arr
        let m = length arr_ends
        in
        scatter (replicate m ne) (iota m) (ArrayUtils.indices_to_values as'.data (arr_ends :> [m]i64))

    def length [n] 't (_: iarray [][n] t) = n

    def from_array_starts [a][n] 't (arr: [a]t) (starts: [n]i64) =
        let part_lengths = ArrayUtils.windowed 2 starts |> map(\wn -> wn[1] - wn[0])
        let lengths = part_lengths ++ (if n > 0 then [(a - (starts |> last))] else [])
        in
        {
            data = arr,
            lengths = lengths
        }

    def replicated_iota [n] (reps:[n]i64) =
        let s1 = scan (+) 0 reps
        let s2 = map2 (\i x -> if i==0 then 0 else x)
                        (iota n) (rotate (-1) s1)
        let tmp = reduce_by_index (replicate (reduce (+) 0 reps) 0) i64.max 0 s2 (iota n)
        in segmented_scan (+) 0 (from_array_lengths tmp reps)

    def segmented_iota [n] (lengths: [n]i64): iarray [][n]i64 =
        let s1 = reduce_comm (+) 0 lengths
        let iotas = segmented_scan (+) 0 (from_array_lengths (replicate s1 1) lengths)
        in map_all (\x -> x-1) iotas

    def expand sz get arr =
        let szs = map sz arr
        let szs_sum = reduce_comm (+) 0 szs
        let idxs = replicated_iota szs
        let iotas = segmented_iota idxs.lengths
        in { data = map2 (\i j -> get arr[i] j) (idxs.data :> [szs_sum]i64) (iotas.data :> [szs_sum]i64), lengths = idxs.lengths }

    def expand_reduce sz get op ne arr = segmented_reduce op ne (expand sz get arr)

    def expand_outer_reduce [n] 'a 'b sz get op ne (arr: [n]a) =
        let sz' x = let s = sz x
                    in if s == 0 then 1 else s
        let get' x i = if sz x == 0 then ne else get x i
        in expand_reduce sz' get' op ne arr :> [n]b

    def from_array_flags xs flags =
        flags
        |> zip (indices flags)
        |> filter (\(_, flag) -> flag)
        |> map (\(i, _) -> i)
        |> from_array_starts xs
}