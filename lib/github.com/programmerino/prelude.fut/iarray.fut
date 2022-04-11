import "util"

type iarray [a][n] 't = { data: [a]t, lengths: [n]i64 }

module IArray =  {

    -- | Creates new array segments from the given array given the ordered lengths that should be constructed
    def from_array_lengths [n][o] 't (xs: [n]t) (lengths: [o]i64) : iarray[n][o] t =
        {
            data = xs,
            lengths = lengths
        }

    -- | Generates an IArray containing one array
    def singleton [n] 't (xs: [n] t): iarray[n][1] t =
        {
            data = xs,
            lengths = [length xs]
        }

    -- | Concatenates two IArrays
    def concat [a][n][b][m] 't (s1: iarray [a][n] t) (s2: iarray [b][m] t) =
        { data = concat s1.data s2.data, lengths = concat s1.lengths s2.lengths }

    local def start_index_of [a][n] 't (s: iarray [a][n] t) (leni: i64) =
        s.lengths
        |> take leni
        |> reduce_comm (+) 0

    local def start_indices [a][n] 't (s: iarray [a][n] t) =
        (indices s.lengths) |> map (start_index_of s)

    -- Returns the indices of the end of segments, not including empty segments
    local def end_indices [a][n] 't (s: iarray [a][n] t) = scan (+) 0 (s.lengths |> filter (\x -> x != 0)) |> map(\x -> x - 1)

    -- | Returns the segment at a given index
    def i [a][n] 't (s: iarray [a][n] t) (i: i64): []t =
        let start = start_index_of s i
        in
        s.data[start : start + s.lengths[i]]

    -- | Returns the segment at a given index, forcing a certain length
    def i_to [a][n] 't (s: iarray [a][n] t) (b: i64) (ind: i64): [b]t =
        i s ind :> [b]t

    -- | Applies operation to values across all segments
    def map_all [a][n] 't 'u (f: t -> u) (s: iarray [a][n] t): iarray [a][n] u =
        {
            data = (map f s.data),
            lengths = s.lengths
        }

    -- | Converts an IArray into a string describing its structure for debugging purposes
    def to_string_structure [a][n] 't strFn (s: iarray [a][n] t): []u8 =
        let stringifyIntArray [a]  (xs: [a]i64) =
            let (acc, _) = loop (acc, first) = ("[", true) for i in xs do (acc ++ (if (not first) then ", " else "") ++ (StringUtils.string_of_int i), false)
            in acc ++ "]"
        in
        "{ data: " ++ (s.data |> map strFn) ++ ", lengths: " ++ (s.lengths |> stringifyIntArray) ++ " }"

    -- | Converts an IArray into a similar data structure represented as an array of booleans where true indicates the start of a sgement and false otherwise, and the underlying array
    def generate_flagged_segments [a][n] 't (s: iarray [a][n] t): ([a]bool, [a]t) =
        ((scatter (replicate a false) (start_indices s) (replicate n true)), s.data)

    -- | Segmented scan. Given a binary associative operator ``op`` with
    -- neutral element ``ne``, computes the inclusive prefix scan of the
    -- segments of ``as``.
    def segmented_scan [a][n] 't (op: t -> t -> t) (ne: t) (arr: iarray [a][n] t): iarray [a][n] t =
        let (flags, as) = generate_flagged_segments arr
        in
        arr with data = 
        (unzip (scan (\(x_flag,x) (y_flag,y) ->
                        (x_flag || y_flag,
                        if y_flag then y else x `op` y))
                (false, ne)
                (zip flags as))).1


    -- | Segmented reduction. Given a binary associative operator ``op``
    -- with neutral element ``ne``, computes the reduction of the segments
    -- of ``as``.  One value is returned per segment.
    def segmented_reduce [a][n] 't (op: t -> t -> t) (ne: t) (arr: iarray [a][n] t) =
        let as' = #[trace] segmented_scan op ne arr
        let arr_ends = end_indices arr
        let m = length arr_ends
        in
        scatter (replicate m ne) (iota m) (ArrayUtils.indices_to_values ne as'.data (arr_ends :> [m]i64))

    def length [a][n] 't (_: iarray [a][n] t) = n

    -- | Creates an iarray from an underlying array and an array of start indices (inclusive)
    def from_array_starts [a][n] 't (arr: [a]t) (starts: [n]i64) =
        let part_lengths = ArrayUtils.windowed 2 starts |> map(\wn -> wn[1] - wn[0])
        let lengths = part_lengths ++ (if n > 0 then [(a - (starts |> last))] else [])
        in
        {
            data = arr,
            lengths = lengths
        }

    -- | Replicated iota. Given a repetition array, the function returns
    -- an array with each index (starting from 0) repeated according to
    -- the repetition array. As an example, replicated_iota [2,3,1]
    -- returns the iarray [[0,0], [1,1,1] ,[2]].
    def replicated_iota [n] (reps:[n]i64) =
        let s1 = scan (+) 0 reps
        let s2 = map2 (\i x -> if i==0 then 0 else x)
                        (iota n) (rotate (-1) s1)
        let tmp = reduce_by_index (replicate (reduce (+) 0 reps) 0) i64.max 0 s2 (iota n)
        in segmented_scan (+) 0 (from_array_lengths tmp reps)

    -- | Segmented iota. Given a lengths array, the function returns an
    -- array of index sequences, each of which is reset according to the
    -- flags array. As an examples, segmented_iota
    -- [3,4] returns the iarray
    -- [[0,1,2], [0,1,2,3]].
    def segmented_iota [n] (lengths: [n]i64): iarray [][n]i64 =
        let s1 = reduce_comm (+) 0 lengths
        let iotas = segmented_scan (+) 0 (from_array_lengths (replicate s1 1) lengths)
        in map_all (\x -> x-1) iotas

    -- | Generic expansion function. The function expands a source array
    -- into a target array given (1) a function that determines, for each
    -- source element, how many target elements it expands to and (2) a
    -- function that computes a particular target element based on a
    -- source element and the target element number associated with the
    -- source. As an example, the expression expand (\x->x) (*) [2,3,1]
    -- returns the iarray [[0,2],[0,3,6],[0]].
    def expand 'a 'b [n] (sz: a -> i64) (get: a -> i64 -> b) (arr:[n]a): iarray [][n] b =
        let szs = #[trace] map sz arr
        let szs_sum = reduce_comm (+) 0 szs
        let idxs = #[trace] replicated_iota szs
        let iotas = #[trace] segmented_iota idxs.lengths
        in #[trace] { data = map2 (\i j -> get arr[i] j) (idxs.data :> [szs_sum]i64) (iotas.data :> [szs_sum]i64), lengths = idxs.lengths }

    -- | Expand followed by a call to segmented_reduce
    def expand_reduce 'a 'b (sz: a -> i64) (get: a -> i64 -> b)
                            (op: b -> b -> b) (ne:b) (arr:[]a) : []b =
        segmented_reduce op ne (expand sz get arr)

    -- | Expansion followed by an ''outer segmented reduce'' that ensures
    -- that each element in the result array corresponds to expanding and
    -- reducing the corresponding element in the source array.
    def expand_outer_reduce 'a 'b [n] (sz: a -> i64) (get: a -> i64 -> b)
                                    (op: b -> b -> b) (ne: b)
                                    (arr: [n]a) : [n]b =
        let sz' x = let s = sz x
                    in if s == 0 then 1 else s
        let get' x i = if sz x == 0 then ne else get x i
        in expand_reduce sz' get' op ne arr :> [n]b

    -- | Creates new array segments from the given array and an array of booleans where true starts a segment and false continues a segment.
    def from_array_flags [n] 't (xs: [n]t) (flags: [n]bool) : iarray[n][] t =
        flags
        |> zip (indices flags)
        |> filter (\(_, flag) -> flag)
        |> map (\(i, _) -> i)
        |> from_array_starts xs
}