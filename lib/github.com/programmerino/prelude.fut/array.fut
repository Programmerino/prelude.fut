-- | More utility functions for arrays.

import "iarray"
import "util"

module type Array = {
    -- | Convert an array into a comma-delimited string with a given function to convert the underlying type to a string
    val to_string [n][m] 'a: (a -> [n]u8) -> [m]a -> []u8

    -- | Reads a range of elements from the first array into the second. src is the source array, src_pos is the starting
    -- index of the source array, dst is the target array, dst_pos is the starting index of the target array, and len is
    -- the number of elements to copy
    val blit [n][m] 'a: [n]a -> i64 -> *[m]a -> i64 -> i64 -> *[m]a

    -- | Pads an array with a given value to reach a given length if the array is shorter than the given length, otherwise
    -- the array is truncated to the given length
    val resize_to [n] 'a: a -> (len: i64) -> [n]a -> [len]a

    -- | Gets the values of a given array referred to by an array of indices of that array
    val indices_to_values [n][m] 'a: a -> [n]a -> [m]i64 -> [m]a

    -- | Returns an array of indices of the given array where the corresponding values met the provided condition
    val find_indices [n] 'a: (a -> bool) -> [n]a -> []i64

    -- | Removes indices from an array
    val remove_indices [n] 'a: [n]a -> [n]i64 -> []a

    -- | Returns an array that contains no duplicate entries according to a provided equality function. If an
    -- element occurs multiple times in the array, then later occurrences are discarded.
    val distinct_by [n] 'a: a -> (a -> a -> bool) -> [n]a -> []a

    -- | Returns an array of sliding windows containing elements drawn from the input array. w is the number
    -- of elements in each window and xs is the input array
    val windowed [n] 'a: (w: i64) -> [n]a -> [][w]a

    -- | Creates segments out of an array where segments start when the value satisfies the predicate
    val split_by [n] 'a: (a -> bool) -> [n]a -> iarray [][] a
}

module Array: Array = {
    def to_string = ArrayUtils.to_string

    def blit [n][m] 'a (src: [n]a) src_pos (dst: *[m]a) dst_pos len =
        let src_pos = assert (src_pos >= 0) src_pos
        let dst_pos = assert (dst_pos >= 0) dst_pos
        let len = assert (len >= 0) len
        let len = assert(len <= n - src_pos) len
        let len = assert(len <= m - dst_pos) len
        in
        scatter dst (iota len |> map(\x -> x + dst_pos)) (src[src_pos:src_pos + len] :> [len]a)

    def resize_to [n] 'a empty len (src: [n]a) =
        let needed = len - n
        in
        (if needed < 0 then src[0:(n - len)]
        else if needed == 0 then src
        else src ++ (replicate needed empty))
        :> [len]a

    def indices_to_values = ArrayUtils.indices_to_values

    def find_indices cond xs =
        xs
        |> zip (indices xs)
        |> filter (\(_, x) -> cond x)
        |> map(\(i, _) -> i)

    def remove_indices xs is =
        xs
        |> zip (indices xs)
        |> filter(\(i, _) -> any (\j -> i == j) is)
        |> map(\(_, x) -> x)

    def distinct_by 'a [n] zero eq (xs: [n]a) =
        let (acc, i) = loop (acc, i) = (replicate n zero, 0) for x in xs do
                                        if any (eq x) acc[0:i] then
                                            (acc, i)
                                        else
                                            (acc with [i] = x, i + 1)
        in
        acc[0:i]

    def windowed = ArrayUtils.windowed

    def split_by cond x =
        let (split_indices, _) =
                x
                |> zip (indices x)
                |> filter (\(_, x) -> cond x)
                |> unzip
        let split_indices =
            split_indices
            |> zip (indices split_indices)
            |> map (\(i, x) -> x - i)
        let split_indices = if (cond (head x)) then split_indices else [0] ++ split_indices
        let noSplits = x |> filter (\x -> cond x |> not)
        let split_indices = split_indices ++ [length noSplits]
        in
        IArray.from_array_lengths noSplits (windowed 2 split_indices |> map(\x -> x[1] - x[0]))

}