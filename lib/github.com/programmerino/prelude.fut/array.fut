import "iarray"
import "util"

module Array = {

    -- | Convert an array into a comma-delimited string with a given function to convert the underlying type to a string
    def to_string strFn x = ArrayUtils.to_string strFn x

    -- | Reads a range of elements from the first array into the second. src is the source array, src_pos is the starting
    -- index of the source array, dst is the target array, dst_pos is the starting index of the target array, and len is
    -- the number of elements to copy
    def blit [n][m] 'a (src: [n]a) (src_pos: i64) (dst: *[m]a) (dst_pos: i64) (len: i64): *[m]a =
        let src_pos = assert (src_pos >= 0) src_pos
        let dst_pos = assert (dst_pos >= 0) dst_pos
        let len = assert (len >= 0) len
        let len = assert(len <= n - src_pos) len
        let len = assert(len <= m - dst_pos) len
        in
        scatter dst (iota len |> map(\x -> x + dst_pos)) (src[src_pos:src_pos + len] :> [len]a)

    -- | Pads an array with a given value to reach a given length if the array is shorter than the given length, otherwise
    -- the array is truncated to the given length
    def resizeTo [n] 'a (empty: a) (len: i64) (src: [n]a): [len]a =
        let needed = len - n
        in
        (if needed < 0 then src[0:(n - len)]
        else if needed == 0 then src
        else src ++ (replicate needed empty))
        :> [len]a

    -- | Gets the values of a given array referred to by an array of indices of that array
    def indices_to_values [n][m] 'a (zero: a) (x: [n]a) (is: [m]i64): [m]a = ArrayUtils.indices_to_values zero x is

    -- | Returns an array of indicies of the given array where the corresponding values met the provided condition
    def find_indices 'a [n] (cond: a -> bool) (xs:[n]a) : []i64 =
        xs
        |> zip (indices xs)
        |> filter (\(_, x) -> cond x)
        |> map(\(i, _) -> i)

    -- | Removes indices from an array
    def remove_indices 'a [n] (xs:[n]a) (is: [n]i64): []a =
        xs
        |> zip (indices xs)
        |> filter(\(i, _) -> any (\j -> i == j) is)
        |> map(\(_, x) -> x)


    -- | Returns an array that contains no duplicate entries according to a provided equality function. If an
    -- element occurs multiple times in the array, then later occurrences are discarded.
    def distinct_by 'a [n] (zero: a) (eq: a -> a -> bool) (xs: [n]a): []a =
        let (acc, i) = loop (acc, i) = (replicate n zero, 0) for x in xs do
                                        if any (eq x) acc[0:i] then
                                            (acc, i)
                                        else
                                            (acc with [i] = x, i + 1)
        in
        acc[0:i]

    -- | Returns an array of sliding windows containing elements drawn from the input array. w is the number
    -- of elements in each window and xs is the input array
    def windowed [n] 'a (w: i64) (xs: [n]a) : [][w]a = ArrayUtils.windowed w xs

    -- | Creates segments out of an array where segments start when the value satisfies the predicate
    def split_by 'a (cond: a -> bool) (x: []a): iarray [][] a =
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