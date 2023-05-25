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

    -- | The same as blit, except len is the length of the src[src_pos:] (in other words, len is maxed)
    val blit_all [n][m] 'a: [n]a -> i64 -> *[m]a -> i64 -> *[m]a

    -- | Pads an array with a given value to reach a given length if the array is shorter than the given length, otherwise
    -- the array is truncated to the given length
    val resize_to [n] 'a: a -> (len: i64) -> [n]a -> [len]a

    -- | Gets the values of a given array referred to by an array of indices of that array
    val indices_to_values [n][m] 'a: [n]a -> [m]i64 -> [m]a

    -- | Returns an array of indices of the given array where the corresponding values met the provided condition
    val find_indices [n] 'a: (a -> bool) -> [n]a -> []i64

    -- | Removes indices from an array
    val remove_indices [n][m] 'a: [n]a -> [m]i64 -> []a

    -- | Returns an array of sliding windows containing elements drawn from the input array. w is the number
    -- of elements in each window and xs is the input array
    val windowed [n] 'a: (w: i64) -> [n]a -> [][w]a

    val differences [n] 'a 'b: (a -> a -> b) -> [n]a -> []b

    -- | Creates segments out of an array where segments start when the value satisfies the predicate
    val split_by [n] 'a: (a -> bool) -> [n]a -> iarray [][] a

    -- | Generates the Cartesian square of (iota n) (e.g. [(0, 0), (0, 1), (0, 2) ... (1, 0), (1, 1) ...])
    val iota_pairs: i64 -> [](i64, i64)

    -- | Generates the Cartesian product of two arrays
    val cart_prod [n] 't: [n]t -> [n]t -> [](t, t)

    -- | Determines if two arrays are equal
    val equals [n] 'a: (a -> a -> bool) -> [n]a -> [n]a -> bool

    -- | Adds the corresponding index to each element
    val indexed [n] 'a: [n]a -> [n](i64, a)

    -- | Map, but the corresponding index of the element is passed as well
    val mapi [n] 'a 'b: (i64 -> a -> b) -> [n]a -> [n]b
}

module Array: Array = {
    def to_string = ArrayUtils.to_string

    def blit = ArrayUtils.blit

    def blit_all [n] 'a (src: [n]a) src_pos (dst: *[]a) dst_pos = blit src src_pos dst dst_pos n

    def resize_to [n] 'a empty len (src: [n]a) =
        let needed = len - n
        in
        (if needed < 0 then src[0:(n - len)]
        else if needed == 0 then src
        else src ++ (replicate needed empty))
        :> [len]a

    def indices_to_values = ArrayUtils.indices_to_values

    def mapi f x = map2 f (indices x) x

    def indexed x = zip (indices x) x

    def find_indices cond xs =
        (indexed xs
        |> filter (\(_, x) -> cond x)
        |> unzip).0

    def remove_indices 't [n][m] (xs: [m]t) (is: [n]i64) =
        let flags = scatter (replicate m true) is (replicate n false)
        in
        (indexed xs
        |> filter(\(i, _) -> flags[i])
        |> unzip).1

    def windowed = ArrayUtils.windowed

    def differences [n] 'a 'b (minus: a -> a -> b) (x: [n]a): []b =
        windowed 2 x
        |> map(\arr -> arr[1] `minus` arr[0])

    def split_by cond x =
        let split_indices =
            find_indices cond x
            |> mapi (\i x -> x - i)
        let split_indices = if (cond (head x)) then split_indices else [0] ++ split_indices
        let noSplits = x |> filter (\x -> cond x |> not)
        let split_indices = split_indices ++ [length noSplits]
        in
        IArray.from_array_lengths noSplits (windowed 2 split_indices |> map(\x -> x[1] - x[0]))

    def iota_pairs n = iota (n**2) |> map(\x -> (x // n, x %% n))

    def cart_prod [n] 't (xs: [n]t) (ys: [n]t) =
        iota_pairs n
        |> map(\(xi, yi) -> (xs[xi], ys[yi]))

    def equals eq x y =
        all (\(x, y) -> x `eq` y) (zip x y)

}