-- | A data structure and functions for achieving 2D irregular arrays.

import "util"

-- | An expansion of the concepts discussed on [this comment](https://github.com/diku-dk/futhark/issues/454#issuecomment-355406287).
-- This implementation works by storing a single-dimensional array and an array of associated start indices and lengths for each
-- subarray/segment. IArrays can be created with `from_array_starts`@IArray, `from_starts_lengths`@IArray, or `from_array_lengths`@IArray
-- and can be accessed with the `i`@IArray function. Eventually, it should support most of the standard prelude functions for arrays.

type iarray [a][n] 't = { data: [a]t, starts: [n]i64, lengths: [n]i64 }

module type IArray = {
    -- | Generates an IArray containing one array segment
    val singleton [a] 't: [a]t -> iarray [a][1] t

    -- | Concatenates two IArrays
    val concat [a][n][b][m] 't: iarray [a][n] t -> iarray [b][m] t -> iarray [a+b][n+m] t

    -- | Returns the segment at a given index
    val i [a][n] 't: (x: iarray [a][n] t) -> (i: i64) -> []t

    -- | Concatenates the segments at the given indices into a new array
    val concat_indices [a][n][i] 't: iarray [a][n] t -> [i]i64 -> []t

    -- | Applies operation to values across all segments
    val map_all [a][n] 't 'u: (t -> u) -> iarray [a][n] t -> iarray [a][n] u

    val length [a][n] 't: iarray [a][n] t -> i64

    -- | Returns the langth of the segment at the given index
    val i_length [a][n] 't: iarray [a][n] t -> i64 -> i64

    -- | Creates an iarray from an underlying list and an array of start
    -- indices (inclusive)
    val from_array_starts [a][n] 't: [a]t -> [n]i64 -> iarray [a][n] t

    -- | Creates an iarray from an underlying list and an array of lengths
    val from_array_lengths [a][n] 't: [a]t -> [n]i64 -> iarray [a][n] t

    -- | Creates an iarray from an underlying list and an array of starts and lengths
    val from_starts_lengths [a][n] 't: [a]t -> [n](i64, i64) -> iarray [a][n] t
}

module IArray: IArray = {

    def singleton xs =
        {
            data = xs,
            starts = [0i64],
            lengths = [length xs]
        }

    def concat [a] 't (s1: iarray [a][] t) (s2: iarray [][] t) =
        { data = s1.data ++ s2.data, starts = s1.starts ++ (s2.starts |> map(\x -> x + a)), lengths = s1.lengths ++ s2.lengths }

    def i_length 't (s: iarray [][] t) (i: i64) = s.lengths[i]

    def i 't (s: iarray [][] t) (i: i64) =
        let start = s.starts[i]
        in
        s.data[start : start + s.lengths[i]]

    def concat_indices 't (s: iarray [][] t) (is: []i64) =
        let length = reduce_comm (+) 0 (is |> map (\i -> s.lengths[i]))
        in
        (loop (acc, offset) = (replicate length s.data[0], 0) for i in is do
            let start = s.starts[i]
            let length = s.lengths[i]
            in (ArrayUtils.blit s.data start acc offset length, offset + length)).0


    def map_all 't f (s: iarray [][] t) =
        {
            data = s.data |> map f,
            starts = s.starts,
            lengths = s.lengths
        }

    def length [n] 't (_: iarray [][n] t) = n

    def from_array_lengths [a][n] 't (arr: [a]t) (lengths: [n]i64) =
        let starts = scan (+) 0 lengths
        in
        {
            data = arr,
            starts = starts,
            lengths = lengths
        }

    -- TODO: windowed uses small arrays which are inefficient, fix this
    def from_array_starts [a][n] 't (arr: [a]t) (starts: [n]i64) =
        let part_lengths = ArrayUtils.windowed 2 starts |> map(\wn -> wn[1] - wn[0])
        let lengths = #[unsafe] (part_lengths ++ (if n > 0 then [(a - (starts |> last))] else []) :> [n]i64)
        in
        {
            data = arr,
            starts = starts,
            lengths = lengths
        }

    def from_starts_lengths [a][n] 't (arr: [a]t) (starts_lengths: [n](i64, i64)) =
        let (starts, lengths) = unzip starts_lengths
        in
        {
            data = arr,
            starts = starts,
            lengths = lengths
        }

}