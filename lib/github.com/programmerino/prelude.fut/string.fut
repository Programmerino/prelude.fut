import "iarray"
import "array"
import "option"
import "util"

type string [n] = [n]u8

module String = {
    -- | split_on_chars c x is an array of all substrings of x that are delimited
    -- by any character in c.
    def split_on_chars (c: []u8) (x: string[]): iarray [][] u8 = Array.split_by (\x -> any (\y -> y == x) c) x

    def length [n] (_: string[n]) = n

    -- | Reverses the input string
    def reverse (str: string[]) = str |> reverse

    -- | concat sep ss concatenates the list of strings ss, inserting the separator string sep between each
    def concat (sep: string[]) (ss: []string[]) = StringUtils.concat sep ss

    -- | lower str is str with all uppercase letters translated to lowercase
    let lower (str: string[]) = str |> map(\x -> if x >= 'A' && x <= 'Z' then x - ('A' - 'a') else x)

    -- | Converts between a single digit u8 ASCII character and the corresponding u8 integer
    def digitToInt (c: u8): option u8 =
        match ([c] |> lower |> head)
        case '0' -> #Some 0
        case '1' -> #Some 1
        case '2' -> #Some 2
        case '3' -> #Some 3
        case '4' -> #Some 4
        case '5' -> #Some 5
        case '6' -> #Some 6
        case '7' -> #Some 7
        case '8' -> #Some 8
        case '9' -> #Some 9
        case 'a' -> #Some 10
        case 'b' -> #Some 11
        case 'c' -> #Some 12
        case 'd' -> #Some 13
        case 'e' -> #Some 14
        case 'f' -> #Some 15
        case _ -> #None

    -- | Returns the string representation of an integer in decimal
    def string_of_int (i: i64) = StringUtils.string_of_int i

    -- | Converts the given string to an integer, interpreted as a decimal number
    def int_of_string (str: string[]) =
        let reversed = str |> reverse
        in
        (loop (answer, factor) = (0, 1) for i < (length str) do ((answer + (reversed[i] - '0')) * factor, factor * 10)).0

    -- | Replaces character old in the provided string, str, with character new
    def replace_char [n] (old: u8) (new: u8) (str: string[n]): string[n] =
        str |> map(\x -> if x == old then new else x)

    -- | Removes all instances of a character from the provided string
    def remove_char (c: u8) (str: string[]) =
        str |> filter (\x -> x != c)
}