-- | Utility functions for dealing with strings based on u8 arrays

import "iarray"
import "array"
import "option"
import "util"

-- | This contains functions for dealing with character arrays (in this library, considered strings).
-- These can be created with standard string syntax (e.g. "hello") which compiles down
-- to a u8 array, and characters can be created with single ticks (e.g 'a'). Entrypoints
-- outputting strings can use the -b option to avoid them being output as an array of
-- numbers

type string [n] = [n]u8

module type String = {
    -- | split_on_chars c x is an array of all substrings of x that are delimited
    -- by any character in c.
    val split_on_chars [n][m]: [n]u8 -> string[m] -> iarray [][] u8
    
    val length [n]: string[n] -> i64

    -- | Reverses the input string
    val reverse [n]: string[n] -> string[n]

    -- | concat sep ss concatenates the list of strings ss, inserting the separator string sep between each
    val concat [n][m][a]: string[n] -> [m]string[a] -> *string[]

    -- | lower str is str with all uppercase letters translated to lowercase
    val lower [n]: string[n] -> string[n]

    -- | Converts between a single digit u8 ASCII character (interpreted as hex) and the corresponding u8 integer
    val digit_to_int: u8 -> option u8

    -- | Converts between a u8 integer and the corresponding single digit u8 ASCII character (as hex).
    val int_to_digit: u8 -> option u8
    
    -- | Returns the string representation of an integer in decimal
    val string_of_int: i64 -> string[]

    -- | Converts the given string to an integer, interpreted as a positive decimal number
    val int_of_string [n]: string[n] -> u64

    -- | Replaces character old in the provided string, str, with character new
    val replace_char [n]: u8 -> u8 -> string[n] -> string[n]

    -- | Removes all instances of a character from the provided string
    val remove_char [n]: u8 -> [n]u8 -> []u8
}

module String: String = {
    def split_on_chars [n] (c: [n]u8) x: iarray [][] u8 =
        let max = (u8.highest |> i64.u8) + 1
        let flags = scatter (replicate max false) (map i64.u8 c) (replicate n true)
        in
        x
        |> Array.split_by (\x -> flags[i16.u8 x])

    def length [n] (_: string[n]) = n

    def reverse str = str |> reverse

    def concat = StringUtils.concat

    def lower (str: string[]) = str |> map(\x -> if x >= 'A' && x <= 'Z' then x - ('A' - 'a') else x)

    def digit_to_int c: option u8 =
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

    def int_to_digit c: option u8 =
        match c
        case 0u8 -> #Some '0'
        case 1 -> #Some '1'
        case 2 -> #Some '2'
        case 3 -> #Some '3'
        case 4 -> #Some '4'
        case 5 -> #Some '5'
        case 6 -> #Some '6'
        case 7 -> #Some '7'
        case 8 -> #Some '8'
        case 9 -> #Some '9'
        case 10 -> #Some 'a'
        case 11 -> #Some 'b'
        case 12 -> #Some 'c'
        case 13 -> #Some 'd'
        case 14 -> #Some 'e'
        case 15 -> #Some 'f'
        case _ -> #None

    def string_of_int = StringUtils.string_of_int

    def int_of_string [n] (str: string[n]) =
        str
        |> map (\x -> (u64.u8 x) - '0')
        |> reduce_comm (\x y -> (x * 10) + y) 0
        

    def replace_char [n] old new str: string[n] =
        str |> map(\x -> if x == old then new else x)

    def remove_char c (str: string[]) =
        str |> filter (\x -> x != c)
}