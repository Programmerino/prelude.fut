-- | ignore

import "string"
import "iarray"
import "option"

-- ==
-- entry: test_split_on_chars
-- input { ",:" "foo,bar:baz" 0i64 } output { "foo" }
-- input { ",:" "foo,bar:baz" 1i64 } output { "bar" }
-- input { ",:" "foo,bar:baz" 2i64 } output { "baz" }
entry test_split_on_chars [n] (c: []u8) (x: string[n]) i =
    let res = String.split_on_chars c x
    in
    IArray.i_to res 3 i

-- ==
-- entry: test_length
-- input { empty([0]u8) }
-- output { 0i64 }
-- input { "foo" }
-- output { 3i64 }
entry test_length [n] (x: string[n]) =
    String.length x


-- ==
-- entry: test_reverse
-- input { "hello" }
-- output { "olleh" }
entry test_reverse = String.reverse

-- ==
-- entry: test_concat
-- input { ", " ["hello", "world"] }
-- output { "hello, world" }
entry test_concat = String.concat

-- ==
-- entry: test_lower
-- input { "HeLLo" }
-- output { "hello" }
entry test_lower = String.lower

-- ==
-- entry: test_digit_to_int
-- input { 97u8 }
-- output { 10u8 }
entry test_digit_to_int x =
    Option.unwrap 0u8 (String.digit_to_int x)

-- ==
-- entry: test_string_of_int
-- input { 123i64 }
-- output { "123" }
entry test_string_of_int = String.string_of_int

-- ==
-- entry: test_int_of_string
-- input { "123" }
-- output { 123u64 }
entry test_int_of_string = String.int_of_string

-- ==
-- entry: test_replace_char
-- input { 111u8 97u8 "hello" }
-- output { "hella" }
entry test_replace_char = String.replace_char

-- ==
-- entry: test_remove_char
-- input { 108u8 "hello" }
-- output { "heo" }
entry test_remove_char = String.remove_char