-- | More utility functions for 64-bit integers.
import "string"

module type i64_ext = {
    -- | Computes floor(log_10(n)) without relying on a floating point conversion.
    -- This is preferable with large integers and might be slightly faster than
    -- using the floating point conversion.
    val floor_log_10: i64 -> i64

    -- | Gets the number of digits in a number (0 counts as 1 digit)
    val digits_in: i64 -> i64
}

module i64_ext: i64_ext = {
    def floor_log_10 n: i64 =
       let (_, accum) = loop (n, accum) = (n, 0) while n > 9 do
                           (n / 10i64, accum + 1)
       in
       accum

    def digits_in n =
        1 + floor_log_10 n
}