-- | ignore
import "i64_ext"

-- ==
-- entry: test_floor_log_10
-- input { 1i64 } output { 0i64 }
-- input { 9i64 } output { 0i64 }
-- input { 10i64 } output { 1i64 }
-- input { 99i64 } output { 1i64 }
-- input { 100i64 } output { 2i64 }
-- input { 999i64 } output { 2i64 }
-- input { 1000i64 } output { 3i64 }
entry test_floor_log_10 = i64_ext.floor_log_10

-- ==
-- entry: test_digits_in
-- input { 0i64 } output { 1i64 }
-- input { 1i64 } output { 1i64 }
-- input { 9i64 } output { 1i64 }
-- input { 10i64 } output { 2i64 }
-- input { 99i64 } output { 2i64 }
-- input { 100i64 } output { 3i64 }
-- input { 999i64 } output { 3i64 }
-- input { 1000i64 } output { 4i64 }
entry test_digits_in = i64_ext.digits_in

--  [(0i64, 1i64, 0i64), (1i64, 1i64, 1i64), (1i64, 2i64, 1i64), (2i64, 2i64, 1i64), (3i64, 2i64, 2i64), (10i64, 3i64, 4i64), (11i64, 3i64, 4i64)]

-- ==
-- entry: test_cdiv
-- input { 0i64 1i64 } output { 0i64 }
-- input { 1i64 1i64 } output { 1i64 }
-- input { 1i64 2i64 } output { 1i64 }
-- input { 2i64 2i64 } output { 1i64 }
-- input { 3i64 2i64 } output { 2i64 }
-- input { 10i64 3i64 } output { 4i64 }
-- input { 11i64 3i64 } output { 4i64 }
entry test_cdiv = i64_ext.cdiv