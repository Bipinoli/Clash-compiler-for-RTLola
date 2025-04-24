module Debug where

import Clash.Prelude

type Input = (Bool, Bool)

func :: HiddenClockResetEnable dom => Signal dom Input -> Signal dom Int
func input = cursor
    where 
        cursor = register 0 nextCursorSignal
        nextCursorSignal = nextCursor <$> cursor <*> input

        nextCursor :: Int -> Input -> Int
        nextCursor cur (push, pop) = 
            case (push, pop) of
                (True, True) -> cur + 1
                (True, False) -> cur
                (False, True) -> cur - 1
                (False, False) -> cur

topEntity :: Clock System -> Reset System -> Enable System -> 
    Signal System Input -> Signal System Int
topEntity clk rst en input = exposeClockResetEnable (func input) clk rst en
