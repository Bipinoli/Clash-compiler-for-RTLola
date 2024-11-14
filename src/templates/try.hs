module Try where

import Clash.Prelude

tryDelay :: HiddenClockResetEnable dom => Signal dom (Signed 32) -> Signal dom (Signed 32)
tryDelay a = delay 0 a