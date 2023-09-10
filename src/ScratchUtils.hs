module ScratchUtils
  ( multilineString,
  )
where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

multilineString :: QuasiQuoter
multilineString =
  QuasiQuoter
    { quoteExp = litE . stringL,
      quoteDec = \_ -> fail "Not implemented",
      quotePat = \_ -> fail "Not implemented",
      quoteType = \_ -> fail "Not implemented"
    }
