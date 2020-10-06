{-# LANGUAGE CPP #-}

import           Cardano.Prelude
import           Hedgehog.Main (defaultMain)

#ifdef linux_HOST_OS
import qualified Test.Cardano.Node.FilePermissions
#endif
import qualified Test.Cardano.Node.Json
import qualified Test.Cardano.Node.POM

main :: IO ()
main = defaultMain
#ifdef linux_HOST_OS
  [ Test.Cardano.Node.Json.tests
  , Test.Cardano.Node.POM.tests
  , Test.Cardano.Node.FilePermissions.tests
  ]
#else
  [ Test.Cardano.Node.Json.tests
  , Test.Cardano.Node.POM.tests
  ]
#endif

