module TestInstances where

import RequestTypes
import Test.QuickCheck (Arbitrary(..))
import Generic.Random (genericArbitrary, uniform)
import Data.Text (Text, pack, unpack)
import Test.QuickCheck.Instances

instance Arbitrary CreateUserRequest where arbitrary =  genericArbitrary uniform