{-# LANGUAGE OverloadedStrings #-}

module CirrusSpec where

import Cirrus

import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Network.AWS.EC2.Types as EC2

spec :: Spec
spec = do
  let hc = HealthCheck { hcHealthyThreshold = 3
                       , hcInterval = 10
                       , hcTarget = "http:80/"
                       , hcTimeout = 2
                       , hcUnhealthyThreshold = 5
                       }

  let ud = UserData [ "yum update -y"
                    , "yum install -y docker"
                    , "service docker start"
                    , "docker run -d -p 80:80 tutum/hello-world"
                    ]

  let cfg = Config { cfgVPC = VPC "vpc-183bcd7c"
                   , cfgInstanceType = InstanceType EC2.T2_Micro
                   , cfgAMI = AMI "ami-0d4cfd66"
                   , cfgCapacity = Capacity { capMin = 2, capMax = 3 }
                   , cfgListeners = [ Listener { lnrPort = 80, lnrProtocol = HTTP } ]
                   , cfgHealthCheck = hc
                   , cfgUserData = Just ud
                   , cfgLoadBalancerSubnets = [ Subnet "subnet-53b3596e", Subnet "subnet-13e6b44a" ]
                   , cfgInstanceSubnets = [ Subnet "subnet-53b3596e", Subnet "subnet-13e6b44a" ]
                   }

  describe "encoding and decoding" $ do
    it "can read YAML configuration" $ do
      yml <- BS.readFile "tests/fixture.yml"

      case decode yml of
        Right c -> c `shouldBe` cfg
        Left err -> fail $ show err

    it "can convert to cloudformation JSON" $ do
      json <- LBS.readFile "tests/fixture.json"

      let t = encode . fromConfig $ cfg

      LBS.concat [t, "\n"] `shouldBe` json
