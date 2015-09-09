{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Cirrus.Types
  ( AMI(..)
  , AutoScalingGroup(..)
  , Capacity(..)
  , Config(..)
  , ElasticLoadBalancer(..)
  , FromConfig(..)
  , GetAtt(..)
  , HealthCheck(..)
  , IngressRule(..)
  , InstanceType(..)
  , LaunchConfiguration(..)
  , Listener(..)
  , Port
  , Protocol(..)
  , Ref(..)
  , SecurityGroup(..)
  , Stack(..)
  , Subnet(..)
  , Tag(..)
  , Template(..)
  , UserData(..)
  , VPC(..)
  ) where

import Control.Monad (mzero)
import Data.Text (Text)
import Data.Yaml
import GHC.Generics
import Network.AWS.Data (FromText, fromText, toText)

import qualified Data.Aeson as AE (withText)
import qualified Data.Text as T (lines)
import qualified Data.Vector as V (fromList)
import qualified Network.AWS.EC2.Types as EC2

-- FromConfig

class FromConfig a where
  fromConfig :: Config -> a

-- Config

data Config = Config { cfgVPC :: VPC
                     , cfgInstanceType :: InstanceType
                     , cfgAMI :: AMI
                     , cfgCapacity :: Capacity
                     , cfgListeners :: [Listener]
                     , cfgHealthCheck :: HealthCheck
                     , cfgUserData :: UserData
                     , cfgLoadBalancerSubnets :: [Subnet]
                     , cfgInstanceSubnets :: [Subnet]
                     } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON (Object o) =
    Config <$> o .:  "vpc"
           <*> o .:? "instanceType" .!= defaultInstanceType
           <*> o .:  "ami"
           <*> o .:? "capacity" .!= defaultCapacity
           <*> o .:  "listeners"
           <*> o .:  "healthCheck"
           <*> o .:  "userData"
           <*> ((o .:  "subnets") >>= (.: "loadBalancer"))
           <*> ((o .:  "subnets") >>= (.: "instance"))
  parseJSON _ = mzero

-- Port (newtype?)

type Port = Int

-- AMI

newtype AMI = AMI { unAMI :: Text } deriving (Eq, Show, ToJSON)

instance FromJSON AMI where
  parseJSON = AE.withText "AMI ID" (pure . AMI)

-- Subnet

newtype Subnet = Subnet Text deriving (Eq, Show, ToJSON)

instance FromJSON Subnet where
  parseJSON = AE.withText "Subnet ID" (pure . Subnet)

-- Health Check

data HealthCheck = HealthCheck { hcHealthyThreshold :: Integer
                               , hcInterval :: Integer
                               , hcTarget :: Text
                               , hcTimeout :: Integer
                               , hcUnhealthyThreshold :: Integer
                               } deriving (Eq, Show)

instance FromJSON HealthCheck where
  parseJSON (Object o) =
    HealthCheck <$> o .:? "healthyThreshold" .!= 1
                <*> o .:? "interval" .!= 15
                <*> o .:  "target"
                <*> o .:? "timeout" .!= 3
                <*> o .:? "unhealthyThreshold" .!= 1
  parseJSON _  = mzero

instance ToJSON HealthCheck where
  toJSON hc = object
    [ "HealthyThreshold" .= hcHealthyThreshold hc
    , "Interval" .= hcInterval hc
    , "Target" .= hcTarget hc
    , "Timeout" .= hcTimeout hc
    , "UnhealthyThreshold" .= hcUnhealthyThreshold hc
    ]

-- User Data

newtype UserData = UserData { unUserData :: [Text] } deriving (Eq, Show)

instance FromJSON UserData where
  parseJSON = AE.withText "User Data" (pure . UserData . T.lines)

instance ToJSON UserData where
  toJSON userData = object [ "Fn::Base64" .= object [ "Fn::Join" .= [ String "\n", a ] ] ]
    where a = Array . V.fromList . concat $ [pre, uds, post]
          uds = map String (unUserData userData)
          pre = [ "#!/bin/bash"
                , "exec > >(tee /tmp/user-data.log | logger -t user-data -s 2>/dev/console) 2>&1"
                , "set -ex"
                ]
          post = [ String "/opt/aws/bin/cfn-signal --resource autoScalingGroup -e $? --stack \\"
                 , toJSON . Ref $ "AWS::StackName"
                 ]

-- EC2 instance type, e.g. t2.micro.

newtype InstanceType = InstanceType { unInstanceType :: EC2.InstanceType }
  deriving (Eq, FromText, Show)

instance FromJSON InstanceType where
  parseJSON = AE.withText "Instance Type" (either fail pure . fromText)

instance ToJSON InstanceType where
  toJSON = toJSON . toText . unInstanceType

defaultInstanceType :: InstanceType
defaultInstanceType = InstanceType EC2.T2_Micro

-- VPC

newtype VPC = VPC { unVPC :: Text } deriving (Eq, Show, ToJSON)

instance FromJSON VPC where
  parseJSON = AE.withText "VPC ID" (pure . VPC)

-- Protocol

data Protocol = HTTP | HTTPS | TCP | SSL
  deriving (Eq, Generic, Read, Show)

instance FromJSON Protocol
instance ToJSON Protocol

--- Listener

data Listener = Listener { lnrPort :: Port
                         , lnrProtocol :: Protocol
                         } deriving (Eq, Show)

instance FromJSON Listener where
  parseJSON (Object o) =
      Listener <$> o .: "port"
               <*> o .: "protocol"
  parseJSON _  = mzero

instance ToJSON Listener where
  toJSON l = object [ "LoadBalancerPort" .= lnrPort l
                    , "InstancePort" .= lnrPort l
                    , "Protocol" .= lnrProtocol l
                    ]

-- Capacity

data Capacity = Capacity { capMin :: Integer
                         , capMax :: Integer
                         } deriving (Eq, Show)

instance FromJSON Capacity where
  parseJSON (Object o) =
      Capacity <$> o .: "min"
               <*> o .: "max"
  parseJSON _  = mzero

defaultCapacity :: Capacity
defaultCapacity = Capacity { capMin = 1, capMax = 1 }

-- Tag

data TagValue = TextTagValue Text
              | RefTagValue Ref
              deriving (Eq, Show)

instance ToJSON TagValue where
  toJSON (RefTagValue r) = toJSON r
  toJSON (TextTagValue t) = toJSON t

data Tag = Tag { tagKey :: Text
               , tagValue :: TagValue
               , tagPropagateAtLaunch :: Bool
               } deriving (Eq, Show)

instance ToJSON Tag where
  toJSON t = object
    [ "Key" .= tagKey t
    , "Value" .= tagValue t
    , "PropagateAtLaunch" .= tagPropagateAtLaunch t
    ]


nameTag :: Tag
nameTag = Tag { tagKey = "Name"
              , tagValue = RefTagValue (Ref "AWS::StackName")
              , tagPropagateAtLaunch = True
              }

-- Ref

data Ref = Ref { referee :: Text } deriving (Eq, Show)

instance ToJSON Ref where
  toJSON r = object [ "Ref" .= referee r ]

-- Fn::GetAtt

data GetAtt = GetAtt { resourceName :: Text
                     , attributeName :: Text
                     } deriving (Eq, Show)

instance ToJSON GetAtt where
  toJSON ga = object [ "Fn::GetAtt" .= [ resourceName ga, attributeName ga ] ]

-- Auto Scaling Group

data AutoScalingGroup = AutoScalingGroup { asgMaxSize :: Integer
                                         , asgMinSize :: Integer
                                         , asgSubnets :: [Subnet]
                                         } deriving (Eq, Show)

instance ToJSON AutoScalingGroup where
  toJSON asg = object
    [ "Type" .= String "AWS::AutoScaling::AutoScalingGroup"
    , "Properties" .= object
      [ "HealthCheckGracePeriod" .= Number 900
      , "HealthCheckType" .= String "ELB"
      , "LaunchConfigurationName" .= Ref "launchConfiguration"
      , "LoadBalancerNames" .= [Ref "elasticLoadBalancer"]
      , "MaxSize" .= asgMaxSize asg
      , "MinSize" .= asgMinSize asg
      , "Tags" .= [nameTag]
      , "TerminationPolicies" .= [String "OldestInstance"]
      , "VPCZoneIdentifier" .= asgSubnets asg
      ]
    , "CreationPolicy" .= object
      [ "ResourceSignal" .= object
        [ "Count" .= asgMinSize asg
        , "Timeout" .= String "PT15M"
        ]
      ]
    , "UpdatePolicy" .= object
      [ "AutoScalingRollingUpdate" .= object
        [ "MinInstancesInService" .= Number 1
        , "MaxBatchSize" .= Number 1
        , "PauseTime" .= String "PT15M"
        , "WaitOnResourceSignals" .= True
        ]
      ]
    ]

instance FromConfig AutoScalingGroup where
  fromConfig c = AutoScalingGroup { asgMaxSize = capMax . cfgCapacity $ c
                                  , asgMinSize = capMin . cfgCapacity $ c
                                  , asgSubnets = cfgInstanceSubnets c
                                  }


-- Ingress Rule

data IngressRule = IngressRule { irIpProtocol :: Text
                               , irFromPort :: Port
                               , irToPort :: Port
                               , irCidrIp :: Text
                               } deriving (Eq, Show)

instance ToJSON IngressRule where
  toJSON ir = object [ "IpProtocol" .= irIpProtocol ir
                     , "FromPort" .= irFromPort ir
                     , "ToPort" .= irToPort ir
                     , "CidrIp" .= irCidrIp ir
                     ]

-- SecurityGroup

data SecurityGroup = SecurityGroup { sgVPC :: VPC
                                   , sgIngress :: [IngressRule]
                                   } deriving (Eq, Show)

instance ToJSON SecurityGroup where
  toJSON sg = object
    [ "Type" .= String "AWS::EC2::SecurityGroup"
    , "Properties" .= object
      [ "GroupDescription" .= String "Generated by cirrus."
      , "SecurityGroupIngress" .= sgIngress sg
      , "VpcId" .= sgVPC sg
      ]
    ]

instance FromConfig SecurityGroup where
  fromConfig c = SecurityGroup { sgVPC = cfgVPC c
                               , sgIngress = map ingressRule (cfgListeners c)
                               }

ingressRule :: Listener -> IngressRule
ingressRule l = IngressRule { irIpProtocol = "tcp"
                            , irFromPort = lnrPort l
                            , irToPort = lnrPort l
                            , irCidrIp = "0.0.0.0/0"
                            }

-- Elastic Load Balancer

data ElasticLoadBalancer = ElasticLoadBalancer { elbHealthCheck :: HealthCheck
                                               , elbSubnets :: [Subnet]
                                               , elbListeners :: [Listener]
                                               } deriving (Eq, Show)


instance ToJSON ElasticLoadBalancer where
  toJSON elb = object
    [ "Type" .= String "AWS::ElasticLoadBalancing::LoadBalancer"
    , "Properties" .= object
      [ "ConnectionDrainingPolicy" .= object
        [ "Enabled" .= True
        , "Timeout" .= Number 60
        ]
      , "CrossZone" .= True
      , "HealthCheck" .= elbHealthCheck elb
      , "Listeners" .= elbListeners elb
      , "SecurityGroups" .= [Ref "securityGroup"]
      , "Subnets" .= elbSubnets elb
      ]
    ]

instance FromConfig ElasticLoadBalancer where
  fromConfig c = ElasticLoadBalancer { elbHealthCheck = cfgHealthCheck c
                                     , elbSubnets = cfgLoadBalancerSubnets c
                                     , elbListeners = cfgListeners c
                                     }

-- Launch Configuration

data LaunchConfiguration = LaunchConfiguration { lcAMI :: AMI
                                               , lcInstanceType :: InstanceType
                                               , lcUserData :: UserData
                                               } deriving (Eq, Show)

instance ToJSON LaunchConfiguration where
  toJSON lc = object
    [ "Type" .= String "AWS::AutoScaling::LaunchConfiguration"
    , "Properties" .= object
      [ "ImageId" .= lcAMI lc
      , "InstanceType" .= lcInstanceType lc
      , "SecurityGroups" .= [Ref "securityGroup"]
      , "UserData" .= lcUserData lc
      ]
    ]

instance FromConfig LaunchConfiguration where
  fromConfig c = LaunchConfiguration { lcAMI = cfgAMI c
                                     , lcInstanceType = cfgInstanceType c
                                     , lcUserData = cfgUserData c
                                     }

-- Template

data Template = Template { tAutoScalingGroup :: AutoScalingGroup
                         , tELB :: ElasticLoadBalancer
                         , tLaunchConfiguration :: LaunchConfiguration
                         , tSecurityGroup :: SecurityGroup
                         } deriving (Eq, Show)

instance ToJSON Template where
  toJSON t = object
    [ "AWSTemplateFormatVersion" .= String "2010-09-09"
    , "Resources" .= object
      [ "autoScalingGroup" .= tAutoScalingGroup t
      , "elasticLoadBalancer" .= tELB t
      , "launchConfiguration" .= tLaunchConfiguration t
      , "securityGroup" .= tSecurityGroup t
      ]
    ]

instance FromConfig Template where
  fromConfig c = Template { tAutoScalingGroup = fromConfig c
                          , tELB = fromConfig c
                          , tLaunchConfiguration = fromConfig c
                          , tSecurityGroup = fromConfig c
                          }

-- Stack

data Stack = Stack { stackName :: Text
                   , stackTemplate :: Template
                   } deriving (Eq, Show)
