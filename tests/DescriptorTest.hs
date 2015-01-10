{-# LANGUAGE OverloadedStrings #-}
module DescriptorTest (descriptorTests) where

import           AssertionHelpers
import           Control.Applicative
import           Data.Aeson
import           Data.Connect.Descriptor
import qualified Data.HashMap.Strict     as HM
import           Data.Maybe              (fromJust)
import qualified Data.Text               as T
import           Network.URI
import           Test.HUnit
import           ValueExtractors
import qualified Data.ByteString.Lazy.Char8 as BLC

toURI :: String -> URI
toURI = fromJust . parseURI

baseURL :: URI
baseURL = toURI "http://my-example.connect.com"

exampleDescriptor1 :: Plugin
exampleDescriptor1 = (pluginDescriptor (PluginKey "my-example-connect") baseURL (Authentication Jwt))
    { pluginName = Just . Name $ "My Example Connect Addon"
    , pluginDescription = Just "This is an example connect descriptor."
    , vendor = Just $ Vendor (Name "Awesome Devs") (toURI "http://awesome-devs.com")
    , lifecycle = Just defaultLifecycle
    , modules = Just exampleModules1
    , enableLicensing = Just False
    , links = HM.fromList
        [ ("documentation", toURI "http://awesome-devs.com/docs")
        , ("source", toURI "http://bitbucket.org/awesome-devs/connect-addon")
        ]
    , scopes = Just [Read, Admin]
    }

exampleModules1 :: Modules
exampleModules1 = Modules exampleJIRAModules emptyConfluenceModules

exampleJIRAModules :: JIRAModules
exampleJIRAModules = emptyJIRAModules
    { jiraWebPanels =
        [ WebPanel
            { wpKey = "test-web-panel"
            , wpName = simpleText "Test Web Panel"
            , wpLocation = "some-location-in-jira"
            , wpUrl = "/panel/location/for"
            , wpConditions = [staticJiraCondition UserIsAdminJiraCondition]
            , wpParams = HM.singleton "single" "ton"
            , wpTooltip = Just . simpleText $ "A random tooltip here."
            , wpWeight = Just 2468
            , wpLayout = Just $ WebPanelLayout (Pixels 963) (Percentage 40)
            }
        ]
    , jiraGeneralPages =
        [ GeneralPage
            { generalPageKey = "test-general-page"
            , generalPageName = simpleText "Test General Page"
            , generalPageLocation = Just "some-other-location-in-jira"
            , generalPageWeight = Just 1234
            , generalPageUrl = "/panel/general-page"
            , generalPageIcon = Just IconDetails
                { iconUrl = "/static/path/to/icon.png"
                , iconWidth = Just 20
                , iconHeight = Just 40
                }
            , generalPageConditions = [staticJiraCondition UserHasIssueHistoryJiraCondition]
            , generalPageParams = HM.empty
            }
        ]
    , jiraWebhooks =
        [ Webhook
            { webhookEvent = JiraIssueDeleted
            , webhookUrl = "/webhook/handle-deletion"
            }
        ]
    }

descriptorTests :: Test
descriptorTests = TestList
    [ basicDescriptorTest
    ]

basicDescriptorTest :: Test
basicDescriptorTest = TestCase $ do
    let jv = toJSON exampleDescriptor1
    BLC.putStrLn . encode $ exampleDescriptor1
    isObject jv @? "Expect the descriptor to be one big json object."
    (getString =<< get "key" jv) `isEqualTo` "my-example-connect"
    (getString =<< get "baseUrl" jv) `isEqualTo` (T.pack . show $ baseURL)
    (getString =<< get "type" =<< get "authentication" jv) `isEqualTo` "jwt"
    (getString =<< get "name" jv) `isEqualTo` "My Example Connect Addon"
    (getString =<< get "description" jv) `isEqualTo` "This is an example connect descriptor."
    (getString =<< get "name" =<< get "vendor" jv) `isEqualTo` "Awesome Devs"
    (getString =<< get "url" =<< get "vendor" jv) `isEqualTo` "http://awesome-devs.com"
    (isObject <$> get "lifecycle" jv) @? "Expected the lifecycle to be an object."
    (isObject <$> get "modules" jv) @? "Expected it to be an array of modules."
    (getString =<< get "documentation" =<< get "links" jv) `isEqualTo` "http://awesome-devs.com/docs"
    (getString =<< get "source" =<< get "links" jv) `isEqualTo` "http://bitbucket.org/awesome-devs/connect-addon"
    (getArray =<< get "scopes" jv) `isEqualTo` ["read", "admin"]
    (not <$> (getBool =<< get "enableLicensing" jv)) @? "Expected licensing to be disabled."
