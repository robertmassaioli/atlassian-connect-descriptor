{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Data.Connect.Descriptor
Description : Defines a strictly typed Atlassian Connect add-on descriptor.
Copyright   : (c) Robert Massioli, 2014
License     : APACHE-2
Maintainer  : rmassaioli@atlassian.com
Stability   : experimental

This module provides the data types to let you write your own typesafe Atlassian Connect descriptor and it comes with
Aeson bindings so that you can easily convert into json: the format that the Atlassian Connect framework expects.

Atlassian Connect is a framework for writing Add-on's that can run inside the Atlassian Cloud products. You can find more
information from the Atlassian Connect documentation <https://developer.atlassian.com/static/connect/docs/guides/introduction.html>.

The plugin descriptor is defined by the 'Plugin' class. The end result of using this Haskell Module should be for you to
end up with a valid 'Plugin'. To turn your plugin into JSON that the Atlassian marketplace can accept just use the 'encode'
function from the Aeson library. For example, here in an example Atlassian Connect Descriptor:

> pluginToJsonString :: Plugin -> ByteString
> pluginToJsonString = encode
>
> exampleDescriptor :: Plugin
> exampleDescriptor = (pluginDescriptor (PluginKey "my-example-connect") baseURL (Authentication Jwt))
>     { pluginName = Just . Name $ "My Example Connect Addon"
>     , pluginDescription = Just "This is an example connect descriptor."
>     , vendor = Just $ Vendor (Name "Awesome Devs") (toURI "http://awesome-devs.com")
>     , lifecycle = Just defaultLifecycle
>     , modules = Just exampleModules
>     , enableLicensing = Just False
>     , links = HM.fromList
>         [ ("documentation", toURI "http://awesome-devs.com/docs")
>         , ("source", toURI "http://bitbucket.org/awesome-devs/connect-addon")
>         ]
>     , scopes = Just [Read, Admin]
>     }
>
> exampleModules :: Modules
> exampleModules = Modules exampleJIRAModules emptyConfluenceModules
>
> exampleJIRAModules :: JIRAModules
> exampleJIRAModules = emptyJIRAModules
>     { jmWebPanels = Just
>         [ WebPanel
>             { wpKey = "test-web-panel"
>             , wpName = simpleText "Test Web Panel"
>             , wpTooltip = Just $ simpleText "This is a test web panel..."
>             , wpLocation = "some-location-in-jira"
>             , wpUrl = "/panel/location/for"
>             , wpConditions = [staticJiraCondition UserIsAdminJiraCondition]
>             , wpWeight = Nothing
>             , wpLayout = Nothing
>             , wpParams = noParams
>             }
>         ]
>     , jmGeneralPages = Just
>         [ JIRAPage
>             { jiraPageKey = "test-general-page"
>             , jiraPageName = simpleText "Test General Page"
>             , jiraPageLocation = Just "some-other-location-in-jira"
>             , jiraPageWeight = Just 1234
>             , jiraPageUrl = "/panel/general-page"
>             , jiraPageIcon = Just IconDetails
>                 { iconUrl = "/static/path/to/icon.png"
>                 , iconWidth = Just 20
>                 , iconHeight = Just 40
>                 }
>             , jiraPageConditions = [staticJiraCondition UserHasIssueHistoryJiraCondition]
>             , jiraPageParams = noParams
>             }
>         ]
>     , jmWebhooks = Just
>         [ Webhook
>             { webhookEvent = JiraIssueDeleted
>             , webhookUrl = "/webhook/handle-deletion"
>             }
>         ]
>     }

You can use this library to make your own. This library will experience change whenever the Atlassian Connect descriptor
changes. There are likely to be many breaking changes but we will keep track of them using the standard Haskell version
structure.
-}
module Data.Connect.Descriptor (
   -- * Atlassian Connect Add-on Descriptor
     Plugin(..)
   , pluginDescriptor
   -- * Basic Types
   , Key(..)
   , PluginKey(..)
   , Timeout(..)
   , Vendor(..)
   , Authentication(..)
   , AuthType(..)
   , IconDetails(..)
   , Name(..)
   , I18nText(..)
   , simpleText
   , URLBean(..)
   , toUrl
   , Length(..)
   , Weight
   , ModuleParams
   , noParams
   -- * Lifecycle
   , Lifecycle(..)
   , emptyLifecycle
   , defaultLifecycle
   -- * Add-on Modules
   , Modules(..)
   , JIRAModules(..)
   , emptyJIRAModules
   , ConfluenceModules(..)
   , emptyConfluenceModules
   -- ** Web Sections, Items and Panels
   , JIRAWebSection(..)
   , WebItem(..)
   , WebItemContext(..)
   , WebPanel(..)
   , WebPanelLayout(..)
   -- ** JIRA Pages
   , JIRAPage(..)
   -- ** JIRA Tab Panels
   , JIRAGenericTabPanel(..)
   , JIRAProjectAdminTabPanel(..)
   -- ** JIRA Specific Modules
   , JIRASearchRequestView(..)
   , JIRAReport(..)
   , JIRAReportCategory(..)
   , Target(..)
   , JIRAWorkflowPostFunction(..)
   , DialogOptions(..)
   , InlineDialogOptions(..)
   , JIRAEntityProperties(..)
   , EntityType(..)
   , KeyConfiguration(..)
   , Extraction(..)
   , ExtractionType(..)
   -- ** Webhooks
   , Webhook(..)
   , WebhookEvent(..)
   -- * Module Conditions
   , Condition(..)
   , ConditionType(..)
   , ConditionSource(..)
   , remoteCondition
   , JIRACondition(..)
   , staticJiraCondition
   , ConfluenceCondition(..)
   , staticConfluenceCondition
   , invertCondition
   -- * Scopes (Permissions)
   , ProductScope(..)
   ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Connect.AesonHelpers
import           Data.Connect.BaseTypes
import           Data.Connect.Conditions
import           Data.Connect.Lifecycle
import           Data.Connect.Modules
import           Data.Connect.Scopes
import           Data.Connect.Webhooks
import qualified Data.HashMap.Strict       as HM
import           Data.Text
import           GHC.Generics
import           Network.URI

-- | A 'Plugin' is the end result of an Atlassian Connect descriptor. It is what you should provide to the Atlassian
-- Marketplace in order to register your plugin and it is what your Atlassian Cloud customers will download to install
-- your Add-on via the Atlassian UPM (Universal Plugin Manager). Only a very small number of fields are strictly required
-- to generate a valid Atlassian Connect plugin descriptor. Everything that is optional is marked by a maybe type.
--
-- Even though we provide documentation here you shoucd check the Atlassian Connect Descriptor documentation if you want
-- to get accurate information on the contents of a plugin: <https://developer.atlassian.com/static/connect/docs/modules/>
data Plugin = Plugin
   { pluginKey         :: PluginKey -- ^ Plugin keys are required. The important detail about this key is that it should
                                    -- be unique across the Atlassian Marketplace. For example, a good key might be
                                    -- @com.yourcompanyorpersonalname.youraddonname@ because it would be unique in the
                                    -- marketplace.
   , pluginBaseUrl     :: URI -- ^ Every plugin must specify a base url where all other relative URI's in the plugin will
                              -- call to in production. This is the url that the Atlassian Marketplace will query for your
                              -- descriptor and the url that your customers will come in on. This is especially important
                              -- for load balanced applications and will also likely be different in your staging and production
                              -- environments.
   , authentication    :: Authentication -- ^ The authentication type that you plugin requires. See 'Authentication' for more details.
   , pluginName        :: Maybe (Name Plugin) -- ^ While your add-on does not require a name it is strongly recommended
                                              -- as this is the human readable name that will appear in the UPM amongst other places.
   , pluginDescription :: Maybe Text -- ^ You should give your add-on a description. This description will appear in multiple
                                     -- locations on the Atlassian Marketplace and UPM and will be used to explain what your
                                     -- add-on does.
   , vendor            :: Maybe Vendor -- ^ You are the 'Vendor'. Put your details here!
   , lifecycle         :: Maybe Lifecycle -- ^ Atlassian Connect addon's have a lifecycle. Register your handlers for
                                          -- the 'Lifecycle' events here so that you can tell, for example, when your
                                          -- addon is installed or enabled.
   , modules           :: Maybe Modules -- ^ The modules that your Atlassian Connect add-on provides to the Cloud application. Look at the 'Modules' documentaiton for more information.
   , apiVersion        :: Maybe Text -- ^ Required if you wish to provide new versions of your addon to a subset of beta customers.
   , enableLicensing   :: Maybe Bool -- ^ If you are giving away a free add-on then you can set this to false, otherwise set it to true.
   , links             :: HM.HashMap Text URI -- ^ A collection of custom links that you wish to publish with your add-on. Like documentation or bug-tracking links.
   , scopes            :: Maybe [ProductScope] -- ^ The scopes that your add-on requires. See 'ProductScope' for more information.
   } deriving (Show, Generic)

instance ToJSON (Name Plugin)

instance ToJSON Plugin where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "plugin"
      }

-- | A helper method to generate a bare-bones Atlassian Connect add-on by providing only the absolutely required fields.
-- You can then use Haskell record syntax to update the plugin with more details. For example:
--
-- > (pluginDescriptor (PluginKey . pack $ "com.company.mycoolplugin") (fromJust . parseURI $ "http://mycoolplugin.company.com") (Authentication Jwt))
-- >    { pluginName = Just . Name . pack $ "My Cool Plugin"
-- >    , pluginDescription = Just . pack $ "Chil and be cool, you have a plugin descriptor."
-- >    }
pluginDescriptor
   :: PluginKey -- ^ The key for your add-on.
   -> URI -- ^ The base url for your add-on.
   -> Authentication -- ^ The authentication that your add-on requires.
   -> Plugin -- ^ A bare-bones Atlassian Connect descriptor.
pluginDescriptor key' url' auth = Plugin
   { pluginKey = key'
   , pluginBaseUrl = url'
   , authentication = auth
   , pluginName = Nothing
   , pluginDescription = Nothing
   , vendor = Nothing
   , apiVersion = Nothing
   , modules = Nothing
   , enableLicensing = Nothing
   , lifecycle = Nothing
   , links = HM.empty
   , scopes = Nothing
   }
