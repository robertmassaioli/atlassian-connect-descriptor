{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Connect.Modules
   ( Modules(..)
   , JIRAModules(..)
   , emptyJIRAModules
   , ConfluenceModules(..)
   , emptyConfluenceModules
   , WebPanel(..)
   , GeneralPage(..)
   ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Connect.AesonHelpers
import           Data.Connect.BaseTypes
import           Data.Connect.Conditions
import           Data.Connect.Webhooks
import qualified Data.HashMap.Strict       as HM
import qualified Data.Text                 as T
import           GHC.Generics

-- | 'Modules' are perhaps the most important part of your Atlassian Connect descriptor. They specify which parts of the
-- host application you wish to inject content into. They provide your entry point into the host application.
--
-- Atlassian Connect provides a large set of pre-defined entry points into the host application. Some of which are common
-- to every application and some of which are unique to the particular application that you are targeting:
--
-- * To see the JIRA modules: <https://developer.atlassian.com/static/connect/docs/modules/jira/index.html>
-- * To see the Confluence modules: <https://developer.atlassian.com/static/connect/docs/modules/confluence/index.html>
--
-- Note: One important point about modules: they must all have a key and that key must be unique inside the same Atlassian
-- Connect addon.
data Modules = Modules
   { jiraModules       :: JIRAModules -- ^ All of the JIRA Modules that you wish to define.
   , confluenceModules :: ConfluenceModules -- ^ All of the Confluence modules that you wish to define.
   } deriving (Show, Generic)

instance ToJSON Modules where
   toJSON modules = case (jm, cm) of
      (Object jiraObject, Object confluenceObject) -> Object $ HM.union jiraObject confluenceObject
      _ -> Null
      where
         jm = toJSON . jiraModules $ modules
         cm = toJSON . confluenceModules $ modules

-- TODO use Endo Modules to add modules for multiple different products to the modules list

-- | A collection of all of the JIRA Modules that you can define. For more documentation on which Modules are supported
-- the Atlassian Connect framework please see 'Modules'. You can also find more documentation on each of the modules.
data JIRAModules = JIRAModules
   { jiraWebPanels    :: [WebPanel]
   , jiraGeneralPages :: [GeneralPage]
   , jiraWebhooks     :: [Webhook]
   } deriving (Show, Generic)

instance ToJSON JIRAModules where
   toJSON = genericToJSON baseOptions

-- | A collection of all of the Confluence Modules that you can define. For more documentation on which Modules are supported
-- the Atlassian Connect framework please see 'Modules'. You can also find more documentation on each of the modules.
data ConfluenceModules = ConfluenceModules
   { confluenceWebPanels :: [WebPanel]
   } deriving (Show, Generic)

instance ToJSON ConfluenceModules where
   toJSON = genericToJSON baseOptions

-- | Empty JIRA Modules; useful when you only want to define a few modules via Haskell record syntax.
emptyJIRAModules :: JIRAModules
emptyJIRAModules = JIRAModules [] [] []

-- | Empty Confluence Modules; useful when you only want to define a few modules via Haskell record syntax.
emptyConfluenceModules :: ConfluenceModules
emptyConfluenceModules = ConfluenceModules []

-- | A 'WebPanel' is an injectable segment of the host application that you can place content inside. Currently the
-- WebPanel has the same structure for both JIRA and Confluence but, potentially, that could change in the future.
-- You can read their Atlassian Connect documentation here:
--
-- * JIRA Web panels: <https://developer.atlassian.com/static/connect/docs/modules/jira/web-panel.html>
-- * Confluence Web panels: <https://developer.atlassian.com/static/connect/docs/modules/confluence/web-panel.html>
--
-- Here is what an example Hello World web panel might look like:
--
-- > helloWorldWebPanel = WebPanel
-- >    { wpKey = "hello-world"
-- >    , wpName = Name "Hello world!"
-- >    , wpUrl = "/panel/show-hello-world"
-- >    , wpLocation = "atl.jira.view.issue.right.context"
-- >    , wpConditions = [staticJiraCondition UserIsLoggedInJiraCondition]
-- >    }
-- >    where
-- >       toURI = fromJust . parseRelativeReference
--
-- WebPanels are a great way to inject your add-on's content into the host application.
data WebPanel = WebPanel
   { wpKey        :: T.Text -- ^ The add-on unique key for this module.
   , wpName       :: Name WebPanel -- ^ The name of this panel, likely to appear in the User Interface.
   , wpUrl        :: T.Text -- ^ The relative URI that the host product will hit to get HTML content.
   , wpLocation   :: T.Text -- ^ The location that this content should be injected in the host product.
   , wpConditions :: [Condition] -- ^ The 'Condition's that need to be met for this module to be displayed.
   } deriving (Show, Generic)

instance ToJSON WebPanel where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "wp"
      }

-- | A 'GeneralPage' makes your add-on take a large section of screen realestate, with the intention of displaying
-- your own page with no other distracting content. This is very useful for pages like Configuration screens or
-- Statistics pages where you really want the user to be fully working with your add-on inside the host product.
--
-- General pages, like Web Panels, are common to JIRA and Confluence and share the same json format. However they have
-- separate documentation:
--
-- * JIRA General Page: <https://developer.atlassian.com/static/connect/docs/modules/jira/general-page.html>
-- * Confluence General Page: <https://developer.atlassian.com/static/connect/docs/modules/confluence/general-page.html>
--
-- Even though, at this point in time, the documentation looks identical.
data GeneralPage = GeneralPage
   { generalPageKey        :: T.Text -- ^ The add-on unique key for this module.
   , generalPageName       :: Name GeneralPage -- ^ The name of this General Page. Likely to be used in the page title.
   , generalPageUrl        :: T.Text -- ^ The relative URI that the host product will hit to get the HTML content for the page.
   , generalPageLocation   :: Maybe T.Text -- ^ The location for this General Page to display; see the docs for your options.
   , generalPageIcon       :: Maybe IconDetails -- ^ The optional icon to use for this general page.
   , generalPageWeight     :: Maybe Integer -- ^ Determines the order that this item appears in any menu or list.
                                            -- Lower numbers mean that it will appear higher in the list.
   , generalPageConditions :: [Condition] -- ^ The 'Condition's that need to be met for this module to be displayed.
   } deriving (Show, Generic)

instance ToJSON GeneralPage where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "generalPage"
      }

instance ToJSON (Name WebPanel) where
   toJSON = nameToValue

instance ToJSON (Name GeneralPage) where
   toJSON = nameToValue

nameToValue :: Name a -> Value
nameToValue (Name name) = object [ T.pack "value" .= name ]
