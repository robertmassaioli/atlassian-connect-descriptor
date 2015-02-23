{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Connect.Modules
   ( Modules(..)
   , JIRAModules(..)
   , emptyJIRAModules
   , ConfluenceModules(..)
   , emptyConfluenceModules
   , JIRAWebSection(..)
   , WebItem(..)
   , WebItemContext(..)
   , WebPanel(..)
   , WebPanelLayout(..)
   , JIRAPage(..)
   , JIRAGenericTabPanel(..)
   , JIRAProjectAdminTabPanel(..)
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
   , ModuleParams
   , noParams
   , Weight
   ) where

{-
Supported JIRA Modules

webSections
webItems
webPanels

generalPages
adminPages
configurePage

jiraProfileTabPanels
jiraVersionTabPanels
jiraProjectTabPanels
jiraProjectAdminTabPanels
jiraIssueTabPanels
jiraComponentTabPanels

jiraSearchRequestViews

jiraReports

webhooks

jiraWorkflowPostFunctions

jiraEntityProperties

-}

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
   { jmWebSections               :: Maybe [JIRAWebSection]
   , jmWebItems                  :: Maybe [WebItem]
   , jmWebPanels                 :: Maybe [WebPanel]
   , jmGeneralPages              :: Maybe [JIRAPage]
   , jmAdminPages                :: Maybe [JIRAPage]
   , jmConfigurePage             :: Maybe JIRAPage
   , jmJiraSearchRequestViews    :: Maybe [JIRASearchRequestView]
   , jmJiraProfileTabPanels      :: Maybe [JIRAGenericTabPanel]
   , jmJiraVersionTabPanels      :: Maybe [JIRAGenericTabPanel]
   , jmJiraProjectTabPanels      :: Maybe [JIRAGenericTabPanel]
   , jmJiraProjectAdminTabPanels :: Maybe [JIRAProjectAdminTabPanel]
   , jmJiraIssueTabPanels        :: Maybe [JIRAGenericTabPanel]
   , jmJiraComponentTabPanels    :: Maybe [JIRAGenericTabPanel]
   , jmJiraReports               :: Maybe [JIRAReport]
   , jmWebhooks                  :: Maybe [Webhook]
   , jmJiraWorkflowPostFunctions :: Maybe [JIRAWorkflowPostFunction]
   , jmJiraEntityProperties      :: Maybe [JIRAEntityProperties]
   } deriving (Show, Generic)

instance ToJSON JIRAModules where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "jm"
      }

-- | A collection of all of the Confluence Modules that you can define. For more documentation on which Modules are supported
-- the Atlassian Connect framework please see 'Modules'. You can also find more documentation on each of the modules.
data ConfluenceModules = ConfluenceModules
   { confluenceWebPanels :: Maybe [WebPanel]
   } deriving (Show, Generic)

instance ToJSON ConfluenceModules where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "confluence"
      }

-- | Empty JIRA Modules; useful when you only want to define a few modules via Haskell record syntax.
emptyJIRAModules :: JIRAModules
emptyJIRAModules
   = JIRAModules
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing

-- | Empty Confluence Modules; useful when you only want to define a few modules via Haskell record syntax.
emptyConfluenceModules :: ConfluenceModules
emptyConfluenceModules = ConfluenceModules Nothing

-- | Represents the weight of an element in a menu.
type Weight = Integer

-- | The standard representation for module parameters.
type ModuleParams = HM.HashMap T.Text T.Text

-- | No parameters. A useful helper when you don't want to pass any parameters to a module.
noParams :: ModuleParams
noParams = HM.empty

-- | A 'JIRAWebSection' represents a location in the host application that you can add 'WebItem's to. In this way you
-- can give your add-on sections to inject content into.
--
-- For more information read the Atlassian Connect documentation:
-- <https://developer.atlassian.com/static/connect/docs/modules/jira/web-section.html>
data JIRAWebSection = JIRAWebSection
   { jwsKey        :: T.Text -- ^ The add-on unique key for this module.
   , jwsName       :: I18nText -- ^ The name of this section, likely to appear in the User Interface.
   , jwsLocation   :: T.Text -- ^ The location in the application interface where the web section should appear.
   , jwsTooltip    :: Maybe I18nText -- ^ The internationalised text to be used in the link's tooltip.
   , jwsConditions :: [Condition] -- ^ The conditions under which to show this web section.
   , jwsWeight     :: Maybe Weight -- ^ The higher the weight the lower down the menu it will appear.
   , jwsParams     :: ModuleParams -- ^ Optional parameters to pass to the web section.
   } deriving (Show, Generic)

instance ToJSON JIRAWebSection where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "tt"
      }

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
   , wpName       :: I18nText -- ^ The name of this panel, likely to appear in the User Interface.
   , wpUrl        :: T.Text -- ^ The relative URI that the host product will hit to get HTML content.
   , wpLocation   :: T.Text -- ^ The location that this content should be injected in the host product.
   , wpConditions :: [Condition] -- ^ The 'Condition's that need to be met for this module to be displayed.
   , wpTooltip    :: Maybe I18nText -- ^ A tooltip that explains what this is for.
   , wpWeight     :: Maybe Weight -- ^ Web panels can be ordered and a higher weight makes you appear lower down the page.
   , wpLayout     :: Maybe WebPanelLayout -- ^ You can specify the dimensions of this panel. This will only be considered in certain locations.
   , wpParams     :: ModuleParams -- ^ You can pass parameters to the web panel.
   } deriving (Show, Generic)

instance ToJSON WebPanel where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "wp"
      }

-- | A 'WebPanelLayout' allows you to specify the dimensions of your Web Panel if that is required.
data WebPanelLayout = WebPanelLayout
   { wplWidth  :: Length
   , wplHeight :: Length
   } deriving (Show, Generic)

instance ToJSON WebPanelLayout where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "wpl"
      }

-- | A 'WebItem' is a like that can be placed in one of an Atlassian Products many menus. Currently the WebItem has the
-- same structure for both JIRA and Confluence. You can read their documentation here:
--
-- * JIRA Web Items: <https://developer.atlassian.com/static/connect/docs/modules/jira/web-item.html>
-- * Confluence Web Items: <https://developer.atlassian.com/static/connect/docs/modules/confluence/web-item.html>
--
-- Web items are very useful for providing links to your Atlassian Connect pages. See 'GeneralPage' or 'AdminPage' for
-- more information.
data WebItem = WebItem
   { wiKey          :: T.Text -- ^ The add-on unique key for this module.
   , wiName         :: I18nText -- ^ The name of this web item. It will appear as the text of the link.
   , wiLocation     :: T.Text -- ^ Where in the product UI this web item will appear.
   , wiUrl          :: T.Text -- ^ The URL to direct the user to. May be relative to the host Product or the Addon depending on the context.
   , wiTooltip      :: Maybe I18nText -- ^ An optional tooltip for the link.
   , wiIcon         :: Maybe IconDetails -- ^ An optional icon to display with the link text or as the link
   , wiWeight       :: Maybe Weight -- ^ The higher the weight the lower down in the location menu this web item will appear.
   , wiTarget       :: Maybe Target -- ^ Determines the way the link is opened. In the page, in a dialog or in an inline dialog.
   , wiStyleClasses :: [T.Text] -- ^ Specifies custom styles for the web item target page
   , wiContext      :: Maybe WebItemContext -- ^ Determines if the url is relative to the page, product or connect addon.
   , wiConditions   :: [Condition] -- ^ Determines the conditions under which to show this link.
   , wiParams       :: ModuleParams -- ^ Optional parameters that you can pass to the web item.
   } deriving (Show, Generic)

instance ToJSON WebItem where
   toJSON = genericToJSON baseOptions
         { fieldLabelModifier = lowerAll . stripFieldNamePrefix "wi"
         }

-- | A 'Target' represents the location that a link will be opened into.
data Target
   = TargetPage -- ^ Open the link into the current page.
   | TargetDialog (Maybe DialogOptions) -- ^ Open the link into a dialog on the page with the given options.
   | TargetInlineDialog (Maybe InlineDialogOptions) -- ^ Open the link into an inline dialog with the given options.
   deriving (Show)

tp :: String -> T.Text
tp = T.pack

instance ToJSON Target where
   toJSON (TargetPage) = object [tp "type" .= tp "page"]
   toJSON (TargetDialog potentialOptions) = object $ tp "type" .= tp "dialog" :
      case potentialOptions of
         Just options -> [tp "options" .= toJSON options]
         Nothing -> []
   toJSON (TargetInlineDialog potentialOptions) = object $ tp "type" .= tp "inlinedialog" :
      case potentialOptions of
               Just options -> [tp "options" .= toJSON options]
               Nothing -> []

-- | Options for a dialog that a link may be opened into.
data DialogOptions = DialogOptions
   { doHeight :: Maybe Integer -- ^ The height of the dialog on the page.
   , doWidth  :: Maybe Integer -- ^ The width of the dialog on the page.
   , doChrome :: Maybe Bool -- ^ Whether the dialog should contain the AUI header and buttons. Default is true
   } deriving (Show, Generic)

instance ToJSON DialogOptions where
   toJSON = genericToJSON baseOptions
         { fieldLabelModifier = lowerAll . stripFieldNamePrefix "do"
         }

-- | Options for an inline dialog that a link may be opened into.
data InlineDialogOptions = InlineDialogOptions
   { idoWidth             :: Maybe T.Text -- ^ Sets how wide the inline-dialog is in pixels
   , idoOnTop             :: Maybe Bool -- ^ Determines if the dialog should be shown above the trigger or not
   , idoIsRelativeToMouse :: Maybe Bool -- ^ Determines if the dialog should be shown relative to where the mouse is at the time of the event trigger
   , idoShowDelay         :: Maybe Integer -- ^ Determines how long in milliseconds after a show trigger is fired until the dialog is shown
   , idoOnHover           :: Maybe Bool -- ^ Determines whether the inline-Dialog will show on a mouseOver or mouseClick of the trigger
   , idoOffsetX           :: Maybe T.Text -- ^ Sets an offset distance of the inline-dialog from the trigger element along the x-axis in pixels
   , idoOffsetY           :: Maybe T.Text -- ^ Sets an offset distance of the inline-dialog from the trigger element along the y-axis in pixels
   , idoPersistent        :: Maybe Bool -- ^ This option, ignores the 'closeOthers' option
   , idoCloseOthers       :: Maybe Bool -- ^ Cetermines if all other dialogs on the screen are closed when this one is opened
   } deriving (Show, Generic)

instance ToJSON InlineDialogOptions where
   toJSON = genericToJSON baseOptions
         { fieldLabelModifier = lowerAll . stripFieldNamePrefix "ido"
         }

-- TODO this cannot have a generic implimentation

-- | Wether to open the url relative to the current page, the current addon or the current product. This lets you control
-- where Atlassian Connect web items point to.
data WebItemContext = PageContext | AddonContext | ProductContext
   deriving(Show, Generic)

instance ToJSON WebItemContext where
   toJSON PageContext = stj "page"
   toJSON AddonContext = stj "addon"
   toJSON ProductContext = stj "product"

stj :: String -> Value
stj = String . T.pack

-- | JIRA has the concept of a TabPanel which is a panel where you can inject content along with a 'WebItem' that is
-- automatically created for you so that you can navigate to the tab panel in question. Tab panels often have a common format
-- and this 'JIRAGenericTabPanel' encapsulates that common format and allows you to apply it to many different scenarios.
data JIRAGenericTabPanel = JIRAGenericTabPanel
   { jtpKey        :: T.Text -- ^ The add-on unique key for this module.
   , jtpName       :: I18nText -- ^ The user facing name of this panel. Likely to appear as the name of the link to the tab panel.
   , jtpUrl        :: T.Text -- ^ The URL to your addon where you will provide the content for the panel.
   , jtpConditions :: [Condition] -- ^ The conditions under which this tapb panel should be displayed.
   , jtpWeight     :: Maybe Weight -- ^ The higher the weight the lower down in the list of tabs the link to this tab panel will be displayed.
   , jtpParams     :: ModuleParams -- ^ Optional parameters for the tab panel.
   } deriving (Show, Generic)

instance ToJSON JIRAGenericTabPanel where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "jtp"
      }

-- TODO update the docs for the JIRAProjectAdminTabPanel based on this question: http://goo.gl/c6QUdd

-- | A 'JIRAProjectAdminTabPanel' is useful for when you want to add a page to the administration screens of a project.
-- This module will create a web item in the sidebar of every project for you and provide a web panel in the JIRA Project
-- Admin section.
data JIRAProjectAdminTabPanel = JIRAProjectAdminTabPanel
   { jpatpKey        :: T.Text -- ^ The add-on unique key for this module.
   , jpatpName       :: I18nText -- ^ The user facing name of this panel. Likely to appear as the name of the link to the tab panel.
   , jpatpUrl        :: T.Text -- ^ The URL to your addon where you will provide the content for the panel.
   , jpatpLocation   :: T.Text -- ^ The location in JIRA Admin that you wish the link to this panel to appear.
   , jpatpConditions :: [Condition] -- ^ The conditions under which this panel should be displayed. UserIsAdminCondition is redundant.
   , jpatpWeight     :: Maybe Weight -- ^ The higher the weight the lower down in the list of tabs the link to this tab panel will be displayed.
   , jpatpParams     :: ModuleParams -- ^ Optional parameters for the tab panel.
   } deriving (Show, Generic)

instance ToJSON JIRAProjectAdminTabPanel where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "jpatp"
      }

-- | A 'JIRAPage' is gives you a location in the host product that you addon can present content and behave just like any
-- other host product page. The main types of pages in JIRA are:
--
-- * General pages: for just getting a geniric chunk of realestate to display your content.
-- * Admin pages: for getting a page in the admin seciton of JIRA to display your content.
-- * Configuration page: Every Atlassian Connect plugin can have a configuration page to configure the plugin when installed.
--
-- This is very useful for pages like Configuration screens or Statistics pages where you really want the user to be
-- immersed in working with your add-on inside the host product.
--
-- General pages, like Web Panels, are common to JIRA and Confluence and share the same json format. However they have
-- separate documentation:
--
-- * JIRA General Page: <https://developer.atlassian.com/static/connect/docs/modules/jira/general-page.html>
-- * Confluence General Page: <https://developer.atlassian.com/static/connect/docs/modules/confluence/general-page.html>
--
-- Even though, at this point in time, the documentation looks identical. You can find the Admin page and Configure page
-- documentation in the official Atlassian Connect documentation too.
data JIRAPage = JIRAPage
   { jiraPageKey        :: T.Text -- ^ The add-on unique key for this module.
   , jiraPageName       :: I18nText -- ^ The name of this JIRA page. Likely to be used in the page title.
   , jiraPageUrl        :: T.Text -- ^ The relative URI that the host product will hit to get the HTML content for the page.
   , jiraPageLocation   :: Maybe T.Text -- ^ The location for this General Page to display; see the docs for your options.
   , jiraPageWeight     :: Maybe Weight -- ^ Determines the order that this item appears in any menu or list.
                                        -- Lower numbers mean that it will appear higher in the list.
   , jiraPageIcon       :: Maybe IconDetails -- ^ The optional icon to use for this JIRA page.
   , jiraPageConditions :: [Condition] -- ^ The 'Condition's that need to be met for this page to be displayed.
   , jiraPageParams     :: ModuleParams -- ^ Optional parameters for the page.
   } deriving (Show, Generic)

instance ToJSON JIRAPage where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "jiraPage"
      }

-- |  A Search Request View allows you to render a custom representation of a search result. Rendering a custom XML
-- format is a common example.
--
-- For more information read the Atlassian Connect documentation:
-- <https://developer.atlassian.com/static/connect/docs/modules/jira/search-request-view.html>
data JIRASearchRequestView = JIRASearchRequestView
   { jsrvKey         :: T.Text -- ^ The add-on unique key for this module.
   , jsrvName        :: I18nText -- ^ The name of this Search Request View. Will appear in the Export menu.
   , jsrvUrl         :: T.Text -- ^ This URL will render the search results.
   , jsrvDescription :: Maybe I18nText -- ^ The description of this view.
   , jsrvWeight      :: Maybe Weight -- ^ A higher weight puts the link further down the export menu.
   , jsrvConditions  :: [Condition] -- ^ The conditions under which this option should show.
   , jsrvParams      :: ModuleParams -- ^ The optional parameters to this search request view.
   } deriving (Show, Generic)

instance ToJSON JIRASearchRequestView where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "jsrv"
      }

-- | A 'JIRAReport' will provide a report on the JIRA issues in the project. It allows you to write custom reporting
-- for JIRA.
--
-- For more information read the Atlassian Connect documentation:
-- <https://developer.atlassian.com/static/connect/docs/modules/jira/report.html>
data JIRAReport = JIRAReport
   { jrKey            :: T.Text -- ^ The add-on unique key for this module.
   , jrName           :: I18nText -- ^ The user facing name of this report.
   , jrUrl            :: T.Text -- ^ The URL that will render the report back to the user.
   , jrDescription    :: I18nText -- ^ The required user facing description of this report.
   , jrReportCategory :: Maybe JIRAReportCategory -- ^ The category that this report should be placed inside.
   , jrWeight         :: Maybe Weight -- ^ A higher weight will push this report further down the list of reports.
   , jrThumbnailUrl   :: Maybe T.Text -- ^ A thumbnail that gives a gist of what this report is supposed to accomplish.
   } deriving (Show, Generic)

instance ToJSON JIRAReport where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "jr"
      }

-- | The report category for a 'JIRAReport'. Useful in organising the different types of reports.
data JIRAReportCategory
   = AgileRC -- ^ This report is visible to agile customers.
   | IssueAnalysisRC -- ^ This report does issue analysis.
   | ForecastManagementRC -- ^ This report considers future of the project state.
   | OtherRC -- ^ Any report that does not fit into the other categories goes here.
   deriving (Show)

instance ToJSON JIRAReportCategory where
   toJSON AgileRC = stj "agile"
   toJSON IssueAnalysisRC = stj "issue_analysis"
   toJSON ForecastManagementRC = stj "forecast_management"
   toJSON OtherRC = stj "other"

-- | A 'JIRAWorkflowPostFunction' carries out any additional processing required after a JIRA workflow transition is executed.
--
-- For more information read the Atlassian Connect documentation:
-- <https://developer.atlassian.com/static/connect/docs/modules/jira/workflow-post-function.html>
--
-- Or you could read the JIRA documentation on Workflow Post Functions to learn more:
-- <https://confluence.atlassian.com/display/Cloud/Advanced+Workflow+Configuration#AdvancedWorkflowConfiguration-postfunctions>
data JIRAWorkflowPostFunction = JIRAWorkflowPostFunction
   { jwpfKey         :: T.Text -- ^ The add-on unique key for this module.
   , jwpfName        :: I18nText -- ^ The user facing name of this workflow post function.
   , jwpfTriggered   :: URLBean -- ^ The add-on URL to hit when the post function is triggered. The URL will be POST'ed
                                -- to and you should read the Atlassian Connect docs for more details.
   , jwpfDescription :: Maybe I18nText -- ^ The user facing description of this post function.
   , jwpfCreate      :: Maybe URLBean -- ^ The add-on URL to the configuration page for the post function when it is created.
   , jwpfEdit        :: Maybe URLBean -- ^ The add-on URL to the configuration edit page of the post function.
   , jwpfView        :: Maybe URLBean -- ^ The add-on URL to the view page for the read-only view of the configuration.
   } deriving (Show, Generic)

instance ToJSON JIRAWorkflowPostFunction where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "jwpf"
      }

-- | 'JIRAEntityProperties' are used to set Key / Value pair information on JIRA Issues that can be searched via JQL and
-- are indexed by JIRA. The can also be accessed directly via rest api so they allow you to store data in client-only
-- Atlassian Connect plugins. Very handy!
--
-- The data stored as entity properties is in the JSON data format so multiple values can be stored against the one property.
--
-- For more information read the JIRA Documentation on the topic:
-- <https://developer.atlassian.com/display/JIRADEV/JIRA+Entity+Properties+Overview>
--
-- Or read the Atlassian Connect documentation on the topic:
-- <https://developer.atlassian.com/static/connect/docs/modules/jira/entity-property.html>
data JIRAEntityProperties = JIRAEntityProperties
   { jepKey               :: T.Text -- ^ The add-on unique key for this module.
   , jepName              :: I18nText -- ^ The user facing name of this entity property.
   , jepEntityType        :: Maybe EntityType -- ^ The entity type that you want to attach this property to. Issue by default.
   , jepKeyConfigurations :: [KeyConfiguration] -- ^ The list of key configurations that you wish to define.
   } deriving (Show, Generic)

instance ToJSON JIRAEntityProperties where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "jep"
      }

-- | An 'EntityType' represents the type of entity that the JIRA Entity Property should be attatched to. By default
-- entity types are attatched to issues.
data EntityType = IssueEntityType
   deriving (Show)

instance ToJSON EntityType where
   toJSON IssueEntityType = stj "issue"

-- | A 'KeyConfiguration' is the key for this particular property and the JSON flattened paths to the elements that
-- should be extracted from this property. For more information see the Atlassian Connect documentation:
-- <https://developer.atlassian.com/static/connect/docs/modules/fragment/index-key-configuration.html>
data KeyConfiguration = KeyConfiguration
   { kcPropertyKey :: T.Text -- ^ The name of the JIRA Entity Property
   , kcExtractions :: [Extraction] -- ^ All of the data extractions from the property that should be indexed.
   } deriving (Show, Generic)

instance ToJSON KeyConfiguration where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "kc"
      }

-- | An 'Extraction' represents a snippet of data that should be extracted from a 'KeyConfiguration' such that it is
-- Indexed by JIRA and capable of being searched in JQL.
data Extraction = Extraction
   { extractionObjectName :: T.Text
   , extractionType       :: ExtractionType
   } deriving (Show, Generic)

instance ToJSON Extraction where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "extraction"
      }

-- | The style in which the data should be extracted and indexed. For example, you may want the data to be treated as a
-- Date or as a Number.
data ExtractionType
   = ExtractionTypeNumber -- ^ Index the data as a numeric type.
   | ExtractionTypeText -- ^ Index the data as a text based type, with words.
   | ExtractionTypeString -- ^ Index the data as an exact string.
   | ExtractionTypeDate -- ^ Index the data as a Date.
   deriving(Show)

instance ToJSON ExtractionType where
   toJSON ExtractionTypeNumber = stj "number"
   toJSON ExtractionTypeText = stj "text"
   toJSON ExtractionTypeString = stj "string"
   toJSON ExtractionTypeDate = stj "date"