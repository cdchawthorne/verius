import Control.Monad (mapM, (>=>))
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe)

import Network.FastCGI (runFastCGI, handleErrors, CGI, CGIResult, getVar, output, getInput)
import Text.Regex.TDFA ((=~))
import Text.StringTemplate (newSTMP)
import Database.HDBC.MySQL (Connection)

import DataTypes (Template, Templates(..), BlogPost(..))
import DBInterface ( getDatabaseConnection
                   , getAllPosts
                   , getPostById
                   , getCommentsByPostId
                   , getParentComment
                   , getPostIdBefore
                   , getPostIdAfter
                   , insertReplyToPost
                   , insertReplyToComment
                   )

import HtmlGeneration.TemplateHtmlGetter (makeTemplateHtmlGetter)
import HtmlGeneration.BlogHome (blogHomeHtml)
import HtmlGeneration.BlogPost (blogPostHtml)

----------------------------------------------------
-- URL PARSING AND DATABASE CONNETION
----------------------------------------------------

-- TODO: is handleErrors doing anything? I think an error can be induced with
--       a bad SQL query
main = runFastCGI $ handleErrors $ cgiMain

cgiMain :: CGI CGIResult
cgiMain =
    getVar "REQUEST_URI" >>= \maybeUri ->
    liftIO getTemplates >>= \templates ->
    liftIO getDatabaseConnection >>= \conn ->
    handleRequest maybeUri templates conn

handleRequest :: Maybe String -> Templates -> Connection -> CGI CGIResult
handleRequest Nothing = handleBadRequest
handleRequest (Just uri)
    | uri == homePageUri                = handleHomePageRequest
    | uri =~ blogHomeRegex              = handleBlogHomeRequest
    | uri =~ blogPostRegex              = handleBlogPostRequest $ extractPostId uri
    | uri =~ blogReplyToPostRegex       = handleBlogReplyToPost $ extractPostId uri
    | uri =~ blogReplyToCommentRegex    = handleBlogReplyToComment $ extractCommentId uri
    | uri == filesUri                   = handleFilesPageRequest
    | uri == aboutUri                   = handleAboutPageRequest
    | otherwise                         = handleBadRequest
    where extractPostId = read . (!! 1) . head . (=~ "^/~cdchawth/blog/([0-9]+)/")
          extractCommentId = read . (!! 1) . head . (=~ blogReplyToCommentRegex)
          homePageUri = "/~cdchawth/"
          blogHomeRegex = "^/~cdchawth/blog/(\\?startAt=[0-9]+)?$"
          blogPostRegex = "^/~cdchawth/blog/[0-9]+/$"
          blogReplyToPostRegex = "^/~cdchawth/blog/[0-9]+/reply/$"
          blogReplyToCommentRegex = "^/~cdchawth/blog/comments/([0-9]+)/reply/$"
          filesUri = "/~cdchawth/files/"
          aboutUri = "/~cdchawth/about/"

----------------------------------------------------
-- Simple Request Handlers
----------------------------------------------------

makeSimpleHandler :: (Templates -> Template) -> Templates -> Connection -> CGI CGIResult
makeSimpleHandler templateGetter templates _ =
    output $ makeTemplateHtmlGetter templateGetter templates

handleHomePageRequest :: Templates -> Connection -> CGI CGIResult
handleHomePageRequest = makeSimpleHandler homePageTemplate

handleFilesPageRequest :: Templates -> Connection -> CGI CGIResult
handleFilesPageRequest = makeSimpleHandler filesPageTemplate

handleAboutPageRequest :: Templates -> Connection -> CGI CGIResult
handleAboutPageRequest = makeSimpleHandler aboutPageTemplate

handleBadRequest :: Templates -> Connection -> CGI CGIResult
handleBadRequest = makeSimpleHandler (\_ -> newSTMP "Well shit.")

----------------------------------------------------
-- Database Request Handlers
----------------------------------------------------

handleBlogHomeRequest :: Templates -> Connection -> CGI CGIResult
handleBlogHomeRequest templates conn =
    liftIO (getAllPosts conn) >>= \posts ->
    getInput "startAt" >>= \startAt ->
    output $ blogHomeHtml templates posts startAt

handleBlogPostRequest :: Int -> Templates -> Connection -> CGI CGIResult
handleBlogPostRequest idNum templates conn =
    liftIO (getPostById conn idNum) >>= \post ->
    liftIO (getCommentsByPostId conn idNum) >>= \comments ->
    liftIO (getPostIdAfter conn $ postDatetimeCreated post) >>= \postAfterId ->
    liftIO (getPostIdBefore conn $ postDatetimeCreated post) >>= \postBeforeId ->
    output $ blogPostHtml templates post comments postAfterId postBeforeId

handleBlogReplyToPost :: Int -> Templates -> Connection -> CGI CGIResult
handleBlogReplyToPost postIdNum templates conn =
    getVar "REQUEST_METHOD" >>= \method ->
    if method == Just "POST"
       then getInput "author" >>= \author ->
            getInput "content" >>= \content ->
            liftIO (insertReplyToPost conn postIdNum
                                      (fromMaybe "" author)
                                      (fromMaybe "" content)) >>
            output "Thank you for commenting."
       else output "Error processing your comment."

handleBlogReplyToComment :: Int -> Templates -> Connection -> CGI CGIResult
handleBlogReplyToComment parentIdNum templates conn =
    getVar "REQUEST_METHOD" >>= \method ->
    if method == Just "POST"
       then getInput "author" >>= \author ->
            getInput "content" >>= \content ->
            liftIO (getParentComment conn parentIdNum) >>= \parentComment ->
            liftIO (insertReplyToComment conn parentComment 
                                         (fromMaybe "" author) 
                                         (fromMaybe "" content)) >>
            output "Thank you for commenting."
        else handleBadRequest templates conn

----------------------------------------------------
-- Templates initialization
----------------------------------------------------

getTemplate :: String -> IO Template
getTemplate =
    readFile . ("templates/" ++) . (++ ".html") >=> return . newSTMP

getTemplates :: IO Templates
getTemplates =
    getTemplate "home_page" >>= \homePageT ->
    getTemplate "blog_home" >>= \blogHomeT ->
    getTemplate "blog_home_post" >>= \blogHomePostT ->
    getTemplate "blog_home_post_sep" >>= \blogHomePostSepT ->
    getTemplate "earlier_posts" >>= \earlierPostsT ->
    getTemplate "later_posts" >>= \laterPostsT ->
    getTemplate "blog_post" >>= \blogPostT ->
    getTemplate "previous_post" >>= \previousPostT ->
    getTemplate "next_post" >>= \nextPostT ->
    getTemplate "blog_comment" >>= \blogCommentT ->
    getTemplate "blog_header" >>= \blogHeaderT ->
    getTemplate "files_page" >>= \filesPageT ->
    getTemplate "about_page" >>= \aboutPageT ->
    getTemplate "sidebar" >>= \sidebarT ->
    getTemplate "boilerplate" >>= \boilerplateT ->
    return $ Templates { homePageTemplate = homePageT
                       , blogHomeTemplate = blogHomeT
                       , blogHomePostTemplate = blogHomePostT
                       , blogHomePostSep = blogHomePostSepT
                       , earlierPostsTemplate = earlierPostsT
                       , laterPostsTemplate = laterPostsT
                       , blogPostTemplate = blogPostT
                       , previousPostTemplate = previousPostT
                       , nextPostTemplate = nextPostT
                       , blogCommentTemplate = blogCommentT
                       , blogHeaderTemplate = blogHeaderT
                       , filesPageTemplate = filesPageT
                       , aboutPageTemplate = aboutPageT
                       , sidebarTemplate = sidebarT
                       , boilerplateTemplate = boilerplateT
                       }
