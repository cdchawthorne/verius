module DBInterface ( getDatabaseConnection
                   , getAllPosts
                   , getPostById
                   , getCommentsByPostId
                   , getParentComment
                   , getPostIdAfter
                   , getPostIdBefore
                   , insertReplyToPost
                   , insertReplyToComment
                   ) where

import Control.Monad.State (State, evalState, get, put)
import Data.Tree (Tree(..), Forest)
import Data.Time.Clock (UTCTime)

import Database.HDBC (quickQuery', run, toSql, fromSql, SqlValue)
import Database.HDBC.MySQL (Connection, defaultMySQLConnectInfo, connectMySQL,
                            MySQLConnectInfo, mysqlDatabase, mysqlUser,
                            mysqlPassword, mysqlUnixSocket)

import DataTypes (BlogPost(..), BlogComment(..))

getDatabaseConnection :: IO Connection
getDatabaseConnection = connectMySQL cdchawthSQLConnectInfo
    where cdchawthSQLConnectInfo = defaultMySQLConnectInfo
                { mysqlDatabase = "cdchawth"
                , mysqlUser = "cdchawth"
                , mysqlPassword = "OkPsMDAafHcpzKHHFdIU"
                , mysqlUnixSocket = "/var/run/mysqld/mysqld.sock"
                }

getAllPosts :: Connection -> IO [BlogPost]
getAllPosts conn = quickQuery' conn allPostsQuery [] >>= return . map postRowToData
    where allPostsQuery = "SELECT * FROM blog_posts ORDER BY datetime_created DESC;"

getPostById :: Connection -> Int -> IO BlogPost
getPostById conn idNum =
    quickQuery' conn postByIdQuery [toSql idNum] >>= return . postRowToData . head
    where postByIdQuery = "SELECT * FROM blog_posts WHERE id = ?;"

getCommentsByPostId :: Connection -> Int -> IO (Forest BlogComment)
getCommentsByPostId conn postIdNum =
    quickQuery' conn commentsByPostIdQuery [toSql postIdNum] >>= return . commentRowsToForest
    where commentsByPostIdQuery = "SELECT * FROM blog_comments \
        \WHERE fk_post_id = ? \
        \ORDER BY CONCAT(lineage, datetime_created, ' -- ', id) ASC;"

getPostIdAfter :: Connection -> UTCTime -> IO (Maybe Int)
getPostIdAfter conn time =
    quickQuery' conn postsAfterQuery [toSql time] >>= return . extractFirstId
    where extractFirstId [] = Nothing
          extractFirstId (post:_) = fromSql $ head post
          postsAfterQuery = "SELECT id FROM blog_posts WHERE datetime_created > ? \
              \ORDER BY datetime_created ASC;"

getPostIdBefore :: Connection -> UTCTime -> IO (Maybe Int)
getPostIdBefore conn time =
    quickQuery' conn postsBeforeQuery [toSql time] >>= return . extractFirstId
    where extractFirstId [] = Nothing
          extractFirstId (post:_) = fromSql $ head post
          postsBeforeQuery = "SELECT id FROM blog_posts WHERE datetime_created < ? \
              \ORDER BY datetime_created DESC;"

getParentComment :: Connection -> Int -> IO BlogComment
getParentComment conn parentIdNum =
    quickQuery' conn getParentCommentQuery [toSql parentIdNum] >>= return . commentRowToData . head
    where getParentCommentQuery = "SELECT * FROM blog_comments WHERE id = ?;"

insertReplyToPost :: Connection -> Int -> String -> String -> IO Integer
insertReplyToPost conn postIdNum author content =
    run conn insertReplyToPostQuery 
        [ toSql postIdNum
        , toSql "/"
        , toSql $ sanitizeHtml author
        , toSql $ sanitizeHtml content
        ]
    where insertReplyToPostQuery = "INSERT INTO blog_comments \
              \(fk_post_id, level, lineage, author, content) \
              \VALUES (?, 0, ?, ?, ?);"

insertReplyToComment :: Connection -> BlogComment -> String -> String -> IO Integer
insertReplyToComment conn parent author content =
    run conn insertReplyToCommentQuery
        [ toSql $ commentPostIdNum parent
        , toSql $ (commentLevel parent) + 1
        , toSql $ (commentLineage parent) ++ (dropLast4 $ show $ commentDatetimeCreated parent) ++ " -- " ++ (show $ commentIdNum parent) ++ "/"
        , toSql $ sanitizeHtml author
        , toSql $ sanitizeHtml content
        ]
    where -- Haskell puts a " UTC" on the end of show timestamp
          dropLast4 = init . init . init . init
          insertReplyToCommentQuery = "INSERT INTO blog_comments \
              \(fk_post_id, level, lineage, author, content) \
              \VALUES (?, ?, ?, ?, ?);"

commentRowsToForest :: [[SqlValue]] -> Forest BlogComment
commentRowsToForest = commentListToForest . map commentRowToData

commentListToForest :: [BlogComment] -> Forest BlogComment
commentListToForest = evalState $ parseChildren 0

parseChildren :: Int -> State [BlogComment] (Forest BlogComment)
parseChildren level =
    get >>= \rows ->
    if shouldContinue level rows
       then put (tail rows) >>
            parseChildren (level+1) >>= \grandChildren ->
            parseChildren level >>= \laterChildren ->
            return ((Node (head rows) grandChildren):laterChildren)
       else return []
    where shouldContinue parentId rows =
              not (null rows) && (level == (commentLevel $ head rows))

commentRowToData :: [SqlValue] -> BlogComment
commentRowToData (idNum : datetime_created : fk_post_id : level :
                  lineage : author : content : []) =
    BlogComment { commentIdNum = fromSql idNum
                , commentDatetimeCreated = fromSql datetime_created
                , commentPostIdNum = fromSql fk_post_id
                , commentLevel = fromSql level
                , commentLineage = fromSql lineage
                , commentAuthor = fromSql author
                , commentContent = fromSql content
                }

postRowToData :: [SqlValue] -> BlogPost
postRowToData (idNum : datetime_created : title : contents : []) = 
    BlogPost { postIdNum = fromSql idNum
             , postDatetimeCreated = fromSql datetime_created
             , postTitle = fromSql title
             , postContent = fromSql contents
             }

-- Implementation derived from Text.Html.stringToHtmlString
sanitizeHtml :: String -> String
sanitizeHtml = concatMap fixChar
    where
        fixChar '<' = "&lt;"
        fixChar '>' = "&gt;"
        fixChar '&' = "&amp;"
        fixChar '"' = "&quot;"
        fixChar ' ' = "&nbsp;"
        fixChar '\n' = "<br />"
        fixChar c   = [c] 
