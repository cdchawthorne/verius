module DataTypes ( Template
                 , Templates(..)
                 , BlogPost(..)
                 , BlogComment(..)
                 ) where

import Data.Time.Clock (UTCTime)
import Text.StringTemplate (StringTemplate)

type Template = StringTemplate String

data Templates = Templates { homePageTemplate :: Template
                           , blogHomeTemplate :: Template
                           , blogHomePostTemplate :: Template
                           , blogHomePostSep :: Template
                           , earlierPostsTemplate :: Template
                           , laterPostsTemplate :: Template
                           , blogPostTemplate :: Template
                           , nextPostTemplate :: Template
                           , previousPostTemplate :: Template
                           , blogCommentTemplate :: Template
                           , blogHeaderTemplate :: Template
                           , filesPageTemplate :: Template
                           , aboutPageTemplate :: Template
                           , sidebarTemplate :: Template
                           , boilerplateTemplate :: Template
                           }

data BlogPost = BlogPost { postIdNum :: Int
                         , postDatetimeCreated :: UTCTime
                         , postTitle :: String
                         , postContent :: String
                         }

data BlogComment = BlogComment { commentIdNum :: Int
                               , commentDatetimeCreated :: UTCTime
                               , commentPostIdNum :: Int
                               , commentLevel :: Int
                               , commentLineage :: String
                               , commentAuthor :: String
                               , commentContent :: String
                               }
