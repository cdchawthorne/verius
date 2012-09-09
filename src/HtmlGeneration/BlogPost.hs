module HtmlGeneration.BlogPost (blogPostHtml) where

import Data.List (intercalate)
import Data.Tree (Tree(..), Forest)
import Text.StringTemplate (render, setManyAttrib, setAttribute)

import DataTypes (Templates(..), BlogPost(..), BlogComment(..))
import HtmlGeneration.ContentWrapping (wrapBlogContent)

blogPostHtml :: Templates -> BlogPost -> Forest BlogComment -> Maybe Int -> Maybe Int -> String
blogPostHtml templates post comments nextPostId previousPostId =
    let
        attrs = postAttributes templates post comments nextPostId previousPostId
        postHtml = render $ setManyAttrib attrs $ blogPostTemplate templates
    in
        wrapBlogContent templates postHtml

postAttributes :: Templates -> BlogPost -> Forest BlogComment -> Maybe Int -> Maybe Int -> [(String, String)]
postAttributes templates (BlogPost idNum datetimeCreated title content)
               comments nextPostId previousPostId =
        [ ("previous_post", maybe "" previousPostLink previousPostId)
        , ("next_post", maybe "" nextPostLink nextPostId)
        , ("post_id", show idNum)
        , ("post_datetime_created", show datetimeCreated)
        , ("post_title", title)
        , ("post_content", content)
        , ("post_comments", commentsHtml templates comments)
        ]
    where previousPostLink prevId = 
              render $ setAttribute "previous_post_id" 
                                    (show prevId)
                                    (previousPostTemplate templates)
          nextPostLink nextId = 
              render $ setAttribute "next_post_id" 
                                    (show nextId)
                                    (nextPostTemplate templates)

commentsHtml :: Templates -> Forest BlogComment -> String
commentsHtml templates = intercalate "<hr />" . map nodeWithChildren
    where nodeWithChildren (Node comment children) =
              commentHtml templates comment ++ "<hr />" ++ commentsHtml templates children

commentHtml :: Templates -> BlogComment -> String
commentHtml templates comment =
    render $ setManyAttrib (commentAttributes comment) $ blogCommentTemplate templates

commentAttributes :: BlogComment -> [(String, String)]
commentAttributes (BlogComment idNum datetimeCreated _ level _ author content) =
        [ ("comment_id", show idNum)
        , ("comment_datetime_created", show datetimeCreated)
        , ("comment_indent_level", show (50*level))
        , ("comment_author", author)
        , ("comment_content", content)
        ]
