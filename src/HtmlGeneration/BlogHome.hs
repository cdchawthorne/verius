module HtmlGeneration.BlogHome (blogHomeHtml) where

import Data.List (intercalate)
import Data.Maybe (maybe)
import Text.StringTemplate (render, setAttribute, setManyAttrib)

import DataTypes (Templates(..), BlogPost(..))
import HtmlGeneration.ContentWrapping (wrapBlogContent)

blogHomeHtml :: Templates -> [BlogPost] -> Maybe String -> String
blogHomeHtml templates blogPosts maybeStartAt =
    let 
        postsOnBlogHome = 5
        startAt = maybe 0 read maybeStartAt
        selectedPosts = take postsOnBlogHome $ drop startAt blogPosts
        posts = blogHomePostsHtml templates selectedPosts
        hasEarlier = not $ null $ drop (postsOnBlogHome + startAt) blogPosts
        attrs = blogHomeAttributes templates posts startAt hasEarlier
        blogHome = render $ setManyAttrib attrs $ blogHomeTemplate templates
    in
        wrapBlogContent templates blogHome

blogHomeAttributes :: Templates -> String -> Int -> Bool -> [(String, String)]
blogHomeAttributes templates posts startAt hasEarlier =
    [ ("recent_posts", posts)
    , ("earlier_posts", if hasEarlier then earlierHtml else [])
    , ("later_posts", if startAt > 0 then laterHtml else [])
    ]
    where earlierHtml = render $ setAttribute "startAt"
                                              (show (startAt + 5))
                                              (earlierPostsTemplate templates)
          laterHtml = render $ setAttribute "startAt"
                                            (show $ max 0 (startAt - 5))
                                            (laterPostsTemplate templates)

blogHomePostsHtml :: Templates -> [BlogPost] -> String
blogHomePostsHtml templates =
    let
        sep = render $ blogHomePostSep templates
    in
        intercalate sep . map (blogHomePostHtml templates)

blogHomePostHtml :: Templates -> BlogPost -> String
blogHomePostHtml templates post =
    render $ setManyAttrib (postAttributes post) $ blogHomePostTemplate templates

postAttributes :: BlogPost -> [(String, String)]
postAttributes post =
    let
        postPreviewSize = 1000
    in
        [ ("post_id", show $ postIdNum post)
        , ("post_datetime_created", show $ postDatetimeCreated post)
        , ("post_title", postTitle post)
        , ("post_content", take postPreviewSize $ postContent post)
        ]
