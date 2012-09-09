module HtmlGeneration.ContentWrapping (wrapBlogContent, wrapContent) where

import Text.StringTemplate (render, setAttribute)

import DataTypes (Templates(..))

wrapContent :: Templates -> String -> String
wrapContent templates = addHtmlBoilerplate templates . addSidebar templates

wrapBlogContent :: Templates -> String -> String
wrapBlogContent templates = wrapContent templates . addBlogHeader templates

addBlogHeader :: Templates -> String -> String
addBlogHeader templates str =
    render $ setAttribute "blog_content" str $ blogHeaderTemplate templates

addSidebar :: Templates -> String -> String
addSidebar templates str =
    render $ setAttribute "page_content" str $ sidebarTemplate templates

addHtmlBoilerplate :: Templates -> String -> String
addHtmlBoilerplate templates str =
    render $ setAttribute "page_content" str $ boilerplateTemplate templates
