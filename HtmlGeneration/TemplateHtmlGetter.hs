module HtmlGeneration.TemplateHtmlGetter (makeTemplateHtmlGetter) where

import Text.StringTemplate (render)
import DataTypes (Template, Templates)
import HtmlGeneration.ContentWrapping (wrapContent)

makeTemplateHtmlGetter :: (Templates -> Template) -> Templates -> String
makeTemplateHtmlGetter templateGetter templates =
    let
        content = render $ templateGetter templates
    in
        wrapContent templates content
