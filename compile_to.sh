#!/usr/bin/env zsh

if [[ $# -gt 0 ]]; then
    target=$(realpath $1)/Verius
else
    target=Verius
fi

cd "$(dirname "$(realpath $0)")"
ghc --make -o ${target} -hidir obj -odir obj \
    src/Verius.hs src/DataTypes.hs src/DBInterface.hs src/HtmlGeneration/BlogHome.hs \
    src/HtmlGeneration/BlogPost.hs src/HtmlGeneration/ContentWrapping.hs \
    src/HtmlGeneration/TemplateHtmlGetter.hs
