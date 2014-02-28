#!/usr/bin/env zsh

if [[ $# -gt 0 ]]; then
    target=$(realpath $1)/verius
else
    target=verius
fi

cd "$(dirname "$(realpath $0)")"
ghc --make -o ${target} -hidir obj -odir obj \
    src/Verius.hs src/DataTypes.hs src/DBInterface.hs src/HtmlGeneration/BlogHome.hs \
    src/HtmlGeneration/BlogPost.hs src/HtmlGeneration/ContentWrapping.hs \
    src/HtmlGeneration/TemplateHtmlGetter.hs
chmod a+x ${target}
