{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Text.Pandoc
import Text.Pandoc.Walk ( walk )

import System.Process ( readProcess )
import System.IO.Unsafe ( unsafePerformIO )


import Hakyll

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "about code"
        , feedDescription = "about code"
        , feedAuthorName  = "Jan Frederik Hake"
        , feedAuthorEmail = "jan_hake@gmx.de"
        , feedRoot        = "https://enter-haken.github.io"
    }


main :: IO ()
main = hakyll $ do
    match ("images/*" .||. "example/**") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.markdown", "cookbook.markdown", "read.markdown", "projects.markdown", "license.markdown" ]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocPostCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
 
    match "notes/*" $ do
        route $ setExtension "html"
        compile $ pandocPostCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/note.html"    defaultContext 
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
    
    match "index.html" $ do
         route idRoute
         compile $ do
             posts <- recentFirst =<< loadAll "posts/*"
             let indexCtx =
                     listField "posts" teaserCtx (return posts) `mappend`
                     constField "title" "home"            `mappend`
                     defaultContext

             getResourceBody
                 >>= applyAsTemplate indexCtx
                 >>= loadAndApplyTemplate "templates/default.html" indexCtx
                 >>= relativizeUrls

    match "notes.html" $ do
         route idRoute
         compile $ do
             notes <- loadAll "notes/*"
             let noteCtx =
                     listField "notes" teaserCtx (return notes) `mappend`
                     constField "title" "notes"            `mappend`
                     defaultContext

             getResourceBody
                 >>= applyAsTemplate noteCtx
                 >>= loadAndApplyTemplate "templates/default.html" noteCtx
                 >>= relativizeUrls


    match "templates/*" $ compile templateCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
    
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
            renderAtom myFeedConfiguration feedCtx posts


postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

teaserCtx :: Context String
teaserCtx = teaserField "teaser" "content" `mappend` postCtx

pandocPostCompiler :: Compiler (Item String)
pandocPostCompiler = pandocCompilerWithTransform
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    graphViz

graphViz :: Pandoc -> Pandoc
graphViz = walk codeBlock

codeBlock :: Block -> Block
codeBlock cb@(CodeBlock (id, classes, namevals) contents) = 
    case lookup "lang" namevals of
        Just f -> RawBlock (Format "html") $ svgDiv $ svg contents 
        nothing -> cb
codeBlock x = x

svg :: String -> String
svg contents = unsafePerformIO $ readProcess "dot" ["-Tsvg"] contents

svgDiv :: String -> String
svgDiv content = "<div class=\"overflow\">" ++ content ++ "</div>"
