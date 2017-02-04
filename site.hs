{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

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
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "example/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.markdown", "cookbook.markdown", "read.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
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
