--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend, (<>))
import Hakyll
import Text.Pandoc.Options
--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
    match ("images/**" .||. "favicon.ico") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/**" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/*" $ version "menu" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
    
    match "posts/*.md" $ version "feed" $ do
        compile $ pandocMathCompiler  

    match "posts/*" $ do 
        route $ setExtension "html"
        compile $ do
            posts <- chronological =<< loadAll ("posts/*" .&&. hasVersion "menu")
            pandocMathCompiler
                >>= loadAndApplyTemplate "templates/post.html" postCtx
                >>= saveSnapshot "teaserSnap"
                >>= loadAndApplyTemplate "templates/default.html" (
                    listField "posts" postCtx (return posts) <>
                    postCtx
                )      
                >>= relativizeUrls

    create ["contents.html"] $ do
        route idRoute
        compile $ do
            posts <- chronological =<< loadAll ("posts/*" .&&. hasVersion "menu")
            let contentsCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Contents"            <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/contents.html" contentsCtx
                >>= loadAndApplyTemplate "templates/default.html" contentsCtx
                >>= relativizeUrls

    match "about.md" $ do
        route   $ setExtension "html"
        compile $ do
            posts <- chronological =<< loadAll ("posts/*" .&&. hasVersion "menu")
            let pageCtx =
                    listField "posts" postCtx (return posts) <>
                    defaultContext
            pandocMathCompiler
                >>= loadAndApplyTemplate "templates/default.html" pageCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- chronological =<< loadAll ("posts/*" .&&. hasVersion "menu")
            teasers <- recentFirst =<< loadAllSnapshots ("posts/*" .&&. hasNoVersion) "teaserSnap"
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    listField "postTeasers" teaserCtx (return teasers) <>
                    constField "title" "Home"                <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <> bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAll ("posts/*" .&&. hasVersion "feed")
            renderAtom feedConfiguration feedCtx posts

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

teaserCtx :: Context String
teaserCtx =  teaserField "teaser" "teaserSnap" <> postCtx

pandocMathCompiler =
    let writerOptions = defaultHakyllWriterOptions {
                          writerHTMLMathMethod = MathJax ""
                        }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Combinatorics of Permutations"
    , feedDescription = "Latest posts on Combinatorics of Permutations."
    , feedAuthorName  = "Vinay Madhusudanan"
    , feedAuthorEmail = "vinay.m20000@gmail.com"
    , feedRoot        = "https://vynm.github.io/Comutations/"
    }
