#!/usr/bin/env stack
{- stack --resolver lts-8.23 --install-ghc
    runghc
    --package shake
-}
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = ".shake" } $ do
    want ["index.html"]

    phony "clean" $ do
        putNormal "Cleaning files..."
        removeFilesAfter "elm-stuff" ["//*"]
        removeFilesAfter ".shake" ["//*"]
        cmd "rm -f index.html"

    "index.html" %> \_ -> do
        sourceFiles <- getDirectoryFiles "" ["src//*.elm"]
        need sourceFiles
        cmd "elm-make --yes src/main.elm"
