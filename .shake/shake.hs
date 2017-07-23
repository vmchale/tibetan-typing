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
        --removeFilesAfter ".shake" ["//*"]
        cmd "rm -f index.html .shake/.shake.database .shake/.shake.lock .shake/shake.o .shake/shake.hi"

    phony "open" $ do
        need ["index.html"]
        putNormal "Opening..."
        cmd "google-chrome index.html"

    phony "deploy" $ do
        need ["index.html"]
        putNormal "Copying to ../../rust/nessa-site/static/tutor.html"
        cmd "cp index.html ../../rust/nessa-site/static/tutor.html"

    "index.html" %> \_ -> do
        sourceFiles <- getDirectoryFiles "" ["src//*.elm"]
        need sourceFiles
        cmd "elm-make --yes src/main.elm"
