#!/usr/bin/env stack
{- stack --resolver lts-8.23 --install-ghc
    runghc
    --package shake
-}
import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = ".shake" } $ do
    want ["index.html", "tutor.js"]

    phony "clean" $ do
        putNormal "Cleaning files..."
        removeFilesAfter "elm-stuff" ["//*"]
        cmd "rm -f tutor.js .shake/.shake.database .shake/.shake.lock .shake/shake.o .shake/shake.hi"

    phony "open" $ do
        need ["index.html", "tutor.js"]
        putNormal "Opening..."
        cmd "google-chrome index.html"

    phony "deploy" $ do
        need ["index.html", "tutor.js"]
        putNormal "Copying to ../../rust/nessa-site/static/tutor.html"
        cmd "cp index.html ../../rust/nessa-site/static/tutor.html"

    phony "commit" $ do
        need ["tutor.js"]
        cmd "cp tutor.js docs/tutor.js"

    "tutor.js" %> \_ -> do
        sourceFiles <- getDirectoryFiles "" ["src//*.elm", "elm-package.json", "index.html"]
        need sourceFiles
        cmd "elm-make --yes src/Main.elm --output tutor.js --warn"
