#!/usr/bin/env stack
{- stack --resolver lts-10.0 --install-ghc
    runghc
    --package shake
-}
import           Development.Shake
import           Development.Shake.CCJS
import           Development.Shake.Clean
import           Development.Shake.Command
import           Development.Shake.Elm
import           Development.Shake.FilePath
import           Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = ".shake" } $ do
    want ["index.html", "tutor.min.js"]

    "build" %> \_ -> do
        need [".shake/shake.hs"]
        command [Cwd ".shake"] "ghc-8.2.2" ["-O", "shake.hs", "-o", "../build"]

    phony "clean" $ do
        putNormal "Cleaning files..."
        cleanElm
        unit $ cmd ["rm", "-f", "tags"]
        cmd "rm -f tutor.js .shake/.shake.database .shake/.shake.lock .shake/shake.o .shake/shake.hi"

    phony "open" $ do
        need ["index.html", "tutor.js"]
        putNormal "Opening..."
        cmd "firefox index.html"

    "deploy" ~> do
        need ["tutor.min.js", "kb.jpg", "index.html"]
        cmd ["cp", "tutor.min.js", "/home/vanessa/programming/rust/nessa-site/static/typing-tutor"]

    phony "commit" $ do
        need ["tutor.js"]
        cmd "cp tutor.js docs/tutor.js"

    ccjs ["tutor.js"] "tutor.min.js"

    elmMake ["elm-package.json", "index.html"] ["src/Main.elm"] "tutor.js"
