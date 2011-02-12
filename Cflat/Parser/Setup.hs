module Main (main) where

import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))

import Data.List (isInfixOf)
import System.Cmd (system)
import System.FilePath
import System.Directory (getCurrentDirectory)

main :: IO ()
main = do
    -- defaultMainWithHooks $ defaultUserHooks {runTests = sampleTest}
    -- defaultMainWithHooks $ autoconfUserHooks {runTests = sampleTest}
    defaultMainWithHooks $ simpleUserHooks {runTests = sampleTest}
    where
        sampleTest _ _ pkgDisc bInfo = do
            -- 現在のディレクトリのパス名を取得
            curr <- getCurrentDirectory
            let dir = buildDir bInfo
                -- data: を取り出す
                testData = dataFiles pkgDisc
                -- 名前に "Test" がつくもののみ
                filterName = filter (("Test" `isInfixOf`) . exeName)
                -- buildable: が False でないもののみ
                filterBuildable = filter (buildable . buildInfo)
                filterTest = filterName . filterBuildable
                -- executable: の実行可能ファイルの完全パス名を作成
                path exec =   curr </> dir
                          -- 不要な拡張子の除去
                          </> (dropExtension . exeName) exec
                          </> (dropExtension . exeName) exec
            -- filterTest の条件に合う executable: の実行可能ファイルを実行
            mapM_ (system . path) $ filterTest $ executables pkgDisc
