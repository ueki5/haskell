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
            -- ���݂̃f�B���N�g���̃p�X�����擾
            curr <- getCurrentDirectory
            let dir = buildDir bInfo
                -- data: �����o��
                testData = dataFiles pkgDisc
                -- ���O�� "Test" �������̂̂�
                filterName = filter (("Test" `isInfixOf`) . exeName)
                -- buildable: �� False �łȂ����̂̂�
                filterBuildable = filter (buildable . buildInfo)
                filterTest = filterName . filterBuildable
                -- executable: �̎��s�\�t�@�C���̊��S�p�X�����쐬
                path exec =   curr </> dir
                          -- �s�v�Ȋg���q�̏���
                          </> (dropExtension . exeName) exec
                          </> (dropExtension . exeName) exec
            -- filterTest �̏����ɍ��� executable: �̎��s�\�t�@�C�������s
            mapM_ (system . path) $ filterTest $ executables pkgDisc
