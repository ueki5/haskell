import DumbTree 

-- �������� 1 �β�����

instance Show DumbTree where
  show = showLR "C"

showLR :: String -> DumbTree -> String
showLR s Empty      = s
showLR s (Fork l r) = "("++showLR "L" l++"^"++showLR "R" r++")"

{-
���Υ����ɤ�ư����ˤϡ����Υե������ DumbTree.hs ��
�����ȥǥ��쥯�ȥ���֤��ơ�ghci �� hugs ���餳�Υե������
���ɤ���trees 4 ��ɾ�����ƤߤƤ���������

$ ghci ex1.hs
   ___         ___ _
  / _ \ /\  /\/ __(_)
 / /_\// /_/ / /  | |      GHC Interactive, version 6.4, for Haskell 98.
/ /_\\/ __  / /___| |      http://www.haskell.org/ghc/
\____/\/ /_/\____/|_|      Type :? for help.

Loading package base-1.0 ... linking ... done.
Compiling DumbTree         ( ./DumbTree.hs, interpreted )
Compiling Main             ( ex1.hs, interpreted )
Ok, modules loaded: Main, DumbTree.
*Main> trees 4
[(L^(L^(L^R))),(L^((L^R)^R)),((L^R)^(L^R)),((L^(L^R))^R),(((L^R)^R)^R)]
-}
