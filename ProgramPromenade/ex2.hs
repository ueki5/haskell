import List
import Ticket hiding (ticket)

-- �������� 2 �β�����

ticket :: Int -> [Char] -> Term
ticket n ds
 = case filter (same n) (allterms ds) of
    s:_ -> s
    []  -> error ("Cannot make "++show n++" with "++intersperse ',' ds++".")

{-
���Υ����ɤ�ư����ˤϡ����Υե������ Ticket.hs ��
�����ȥǥ��쥯�ȥ���֤��ơ�ghci �� hugs ���餳�Υե������
���ɤ���ticket 4 "1199" ��ɾ�����ƤߤƤ���������

$ ghci ex2.hs
   ___         ___ _
  / _ \ /\  /\/ __(_)
 / /_\// /_/ / /  | |      GHC Interactive, version 6.4, for Haskell 98.
/ /_\\/ __  / /___| |      http://www.haskell.org/ghc/
\____/\/ /_/\____/|_|      Type :? for help.

Loading package base-1.0 ... linking ... done.
Compiling Ticket           ( ./Ticket.hs, interpreted )
Compiling Main             ( ex2.hs, interpreted )
Ok, modules loaded: Main, Ticket.
*Main> ticket 4 "1199"
*** Exception: Cannot make 4 with 1,1,9,9.
-}