-- �e���W���[���̓t�@�C�������u���W���[����.hs�v�Ƃ��ĕۑ�
module Mark ( Mark(..), isEmpty, next ) where
	   -- Mark(..) �f�[�^�^���Ƃ��̍\�z�q�����G�N�X�|�[�g
	   -- Mark     �f�[�^�^���̂݃G�N�X�|�[�g

data Mark = O	-- ���C���̈�
	  | X	-- �~�C���̈�
	  | U	-- �󂫁C�����u����Ă��Ȃ�
	  | B	-- ��Q���C�����u���Ȃ�
            deriving Eq

instance Show Mark where
  show O = "O"
  show X = "X"
  show U = "."
  show B = "*"

isEmpty :: Mark -> Bool
isEmpty = (U ==)

next :: Mark -> Mark		-- ��Ԃ���i�߂�
next O = X
next X = O
