module Ch14.State where
newtype State s a = State {
      runState :: s -> (a, s)
    }
returnState :: a -> State s a
returnState a = State $ \s -> (a, s)
-- bindState :: State s a -> (a -> State s b) -> State s b
-- bindState step makeStep =  \oldState -> 
--     let (result, newState) = runState step $ oldState
--     in State (runState (makeStep result) $ newState)
bindState :: State s a -> (a -> State s b) -> s -> State s b
bindState step makeStep oldState = 
    let (result, newState) = runState step $ oldState
    in makeStep result
