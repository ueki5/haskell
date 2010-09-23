module Ch15.MoviewReview where
mylis = 
  [("name", Just "Attila \"The Hun\""),
   ("occupation", Just "Khan"),
   ("title", Just "Title")]
mylis2 = 
  [("title", Just "Attila \"The Hun\""),
   ("user", Just "Khan"),
   ("review", Just "Review")]

data MovieReview = MovieReview {
  revTitle :: String
  , revUser :: String
  , revReview :: String 
  } deriving (Show)

simpleReview :: [(String, Maybe String)] -> Maybe MovieReview
simpleReview alist = 
  case lookup "title" alist of
    Just (Just title@(x:xs)) -> 
      case lookup "user" alist of
        Just (Just user@(_:_)) ->
          case lookup "review" alist of
            Just (Just review@(_:_)) -> Just (MovieReview title user review) 
            _  -> Nothing --no review
        _  -> Nothing --no user
    _  -> Nothing --no title

simpleReview' alist = do
  title <- lookup' "title" alist
  user <- lookup' "user" alist
  review <- lookup' "review" alist
  return (Just (MovieReview title user review))
  
lookup' :: String -> 
           [(String, Maybe String)] -> 
           Maybe String
lookup' s a = case lookup s a of
  Just (Just value@(_:_)) -> (Just value)
  _ -> Nothing
