module HTML where
import MyParsec
type HTML = [HTMLItem]
--type HTMLItem = Text String | Param Href
data HTMLItem = Text String | Param Href
data Href = PageLink { hrefPageName :: String }

-- text :: Parser HTML
-- test = many component
--   where component =  do name <- try(wikiName)
--                         return (Param $ PageLink name)
--                  <|> do url <- try(urlAutoLink)
--                         return (Text $ a_href url url)
--                  <|> do url <- anyChar
--                         return (Text $ escapeChar c)