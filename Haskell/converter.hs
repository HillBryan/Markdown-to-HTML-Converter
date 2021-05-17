 import System.IO
 import Data.Char
 import Data.List


 data Tag = H1 String | H2 String | H3 String | H4 String | H5 String
          | Block String | P String | Blank
          | Ol Integer [Tag] | Ul Integer [Tag] | Lio Integer String| Liu Integer String| Code String | Hr
          | Link String | Image String
          | Html [Tag] | Head [Tag] | Body [Tag]
    deriving (Show, Eq)



 --INLINE PARSING
 
 --LINKS
 --confirming structure
 inLineLinkHelper :: String -> Int -> [Int]
 inLineLinkHelper "" index = []
 inLineLinkHelper ('[':xs) index = index : inLineLinkHelper xs (index + 1)
 inLineLinkHelper (']':'(':xs) index = index : inLineLinkHelper xs (index + 1)
 inLineLinkHelper (')':xs) index = [index]
 inLineLinkHelper (']':xs) index = index : inLineLinkHelper xs (index + 1)
 inLineLinkHelper (x:xs) index = inLineLinkHelper xs (index + 1)
 
 --Checking for strinctly increasing sequence
 checkLinkList :: [Int] -> Bool
 checkLinkList [] = True
 checkLinkList [x] = True
 checkLinkList (x:y:xs) = (x < y) && checkLinkList(y:xs)
 
 --Getting title from brackets
 getTitle :: String -> Bool -> String
 getTitle "" found = ""
 getTitle ('[':x:xs) found = x : getTitle xs True
 getTitle (']':xs) found = ""
 getTitle (x:xs) found | found == True = x : getTitle xs found
                       | True          = getTitle xs found
 --Getting Link from parenthesis
 getLink :: String -> Bool -> String
 getLink "" found = ""
 getLink (']':'(':x:xs) found = x : getLink xs True
 getLink (')':xs) found = ""
 getLink (x:xs) found | (found == True) = x : getLink xs found
                |  True           = getLink xs found

 inLineJump :: String -> Bool -> String
 inLineJump "" found = ""
 inLineJump (')':x:xs) found = x : inLineJump xs True
 inLineJump (x:xs) found | found == True = x : inLineJump xs found
                         | True          = inLineJump xs found

 link =  "_Link: _  [Google](www.google.com)  _Link: _  [Google](www.google.com)"

 --parser to check for links
 inLineLinkParser :: String -> String
 inLineLinkParser "" = ""
 inLineLinkParser (x:xs) = if ((length $ inLineLinkHelper (x:xs) 0) == 3 && (checkLinkList $ inLineLinkHelper (x:xs) 0) && (head $ inLineLinkHelper (x:xs) 0) == 0)
                           then "<a href=\"https://" ++ getLink (x:xs) False ++ "\">" ++ getTitle (x:xs) False ++ "</a>" ++ inLineLinkParser (inLineJump xs False)
                           else x : inLineLinkParser xs

 --IMAGES
 --confirming structure
 inLineImageHelper :: String -> Int -> [Int]
 inLineImageHelper "" index = []
 inLineImageHelper ('!':'[':xs) index = index : inLineImageHelper xs (index + 1)
 inLineImageHelper (']':'(':xs) index = index : inLineImageHelper xs (index + 1)
 inLineImageHelper (')':xs) index = [index]
 inLineImageHelper (']':xs) index = index : inLineImageHelper xs (index + 1)
 inLineImageHelper (x:xs) index = inLineImageHelper xs (index + 1)
 
 --Checking for strinctly increasing sequence
 checkImageList :: [Int] -> Bool
 checkImageList [] = True
 checkImageList [x] = True
 checkImageList (x:y:xs) = (x < y) && checkImageList(y:xs)

 --parser to check for links
 inLineImageParser :: String -> String
 inLineImageParser "" = ""
 inLineImageParser (x:xs) = if ((length $ inLineImageHelper (x:xs) 0) == 3 && (checkImageList $ inLineImageHelper (x:xs) 0) && (head $ inLineImageHelper (x:xs) 0) == 0)
                      then "<img src=\"" ++ getLink (x:xs) False ++ "\" alt=\"" ++ getTitle (x:xs) False ++ "\"></img>" ++ inLineImageParser (inLineJump xs False)
                      else x : inLineImageParser xs
 
 --BOLD
   --Bold marker **text**
 inLineBoldHelper :: String -> Int -> [Int]
 inLineBoldHelper "" index = []
 inLineBoldHelper ('*':'*':x) index = index : (inLineBoldHelper x (index + 1))
 inLineBoldHelper (x:xs) index = inLineBoldHelper xs (index + 1)
 
 inLineBold :: String -> Int -> [Int] -> String
 inLineBold "" index list = ""
 inLineBold ('*':'*':xs) index list | ((index * (-1)) `elem` list && index /= 0) = "</strong>" ++ inLineBold xs (index + 1) list
                                    | (index `elem` list) =  "<strong>" ++ inLineBold xs (index + 1) list
 inLineBold (x:xs) index list = x : inLineBold xs (index + 1) list
 
 inLineBoldParser :: String -> String
 inLineBoldParser str = 
     if ((length (inLineBoldHelper str 0)) <= 1) then str
     else if (odd $ length (inLineBoldHelper str 0)) then inLineBold str 0 (arrTransformer (init (inLineBoldHelper str 0)) 0)
          else inLineBold str 0 (arrTransformer (inLineBoldHelper str 0) 0)


   --Bold markder __text__
 inLineBoldHelperU :: String -> Int -> [Int]
 inLineBoldHelperU "" index = []
 inLineBoldHelperU ('_':'_':x) index = index : (inLineBoldHelperU x (index + 1))
 inLineBoldHelperU (x:xs) index = inLineBoldHelperU xs (index + 1)
 
 inLineBoldU :: String -> Int -> [Int] -> String
 inLineBoldU "" index list = ""
 inLineBoldU ('_':'_':xs) index list | ((index * (-1)) `elem` list && index /= 0) = "</strong>" ++ inLineBoldU xs (index + 1) list
                                    | (index `elem` list) =  "<strong>" ++ inLineBoldU xs (index + 1) list
 inLineBoldU (x:xs) index list = x : inLineBoldU xs (index + 1) list
 
 inLineBoldParserU :: String -> String
 inLineBoldParserU str = 
     if ((length (inLineBoldHelperU str 0)) <= 1) then str
     else if (odd $ length (inLineBoldHelperU str 0)) then inLineBoldU str 0 (arrTransformer (init (inLineBoldHelperU str 0)) 0)
          else inLineBoldU str 0 (arrTransformer (inLineBoldHelperU str 0) 0)

 --ITALICS
   --Italic marker *text*
 inLineItalicsHelper :: String -> Int -> [Int]
 inLineItalicsHelper "" index = []
 inLineItalicsHelper ('*':x) index = index : (inLineItalicsHelper x (index + 1))
 inLineItalicsHelper (x:xs) index = inLineItalicsHelper xs (index + 1)
 
 inLineItalics :: String -> Int -> [Int] -> String
 inLineItalics "" index list = ""
 inLineItalics ('*':xs) index list | ((index * (-1)) `elem` list && index /= 0) = "</em>" ++ inLineItalics xs (index + 1) list
                                    | (index `elem` list) =  "<em>" ++ inLineItalics xs (index + 1) list
 inLineItalics (x:xs) index list = x : inLineItalics xs (index + 1) list
 
 inLineItalicsParser :: String -> String
 inLineItalicsParser str = 
     if ((length (inLineItalicsHelper str 0)) <= 1) then str
     else if (odd $ length (inLineItalicsHelper str 0)) then inLineItalics str 0 (arrTransformer (init (inLineItalicsHelper str 0)) 0)
          else inLineItalics str 0 (arrTransformer (inLineItalicsHelper str 0) 0)

   --Italics marker _text_
 inLineItalicsHelperU :: String -> Int -> [Int]
 inLineItalicsHelperU "" index = []
 inLineItalicsHelperU ('_':x) index = index : (inLineItalicsHelperU x (index + 1))
 inLineItalicsHelperU (x:xs) index = inLineItalicsHelperU xs (index + 1)
 
 inLineItalicsU :: String -> Int -> [Int] -> String
 inLineItalicsU "" index list = ""
 inLineItalicsU ('_':xs) index list | ((index * (-1)) `elem` list && index /= 0) = "</em>" ++ inLineItalicsU xs (index + 1) list
                                    | (index `elem` list) =  "<em>" ++ inLineItalicsU xs (index + 1) list
 inLineItalicsU (x:xs) index list = x : inLineItalicsU xs (index + 1) list
 
 inLineItalicsParserU :: String -> String
 inLineItalicsParserU str = 
     if ((length (inLineItalicsHelperU str 0)) <= 1) then str
     else if (odd $ length (inLineItalicsHelperU str 0)) then inLineItalicsU str 0 (arrTransformer (init (inLineItalicsHelperU str 0)) 0)
          else inLineItalicsU str 0 (arrTransformer (inLineItalicsHelperU str 0) 0)



 --StrikeThrough
 inLineStrikeHelper :: String -> Int -> [Int]
 inLineStrikeHelper "" index = []
 inLineStrikeHelper ('~':'~':x) index = index : (inLineStrikeHelper x (index + 1))
 inLineStrikeHelper (x:xs) index = inLineStrikeHelper xs (index + 1)
 
 inLineStrike :: String -> Int -> [Int] -> String
 inLineStrike "" index list = ""
 inLineStrike ('~':'~':xs) index list | ((index * (-1)) `elem` list && index /= 0) = "</strike>" ++ inLineStrike xs (index + 1) list
                                    | (index `elem` list) =  "<strike>" ++ inLineStrike xs (index + 1) list
 inLineStrike (x:xs) index list = x : inLineStrike xs (index + 1) list
 
 inLineStrikeParser :: String -> String
 inLineStrikeParser str = 
     if ((length (inLineStrikeHelper str 0)) <= 1) then str
     else if (odd $ length (inLineStrikeHelper str 0)) then inLineStrike str 0 (arrTransformer (init (inLineStrikeHelper str 0)) 0)
          else inLineStrike str 0 (arrTransformer (inLineStrikeHelper str 0) 0)
 
 arrTransformer :: [Int] -> Int -> [Int]
 arrTransformer [] index = []
 arrTransformer (x:xs) index | odd index = (x * (-1)) : (arrTransformer xs (index + 1))
                             | True      = x : (arrTransformer xs (index + 1))
 
 inLineParser :: String -> String
 inLineParser "" = ""
 inLineParser str = inLineLinkParser (inLineImageParser (inLineStrikeParser (inLineItalicsParserU (inLineItalicsParser (inLineBoldParserU (inLineBoldParser str))))))

 --BLOCK PARSING

 removeTabs :: String -> String
 removeTabs "" = ""
 removeTabs ('\t':xs) = removeTabs xs
 removeTabs str = str

 countTabs :: String -> Integer
 countTabs "" = 0
 countTabs (x:xs) | x == '\t' = 1 + countTabs xs
                  | True      = countTabs xs

 lexer :: [String] -> [(Integer, Tag)]
 lexer []= []
 lexer (x:xs) = [(countTabs x, classify (removeTabs $ inLineParser x) (countTabs x))] ++ (lexer xs )
 
 blankFilter :: [(Integer, Tag)] -> [(Integer, Tag)]
 blankFilter [] = []
 blankFilter ((depth, Blank):xs) = (0, Blank) : blankFilter xs
 blankFilter (x:xs) = x : blankFilter xs
 
 parser :: [(Integer, Tag)] -> Tag
 parser [] = Blank
 parser ((depth, Html list):xs) = Html (parser xs : list)
 parser ((depth, Body list):xs) = Body ((dfs xs depth) ++ list)
 
 
 htmlGenerator :: Tag -> [(Integer, Tag)] -> String
 htmlGenerator Blank tags = ""
 htmlGenerator ((Html list)) tags = "<html>\n\n\t<head>\n" ++
     "\t\t<link href=\"https://cdn.jsdelivr.net/npm/bootstrap@5.0.0-beta3/dist/css/bootstrap.min.css\" rel=\"stylesheet\"" ++
     "integrity=\"sha384-eOJMYsd53ii+scO/bJGFsiCZc+5NDVN2yr8+0RDqr0Ql0h+rP48ckxlpbzKgwra6\"" ++
     "crossorigin=\"anonymous\">\n\t</head>\n\n" ++ (htmlGenList list tags) ++ "\n</html>"
 
 htmlGenList :: [Tag] -> [(Integer, Tag)] -> String
 htmlGenList [] tags = ""
 htmlGenList ((Body list):xs) tags =  "\t<body>\n" ++ (htmlGenList list tags) ++ "\t<body>\n"
 htmlGenList ((H1 str):xs) tags = "\t\t<h1>" ++ str ++ "</h1>\n" ++ (htmlGenList xs tags)
 htmlGenList ((H2 str):xs) tags = "\t\t<h2>" ++ str ++ "</h2>\n" ++ (htmlGenList xs tags)
 htmlGenList ((H3 str):xs) tags = "\t\t<h3>" ++ str ++ "</h3>\n" ++ (htmlGenList xs tags)
 htmlGenList ((H4 str):xs) tags = "\t\t<h4>" ++ str ++ "</h4>\n" ++ (htmlGenList xs tags)
 htmlGenList ((H5 str):xs) tags = "\t\t<h5>" ++ str ++ "</h5>\n" ++ (htmlGenList xs tags)
 htmlGenList ((Hr):xs) tags = "\t\t<hr>\n" ++ (htmlGenList xs tags)
 htmlGenList ((Blank):xs) tags = "\n" ++ (htmlGenList xs tags)
 htmlGenList ((P str):xs) tags = "\t\t<p>" ++ str ++ "</p>\n" ++ (htmlGenList xs tags)
 htmlGenList ((Ul depth list):xs) tags = genTabs depth ++ "\t\t<ul>\n" ++ (htmlGenList list tags) ++ genTabs depth ++ "\t\t</ul>\n" ++ (htmlGenList xs tags)
 htmlGenList ((Ol depth list):xs) tags = genTabs depth ++ "\t\t<ol>\n" ++ (htmlGenList list tags) ++ genTabs depth ++ "\t\t</ol>\n" ++ (htmlGenList xs tags)
 htmlGenList ((Liu depth str):xs) tags = genTabs depth ++ "\t\t<li>" ++ str ++ "</li>\n" ++ (htmlGenList xs tags)
 htmlGenList ((Lio depth str):xs) tags = genTabs depth ++ "\t\t<li>" ++ str ++ "</li>\n" ++ (htmlGenList xs tags)
 
 genTabs :: Integer -> String
 genTabs 0 = ""
 genTabs x = "\t" ++ genTabs (x - 1)
 
 dfs :: [(Integer, Tag)] -> Integer -> [Tag]
 dfs [] depth = []
 dfs ((d, H1 str):xs) depth = if (depth == d) then H1 str : dfs xs depth else []
 dfs ((d, H2 str):xs) depth = if (depth == d) then H2 str : dfs xs depth else []
 dfs ((d, H3 str):xs) depth = if (depth == d) then H3 str : dfs xs depth else []
 dfs ((d, H4 str):xs) depth = if (depth == d) then H4 str : dfs xs depth else []
 dfs ((d, H5 str):xs) depth = if (depth == d) then H5 str : dfs xs depth else []
 dfs ((d, P str):xs) depth = if (depth == d) then P str : dfs xs depth else []
 dfs ((d, Blank):xs) depth = if (depth == d) then  Blank : dfs xs depth else []
 dfs ((d, Hr):xs) depth = if (depth == d) then  Hr : dfs xs depth else []
 dfs ((d, Liu depthT str):xs) depth = if (d == depth) then Liu depthT str : dfs xs depth
                                      else if (d > depth) then 
                                           (Ul depth (dfs ([(d, Liu depthT str)] ++ xs) d)) : dfs (jump xs depth False) depth
                                           else []
 dfs ((d, Lio depthT str):xs) depth = if (d == depth) then Lio depthT str : dfs xs depth
                                      else if (d > depth) then 
                                           (Ol depth (dfs ([(d, Lio depthT str)] ++ xs) d)) : dfs (jump xs depth False) depth
                                           else []


 
 jump :: [(Integer, Tag)] -> Integer -> Bool -> [(Integer, Tag)]
 jump [] depth found = []
 jump ((d, Lio depthT str):xs) depth found = if (d > depth && not found) then jump xs depth found
                                       else if (depth == d && not found) then (d, Lio depthT str) : jump xs depth True
                                       else (d, Lio depthT str) : jump xs depth found
 jump ((d, Liu depthT str):xs) depth found = if (d > depth && not found) then jump xs depth found
                                       else if (depth == d && not found) then (d, Liu depthT str) : jump xs depth True
                                       else (d, Liu depthT str) : jump xs depth True
 jump (x:xs) depth found = x : jump xs depth True
 
 

 
 classify :: String -> Integer -> Tag
 classify [] depth = Blank
 classify ('#':'#':'#':'#':'#':' ':x) depth = H5 x
 classify ('#':'#':'#':'#':' ':x) depth = H4 x
 classify ('#':'#':'#':' ':x) depth = H3 x
 classify ('#':'#':' ':x) depth = H2 x
 classify ('#':' ':x) depth = H1 x
 classify ('-':'-':'-':x) depth = Hr
 classify ('-':' ':x) depth = Liu depth (inLineParser x)
 classify (x:'.':' ':xr) depth = if (isNumber(x)) then Lio depth (inLineParser xr) else P (x:'.':xr)
 classify str depth = P str
 





 main :: IO ()
 main = do
     fileName <- getLine
     contents <- readFile fileName
     let line = lines contents
     let lexed = [(0, Html [])] ++ [(0, Body [])] ++ lexer line
     let parsed =  parser $ blankFilter $ lexed
     let html = htmlGenerator parsed lexed
     --print lexed
     writeFile "out.html" html