
module Apply(applyHintFile, applyHintFiles, applyHintString) where

import HSE.All
import Hint.All
import Control.Applicative
import Control.Arrow
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Settings
import Idea
import Util


-- | Apply hints to a single file.
applyHintFile :: ParseFlags -> [Setting] -> FilePath -> IO [Idea]
applyHintFile flags s file = do
    res <- parseModuleFile flags s file
    return $ case res of
        Left err -> [err]
        Right m -> executeHints s [m]


-- | Apply hints to the contents of a single file.
applyHintString :: ParseFlags -> [Setting] -> FilePath -> String -> IO [Idea]
applyHintString flags s file src = do
    res <- parseModuleString flags s file src
    return $ case res of
        Left err -> [err]
        Right m -> executeHints s [m]


-- | Apply hints to multiple files, allowing cross-file hints to fire.
applyHintFiles :: ParseFlags -> [Setting] -> [FilePath] -> IO [Idea]
applyHintFiles flags s files = do
    (err, ms) <- unzipEither <$> mapM (parseModuleFile flags s) files
    return $ err ++ executeHints s ms


-- | Given a list of settings (a way to classify) and a list of hints, run them over a list of modules.
executeHints :: [Setting] -> [Module_] -> [Idea]
executeHints s ms = concat $
    [ map (classify $ s ++ mapMaybe readPragma (universeBi m)) $
        order "" (hintModule hints nm m) ++
        concat [order (fromNamed d) $ decHints d | d <- moduleDecls m]
    | (nm,m) <- mns
    , let decHints = hintDecl hints nm m -- partially apply
    , let order n = map (\i -> i{func = (moduleName m,n)}) . sortBy (comparing loc)] ++
    [map (classify s) (hintModules hints mns)]
    where
        mns = map (scopeCreate &&& id) ms

        hints = (if length ms <= 1 then noModules else id) $ mconcat $ allHints s
        noModules h = h{hintModules = \_ -> []} `mappend` mempty{hintModule = \a b -> hintModules h [(a,b)]}


-- | Like 'parseModuleString', but also load the file from disk.
parseModuleFile :: ParseFlags -> [Setting] -> FilePath -> IO (Either Idea Module_)
parseModuleFile flags s file = do
    src <- readFileEncoding (encoding flags) file
    parseModuleString flags s file src


-- | Return either an idea (a parse error) or the module. In IO because might call the C pre processor.
parseModuleString :: ParseFlags -> [Setting] -> FilePath -> String -> IO (Either Idea Module_)
parseModuleString flags s file src = do
    res <- parseString (parseFlagsAddFixities [x | Infix x <- s] flags) file src
    case snd res of
        ParseOk m -> return $ Right m
        ParseFailed sl msg | length src `seq` True -> do
            -- figure out the best line number to grab context from, by reparsing
            (str2,pr2) <- parseString (parseFlagsNoLocations flags) "" src
            let ctxt = case pr2 of
                    ParseFailed sl2 _ -> context (srcLine sl2) str2
                    _ -> context (srcLine sl) src
            return $ Left $ classify s $ ParseFailure Warning "Parse error" sl msg ctxt


-- | Given a line number, and some source code, put bird ticks around the appropriate bit.
context :: Int -> String -> String
context lineNo src =
    unlines $ trimBy (all isSpace) $
    zipWith (++) ticks $ take 5 $ drop (lineNo - 3) $ lines src ++ [""]
    where ticks = ["  ","  ","> ","  ","  "]


-- | Find which hints a list of settings implies.
allHints :: [Setting] -> [Hint]
allHints xs = dynamicHints xs : map f builtin
    where builtin = nub $ concat [if x == "All" then map fst builtinHints else [x] | Builtin x <- xs]
          f x = fromMaybe (error $ "Unknown builtin hints: HLint.Builtin." ++ x) $ lookup x builtinHints


-- | Given some settings, make sure the severity field of the Idea is correct.
classify :: [Setting] -> Idea -> Idea
classify xs i = i{severity = foldl' (f i) (severity i) $ filter isClassify xs}
    where
        -- figure out if we need to change the severity
        f :: Idea -> Severity -> Setting -> Severity
        f i r c | matchHint (hintS c) (hint i) && matchFunc (funcS c) (func_ i) = severityS c
                | otherwise = r

        func_ x = if isParseFailure x then ("","") else func x
        matchHint = (~=)
        matchFunc (x1,x2) (y1,y2) = (x1~=y1) && (x2~=y2)
        x ~= y = null x || x == y
