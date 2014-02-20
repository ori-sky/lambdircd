module Plugin.Load where

import System.Plugins.Load
import qualified Plugin as Plugin

loadPlugin :: String -> IO (Maybe Plugin.Interface)
loadPlugin name = do
    p <- pdynload ("plugins/"++name++".o") ["."] [] "Plugin.Interface" "plugin"
    case p of
        LoadSuccess _ a -> return $ Just $ case Plugin.name a of
            ""  -> a {Plugin.name=name}
            _   -> a
        LoadFailure e   -> mapM_ putStrLn e >> return Nothing
