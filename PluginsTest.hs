import System.Plugins
import Plugin

main = do
    p <- pdynload "HelloPlugin.o" ["."] [] "Plugin.Interface" "plugin"
    case p of
        LoadSuccess _ a -> print (Plugin.name a)
        LoadFailure e   -> mapM_ putStrLn e
