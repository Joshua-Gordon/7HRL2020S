module Events where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Types

handleEvent :: Event -> IO World -> IO (IO World)
handleEvent (EventKey key ks' modifiers _) iw = do
    w <- iw
    let ks = (ks' == Down)
    return $ case key of
      Char 'w' -> return $ w{up=ks}
      Char 'a' -> return $ w{left=ks}
      Char 's' -> return $ w{down=ks}
      Char 'd' -> return $ w{right=ks}
      _ -> return w
handleEvent _ w = return w


