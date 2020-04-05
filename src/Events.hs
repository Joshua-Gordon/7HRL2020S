module Events where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Types

handleEvent :: Event -> World -> IO World
handleEvent (EventKey key ks' modifiers _) w = do
    let ks = (ks' == Down)
    return $ case key of
      Char 'w' ->  w{up=ks}
      Char 'a' ->  w{left=ks}
      Char 's' ->  w{down=ks}
      Char 'd' ->  w{right=ks}
      _ -> w
handleEvent _ w = return w


