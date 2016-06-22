> module Mvc.Gtk.Cmd where

> import Control.Monad (join, liftM)

> type Cmd message = [IO message]

> (!) :: model -> Cmd message -> (model, Cmd message)
> (!) = (,)

> none :: Cmd message
> none = []

> single :: IO message -> Cmd message
> single = (:[])

> batch :: [Cmd message] -> Cmd message
> batch = join

> execute :: Cmd message -> IO [message]
> execute = sequence

> nestedCommand :: (inner -> messageInner -> messageOuter) -> (inner, Cmd messageInner) -> Cmd messageOuter
> nestedCommand f (inner, cmdInner) = map (\iMsgM -> liftM (f inner) iMsgM) cmdInner

> postpone :: message -> Cmd message
> postpone msg = [return msg]

> wrapInner :: (innerMessage -> outerMessage) -> Cmd innerMessage -> Cmd outerMessage
> wrapInner wrapper = map (fmap wrapper)
