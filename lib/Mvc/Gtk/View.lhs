> module Mvc.Gtk.View (
>   ViewM
> , runViewM
> , getView
> , modifyView
> , changeView
> , sendMessage
> ) where

This module defines a state monad which stores the current view.

> import Control.Concurrent.SplitChan as SplitChan
> import Control.Monad.Trans (lift)
> import Control.Monad.Trans.RWS

> type ViewM message view a = RWST (SinkChan (Maybe message)) () view IO a

> runViewM :: ViewM message view a -> SinkChan (Maybe message) -> view -> IO (a, view)
> runViewM action chan view = do (a, view', ()) <- runRWST action chan view
>                                return (a, view')

> getView :: ViewM message view view
> getView = get

> modifyView :: (view -> view) -> ViewM message view ()
> modifyView = modify

> changeView :: view -> ViewM message view ()
> changeView = put

> sendMessage :: message -> ViewM message view ()
> sendMessage message = do
>   chan <- ask
>   lift $ SplitChan.writeSplitChan chan (Just message)
