> module Mvc.Gtk (
>   runProgramWithFlags
> , module Graphics.UI.Gtk
> , module Mvc.Gtk.Cmd
> , module Mvc.Gtk.View
> ) where

> import Control.Concurrent
> import Control.Concurrent.SplitChan (SourceChan, SinkChan)
> import qualified Control.Concurrent.SplitChan as SplitChan
> import Data.IORef as IORef

> import Graphics.UI.Gtk

> import Mvc.Gtk.Cmd
> import Mvc.Gtk.View

> data Program args model message gtkUi = Program {
>   progRefState :: IORef (model, gtkUi)
> , progUpdate :: message -> model -> (model, Cmd message)
> , progUpdateView :: model -> ViewM message gtkUi ()
> , progFinalize :: model -> gtkUi -> IO ()
> }

> runProgramWithFlags :: Show message => args
>                        -> (args -> (model, Cmd message))
>                        -> (message -> model -> (model, Cmd message))
>                        -> (model -> (message -> IO ()) -> IO gtkUi)
>                        -> (model -> ViewM message gtkUi ())
>                        -> (model -> gtkUi -> IO ())
>                        -> IO ()
> runProgramWithFlags args modelInit update initView updateView finalize = do
>     (jobSource, jobSink) <- SplitChan.newSplitChan
>     (messageSource, messageSink) <- SplitChan.newSplitChan

>     let (model, cmd) = modelInit args
>     gtkUi <- initView model (sendMsg messageSink)
>     refState <- IORef.newIORef (model, gtkUi)
>     let program = Program refState update updateView finalize

>     _workerThreadId <- forkIO $ workerThread jobSource messageSink
>     _modelUpdateThreadId <- forkIO $
>       modelUpdateThread program (runJob jobSink) messageSource messageSink

>     runJob jobSink cmd
>     mainGUI

>   where runJob :: Show message => SinkChan (Maybe (Cmd message)) -> Cmd message -> IO ()
>         runJob chan task = postGUIAsync $ SplitChan.writeSplitChan chan (Just task)

>         sendMsg :: SinkChan (Maybe message) -> message -> IO ()
>         sendMsg chan = SplitChan.writeSplitChan chan . Just


> workerThread :: SourceChan (Maybe (Cmd message)) -> SinkChan (Maybe message) -> IO ()
> workerThread jobSource messageSink =
>     SplitChan.readSplitChan jobSource >>= backgroundWork
>   where backgroundWork Nothing = SplitChan.writeSplitChan messageSink Nothing
>         backgroundWork (Just job) = do
>           putStrLn "Got work"
>           execute job >>= mapM_ (SplitChan.writeSplitChan messageSink . Just)
>           SplitChan.readSplitChan jobSource >>= backgroundWork

> modelUpdateThread :: Show message => Program args model message gtkUi
>                      -> (Cmd message -> IO ())
>                      -> SourceChan (Maybe message)
>                      -> SinkChan (Maybe message)
>                      -> IO ()
> modelUpdateThread program runJob messageSource messageSink =
>     SplitChan.readSplitChan messageSource >>= modelUpdateWork >> return ()
>   where modelUpdateWork Nothing = do
>           (model, gtkUi) <- IORef.readIORef (progRefState program)
>           progFinalize program model gtkUi

>         modelUpdateWork (Just message) = do
>           putStrLn $ "Message: " ++ show message
>           (model, gtkUi) <- IORef.readIORef (progRefState program)
>           let (model', cmd) = progUpdate program message model
>           runJob cmd
>           postGUISync $ do
>             ((), gtkUi') <- runViewM (progUpdateView program model') messageSink gtkUi
>             IORef.writeIORef (progRefState program) (model', gtkUi')
>           SplitChan.readSplitChan messageSource >>= modelUpdateWork
