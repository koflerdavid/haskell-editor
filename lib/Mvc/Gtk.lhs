> module Mvc.Gtk (
>   runProgramWithFlags
> , module Graphics.UI.Gtk
> , module Mvc.Gtk.Cmd
> ) where

> import           Control.Concurrent
> import           Control.Concurrent.SplitChan (SinkChan, SourceChan)
> import qualified Control.Concurrent.SplitChan as SplitChan
> import           Control.Monad.Loops          (whileJust_)
> import           Data.Functor                 (void)
> import           Data.IORef                   as IORef

> import           Graphics.UI.Gtk

> import           Mvc.Gtk.Cmd

> data Program args model message gtkUi = Program {
>   progRefState   :: IORef (model, gtkUi)
> , progUpdate     :: message -> model -> (model, Cmd message)
> , progUpdateView :: message -> model -> (message -> IO ()) -> gtkUi -> IO gtkUi
> , progFinalize   :: model -> gtkUi -> IO ()
> }

> runProgramWithFlags :: Show message => args
>                        -> (args -> (model, Cmd message))
>                        -> (message -> model -> (model, Cmd message))
>                        -> (model -> (message -> IO ()) -> IO gtkUi)
>                        -> (message -> model -> (message -> IO ()) -> gtkUi -> IO gtkUi)
>                        -> (model -> gtkUi -> IO ())
>                        -> IO ()
> runProgramWithFlags args modelInit update initView updateView finalize = do
>     (jobSource, jobSink) <- SplitChan.newSplitChan
>     (messageSource, messageSink) <- SplitChan.newSplitChan

>     let (model, cmd) = modelInit args
>     gtkUi <- initView model (sendMsg messageSink)
>     refState <- IORef.newIORef (model, gtkUi)
>     let program = Program refState update updateView finalize

>     void $ forkIO $ workerThread jobSource messageSink
>     void $ forkIO $ modelUpdateThread program (runJob jobSink) messageSource messageSink

>     runJob jobSink cmd
>     mainGUI

>   where runJob :: Show message => SinkChan (Maybe (Cmd message)) -> Cmd message -> IO ()
>         runJob chan task = postGUIAsync $ SplitChan.writeSplitChan chan (Just task)

>         sendMsg :: SinkChan (Maybe message) -> message -> IO ()
>         sendMsg chan = SplitChan.writeSplitChan chan . Just


> workerThread :: SourceChan (Maybe (Cmd message)) -> SinkChan (Maybe message) -> IO ()
> workerThread jobSource messageSink = do
>   whileJust_ (SplitChan.readSplitChan jobSource) $ \ job -> do
>     execute job >>= mapM_ (SplitChan.writeSplitChan messageSink . Just)

>   SplitChan.writeSplitChan messageSink Nothing

> modelUpdateThread :: Show message => Program args model message gtkUi
>                      -> (Cmd message -> IO ())
>                      -> SourceChan (Maybe message)
>                      -> SinkChan (Maybe message)
>                      -> IO ()
> modelUpdateThread program runJob messageSource messageSink = do
>   whileJust_ (SplitChan.readSplitChan messageSource) $ \ message -> do
>     (model, gtkUi) <- IORef.readIORef (progRefState program)
>     let (model', cmd) = progUpdate program message model
>     runJob cmd
>     postGUISync $ do
>       let sendMessage = SplitChan.writeSplitChan messageSink . Just
>       gtkUi' <- progUpdateView program message model' sendMessage gtkUi
>       IORef.writeIORef (progRefState program) (model', gtkUi')

>   (model, gtkUi) <- IORef.readIORef (progRefState program)
>   progFinalize program model gtkUi
