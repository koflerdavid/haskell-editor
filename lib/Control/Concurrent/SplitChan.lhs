> module Control.Concurrent.SplitChan (
>   SourceChan
> , SinkChan
> , splitChan
> , newSplitChan
> , writeSplitChan
> , readSplitChan
> , dupSplitChan
> ) where

This module implements a newtype wrapper on top of `Chan'.
The purpose is to create typesafe read-only and a write-only views of `Chan'.
They are implemented using `newtype' wrappers, so they should be reasonably cheap.

> import Control.Concurrent.Chan

> newtype SourceChan a = SourceChan (Chan a)
> newtype SinkChan a   = SinkChan (Chan a)

> splitChan :: Chan a -> (SourceChan a, SinkChan a)
> splitChan chan = (SourceChan chan, SinkChan chan)

> newSplitChan :: IO (SourceChan a, SinkChan a)
> newSplitChan = fmap splitChan newChan

> writeSplitChan :: SinkChan a -> a -> IO ()
> writeSplitChan (SinkChan chan) = writeChan chan

> readSplitChan :: SourceChan a -> IO a
> readSplitChan (SourceChan chan) = readChan chan

> dupSplitChan :: SourceChan a -> IO (SourceChan a)
> dupSplitChan (SourceChan chan) = fmap SourceChan (dupChan chan)
