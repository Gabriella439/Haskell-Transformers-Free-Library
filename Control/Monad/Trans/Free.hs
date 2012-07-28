{-|
    Free monads build syntax trees.  See the example sections for details.

    A free monad over a functor resembles a list of that functor:

    * 'return' behaves like @[]@ by not using the functor at all

    * 'wrap' behaves like @(:)@ by prepending another layer of the functor

    * 'liftF' behaves like @singleton@ by creating a list from a single layer of
      the functor.
-}

module Control.Monad.Trans.Free (
    -- * Usage
    -- $usage

    -- * Free monad
    Free,
    FreeF(..),
    runFree,

    -- * Free monad transformer
    FreeT(..),

    -- * Free monad operations
    wrap,
    liftF

    -- * Free monad example
    -- $freeexample

    -- * Free monad transformer example
    -- $freetexample
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Functor.Identity

{- $usage
    You can assemble values of type @Free f a@ or @FreeT f m a@ by hand using
    the smart constructors 'return' (from @Control.Monad@) and 'wrap':

> return ::              r  -> FreeT f m r
> wrap   :: f (FreeT f m r) -> FreeT f m r

    Use 'runFree' to deconstruct values of type @Free f r@:

> case runFree x of
>     Pure a -> ...
>     Free w -> ...

    Use 'runFreeT' to deconstruct values of type @FreeT f m r@ and bind the
    result in the base monad @m@.  You can then pattern match against the bound
    value:

> do x <- runFreeT f
>    case x of
>        Pure a -> ...
>        Free w -> ...
-}

-- | The signature for 'Free'
data FreeF f a x = Pure a | Free (f x)

{-|
    A free monad transformer alternates nesting the base monad @m@ and the base
    functor @f@, terminating with a value of type @a@.

    * @f@ - The functor that generates the free monad transformer

    * @m@ - The base monad

    * @a@ - The return value
-}
newtype FreeT f m a = FreeT { runFreeT :: m (FreeF f a (FreeT f m a)) }

instance (Functor f, Monad m) => Functor (FreeT f m) where
    fmap = liftM

instance (Functor f, Monad m) => Applicative (FreeT f m) where
    pure  = return
    (<*>) = ap

instance (Functor f, Monad m) => Monad (FreeT f m) where
    return  = FreeT . return . Pure
    m >>= f = FreeT $ do
        x <- runFreeT m
        runFreeT $ case x of
            Pure a -> f a
            Free w -> wrap $ fmap (>>= f) w

instance MonadTrans (FreeT f) where
    lift = FreeT . liftM Pure

instance (Functor f, MonadIO m) => MonadIO (FreeT f m) where
    liftIO = lift . liftIO 

-- | Prepend one layer of the functor to the free monad
wrap :: (Monad m) => f (FreeT f m a) -> FreeT f m a
wrap = FreeT . return . Free

-- | Convert one layer of a functor into an operation in the free monad
liftF :: (Functor f, Monad m) => f a -> FreeT f m a
liftF x = wrap $ fmap return x

{-|
    @Free f a@ is a list of nested @f@s terminating with a return value of type
    @a@.

    * @f@ - The functor that generates the free monad

    * @a@ - The return value
-}
type Free f = FreeT f Identity

-- | Observation function that exposes the next step
runFree :: Free f r -> FreeF f r (Free f r)
runFree = runIdentity . runFreeT

{- $freeexample
    To create a syntax tree, first define the signature for a single step in the
    syntax tree:

> data TeletypeF next = PutString String next | GetString (String -> next)

    ... then make the signature a 'Functor', where 'fmap' applies the given
    function to the @next@ step:

> instance Functor TeletypeF where
>     fmap f (PutString str x) = PutString str (f x)
>     fmap f (GetString     k) = GetString (f . k)

    The 'Free' type constructor generates the corresponding syntax tree from
    this signature:

> type Teletype a = Free TeletypeF a

    'liftF' creates primitive operations for building the syntax tree:

> putString :: String -> Teletype ()
> putString str = liftF $ PutString str ()
>
> getString :: Teletype String
> getString = liftF $ GetString id

    The syntax tree is automatically a monad, so you can assemble these
    operations into larger syntax trees using @do@ notation:

> echo :: Teletype a
> echo = forever $ do
>     str <- getString
>     putString str

    ... which is equivalent to the following hand-written syntax tree:

> echo' :: Teletype r
> echo' = wrap $ GetString $ \str -> wrap $ PutString str echo'

    You then interpret the syntax tree using 'runFree' to inspect the tree one
    step at a time.

> runIO :: Teletype a -> IO a
> runIO t = case runFree t of
>     Pure r -> return r
>     Free (PutString str t') -> do
>         putStrLn str
>         runIO t'
>     Free (GetString     k ) -> do
>         str <- getLine
>         runIO (k str)

>>> runIO echo
A<Enter>
A
Test<Enter>
Test
...

    You can write pure interpreters, too:

> runPure :: Teletype a -> [String] -> [String]
> runPure t strs = case runFree t of
>     Pure r -> []
>     Free (PutString str t') -> str:runPure t' strs
>     Free (GetString     k ) -> case strs of
>         []        -> []
>         str:strs' -> runPure (k str) strs'

>>> runPure echo ["A", "Test"]
["A","Test"]
-}

{- $freetexample
    The Free monad transformer 'FreeT' lets us invoke the base monad to build
    the syntax tree.  For example, you can use 'IO' to prompt the user to select
    each step of the syntax tree using the following monad:

> FreeT TeletypeF IO r

    Our original primitives actually had the following more polymorphic types,
    so you can reuse them:

> putString :: (Monad m) => String -> FreeT TeletypeF m ()
> putString str = liftF $ PutString str ()
>
> getString :: (Monad m) => FreeT TeletypeF m String
> getString = liftF $ GetString id

    Now the user can build the syntax tree from the command line:

> prompt :: FreeT TeletypeF IO ()
> prompt = do
>     lift $ putStrLn "Supply the next step:
>     cmd <- lift getLine
>     case cmd of
>         "forward" -> do
>             str <- getString
>             putString str
>             prompt
>         "greet" -> do
>             putString "Hello, world!"
>             prompt
>         _ -> return ()

    You can then run the syntax tree as the user builds it:

> -- The 'FreeT' version of 'runIO'
> runTIO :: FreeT TeletypeF IO r -> IO r
> runTIO t = do
>     x <- runFreeT t
>     case x of
>         Pure r -> return r
>         Free (PutString str t') -> do
>             putStrLn str
>             runTIO t'
>         Free (GetString k) -> do
>             str <- getLine
>             runTIO (k str)

>>> runTIO prompt
Supply the next step:
greet<Enter>
Hello, world!
Supply the next step:
forward<Enter>
test<Enter>
test
Supply the next step:
quit<Enter>

-}
