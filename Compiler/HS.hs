-- | A module to interface with Haskell compilers.
--
-- You can dynamically run and load code to a running application.
--
-- While the exposed API of this module attempts to be compiler agnostic, no
-- other Haskell compiler but GHC has the needed functionality to implement
-- this. For now.
--

{-# LANGUAGE AutoDeriveTypeable, RankNTypes, OverloadedStrings #-}

module Compiler.HS ( evalExpression, loadCode ) where

import Data.Dynamic
import Data.IORef
import Data.Foldable
import Data.Time.Clock
import Data.Monoid
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Monad
import Control.Concurrent
import Control.Exception
import System.IO.Unsafe
import System.Temporary
import System.Posix.Files

import GHC
import GHC.Paths
import DriverPhases
import MonadUtils hiding ( foldlM )
import Exception
import StringBuffer
import Name

ghcThreadCommunication :: forall a. MVar (Ghc a, MVar (Either SomeException a))
ghcThreadCommunication = unsafePerformIO newEmptyMVar
{-# NOINLINE ghcThreadCommunication #-}

ghcThreadId :: IORef (Maybe (Maybe ThreadId))
ghcThreadId = unsafePerformIO $ newIORef Nothing
{-# NOINLINE ghcThreadId #-}

ensureGHCThreadIsAlive :: IO ()
ensureGHCThreadIsAlive = mask_ $ do
    should_launch_it <- atomicModifyIORef' ghcThreadId $ \old ->
        case old of
            Nothing -> ( Just Nothing, True )
            _ -> ( old, False )

    when should_launch_it $ do
        tid <- forkIOWithUnmask $ \unmask -> unmask ghcThread
        atomicModifyIORef' ghcThreadId $ \_ ->
            ( Just (Just tid), () )

inGHCMonad :: Ghc a -> IO a
inGHCMonad action = do
    ensureGHCThreadIsAlive

    result_mvar <- newEmptyMVar
    putMVar ghcThreadCommunication (action, result_mvar)
    result <- takeMVar result_mvar
    case result of
        Left exc -> throwIO exc
        Right result -> return result

ghcThread :: IO ()
ghcThread = forever $ do
    runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        void $ setSessionDynFlags dflags {
              hscTarget = HscInterpreted
            , ghcLink = LinkInMemory }
        defaultCleanupHandler dflags $ do
        -- TODO:
        -- We create a new GHC session every time we do something here. It's a
        -- bit inefficient. The problem is that if we don't do that we leak a
        -- bit of memory. Se the TODO item is to find out (by profiling?) where
        -- memory is being retained. Just write 'forever $' here to switch to
        -- single GHC session being used.
            (action, result_mvar) <- liftIO $ takeMVar ghcThreadCommunication
            result <- gtry action
            liftIO $ putMVar result_mvar result

-- | Evaluates a Haskell expression and returns the result as a `Dynamic`.
--
-- The expression can do anything. It is not safe to run untrusted code.
evalExpression :: [T.Text] -- ^ Modules to expose.
               -> T.Text   -- ^ The expression itself.
               -> IO Dynamic
evalExpression exposed_modules expr = inGHCMonad $ do
    setTargets []
    setContext $ fmap (\x -> IIDecl $ simpleImportDecl
                             (mkModuleName $ T.unpack x)) exposed_modules
    result <- dynCompileExpr (T.unpack expr)
    setContext []
    return result

-- | Compiles and interprets Haskell source code.
--
-- This is clean; no files will be left anywhere on the disk. However,
-- temporary files are created and used. This means the program needs to have
-- write access to the current directory.
--
-- Only one top-level definition from the code is used.
loadCode :: T.Text         -- ^ Name of the source code module.
         -> S.Set T.Text   -- ^ Name of the top-level definitions to return.
         -> T.Text         -- ^ Source code
         -> IO (M.Map T.Text Dynamic)
loadCode module_name top_levels src = do
    withTemporaryFile $ \handle fpath -> inGHCMonad $ do
        now <- liftIO getCurrentTime
        liftIO $ T.hPutStr handle src

        -- HACK: remove the GHC generated .hi and .o files at the end
        flip gfinally (liftIO $ do
            void $ etry $ removeLink (fpath <> ".hi")
            void $ etry $ removeLink (fpath <> ".o")) $ do

            let mod_name = mkModuleName $ T.unpack module_name
            setTargets [ Target { targetId = TargetFile fpath (Just $ Cpp HsSrcFile)
                                , targetAllowObjCode = False
                                , targetContents =
                                Just ( stringToStringBuffer $ T.unpack src, now ) } ]
            void $ load LoadAllTargets
            setContext [ IIDecl $ (simpleImportDecl mod_name) {
                            ideclQualified = True
                        , ideclAs = Just $ mkModuleName "Ex"
                        } ]

            mod <- findModule mod_name Nothing
            modinfo <- fromJust <$> getModuleInfo mod
            let names = modInfoExports modinfo
            let exported_names = S.fromList (fmap (T.pack . getOccString) names)

            results <- foldlM (\map top_level ->
                if S.member top_level exported_names
                then M.insert top_level <$>
                    dynCompileExpr ("Ex." <> T.unpack top_level) <*>
                    pure map
                else pure map)
                    M.empty top_levels

            setContext [ ]
            setTargets [ ]
            void $ load LoadAllTargets

            return results
  where
    etry :: IO a -> IO (Either SomeException a)
    etry = try


