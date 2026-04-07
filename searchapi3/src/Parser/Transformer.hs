{-# LANGUAGE OverloadedStrings #-}

module Parser.Transformer where

import Control.Applicative       (Alternative(many, (<|>), empty))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Functor              ((<&>))
import Data.List.NonEmpty        (NonEmpty (..))
import Data.Text                 (Text)

-- | Parser monad transformer
newtype ParserT s m a =
    ParserT { runParserT :: s -> m (Either Text (s, a)) }

-- Functor instance for ParserT
instance Functor m => Functor (ParserT s m) where
    fmap f (ParserT run) =
        ParserT $ \s -> fmap (<&> \(s', a) -> (s', f a)) (run s)

-- Applicative instance for ParserT
instance Monad m => Applicative (ParserT s m) where
    pure x = ParserT $ \s -> pure (Right (s, x))

    ParserT pf <*> ParserT px = ParserT $ \s ->
        pf s >>= \ef ->
            case ef of
                Left l -> pure (Left l)
                Right (s', f) ->
                    px s' <&> \ex ->
                        case ex of
                            Left l -> Left l
                            Right (s'', x) -> Right (s'', f x)

-- Alternative instance for ParserT
instance Monad m => Alternative (ParserT s m) where
    empty = ParserT $ \_ -> pure (Left "No more Alternatives")

    ParserT pa <|> ParserT pb = ParserT $ \s ->
        pa s >>= \ra ->
            case ra of
                Left{} -> pb s
                r      -> pure r

-- Monad instance for ParserT
instance Monad m => Monad (ParserT s m) where
    return = pure

    ParserT runA >>= pf = ParserT $ \s ->
        runA s >>= \ea ->
            case ea of
                Left l        -> pure (Left l)
                Right (s', a) ->
                    let ParserT runB = pf a
                    in runB s'

instance MonadTrans (ParserT s) where
    lift m = ParserT $ \s -> m >>= \a -> pure (Right (s, a))

sepBy1 :: Monad m => ParserT s m b -> ParserT s m a -> ParserT s m (NonEmpty a)
sepBy1 sep p = (:|) <$> p <*> many (sep *> p)