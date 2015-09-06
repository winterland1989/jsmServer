-- | A simple json/jsonb persistent type

{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Database.Persist.Postgresql.Json
       (
         Jsonb(..)
       , Json(..)
       , (@>.)
       , (<@.)
       ) where

import           Data.Aeson
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding   as TE
import           Data.Time            ()
import           Database.Persist
import           Database.Persist.Sql


-- | Postgresql JSON type
newtype Json
  = Json A.Value
    deriving ( Show , Eq , FromJSON , ToJSON )


instance PersistField Json where
  toPersistValue v = PersistText $ TE.decodeUtf8 $ BSL.toStrict $ A.encode v
  fromPersistValue (PersistByteString v)
    = case A.decode (BSL.fromStrict v) of
        Nothing -> Left "Invalid JSON"
        Just j  -> Right j
  fromPersistValue _ = Left "Not PersistText"

instance PersistFieldSql Json where
   sqlType _ = SqlOther "JSON"

-- | Postgresql JSONB type
newtype Jsonb
  = Jsonb A.Value
    deriving ( Show , Eq , FromJSON , ToJSON )

instance PersistField Jsonb where
  toPersistValue v = PersistText $ TE.decodeUtf8 $ BSL.toStrict $ A.encode v
  fromPersistValue (PersistByteString v)
    = case A.decode (BSL.fromStrict v) of
        Nothing -> Left "Invalid jsonb"
        Just j  -> Right j
  fromPersistValue _ = Left "Invalid PersistValue for JSONB. PersistByteString required."

instance PersistFieldSql Jsonb where
   sqlType _ = SqlOther "JSONB"

infixr 6 @>., <@.

-- | Does the left JSON value contain within it the right value?
--
-- > '{"a":1, "b":2}'::jsonb @> '{"b":2}'::jsonb
--
-- See tests for example usage
(@>.) :: EntityField record Jsonb -> A.Value -> Filter record
(@>.) field val = Filter field jval specifier
  where
    specifier = BackendSpecificFilter "@>"
    jval = Left $ Jsonb val

-- | Does the right JSON value contain within it the left value?
--
-- > '{"a":1, "b":2}'::jsonb <@ '{"b":2}'::jsonb
--
-- See tests for example usage
(<@.) :: EntityField record Jsonb -> A.Value -> Filter record
(<@.) field val = Filter field jval specifier
  where
    specifier = BackendSpecificFilter "<@"
    jval = Left $ Jsonb val
