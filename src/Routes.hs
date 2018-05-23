{-# LANGUAGE OverloadedStrings #-}

module Routes where

import           Control.Monad.Trans        (lift, liftIO)
import           Control.Monad.Trans.Reader (ask)
import           Data.Pool                  (withResource)
import           Data.Text.Lazy             (Text, toStrict, unpack)
import           Database.Bolt
import           Web.Scotty.Trans           (ActionT, file, param, json, rescue)

import           Type
import           Data

-- |Run BOLT action in scotty 'ActionT' monad tansformer
runQ :: BoltActionT IO a -> ActionT Text WebM a
runQ act = do ss <- lift ask
              liftIO $ withResource (pool ss) (`run` act)

-- |Main page route
mainR :: ActionT Text WebM ()
mainR = do runQ $ findShortestPath (Molecule 101 def def) (Molecule 102 def def)
           file "index.html"

searchR :: ActionT Text WebM ()
searchR = do q <- param "q" :: ActionT Text WebM Text
             results <- runQ $ queryReaction ((read (unpack q)) :: Int)
             json results
