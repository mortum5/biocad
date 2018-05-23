{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Data
    ( ServerState (..), WebM (..)
    , constructState
    , createDb
    , queryReaction
    , findShortestPath
    ) where

import           Control.Monad.Trans        (liftIO)
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Data.List                  (nub)
import           Data.Maybe                 (fromJust)
import           Data.Map.Strict            (fromList, (!))
import           Data.Monoid                ((<>))
import           Data.Pool                  (Pool, createPool)
import           Data.Text                  (Text)
import           Database.Bolt

import           Type

-- |A pool of connections to Neo4j server
data ServerState = ServerState { pool :: Pool Pipe }

-- |Reader monad over IO to store connection pool
type WebM = ReaderT ServerState IO

createReaction :: Reaction -> BoltActionT IO ()
createReaction r = do res <- queryP cypher params
                      return ()
  where cypher = "CREATE (ml1:Molecule {id: {id1}, smiles : {smiles1}, "     <>
                 "iupacName: {iupac1}}), (ml2:Molecule {id: {id2}, smiles: " <>
                 "{smiles2}, iupacName : {iupac2}}), (ml3:Molecule {id:"     <>
                 " {id3}, smiles: {smiles3}, iupacName: {iupac3}}),"         <>
                 " (ct1:Catalyst {id: {id4}, smiles: {smiles4}, name "       <>
                 ": {name4}}), (re1:Reaction {id: {id5}, name: {name5}}),"   <>
                 " (ml1)-[:REAGENT_IN]->(re1), (ml2)-[:REAGENT_IN]->(re1),"  <>
                 " (ct1)-[:ACCELERATE {temperature: {temperature}, pressure:"<>
                 " {pressure}}]->(re1), (re1)-[:PRODUCT_FROM {amount: "      <>
                 "{amount}}]->(ml3)"
        params = fromList $ toParams r 0

createDb = mapM_ (createReaction . createSimpleReaction) [1..20]

queryReaction :: Int -> BoltActionT IO Reaction
queryReaction x = do records <- queryP cypher params
                     react <- toReaction records
                     liftIO $ putStrLn (show react)
                     return react
  where cypher = "MATCH (re:Reaction)<-[rc]-(ct:Catalyst), (re:Reaction)<--" <>
                 "(ml:Molecule), (re:Reaction)-[rm]->(mr:Molecule) WHERE "   <>
                 "re.id = {id} RETURN re,ct,rc,ml,rm,mr"
        params = fromList [("id", I x)]

findShortestPath :: Molecule -> Molecule -> BoltActionT IO [Chem]
findShortestPath from to = do records <- queryP cypher params
                              path :: Path <- (head records) `at` "path" >>= exact
                              res <- traverse (toChem) (pathNodes path)
                              liftIO $ putStrLn (show res)
                              return res
  where cypher = "MATCH path = shortestPath((ml:Molecule)-[*1..10]-"         <>
                 "(mr:Molecule)) WHERE ml.id = {id1} AND mr.id = {id2}"    <>
                 " RETURN path"
        params = fromList $ (toParams from 1) ++ (toParams to 2)

-- |Create pool of connections (4 stripes, 500 ms timeout, 1 resource per stripe)
constructState :: BoltCfg -> IO ServerState
constructState bcfg = do pool <- createPool (connect bcfg) close 4 500 1
                         return (ServerState pool)
