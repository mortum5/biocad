BiocadReaction
========

Usage example
-----------

Run neo4j localy.

Example for running from Docker
```bash
docker run --publish=7474:7474 --publish=7687:7687 --volume=$HOME/neo4j/data:/data neo4j
```
Run following command
```haskell
stack build
PORT=8080 stack exec biocad
```

Implemented Features
-------------
* Datatypes
* Function queryReaction which return reaction by id
* Function findShortestPath from one molecule to another
* Change web site for using queryReaction func and showing several fields from Reaction
*
