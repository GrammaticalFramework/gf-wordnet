module SenseSchema(synset,gloss,domain,example,secondary_example,
                   vector,embedding,pair,global
                  ) where

import PGF2

synset            = mkApp "synset"  []
gloss             = mkApp "gloss"   []
domain            = mkApp "domain"  []
example           = mkApp "example" []
secondary_example = mkApp "secondary_example" []
vector vs         = mkApp "vector" (map mkFloat vs)
embedding         = mkApp "embedding" []
pair x y          = mkApp "pair" [x,y]
global            = mkApp "global" []
