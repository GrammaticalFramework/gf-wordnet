module SenseSchema(synset,gloss,domain,example,secondary_example) where

import PGF2

synset            = mkApp "synset"  []
gloss             = mkApp "gloss"   []
domain            = mkApp "domain"  []
example           = mkApp "example" []
secondary_example = mkApp "secondary_example" []
