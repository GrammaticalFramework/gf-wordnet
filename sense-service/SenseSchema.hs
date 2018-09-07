module SenseSchema(synset,gloss,domain,example) where

import PGF2

synset  = mkApp "synset"  []
gloss   = mkApp "gloss"   []
domain  = mkApp "domain"  []
example = mkApp "example" []
