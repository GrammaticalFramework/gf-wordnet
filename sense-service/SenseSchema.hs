module SenseSchema(synset,gloss,domain,example,secondary_example,modifier,ep) where

import PGF2

synset            = mkApp "synset"  []
gloss             = mkApp "gloss"   []
domain            = mkApp "domain"  []
example           = mkApp "example" []
secondary_example = mkApp "secondary_example" []
modifier          = mkApp "modifier" []
ep e p            = mkApp "ep" [e,mkFloat p]
