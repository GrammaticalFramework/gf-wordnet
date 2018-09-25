module SenseSchema(synset,gloss,domain,example,secondary_example,head_,modifier,ep) where

import PGF2

synset            = mkApp "synset"  []
gloss             = mkApp "gloss"   []
domain            = mkApp "domain"  []
example           = mkApp "example" []
secondary_example = mkApp "secondary_example" []
head_             = mkApp "head" []
modifier          = mkApp "modifier" []
ep e p            = mkApp "ep" [e,mkFloat p]
