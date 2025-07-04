--1 Constructors: the Resource Syntax API

resource WordNet = Predef ** open Parse in {

-- For developers: this document is tagged to support GF-Doc and synopsis    --%
-- generation:    --%
--  --% ignore this line in documentation    --%
--  --: this is a ground constructor  --%
-- Moreover, follow the format  --%
--   oper : Typ                 --%
--   = def ; --%                --%
--
-- This module gives access to the syntactic constructions of the
-- GF Resource Grammar library. Its main principle is simple:
-- to construct an object of type $C$, use the function $mkC$.
--
-- For example, an object of type $S$ corresponding to the string
--
-- $John loves Mary$
--
-- is written
--
-- $mkS (mkCl (mkNP (mkPN "John")) (mkV2 "love") (mkNP (mkPN "Mary")))$
--
-- This module defines the syntactic constructors, which take trees as arguments.
-- Lexical constructors, which take strings as arguments, are defined in the
-- $Paradigms$ modules separately for each language.
--
-- The recommended usage of this module is via the wrapper module $Syntax$,
-- which also contains the $Structural$ (structural words).
-- Together with $Paradigms$, $Syntax$ gives everything that is needed
-- to implement the concrete syntax for a language.

--2 Principles of organization

-- To make the library easier to grasp and navigate, we have followed
-- a set of principles when organizing it:
-- + Each category $C$ has an overloaded constructor $mkC$, with value type $C$.
-- + With $mkC$, it is possible to construct any tree of type $C$, except
--   atomic ones, i.e. those that take no arguments, and
--   those whose argument types are exactly the same as in some other instance
-- + To achieve completeness, the library therefore also has
--   for each atomic tree of type $C$, a constant suffixed $C$, and,
--   for other missing constructions, some operation suffixed $C$.
--   These constructors are listed immediately after the $mkC$ group.
-- + Those atomic constructors that are given in $Structural$ are not repeated here.
-- + In addition to the minimally complete set of constructions, many $mkC$ groups
--   include some frequently needed special cases, with two possible logics:
--   default value (to decrease the number of arguments), and
--   direct arguments of an intervening constructor (to flatten the terms).
-- + If such a special case is applied to some category in some rule, it is
--   also applied to all other rules in which the category appears.
-- + The constructors in a group are listed, roughly,
--   *from the most common to the most general*. This does not of course specify
--   a total order.
-- + Optional argument types are marked in parentheses. Although parentheses make no
--   difference in the way the GF compiler treats the types, their presence indicates
--   to the reader that the corresponding arguments can be left out; internally, the
--   library has an overload case for each such combination.
-- + Each constructor case is equipped with an example that is built by that
--   case but could not be built with any other one.
--
--

oper

--3 Phr: phrases in a text

-- Phrases are built from utterances by adding a phrasal conjunction
-- and a vocative, both of which are by default empty.

    mkPhr = overload { --%
      mkPhr : PConj -> Utt -> Voc -> Phr   -- but sleep, my friend  --:
      = PhrUtt ; --%
      mkPhr : PConj -> Utt -> Voc -> Mark -> Phr   -- but sleep, my friend  --:
      = PhrUttMark ; --%
      mkPhr : Utt -> Voc -> Phr -- come here John --%
      = \u,v -> PhrUtt NoPConj u v ; --%
      mkPhr : PConj -> Utt -> Phr -- but come here --%
      = \u,v -> PhrUtt u v NoVoc ; --%
      mkPhr : Utt -> Phr   -- come here --%
      = \u -> PhrUtt NoPConj u NoVoc   ;  --%
      mkPhr : Utt -> Mark -> Phr   -- come here --%
      = \u -> PhrUttMark NoPConj u NoVoc   ;  --%

-- A phrase can also be directly built by a sentence, a present-tense
-- clause, a question, or a positive singular imperative.

      mkPhr : S -> Phr   -- she won't sleep
         = \s -> PhrUtt NoPConj (UttS s) NoVoc ; --%
      mkPhr : Cl -> Phr   -- she sleeps
         = \s -> PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos s)) NoVoc ; --%
      mkPhr : QS -> Phr   -- would she sleep
         =    \s -> PhrUtt NoPConj (UttQS s) NoVoc ;  --%
      mkPhr : Imp -> Phr  -- sleep
         =  \s -> PhrUtt NoPConj (UttImpSg PPos s) NoVoc --%
      } ; --%

    mkPhrMark = overload { --%
      mkPhrMark : S -> Phr   -- she won't sleep
         = \s -> PhrUttMark NoPConj (UttS s) NoVoc FullStop ; --%
      mkPhrMark : Cl -> Phr   -- she sleeps
         = \s -> PhrUttMark NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos s)) NoVoc FullStop ; --%
      mkPhrMark : QS -> Phr   -- would she sleep
         =    \s -> PhrUttMark NoPConj (UttQS s) NoVoc QuestMark ;  --%
      mkPhrMark : Imp -> Phr  -- sleep
         =  \s -> PhrUttMark NoPConj (UttImpSg PPos s) NoVoc ExclMark --%
      } ; --%

--3 PConj, phrasal conjunctions

-- Any conjunction can be used as a phrasal conjunction.
-- More phrasal conjunctions are defined in $Structural$.

      mkPConj : Conj -> PConj   -- and   --:
        = PConjConj ; --%
      noPConj : PConj --: --%
        = NoPConj ; --%


--3 Voc, vocatives

-- Any noun phrase can be turned into a vocative.
-- More vocatives are defined in $Structural$.

    mkVoc : NP -> Voc  -- my friend   --:
      = VocNP ; --%
    noVoc : Voc --%
      = NoVoc ; --%


--3 Utt, utterances

-- Utterances are formed from sentences, clauses, questions, and imperatives.

    mkUtt = overload {
      mkUtt : S -> Utt                     -- she slept   --:
      = UttS ; --%
      mkUtt : Cl -> Utt                    -- she sleeps
      = \c -> UttS (UseCl (TTAnt TPres ASimul) PPos c) ; --%
      mkUtt : QS -> Utt                    -- who didn't sleep   --:
      = UttQS   ; --%
      mkUtt : QCl -> Utt                   -- who sleeps
      = \c -> UttQS (UseQCl (TTAnt TPres ASimul) PPos c) ; --%
      mkUtt : (ImpForm) -> (Pol) -> Imp -> Utt  -- don't be men   --:
      = mkUttImp  ; --%
      mkUtt : ImpForm -> Imp -> Utt -- be men --%
      = \f -> mkUttImp f PPos ; --%
      mkUtt : Pol -> Imp -> Utt  -- don't be men --%
      = UttImpSg  ;  --%
      mkUtt : Imp -> Utt  -- love yourself --%
      = UttImpSg PPos  ;  --%

-- Utterances can also be formed from interrogative phrases and
-- interrogative adverbials, noun phrases, adverbs, and verb phrases.

      mkUtt : IP   -> Utt     -- who   --:
      = UttIP    ; --%
      mkUtt : IAdv -> Utt     -- why   --:
      = UttIAdv  ; --%
      mkUtt : NP   -> Utt     -- this man  --:
      = UttNP    ; --%
      mkUtt : Adv  -> Utt     -- here   --:
      = UttAdv   ; --%
      mkUtt : VP   -> Utt     -- to sleep  --:
      = UttVP ASimul PPos it_Pron ; --%
      mkUtt : CN   -> Utt     -- beer      --:
      =    UttCN ; --%
      mkUtt : AP   -> Utt     -- good   --:
      =    UttAP it_Pron ; --%
      mkUtt : Card -> Utt     -- five   --:
      =    UttCard ; --%
    } ; --%

-- The plural first-person imperative is a special construction.

      lets_Utt : VP ->  Utt  -- let's sleep    --:
      = ImpPl1 ; --%


--2 Auxiliary parameters for phrases and sentences

--3 Pol, polarity

-- Polarity is a parameter that sets a clause to positive or negative
-- form. Since positive is the default, it need never be given explicitly.

      positivePol : Pol   -- she sleeps [default]   --:
        = PPos ; --%
      negativePol : Pol   -- she doesn't sleep    --:
        = PNeg ; --%

--3 Ant, anteriority

-- Anteriority is a parameter that presents an event as simultaneous or
-- anterior to some other reference time.
-- Since simultaneous is the default, it need never be given explicitly.

      simultaneousAnt : Ant   -- she sleeps [default]   --:
        = ASimul ; --%
      anteriorAnt : Ant   -- she has slept       --# notpresent  --:
        = AAnter ; --# notpresent --%

--3 Tense, tense

-- Tense is a parameter that relates the time of an event
-- to the time of speaking about it.
-- Since present is the default, it need never be given explicitly.

      presentTense     : Tense  -- she sleeps [default]   --:
        = TPres ; --%
      pastTense        : Tense  -- she slept           --# notpresent  --:
        = TPast ; --# notpresent --%
      futureTense      : Tense  -- she will sleep        --# notpresent  --:
        = TFut ; --# notpresent --%
      conditionalTense : Tense  -- she would sleep       --# notpresent   --:
        = TCond ; --# notpresent --%

--3 Temp, temporal and aspectual features

-- Temp is a combination of Tense and Ant. In extra modules for some
-- languages, it can also involve aspect and other things.

      mkTemp : Tense -> Ant -> Temp -- e.g. past + anterior
        = TTAnt ; --%

--3 ImpForm, imperative form

-- Imperative form is a parameter that sets the form of imperative
-- by reference to the person or persons addressed.
-- Since singular is the default, it need never be given explicitly.

      singularImpForm : ImpForm   -- be a man [default]   --:
      = IFSg ;  --%
      pluralImpForm   : ImpForm   -- be men  --:
      = IFPl ;  --%
      politeImpForm   : ImpForm   -- be a man [polite singular]  --:
      = IFPol ;  --%

-- This is how imperatives are implemented internally. --%

  param ImpForm = IFSg | IFPl | IFPol ; --%

  oper --%
  mkUttImp : ImpForm -> Pol -> Imp -> Utt --%
  = \f,p,i -> case f of { --%
      IFSg  => UttImpSg p i ; --%
      IFPl  => UttImpPl p i ; --%
      IFPol => UttImpPol p i --%
      } ; --%


--2 Sentences and clauses

--3 S, sentences

-- A sentence has a fixed tense, anteriority and polarity.

    mkS = overload {  --%
      mkS : Cl  -> S  --%
      = UseCl (TTAnt TPres ASimul) PPos ;   --%
      mkS : Tense -> Cl -> S    --%
      = \t -> UseCl (TTAnt t ASimul) PPos ;   --%
      mkS : Ant -> Cl -> S   --%
      = \a -> UseCl (TTAnt TPres a) PPos ;   --%
      mkS : Pol -> Cl -> S    --%
      = \p -> UseCl (TTAnt TPres ASimul) p ;   --%
      mkS : Tense -> Ant -> Cl -> S   --%
      = \t,a -> UseCl (TTAnt t a) PPos ;   --%
      mkS : Tense -> Pol -> Cl -> S   --%
      = \t,p -> UseCl (TTAnt t ASimul) p ;   --%
      mkS : Ant -> Pol -> Cl -> S   --%
      = \a,p -> UseCl (TTAnt TPres a) p ;   --%
      mkS : Tense -> Ant -> Pol -> Cl  -> S -- she wouldn't have slept
      = \t,a -> UseCl (TTAnt t a) ;   --%
      mkS : Temp -> Pol -> Cl -> S -- she wouldn't have slept  --:
      = UseCl ; --%

-- Sentences can be combined with conjunctions. This can apply to a pair
-- of sentences, but also to a list of more than two.

      mkS : Conj -> S -> S -> S   -- she sleeps and I run
      = \c,x,y -> ConjS c (BaseS x y) ; --%
      mkS : Conj -> ListS  -> S   -- she sleeps, I run and you walk  --:
      = \c,xy -> ConjS c xy ; --%

-- A sentence can be prefixed by an adverb.

      mkS : Adv -> S -> S           -- today, she sleeps   --:
      = AdvS ; --%
      } ;

--3 Cl, clauses

-- A clause has a variable tense, anteriority and polarity.
-- A clause can be built from a subject noun phrase
-- with a verb, adjective, or noun, and appropriate arguments.

    mkCl = overload {

      mkCl : NP -> V -> Cl                -- she sleeps
      = \s,v -> PredVP s (UseV v) ; --%
      mkCl : NP -> V2 -> NP -> Cl         -- she loves him
      = \s,v,o -> PredVP s (ComplSlash (SlashV2a v) o) ; --%
      mkCl : NP -> V3 -> NP -> NP -> Cl   -- she sends it to him
      = \s,v,o,i -> PredVP s (ComplSlash (Slash3V3 v i) o) ; --%
      mkCl : NP  -> VV -> VP -> Cl        -- she wants to sleep
        = \s,v,vp -> PredVP s (ComplVV v ASimul PPos vp) ; --%
      mkCl : NP  -> VS -> S  -> Cl        -- she says that she sleeps
        = \s,v,p -> PredVP s (ComplVS v p) ; --%
      mkCl : NP  -> VQ -> QS -> Cl        -- she wonders who sleeps
        = \s,v,q -> PredVP s (ComplVQ v q) ; --%
      mkCl : NP  -> VA -> A -> Cl         -- she becomes old
        = \s,v,q -> PredVP s (ComplVA v (PositA q)) ; --%
      mkCl : NP  -> VA -> AP -> Cl        -- she becomes very old
        = \s,v,q -> PredVP s (ComplVA v q) ; --%
      mkCl : NP  -> V2A -> NP -> A -> Cl  -- she paints it red
        = \s,v,n,q -> PredVP s (ComplSlash (SlashV2A v (PositA q)) n) ; --%
      mkCl : NP  -> V2A -> NP -> AP -> Cl -- she paints it very red
        = \s,v,n,q -> PredVP s (ComplSlash (SlashV2A v q) n) ; --%
      mkCl : NP  -> V2S -> NP -> S -> Cl          -- she answers to him that we sleep
        = \s,v,n,q -> PredVP s (ComplSlash (SlashV2S v q) n) ; --%
      mkCl : NP  -> V2Q -> NP -> QS -> Cl         -- she asks him who sleeps
        = \s,v,n,q -> PredVP s (ComplSlash (SlashV2Q v q) n) ; --%
      mkCl : NP  -> V2V -> NP -> VP -> Cl         -- she begs him to sleep
        = \s,v,n,q -> PredVP s (ComplSlash (SlashV2V v ASimul PPos q) n) ; --%
      mkCl : NP  -> VPSlash -> NP -> Cl         -- she begs him to sleep here
        = \s,v,n -> PredVP s (ComplSlash v n) ; --%
      mkCl : NP -> A  -> Cl    -- she is old
        = \x,y -> PredVP x (UseComp (CompAP (PositA y))) ; --%
      mkCl : NP -> A -> NP -> Cl -- she is older than him
        = \x,y,z -> PredVP x (UseComp (CompAP (ComparA y z))) ; --%
      mkCl : NP -> A2 -> NP -> Cl -- she is married to him
	= \x,y,z -> PredVP x (UseComp (CompAP (ComplA2 y z))) ; --%
      mkCl : NP -> AP -> Cl    -- she is very old
	= \x,y -> PredVP x (UseComp (CompAP y)) ; --%
      mkCl : NP -> NP -> Cl    -- she is the woman
        = \x,y -> PredVP x (UseComp (CompNP y)) ; --%
      mkCl : NP -> N -> Cl    -- she is a woman
        = \x,y -> PredVP x (UseComp (CompCN (UseN y))) ; --%
      mkCl : NP -> CN -> Cl    -- she is an old woman
	= \x,y -> PredVP x (UseComp (CompCN y)) ; --%
      mkCl : NP -> Adv -> Cl   -- she is here
	= \x,y -> PredVP x (UseComp (CompAdv y)) ; --%

-- As the general rule, a clause can be built from a subject noun phrase and
-- a verb phrase.

      mkCl : NP -> VP -> Cl   -- she always sleeps   --:
      = PredVP  ; --%

-- Existentials are a special form of clauses.

      mkCl : N -> Cl           -- there is a house
      = \y -> ExistNP (DetCN (DetQuant IndefArt NumSg) (UseN y)) ; --%
      mkCl : CN -> Cl          -- there is an old house
      = \y -> ExistNP (DetCN (DetQuant IndefArt NumSg) y) ; --%
      mkCl : NP -> Cl          -- there are many houses   --:
      = ExistNP ; --%

-- There are also special forms in which a noun phrase or an adverb is
-- emphasized.

      mkCl : NP  -> RS -> Cl   -- it is she who sleeps   --:
      = CleftNP    ; --%
      mkCl : Adv -> S  -> Cl   -- it is here that she sleeps    --:
      = CleftAdv   ; --%

-- Subjectless verb phrases are used for impersonal actions.

      mkCl : V -> Cl   -- it rains
      = \v -> ImpersCl (UseV v) ; --%
      mkCl : VP -> Cl  -- it is raining    --:
      = ImpersCl   ;  --%
      mkCl : SC -> VP -> Cl  -- that she sleeps is good --:
      = PredSCVP ; --%

      } ;

-- Generic clauses are those with an impersonal subject.

      genericCl : VP ->  Cl    -- one sleeps
      = GenericCl ; --%

--2 Verb phrases and imperatives

--3 VP, verb phrases

-- A verb phrase is formed from a verb with appropriate arguments.

    mkVP = overload {
      mkVP : V   -> VP                -- sleep --:
      = UseV      ; --%
      mkVP : V2  -> NP -> VP          -- love him
      = \v,o -> ComplSlash (SlashV2a v) o ; --%
      mkVP : V3  -> NP -> NP -> VP    -- send a message to him
      = \v,o,i -> ComplSlash (Slash3V3 v i) o ; --%
      mkVP : VV  -> VP -> VP          -- want to sleep  --:
      = \v -> ComplVV v ASimul PPos  ; --%
      mkVP : VS  -> S  -> VP          -- know that she sleeps  --:
      = ComplVS   ; --%
      mkVP : VQ  -> QS -> VP          -- wonder if she sleeps  --:
      = ComplVQ   ; --%
      mkVP : VA  -> AP -> VP          -- become red  --:
      = ComplVA   ; --%
      mkVP : V2A -> NP -> AP -> VP    -- paint it red
      = \v,n,q -> ComplSlash (SlashV2A v q) n  ; --%
      mkVP : V2S -> NP -> S  -> VP         -- answer to him that we sleep
        = \v,n,q -> (ComplSlash (SlashV2S v q) n) ; --%
      mkVP : V2Q -> NP -> QS -> VP         -- ask him who sleeps
        = \v,n,q -> (ComplSlash (SlashV2Q v q) n) ; --%
      mkVP : V2V -> NP -> VP -> VP         -- beg him to sleep
        = \v,n,q -> (ComplSlash (SlashV2V v ASimul PPos q) n) ; --%

-- The verb can also be a copula ("be"), and the relevant argument is
-- then the complement adjective or noun phrase.

      mkVP : A -> VP               -- be warm
      = \a -> UseComp (CompAP (PositA a)) ; --%
      mkVP : A -> NP -> VP         -- be older than him
      = \y,z -> (UseComp (CompAP (ComparA y z))) ; --%
      mkVP : A2 -> NP -> VP        -- be married to him
      = \y,z -> (UseComp (CompAP (ComplA2 y z))) ; --%
      mkVP : AP -> VP              -- be warm
      = \a -> UseComp (CompAP a)   ; --%
      mkVP : N -> VP               -- be a man
      = \y -> UseComp (CompCN (UseN y)) ; --%
      mkVP : CN -> VP              -- be an old man
      = \y -> UseComp (CompCN y) ; --%
      mkVP : NP -> VP              -- be the man
      = \a -> UseComp (CompNP a)   ; --%
      mkVP : Adv -> VP             -- be here
      = \a -> UseComp (CompAdv a)   ; --%

-- A verb phrase can be modified with a postverbal or a preverbal adverb.

      mkVP : VP -> Adv -> VP          -- sleep here   --:
      = \vp,adv -> [default: vp | AdvVP vp adv]     ; --%
      mkVP : AdV -> VP -> VP          -- always sleep   --:
      = \adv,vp -> [default: vp | AdVVP adv vp] ; --%

-- Objectless verb phrases can be taken to verb phrases in two ways.

      mkVP : VPSlash -> NP -> VP      -- paint it black  --:
      = ComplSlash ; --%
      mkVP : VPSlash -> VP            -- paint itself black --:
        = \v -> ReflVPSlash v ReflPron ; --%

      mkVP : Comp -> VP               -- be warm --:
        = UseComp ; --%

      } ; --%

-- Two-place verbs can be used reflexively, and VPSlash more generally.
    reflexiveVP = overload { --%
      reflexiveVP : V2 -> VP        -- love itself
      = \v -> ReflVPSlash (SlashV2a v) ReflPron ; --%
      reflexiveVP : VPSlash -> VP   -- paint itself black
        = \v -> ReflVPSlash v ReflPron ; --%
      } ; --%


-- Two-place verbs can also be used in the passive, with or without an agent.

    passiveVP = overload { --%
      passiveVP : V2 ->       VP   -- be loved
      = \v -> PassVPSlash (SlashV2a v) ; --%
      passiveVP : VPSlash ->  VP   -- be loved
      = \vps -> PassVPSlash vps ; --%
      passiveVP : V2 -> NP -> VP   -- be loved by her
      = \v,np -> PassAgentVPSlash (SlashV2a v) np ; --%
      passiveVP : VPSlash -> NP -> VP   -- be loved by her
      = \vps,np -> PassAgentVPSlash vps np ; --%
      } ; --%

-- A verb phrase can be turned into the progressive form.

      progressiveVP : VP -> VP   -- be sleeping
      = ProgrVP ; --%

--3 Comp, verb phrase complements

   mkComp = overload { --%
     mkComp : AP -> Comp -- very old --:
     = CompAP ; --%
     mkComp : NP -> Comp -- this man --:
     = CompNP ; --%
     mkComp : Adv -> Comp -- here --:
     = CompAdv ; --%
     } ; --%

--3 SC, embedded sentence

   mkSC = overload { --%
     mkSC : S -> SC -- that he sleeps --:
     = EmbedS ; --%
     mkSC : QS -> SC -- whether he sleeps --:
     = EmbedQS ; --%
     mkSC : VP -> SC -- to sleep --:
     = EmbedVP ASimul PPos it_Pron ; --%
     } ; --%


--3 Imp, imperatives

-- Imperatives are formed from verbs and their arguments; as the general
-- rule, from verb phrases.

    mkImp = overload {  --%
      mkImp : VP -> Imp                -- come to my house
      = ImpVP      ;  --%
      mkImp : V  -> Imp                -- come
      = \v -> ImpVP (UseV v)  ;   --%
      mkImp : V2 -> NP -> Imp          -- buy it
      = \v,np -> ImpVP (ComplSlash (SlashV2a v) np) ; --%
      } ;  --%


--2 Noun phrases and determiners

--3 NP, noun phrases

-- A noun phrases can be built from a determiner and a common noun ($CN$) .
-- For determiners, the special cases of quantifiers, numerals, integers,
-- and possessive pronouns are provided. For common nouns, the
-- special case of a simple common noun ($N$) is always provided.

    mkNP = overload {
      mkNP : Quant -> N  -> NP          -- this man
          = \q,n -> DetCN (DetQuant q NumSg) (UseN n) ; --%
      mkNP : Quant -> CN -> NP          -- this old man
          = \q,n -> DetCN (DetQuant q NumSg) n ; --%
      mkNP : Quant -> Num -> CN -> NP   -- these five old men
          = \q,nu,n -> DetCN (DetQuant q nu) n ; --%
      mkNP : Quant -> Num -> Ord -> CN -> NP   -- these five best old men --%
          = \q,nu,or,n -> DetCN (DetQuantOrd q nu or) n ; --%
      mkNP : Quant -> Num -> N  -> NP   -- these five men
          = \q,nu,n -> DetCN (DetQuant q nu) (UseN n) ; --%
      mkNP : Det -> CN -> NP      -- the first old man   --:
          =  DetCN    ; --%
      mkNP : Det -> N -> NP       -- the first man
          =  \d,n -> DetCN d (UseN n)   ; --%
      mkNP : Numeral -> CN -> NP      -- fifty old men
	  = \d,n -> DetCN (DetQuant IndefArt (NumCard (NumNumeral d))) n ; --%
      mkNP : Numeral -> N -> NP       -- fifty men
	  = \d,n -> DetCN (DetQuant IndefArt (NumCard (NumNumeral d))) (UseN n) ; --%
      mkNP : Decimal -> CN -> NP      -- 51 old men
	  = \d,n -> DetCN (DetQuant IndefArt (NumCard (NumDecimal d))) n ; --%
      mkNP : Decimal -> N -> NP       -- 51 men
	  = \d,n -> DetCN (DetQuant IndefArt (NumCard (NumDecimal d))) (UseN n) ; --%
      mkNP : Card -> CN -> NP     -- forty-five old men
	  =  \d,n -> DetCN (DetQuant IndefArt (NumCard d)) n ; --%
      mkNP : Card -> N -> NP       -- forty-five men
	  =  \d,n -> DetCN (DetQuant IndefArt (NumCard d)) (UseN n) ; --%
      mkNP : Pron -> CN -> NP   -- my old man
          = \p,n -> DetCN (DetQuant (PossPron p) NumSg) n ; --%
      mkNP : Pron -> N  -> NP   -- my man
          = \p,n -> DetCN (DetQuant (PossPron p) NumSg) (UseN n) ; --%

-- Proper names and pronouns can be used as noun phrases.

      mkNP : PN -> NP
      = UsePN    ; --%
    mkNP : LN -> NP               -- Sweden  --:
      = UseLN    ; --%
      mkNP : Pron -> NP           -- he  --:
      = UsePron  ; --%

-- Determiners alone can form noun phrases.

      mkNP : Quant -> NP           -- this
          =  \q -> UseDAP (DetDAP (DetQuant q NumSg)) ; --%
      mkNP : Quant -> Num -> NP    -- these five
          =  \q,n -> UseDAP (DetDAP (DetQuant q n)) ; --%
      mkNP : Det -> NP             -- these five best  --:
          =  \d -> UseDAP (DetDAP d) ; --%

-- Determinesless mass noun phrases.

      mkNP : CN -> NP  -- old beer   --:
          = MassNP ; --%
      mkNP : N -> NP  -- beer
          = \n -> MassNP (UseN n) ; --%

-- A noun phrase once formed can be prefixed by a predeterminer and
-- suffixed by a past participle or an adverb.

      mkNP : Predet -> NP -> NP  -- only the man --:
      = \pd,np -> [default: np | PredetNP pd np] ; --%
      mkNP : NP -> Adv -> NP     -- Paris today --:
      = \np,adv -> [default: np | AdvNP np adv] ; --%
      mkNP : NP -> RS -> NP      -- John, who walks --:
      = \np,rs -> [default: np | RelNP np rs] ; --%

-- A conjunction can be formed both from two noun phrases and a longer
-- list of them.

      mkNP : Conj -> NP -> NP -> NP
      = \c,x,y -> ConjNP c (BaseNP x y) ; --%
      mkNP : Conj -> ListNP -> NP --:
      = \c,xy -> ConjNP c xy ; --%

      mkNP : NP -> NP = \np -> np ;   -- mkNP (expr "Qid") will also return NPs

      } ; --%

-- Pronouns can be used as noun phrases.

      i_NP : NP          -- I
      = mkNP i_Pron ;
      you_NP : NP        -- you (singular)
      = mkNP youSg_Pron ;
      youPol_NP : NP     -- you (polite singular)
      = mkNP youPol_Pron ;
      he_NP : NP         -- he
      = mkNP he_Pron ;
      she_NP : NP        -- she
      = mkNP she_Pron ;
      it_NP : NP         -- it
      = mkNP it_Pron ;
      we_NP : NP         -- we
      = mkNP we_Pron ;
      youPl_NP : NP      -- you (plural)
      = mkNP youPl_Pron ;
      they_NP : NP       -- they
      = mkNP they_Pron ;

    this_NP : NP -- this
    = UseDAP (DetDAP (DetQuant this_Quant NumSg)) ;  --%
    that_NP : NP -- that
    = UseDAP (DetDAP (DetQuant that_Quant NumSg)) ;  --%
    these_NP : NP
    = UseDAP (DetDAP (DetQuant this_Quant NumPl)) ;  --%
    those_NP : NP
    = UseDAP (DetDAP (DetQuant that_Quant NumPl)) ;  --%


--3 Det, determiners

-- A determiner is either a singular or a plural one.
-- Quantifiers that have both singular and plural forms are by default used as
-- singular determiners. If a numeral is added, the plural form is chosen.
-- A determiner also has an optional ordinal.

    mkDet = overload { --%

      mkDet : Quant ->  Det       -- this
        = \q -> DetQuant q NumSg  ; --%
      mkDet : Quant -> Card -> Det   -- these five
        = \d,nu -> (DetQuant d (NumCard nu)) ; --%
      mkDet : Quant -> Ord -> Det     -- the best
        = \q,o -> DetQuantOrd q NumSg o  ; --%
      mkDet : Quant -> Num -> Ord -> Det  -- these five best --:
        = DetQuantOrd  ; --%
      mkDet : Quant -> Num -> Det -- these five  --:
        = DetQuant ; --%

-- Numerals, their special cases integers and digits, and possessive pronouns can be
-- used as determiners.

      mkDet : Card ->  Det     -- forty
	= \c -> DetQuant IndefArt (NumCard c)  ; --%
      mkDet : Decimal -> Det    -- 51, 3.14
	= \d -> DetQuant IndefArt (NumCard (NumDecimal d)) ; --%
      mkDet : Numeral -> Det  -- five
	= \d -> DetQuant IndefArt (NumCard (NumNumeral d)) ; --%
      mkDet : Pron -> Det     -- my
        = \p -> DetQuant (PossPron p) NumSg ; --%
      mkDet : Pron -> Num -> Det -- my five
        = \p -> DetQuant (PossPron p) ; --%

      } ; --%


      the_Det   : Det -- the (house)
        = theSg_Det ; --%
      a_Det     : Det -- a (house)
        = aSg_Det ; --%
      theSg_Det : Det -- the (houses)
        = DetQuant DefArt NumSg ; --%
      thePl_Det : Det -- the (houses)
        = DetQuant DefArt NumPl ; --%
      aSg_Det   : Det -- a (house)
        = DetQuant IndefArt NumSg ; --%
      aPl_Det   : Det -- (houses)
        = DetQuant IndefArt NumPl ; --%
      this_Det : Det
      = (DetQuant this_Quant NumSg) ; --%
      that_Det : Det
      = (DetQuant that_Quant NumSg) ; --%
      these_Det : Det
      = (DetQuant this_Quant NumPl) ; --%
      those_Det : Det
      = (DetQuant that_Quant NumPl) ; --%



--3 Quant, quantifiers

-- There are definite and indefinite articles.

    mkQuant = overload { --%
      mkQuant : Pron -> Quant   -- my  --:
      = PossPron ; --%
      } ; --%

    the_Quant : Quant    -- the --:
      = DefArt ; --%
    a_Quant   : Quant    -- a  --:
      = IndefArt ; --%

--3 Num, cardinal numerals

-- Numerals can be formed from number words ($Numeral$), their special case digits,
-- and from symbolic integers.

    mkNum = overload { --%
      mkNum : Int -> Num   -- thirty-five (given by "35"; range 1-999)
        = \n -> NumCard (int2card n) ; --%
      mkNum : Float -> Num   -- thirty-five (given by "35"; range 1-999)
        = \f -> NumCard (NumDecimal (float2decimal f)) ; --%
      mkNum : Numeral -> Num  -- twenty
        = \d -> NumCard (NumNumeral d) ; --%
      mkNum : Decimal -> Num   -- 21
        = \d -> NumCard (NumDecimal d)      ; --%
      mkNum : Card -> Num  -- almost ten --:
        = NumCard ; --%

-- A numeral can be modified by an adnumeral.

      mkNum : AdN -> Card -> Num  -- almost ten
        = \a,c -> NumCard (AdNum a c)
      } ; --%

-- Dummy numbers are sometimes to select the grammatical number of a determiner.

      singularNum : Num              -- singular --:
      = NumSg       ; --%
      pluralNum : Num                -- plural --:
      = NumPl       ; --%


-- Cardinals are the non-dummy numerals.

    mkCard = overload {  --%
      mkCard : Int -> Card   -- thirty-five (given as "35"; range 1-999)
        = int2card ; --%
      mkCard : Numeral -> Card   -- twenty  --:
        = NumNumeral ; --%
      mkCard : Decimal -> Card      -- 51, 3.14  --:
        = NumDecimal ; --%
      mkCard : AdN -> Card -> Card  -- almost fifty
        = AdNum ; --%
      } ; --%

--3 Ord, ordinal numerals

-- Just like cardinals, ordinals can be formed from number words ($Numeral$)
-- and from symbolic integers.

    mkOrd = overload { --%
      mkOrd : Numeral -> Ord   -- twentieth  --:
      = OrdNumeral ; --%
      mkOrd : Digits -> Ord         -- 51st --:
      = OrdDigits      ; --%
      mkOrd : Int -> Ord       -- fifth
      = int2ord ; --%

-- Also adjectives in the superlative form can appear on ordinal positions.

      mkOrd : A -> Ord           -- largest  --:
      = OrdSuperl ; --%
      } ; --%


--3 AdN, adnumerals

-- Comparison adverbs can be used as adnumerals.

      mkAdN : CAdv -> AdN  -- more than --:
      = AdnCAdv PPos ; --%

--3 Numeral, number words

    mkNumeral : Int -> Numeral = int2numeral ;

--3 Digits, numerals as sequences of digits

    mkDigits : Int -> Digits = int2digits ;

--3 numerals with a fractional part

  mkDecimal = overload {  --%
    mkDecimal : Int -> Decimal = int2decimal ;
    mkDecimal : Float -> Decimal = float2decimal
  } ;

--2 Nouns

--3 CN, common noun phrases

    mkCN = overload { --%

-- The simplest way of forming common noun phrases is from atomic nouns $N$.

      mkCN : N  -> CN            -- house  --:
      = UseN     ; --%

-- Common noun phrases can be formed from relational nouns by providing arguments.

      mkCN : N2 -> NP -> CN      -- mother of John  --:
      = ComplN2  ; --%
      mkCN : N3 -> NP -> NP -> CN      -- distance from this city to Paris --:
      = \f,x -> ComplN2 (ComplN3 f x)  ; --%

-- A common noun phrase can be modified by an adjectival phrase. We give special
-- cases of this, where one or both of the arguments are atomic.


      mkCN :  A ->  N  -> CN     -- big house
      = \x,y -> AdjCN (PositA x) (UseN y) ; --%
      mkCN :  A -> CN  -> CN     -- big blue house
      = \x,y -> [default: y | AdjCN (PositA x) y] ; --%
      mkCN : AP ->  N  -> CN     -- very big house
      = \x,y -> AdjCN x (UseN y) ; --%
      mkCN : AP -> CN  -> CN     -- very big blue house
      = \x,y -> [default: y | AdjCN x y]  ; --%

-- A common noun phrase can be modified by a relative clause or an adverb.

      mkCN :  N -> RS  -> CN     -- house that she owns
      = \x,y -> RelCN (UseN x) y   ; --%
      mkCN : CN -> RS  -> CN     -- big house that she loves --:
      = \x,y -> [default: x | RelCN x y] ; --%
      mkCN :  N -> Adv -> CN     -- house on the hill
      = \x,y -> AdvCN (UseN x) y  ; --%
      mkCN : CN -> Adv -> CN     -- big house on the hill
      = \x,y -> [default: x | AdvCN x y]  ; --%

-- For some nouns it makes sense to modify them by sentences,
-- questions, or infinitives. But syntactically this is possible for
-- all nouns.

      mkCN : CN -> S   -> CN     -- rule that she sleeps
      = \cn,s -> [default: cn | SentCN cn (EmbedS s)] ; --%
      mkCN : CN -> QS  -> CN     -- question if she sleeps
      = \cn,s -> [default: cn | SentCN cn (EmbedQS s)] ; --%
      mkCN : CN -> VP  -> CN     -- reason to sleep
      = \cn,s -> [default: cn | SentCN cn (EmbedVP ASimul PPos it_Pron s)] ; --%
      mkCN : CN -> SC  -> CN     -- reason to sleep --:
      = \cn,s -> [default: cn | SentCN cn s] ; --%

-- A noun can be used in apposition to a noun phrase, especially a proper name.

      mkCN :  N -> NP  -> CN     -- king John
      = \x,y -> ApposCN (UseN x) y ; --%
      mkCN : CN -> NP  -> CN     -- old king John
      = ApposCN ; --%

      mkCN : CN -> CN = \cn -> cn ;   -- mkCN (expr "Qid") will also return CNs

      } ; --%


--2 Adjectives and adverbs

--3 AP, adjectival phrases

    mkAP = overload { --%

-- Adjectival phrases can be formed from atomic adjectives by using the positive form or
-- the comparative with a complement

      mkAP : A -> AP           -- warm   --:
      = PositA   ; --%
      mkAP : A -> NP -> AP     -- warmer than Paris --:
      = ComparA  ; --%

-- Relational adjectives can be used with a complement or a reflexive

      mkAP : A2 -> NP -> AP    -- married to her --:
      = ComplA2  ; --%
      mkAP : A2 -> AP          -- married --:
      = UseA2   ; --%

-- Some adjectival phrases can take as complements sentences,
-- questions, or infinitives. Syntactically this is possible for
-- all adjectives.

      mkAP : AP -> S -> AP    -- probable that she sleeps
      =  \ap,s -> SentAP ap (EmbedS s) ; --%
      mkAP : AP -> QS -> AP    -- uncertain if she sleeps
      =  \ap,s -> SentAP ap (EmbedQS s) ; --%
      mkAP : AP -> VP -> AP    -- ready to go
      =  \ap,s -> SentAP ap (EmbedVP ASimul PPos it_Pron s) ; --%
      mkAP : AP -> SC -> AP    -- ready to go --:
      =  \ap,s -> SentAP ap s ; --%

-- An adjectival phrase can be modified by an adadjective.

      mkAP : AdA -> A -> AP   -- very old
      =\x,y -> AdAP x (PositA y) ; --%
      mkAP : AdA -> AP -> AP   -- very very old  --:
      = \ada,ap -> [default: ap | AdAP ada ap] ; --%

-- Conjunction can be formed from two or more adjectival phrases.

      mkAP : Conj -> AP -> AP -> AP -- old and big
      = \c,x,y -> ConjAP c (BaseAP x y) ; --%
      mkAP : Conj -> ListAP -> AP   -- old, big and warm --:
      = \c,xy -> ConjAP c xy ; --%

-- Two more constructions.

      mkAP : CAdv -> AP -> NP -> AP   -- as old as she
      = \adv,ap,np -> CAdvAP PPos adv ap (CompNP np) ; --%
      } ; --%

      reflAP   : A2 -> AP             -- married to himself --:
      = \a -> ReflA2 a ReflPron ; --%
      comparAP : A -> AP              -- warmer
      = UseComparA ; --%

--3 Adv, adverbial phrases

    mkAdv = overload { --%

-- Adverbs can be formed from adjectives.

      mkAdv : A -> Adv            -- warmly   --:
      = PositAdvAdj  ; --%

-- Prepositional phrases are treated as adverbs.

      mkAdv : Prep -> NP -> Adv          -- in the house --:
      = PrepNP       ; --%

-- Subordinate sentences are treated as adverbs.

      mkAdv : Subj -> S -> Adv   -- when she sleeps  --:
      = SubjS ; --%
      
      mkAdv : Time -> Adv
      = time2adv ;

-- Adverbs can be modified by adadjectives.

      mkAdv : AdA -> Adv -> Adv        -- very warmly --:
      = AdAdv   ; --%

-- Conjunction can be formed from two or more adverbial phrases.

      mkAdv : Conj -> Adv -> Adv -> Adv  -- here and now
      = \c,x,y -> ConjAdv c (BaseAdv x y) ; --%
      mkAdv : Conj -> ListAdv -> Adv   -- with John, here and now --:
      = \c,xy -> ConjAdv c xy ; --%
      } ; --%


--2 Questions and relatives

--3 QS, question sentences

    mkQS = overload { --%

-- Just like a sentence $S$ is built from a clause $Cl$,
-- a question sentence $QS$ is built from
-- a question clause $QCl$ by fixing tense, anteriority and polarity.
-- Any of these arguments can be omitted, which results in the
-- default (present, simultaneous, and positive, respectively).

      mkQS : QCl  -> QS  --%
      = UseQCl (TTAnt TPres ASimul) PPos ; --%
      mkQS : Tense -> QCl -> QS  --%
      =  \t -> UseQCl (TTAnt t ASimul) PPos ; --%
      mkQS : Ant -> QCl -> QS  --%
      = \a -> UseQCl (TTAnt TPres a) PPos ; --%
      mkQS : Pol -> QCl -> QS  --%
      = \p -> UseQCl (TTAnt TPres ASimul) p ; --%
      mkQS : Tense -> Ant -> QCl -> QS --%
      = \t,a -> UseQCl (TTAnt t a) PPos ; --%
      mkQS : Tense -> Pol -> QCl -> QS --%
      = \t,p -> UseQCl (TTAnt t ASimul) p ; --%
      mkQS : Ant -> Pol -> QCl -> QS --%
      = \a,p -> UseQCl (TTAnt TPres a) p ; --%
      mkQS : Tense -> Ant -> Pol -> QCl -> QS -- who wouldn't have slept
      = \t,a -> UseQCl (TTAnt t a) ; --%
      mkQS : Temp -> Pol -> QCl -> QS -- who wouldn't have slept  --%
      = UseQCl ; --%

-- Since 'yes-no' question clauses can be built from clauses (see below),
-- we give a shortcut
-- for building a question sentence directly from a clause, using the defaults
-- present, simultaneous, and positive.

      mkQS : Cl -> QS
      = \x -> UseQCl (TTAnt TPres ASimul) PPos (QuestCl x) ; --%
      } ; --%


--3 QCl, question clauses

    mkQCl = overload { --%

-- 'Yes-no' question clauses are built from 'declarative' clauses.

      mkQCl : Cl -> QCl -- does she sleep  --:
      = QuestCl ; --%

-- 'Wh' questions are built from interrogative pronouns in subject
-- or object position. The former uses a verb phrase; we don't give
-- shortcuts for verb-argument sequences as we do for clauses.
-- The latter uses the 'slash' category of objectless clauses
-- (see below); we give the common special case with a two-place verb.

      mkQCl : IP -> VP -> QCl               -- who sleeps  --:
      = QuestVP ; --%
      mkQCl : IP -> V -> QCl                -- who sleeps
      = \s,v -> QuestVP s (UseV v) ; --%
      mkQCl : IP -> V2 -> NP -> QCl         -- who loves her
      = \s,v,o -> QuestVP s (ComplSlash (SlashV2a v) o) ; --%
      mkQCl : IP -> V3 -> NP -> NP -> QCl   -- who sends it to her
      = \s,v,o,i -> QuestVP s (ComplSlash (Slash3V3 v i) o) ; --%
      mkQCl : IP  -> VV -> VP -> QCl        -- who wants to sleep
        = \s,v,vp -> QuestVP s (ComplVV v ASimul PPos vp) ; --%
      mkQCl : IP  -> VS -> S  -> QCl        -- who says that she sleeps
        = \s,v,p -> QuestVP s (ComplVS v p) ; --%
      mkQCl : IP  -> VQ -> QS -> QCl        -- who wonders who sleeps
        = \s,v,q -> QuestVP s (ComplVQ v q) ; --%
      mkQCl : IP  -> VA -> A -> QCl        -- who becomes old
        = \s,v,q -> QuestVP s (ComplVA v (PositA q)) ; --%
      mkQCl : IP  -> VA -> AP -> QCl        -- who becomes old
        = \s,v,q -> QuestVP s (ComplVA v q) ; --%
      mkQCl : IP  -> V2A -> NP -> A -> QCl -- who paints it red
        = \s,v,n,q -> QuestVP s (ComplSlash (SlashV2A v (PositA q)) n) ; --%
      mkQCl : IP  -> V2A -> NP -> AP -> QCl -- who paints it red
        = \s,v,n,q -> QuestVP s (ComplSlash (SlashV2A v q) n) ; --%
      mkQCl : IP  -> V2S -> NP -> S -> QCl          -- who tells her that we sleep
        = \s,v,n,q -> QuestVP s (ComplSlash (SlashV2S v q) n) ; --%
      mkQCl : IP  -> V2Q -> NP -> QS -> QCl         -- who asks her who sleeps
        = \s,v,n,q -> QuestVP s (ComplSlash (SlashV2Q v q) n) ; --%
      mkQCl : IP  -> V2V -> NP -> VP -> QCl         -- who forces her to sleep
        = \s,v,n,q -> QuestVP s (ComplSlash (SlashV2V v ASimul PPos q) n) ; --%
      mkQCl : IP -> A  -> QCl    -- who is old
        = \x,y -> QuestVP x (UseComp (CompAP (PositA y))) ; --%
      mkQCl : IP -> A -> NP -> QCl -- who is older than her
        = \x,y,z -> QuestVP x (UseComp (CompAP (ComparA y z))) ; --%
      mkQCl : IP -> A2 -> NP -> QCl -- who is married to her
	= \x,y,z -> QuestVP x (UseComp (CompAP (ComplA2 y z))) ; --%
      mkQCl : IP -> AP -> QCl    -- who is very old
	= \x,y -> QuestVP x (UseComp (CompAP y)) ; --%
      mkQCl : IP -> NP -> QCl    -- who is the man
        = \x,y -> QuestVP x (UseComp (CompNP y)) ; --%
      mkQCl : IP -> N -> QCl    -- who is a man
        = \x,y -> QuestVP x (UseComp (CompCN (UseN y))) ; --%
      mkQCl : IP -> CN -> QCl    -- who is an old man
	= \x,y -> QuestVP x (UseComp (CompCN y)) ; --%
      mkQCl : IP -> Adv -> QCl   -- who is here
	= \x,y -> QuestVP x (UseComp (CompAdv y)) ; --%
      mkQCl : IP -> NP -> V2 -> QCl        -- who does she love
      = \ip,np,v -> QuestSlash ip (SlashVP np (SlashV2a v)) ; --%
      mkQCl : IP -> ClSlash -> QCl         -- who does she love today   --:
      = QuestSlash   ; --%

-- Adverbial 'wh' questions are built with interrogative adverbials, with the
-- special case of prepositional phrases with interrogative pronouns.

      mkQCl : IAdv -> Cl -> QCl            -- why does she sleep   --:
      = QuestIAdv    ; --%
      mkQCl : Prep -> IP -> Cl -> QCl      -- with whom does she sleep
      = \p,ip -> QuestIAdv (PrepIP p ip)  ; --%

-- An interrogative adverbial can serve as the complement of a copula.

      mkQCl : IAdv -> NP -> QCl   -- where is she
      = \a -> QuestIComp (CompIAdv a)   ; --%

-- Asking about a known subject.

      mkQCl : IComp -> NP -> QCl   -- who is this man  --:
      = \a -> QuestIComp a   ; --%

-- Existentials are a special construction.

      mkQCl : IP -> QCl         -- which cities are there  --:
      = ExistIP ; --%
      } ; --%


--3 IComp, interrogative complements

    mkIComp = overload { --%
      mkIComp : IAdv -> IComp  -- where (is it) --:
      = CompIAdv ; --%
      mkIComp : IP -> IComp    -- who (is it) --:
      = CompIP ; --%
      } ; --%

--3 IP, interrogative pronouns

    mkIP = overload { --%

-- Interrogative pronouns
-- can be formed much like noun phrases, by using interrogative quantifiers.

      mkIP : IDet -> CN -> IP          -- which five big cities  --:
      = IdetCN ; --%
      mkIP : IDet -> N -> IP      -- which five cities
      = \i,n -> IdetCN i (UseN n)  ; --%
      mkIP : IDet -> IP      -- which five --:
      = IdetIP  ; --%
      mkIP : IQuant -> CN -> IP    -- which big city
                     =  \i,n -> IdetCN (IdetQuant i NumSg) n ; --%
      mkIP : IQuant -> Num -> CN -> IP          -- which five big cities
                     =  \i,nu,n -> IdetCN (IdetQuant i nu) n ; --%
      mkIP : IQuant -> N -> IP      -- which city
                     =  \i,n -> IdetCN (IdetQuant i NumSg) (UseN n) ; --%


-- An interrogative pronoun can be modified by an adverb.

      mkIP : IP -> Adv -> IP        -- who in Paris --:
      = AdvIP ; --%
      } ; --%

    what_IP : IP  -- what (singular)
    = whatSg_IP ; --%
    who_IP : IP   -- who (singular)
    = whoSg_IP ; --%

-- More interrogative pronouns and determiners can be found in $Structural$.



--3 IAdv, interrogative adverbs.

-- In addition to the interrogative adverbs defined in the $Structural$ lexicon, they
-- can be formed as prepositional phrases from interrogative pronouns.

    mkIAdv = overload {  --%
      mkIAdv : Prep -> IP -> IAdv --  in which city
      = PrepIP ; --%
      mkIAdv : IAdv -> Adv -> IAdv --  where in Paris
      = AdvIAdv ; --%
      } ; --%

-- More interrogative adverbs are given in $Structural$.

--3 IDet, interrogative determiners
    mkIDet = overload { --%
      mkIDet : IQuant -> Num -> IDet          -- which (songs)
      =  \i,nu -> IdetQuant i nu ; --%
      mkIDet : IQuant -> IDet
      =  \i -> IdetQuant i NumSg ; --%
      } ; --%

    which_IDet : IDet
    = whichSg_IDet ; --%
    whichSg_IDet : IDet  --%
    = IdetQuant which_IQuant NumSg ; --%
    whichPl_IDet : IDet
    = IdetQuant which_IQuant NumPl ; --%




--3 RS, relative sentences

-- Just like a sentence $S$ is built from a clause $Cl$,
-- a relative sentence $RS$ is built from
-- a relative clause $RCl$ by fixing the tense, anteriority and polarity.
-- Any of these arguments
-- can be omitted, which results in the default (present, simultaneous,
-- and positive, respectively).

    mkRS = overload { --%

      mkRS : RCl  -> RS --%
      = UseRCl (TTAnt TPres ASimul) PPos ; --%
      mkRS : Tense -> RCl -> RS --%
      = \t -> UseRCl (TTAnt t ASimul) PPos ; --%
      mkRS : Ant -> RCl -> RS --%
      = \a -> UseRCl (TTAnt TPres a) PPos ; --%
      mkRS : Pol -> RCl -> RS --%
      = \p -> UseRCl (TTAnt TPres ASimul) p ; --%
      mkRS : Tense -> Ant -> RCl -> RS --%
      = \t,a -> UseRCl (TTAnt t a) PPos ; --%
      mkRS : Tense -> Pol -> RCl -> RS --%
      = \t,p -> UseRCl (TTAnt t ASimul) p ; --%
      mkRS : Ant -> Pol -> RCl -> RS --%
      = \a,p -> UseRCl (TTAnt TPres a) p ; --%
      mkRS : Tense -> Ant -> Pol -> RCl -> RS -- that wouldn't have slept
      = \t,a -> UseRCl (TTAnt t a) ; --%
      mkRS : Temp -> Pol -> RCl -> RS -- that wouldn't have slept
      = UseRCl ; --%
      mkRS : Conj -> RS -> RS -> RS -- who sleeps and whose mother runsx
      = \c,x,y -> ConjRS c (BaseRS x y) ; --%
      mkRS : Conj -> ListRS -> RS -- who sleeps, whom I see and who sleeps --:
      = \c,xy -> ConjRS c xy ; --%
      } ; --%

--3 RCl, relative clauses

    mkRCl = overload { --%

-- Relative clauses are built from relative pronouns in subject or object position.
-- The former uses a verb phrase; we don't give
-- shortcuts for verb-argument sequences as we do for clauses.
-- The latter uses the 'slash' category of objectless clauses (see below);
-- we give the common special case with a two-place verb.

      mkRCl : RP -> VP -> RCl        -- that loves she   --:
      = RelVP     ; --%

      mkRCl : RP -> V -> RCl                -- who sleeps
      = \s,v -> RelVP s (UseV v) ; --%
      mkRCl : RP -> V2 -> NP -> RCl         -- who loves her
      = \s,v,o -> RelVP s (ComplSlash (SlashV2a v) o) ; --%
      mkRCl : RP -> V3 -> NP -> NP -> RCl   -- who sends it to her
      = \s,v,o,i -> RelVP s (ComplSlash (Slash3V3 v i) o) ; --%
      mkRCl : RP  -> VV -> VP -> RCl        -- who wants to sleep
        = \s,v,vp -> RelVP s (ComplVV v ASimul PPos vp) ; --%
      mkRCl : RP  -> VS -> S  -> RCl        -- who says that she sleeps
        = \s,v,p -> RelVP s (ComplVS v p) ; --%
      mkRCl : RP  -> VQ -> QS -> RCl        -- who wonders who sleeps
        = \s,v,q -> RelVP s (ComplVQ v q) ; --%
      mkRCl : RP  -> VA -> A -> RCl        -- who becomes old
        = \s,v,q -> RelVP s (ComplVA v (PositA q)) ; --%
      mkRCl : RP  -> VA -> AP -> RCl        -- who becomes old
        = \s,v,q -> RelVP s (ComplVA v q) ; --%
      mkRCl : RP  -> V2A -> NP -> A -> RCl -- who paints it red
        = \s,v,n,q -> RelVP s (ComplSlash (SlashV2A v (PositA q)) n) ; --%
      mkRCl : RP  -> V2A -> NP -> AP -> RCl -- who paints it red
        = \s,v,n,q -> RelVP s (ComplSlash (SlashV2A v q) n) ; --%
      mkRCl : RP  -> V2S -> NP -> S -> RCl          -- who tells her that we sleep
        = \s,v,n,q -> RelVP s (ComplSlash (SlashV2S v q) n) ; --%
      mkRCl : RP  -> V2Q -> NP -> QS -> RCl         -- who asks her who sleeps
        = \s,v,n,q -> RelVP s (ComplSlash (SlashV2Q v q) n) ; --%
      mkRCl : RP  -> V2V -> NP -> VP -> RCl         -- who forces her to sleep
        = \s,v,n,q -> RelVP s (ComplSlash (SlashV2V v ASimul PPos q) n) ; --%
      mkRCl : RP -> A  -> RCl    -- who is old
        = \x,y -> RelVP x (UseComp (CompAP (PositA y))) ; --%
      mkRCl : RP -> A -> NP -> RCl -- who is older than her
        = \x,y,z -> RelVP x (UseComp (CompAP (ComparA y z))) ; --%
      mkRCl : RP -> A2 -> NP -> RCl -- who is married to her
	= \x,y,z -> RelVP x (UseComp (CompAP (ComplA2 y z))) ; --%
      mkRCl : RP -> AP -> RCl    -- who is very old
	= \x,y -> RelVP x (UseComp (CompAP y)) ; --%
      mkRCl : RP -> NP -> RCl    -- who is the man
        = \x,y -> RelVP x (UseComp (CompNP y)) ; --%
      mkRCl : RP -> N -> RCl    -- who is a man
        = \x,y -> RelVP x (UseComp (CompCN (UseN y))) ; --%
      mkRCl : RP -> CN -> RCl    -- who is an old man
	= \x,y -> RelVP x (UseComp (CompCN y)) ; --%
      mkRCl : RP -> Adv -> RCl   -- who is here
	= \x,y -> RelVP x (UseComp (CompAdv y)) ; --%
      mkRCl : RP -> NP -> V2 -> RCl        -- who does she love
      = \ip,np,v -> RelSlash ip (SlashVP np (SlashV2a v)) ; --%
      mkRCl : RP -> ClSlash -> RCl         -- who does she love today   --:
      = RelSlash   ; --%

-- There is a simple 'such that' construction for forming relative
-- clauses from clauses.

      mkRCl : Cl -> RCl              -- such that she loves him
      = RelCl ; --%
      } ; --%

--3 RP, relative pronouns

-- There is an atomic relative pronoun

    which_RP : RP                        -- which/who  --:
      = IdRP ; --%

-- A relative pronoun can be made into a kind of a prepositional phrase.

    mkRP : Prep -> NP -> RP -> RP    -- all the houses in which --:
      = FunRP ; --%


--3 SSlash, objectless sentences

    mkSSlash = overload { --%
      mkSSlash : Temp -> Pol -> ClSlash -> SSlash  --:
      = UseSlash --%
      } ; --%

--3 ClSlash, objectless clauses

    mkClSlash = overload { --%

-- Objectless sentences are used in questions and relative clauses.
-- The most common way of constructing them is by using a two-place verb
-- with a subject but without an object.

      mkClSlash : NP -> VPSlash -> ClSlash        -- (whom) he sees here --:
      = \np,vps -> SlashVP np vps ; --%
      mkClSlash : NP -> V2 -> ClSlash        -- (whom) he sees
      = \np,v2 -> SlashVP np (SlashV2a v2) ; --%

-- The two-place verb can be separated from the subject by a verb-complement verb.

      mkClSlash : NP -> VV -> V2 -> ClSlash  -- (whom) he wants to see
               = \np,vv,v2 -> SlashVP np (SlashVV vv ASimul PPos (SlashV2a v2))  ; --%

-- The missing object can also be the noun phrase in a prepositional phrase.

      mkClSlash : Cl -> Prep -> ClSlash      -- (with whom) he sleeps --:
      = SlashPrep ; --%

-- An objectless sentence can be modified by an adverb.

      mkClSlash : ClSlash -> Adv -> ClSlash    -- (whom) he sees tomorrow  --:
      = AdvSlash ; --%

-- Slash can be transferred to an embedded sentence.

      mkClSlash : NP -> VS -> SSlash -> ClSlash -- (whom) she says that he loves --:
      = SlashVS --%

      } ; --%


--3 VPSlash, verb phrases missing an object

    mkVPSlash = overload { --%

-- This is the deep level of many-argument predication, permitting extraction.

      mkVPSlash : V2  -> VPSlash         -- (whom) (she) loves --:
        = SlashV2a ; --%
      mkVPSlash : V3  -> NP -> VPSlash   -- (whom) (she) gives an apple  --:
        = Slash2V3 ; --%
      mkVPSlash : V2A -> AP -> VPSlash   -- (whom) (she) paints red  --:
        = SlashV2A ; --%
      mkVPSlash : V2Q -> QS -> VPSlash   -- (whom) (she) asks who sleeps  --:
        = SlashV2Q ; --%
      mkVPSlash : V2S -> S  -> VPSlash   -- (whom) (she) tells that we sleep  --:
        = SlashV2S ; --%
      mkVPSlash : V2V -> VP -> VPSlash   -- (whom) (she) forces to sleep  --:
        = \v -> SlashV2V v ASimul PPos ; --%
      mkVPSlash : VV  -> VPSlash -> VPSlash  -- want always to buy --:
        = \v -> SlashVV v ASimul PPos ; --%
      mkVPSlash : V2V -> NP -> VPSlash -> VPSlash -- beg me always to buy --:
        = \v,np -> SlashV2VNP v np ASimul PPos ; --%
      } ; --%


--2 Lists for coordination

-- The rules in this section are very uniform: a list can be built from two or more
-- expressions of the same category.

--3 ListS, sentence lists

  mkListS = overload { --%
   mkListS : S -> S -> ListS  -- list of two --:
   = BaseS ; --%
   mkListS : S -> ListS -> ListS  -- list of more --:
   = ConsS  ; --%
   } ; --%

--3 ListAdv, adverb lists

  mkListAdv = overload {  --%
   mkListAdv : Adv -> Adv -> ListAdv  -- list of two --:
   = BaseAdv ; --%
   mkListAdv : Adv -> ListAdv -> ListAdv  -- list of more --:
   = ConsAdv  ; --%
   } ; --%



--3 ListAP, adjectival phrase lists

  mkListAP = overload {  --%
   mkListAP : AP -> AP -> ListAP  -- list of two --:
   = BaseAP ; --%
   mkListAP : AP -> ListAP -> ListAP  -- list of more --:
   = ConsAP  ; --%
   } ; --%



--3 ListNP, noun phrase lists

  mkListNP = overload {  --%
   mkListNP : NP -> NP -> ListNP  -- list of two --:
   = BaseNP ; --%
   mkListNP : NP -> ListNP -> ListNP  -- list of more --:
   = ConsNP  ; --%
   } ; --%

--3 ListRS, relative clause lists

  mkListRS = overload {  --%
   mkListRS : RS -> RS -> ListRS  -- list of two --:
   = BaseRS ; --%
   mkListRS : RS -> ListRS -> ListRS  -- list of more --:
   = ConsRS  ; --%
   } ; --%

-- numerals from strings

oper
  int2ord : Int -> Ord = \n -> case compareInt n 1000000 of {
    LT => OrdNumeral (int2numeral n) ;
    _  => OrdDigits (int2digits n)
    } ;

  int2card : Int -> Card = \n -> case compareInt n 1000000 of {
    LT => NumNumeral (int2numeral n) ;
    _  => NumDecimal (int2decimal n)
    } ;

}
