concrete ParseExtendFao of ParseExtend =
  ExtendFao - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron],
  NumeralFao - [num], PunctuationX ** open Prelude, ResFao, (P = ParamX) in {

lincat
  CNN = {s : Species => Case => Str ; n : Number ; g : Gender} ;

lin UttAP  p ap  = {s = ap.s ! p.g ! p.n ! Nom} ;
    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;
    gen_Quant = {s = \\_,_,_,_ => [] ; sp = Def} ;

    UttVPS p vps = {s = vps.s ! p.g ! persNum p.n p.p} ;
    ReflA2 a rnp = {
      s = \\g,n,c => a.s ! g ! n ! c ++ a.c2.s ++ rnp.s ! a.c2.c
    } ;
    ReflVPSlash vps rnp = {
      Converb = vps.Converb ++ vps.particle ++ vps.c2.s ++ rnp.s ! vps.c2.c ++ vps.sc ;
      Indicative = \\t,pol,g,p => vps.Indicative ! t ! p ++ vps.particle ++ negStr pol ++ vps.c2.s ++ rnp.s ! vps.c2.c ++ vps.sc ;
      Nonfinite = vps.Nonfinite ++ vps.particle ++ vps.c2.s ++ rnp.s ! vps.c2.c ++ vps.sc ;
      Participle = \\t => vps.Participle ! t ++ vps.particle ++ vps.c2.s ++ rnp.s ! vps.c2.c ++ vps.sc
    } ;
    BaseCNN n1 cn1 n2 cn2 = {
      s = \\sp,c => cn1.s ! sp ! n1.n ! c ++ cn2.s ! sp ! n2.n ! c ;
      n = Pl ;
      g = cn2.g
    } ;
    DetCNN quant conj cnn = mkNP (quant.s ! False ! cnn.g ! cnn.n ! Nom ++ cnn.s ! quant.sp ! Nom) cnn.g cnn.n P3 ;
    ReflPossCNN conj cnn = mkNP (cnn.s ! Def ! Nom) cnn.g cnn.n P3 ;
    PossCNN_RNP quant conj cnn rnp = mkNP (quant.s ! False ! cnn.g ! cnn.n ! Nom ++ cnn.s ! quant.sp ! Nom ++ rnp.s ! Gen) cnn.g cnn.n P3 ;

    GenIP ip = {s = ip.s} ;
    GenRP num cn = {s = cn.s ! Def ! num.n ! Gen} ;
    GenModNP num np cn = mkNP (np.s ! Gen ++ cn.s ! Def ! num.n ! Nom) cn.g num.n P3 ;
    GenModIP num ip cn = {s = ip.s ++ cn.s ! Def ! num.n ! Nom ; n = num.n} ;
    CompBareCN cn = {s = \\_,n => cn.s ! Indef ! n ! Nom} ;

    StrandQuestSlash ip cls = {s = \\t,pol => ip.s ++ cls.s ! t ! pol} ;
    StrandRelSlash rp cls = {s = \\t,pol,_,_ => rp.s ++ cls.s ! t ! pol} ;
    EmptyRelSlash cls = {s = \\t,pol,_,_ => cls.s ! t ! pol} ;

    MkVPS temp pol vp =
      let tense = case temp.t of {P.Pres => Pres ; P.Past => Past ; P.Fut => Pres ; P.Cond => Past} in {
        s = \\g,p => temp.s ++ pol.s ++ vp.Indicative ! tense ! pol.p ! g ! p
      } ;
    ConjVPS conj xs = {s = \\g,p => xs.s1 ! g ! p ++ conj.s ++ xs.s2 ! g ! p} ;
    PredVPS np vps = {s = np.s ! Nom ++ vps.s ! np.g ! persNum np.n np.p} ;
    SQuestVPS np vps = {s = np.s ! Nom ++ vps.s ! np.g ! persNum np.n np.p} ;
    QuestVPS ip vps = {s = ip.s ++ vps.s ! Masc ! persNum ip.n P3} ;
    RelVPS rp vps = {s = \\g,p => rp.s ++ vps.s ! g ! p} ;

    BaseVPS x y = {s1 = x.s ; s2 = y.s} ;
    ConsVPS x xs = {s1 = \\g,p => x.s ! g ! p ++ "," ++ xs.s1 ! g ! p ; s2 = xs.s2} ;

    ExistS temp pol np =
      let tense = case temp.t of {P.Pres => Pres ; P.Past => Past ; P.Fut => Pres ; P.Cond => Past} in {
        s = temp.s ++ "tað" ++ copula ! tense ! persNum np.n P3 ++ negStr pol.p ++ np.s ! Nom
      } ;
    ExistNPQS temp pol np =
      let tense = case temp.t of {P.Pres => Pres ; P.Past => Past ; P.Fut => Pres ; P.Cond => Past} in {
        s = temp.s ++ "er tað" ++ np.s ! Nom
      } ;
    ExistIPQS temp pol ip =
      let tense = case temp.t of {P.Pres => Pres ; P.Past => Past ; P.Fut => Pres ; P.Cond => Past} in {
        s = temp.s ++ ip.s ++ copula ! tense ! persNum ip.n P3 ++ negStr pol.p
      } ;

    MkVPI vp = {s = vp.Nonfinite} ;
    ConjVPI conj xs = {s = xs.s1 ++ conj.s ++ xs.s2} ;
    ComplVPIVV vv vpi = {
      Converb = vv.Converb ++ vv.particle ++ vpi.s ;
      Indicative = \\t,pol,g,p => vv.Indicative ! t ! p ++ vv.particle ++ negStr pol ++ vpi.s ;
      Nonfinite = vv.Nonfinite ++ vv.particle ++ vpi.s ;
      Participle = \\t => vv.Participle ! t ++ vv.particle ++ vpi.s
    } ;
    BaseVPI x y = {s1 = x.s ; s2 = y.s} ;
    ConsVPI x xs = {s1 = x.s ++ "," ++ xs.s1 ; s2 = xs.s2} ;

    MkVPS2 temp pol vps =
      let tense = case temp.t of {P.Pres => Pres ; P.Past => Past ; P.Fut => Pres ; P.Cond => Past} in {
        s = \\g,p => temp.s ++ pol.s ++ vps.Indicative ! tense ! p ++ vps.particle ++ negStr pol.p ++ vps.sc ;
        c2 = vps.c2 ;
        sc = vps.sc
      } ;
    ConjVPS2 conj xs = {s = \\g,p => xs.s1 ! g ! p ++ conj.s ++ xs.s2 ! g ! p ; c2 = xs.c2 ; sc = xs.sc} ;
    ComplVPS2 vps np = {s = \\g,p => vps.s ! g ! p ++ vps.c2.s ++ np.s ! vps.c2.c ++ vps.sc} ;
    ReflVPS2 vps rnp = {s = \\g,p => vps.s ! g ! p ++ vps.c2.s ++ rnp.s ! vps.c2.c ++ vps.sc} ;
    BaseVPS2 x y = {s1 = x.s ; s2 = y.s ; c2 = y.c2 ; sc = y.sc} ;
    ConsVPS2 x xs = {s1 = \\g,p => x.s ! g ! p ++ "," ++ xs.s1 ! g ! p ; s2 = xs.s2 ; c2 = xs.c2 ; sc = xs.sc} ;

    MkVPI2 vps = {s = vps.Nonfinite ++ vps.particle ; c2 = vps.c2 ; sc = vps.sc} ;
    ConjVPI2 conj xs = {s = xs.s1 ++ conj.s ++ xs.s2 ; c2 = xs.c2 ; sc = xs.sc} ;
    ComplVPI2 vpi np = {s = vpi.s ++ vpi.c2.s ++ np.s ! vpi.c2.c ++ vpi.sc} ;
    BaseVPI2 x y = {s1 = x.s ; s2 = y.s ; c2 = y.c2 ; sc = y.sc} ;
    ConsVPI2 x xs = {s1 = x.s ++ "," ++ xs.s1 ; s2 = xs.s2 ; c2 = xs.c2 ; sc = xs.sc} ;

    ConjComp conj xs = {s = \\g,n => xs.s1 ! g ! n ++ conj.s ++ xs.s2 ! g ! n} ;
    BaseComp x y = {s1 = x.s ; s2 = y.s} ;
    ConsComp x xs = {s1 = \\g,n => x.s ! g ! n ++ "," ++ xs.s1 ! g ! n ; s2 = xs.s2} ;
    ConjImp conj xs = {s = \\pol,n => xs.s1 ! pol ! n ++ conj.s ++ xs.s2 ! pol ! n} ;
    BaseImp x y = {s1 = x.s ; s2 = y.s} ;
    ConsImp x xs = {s1 = \\pol,n => x.s ! pol ! n ++ "," ++ xs.s1 ! pol ! n ; s2 = xs.s2} ;

    ProDrop p = p ** {s = \\_ => []} ;
    ICompAP ap = {s = ap.s ! Neuter ! Sg ! Nom} ;
    IAdvAdv adv = {s = adv.s} ;
    CompIQuant iq = {s = iq.s} ;
    PrepCN prep cn = {s = prep.s ++ cn.s ! Indef ! Sg ! prep.c} ;
    FocusObj np ss = {s = np.s ! Acc ++ ss.s} ;
    FocusAdv adv s = {s = adv.s ++ s.s} ;
    FocusAdV adv s = {s = adv.s ++ s.s} ;
    PresPartAP vp = {s = \\_,_,_ => vp.Participle ! Pres} ;
    EmbedPresPart vp = {s = vp.Participle ! Pres} ;
    PastPartAP vps = {s = \\_,_,_ => vps.Participle ! Past ++ vps.particle ++ vps.sc} ;
    PastPartAgentAP vps np = {s = \\_,_,_ => vps.Participle ! Past ++ vps.particle ++ vps.sc ++ "av" ++ np.s ! Dat} ;
    PassVPSlash vps = {
      Converb = "verið" ++ vps.Participle ! Past ++ vps.particle ++ vps.sc ;
      Indicative = \\t,pol,_,p => copula ! t ! p ++ negStr pol ++ vps.Participle ! Past ++ vps.particle ++ vps.sc ;
      Nonfinite = "vera" ++ vps.Participle ! Past ++ vps.particle ++ vps.sc ;
      Participle = \\_ => "verið" ++ vps.Participle ! Past ++ vps.particle ++ vps.sc
    } ;
    PassAgentVPSlash vps np = {
      Converb = "verið" ++ vps.Participle ! Past ++ vps.particle ++ vps.sc ++ "av" ++ np.s ! Dat ;
      Indicative = \\t,pol,_,p => copula ! t ! p ++ negStr pol ++ vps.Participle ! Past ++ vps.particle ++ vps.sc ++ "av" ++ np.s ! Dat ;
      Nonfinite = "vera" ++ vps.Participle ! Past ++ vps.particle ++ vps.sc ++ "av" ++ np.s ! Dat ;
      Participle = \\_ => "verið" ++ vps.Participle ! Past ++ vps.particle ++ vps.sc ++ "av" ++ np.s ! Dat
    } ;
    ProgrVPSlash vps = vps ** {
      Nonfinite = "vera við at" ++ vps.Nonfinite ;
      Participle = \\_ => "verið við at" ++ vps.Nonfinite
    } ;
    ExistsNP np = {
      Converb = "tað finst" ++ np.s ! Nom ;
      Indicative = \\t,pol => "tað" ++ copula ! t ! persNum np.n P3 ++ negStr pol ++ np.s ! Nom ;
      Nonfinite = "vera" ++ np.s ! Nom ;
      Participle = \\_ => "verið" ++ np.s ! Nom
    } ;
    AdvIsNP adv np = {
      Converb = adv.s ++ copula ! Pres ! persNum np.n P3 ++ np.s ! Nom ;
      Indicative = \\t,pol => adv.s ++ copula ! t ! persNum np.n P3 ++ negStr pol ++ np.s ! Nom ;
      Nonfinite = adv.s ++ "vera" ++ np.s ! Nom ;
      Participle = \\_ => adv.s ++ "verið" ++ np.s ! Nom
    } ;

    ComplBareVS vs s = {
      Converb = vs.Converb ++ vs.particle ++ s.s ;
      Indicative = \\t,pol,g,p => vs.Indicative ! t ! p ++ vs.particle ++ negStr pol ++ s.s ;
      Nonfinite = vs.Nonfinite ++ vs.particle ++ s.s ;
      Participle = \\t => vs.Participle ! t ++ vs.particle ++ s.s
    } ;
    SlashBareV2S v s = v ** {c2 = v.c2 ; sc = s.s} ;
    ComplDirectVS vs utt = {
      Converb = vs.Converb ++ vs.particle ++ utt.s ;
      Indicative = \\t,pol,g,p => vs.Indicative ! t ! p ++ vs.particle ++ negStr pol ++ utt.s ;
      Nonfinite = vs.Nonfinite ++ vs.particle ++ utt.s ;
      Participle = \\t => vs.Participle ! t ++ vs.particle ++ utt.s
    } ;
    ComplDirectVQ vq utt = {
      Converb = vq.Converb ++ vq.particle ++ utt.s ;
      Indicative = \\t,pol,g,p => vq.Indicative ! t ! p ++ vq.particle ++ negStr pol ++ utt.s ;
      Nonfinite = vq.Nonfinite ++ vq.particle ++ utt.s ;
      Participle = \\t => vq.Participle ! t ++ vq.particle ++ utt.s
    } ;
    FrontComplDirectVS np vs utt = {
      Converb = utt.s ++ np.s ! Nom ++ vs.Converb ;
      Indicative = \\t,pol => utt.s ++ np.s ! Nom ++ vs.Indicative ! t ! persNum np.n np.p ++ negStr pol ;
      Nonfinite = utt.s ++ np.s ! Nom ++ vs.Nonfinite ;
      Participle = \\t => utt.s ++ np.s ! Nom ++ vs.Participle ! t
    } ;
    FrontComplDirectVQ np vq utt = {
      Converb = utt.s ++ np.s ! Nom ++ vq.Converb ;
      Indicative = \\t,pol => utt.s ++ np.s ! Nom ++ vq.Indicative ! t ! persNum np.n np.p ++ negStr pol ;
      Nonfinite = utt.s ++ np.s ! Nom ++ vq.Nonfinite ;
      Participle = \\t => utt.s ++ np.s ! Nom ++ vq.Participle ! t
    } ;
    PredAPVP ap vp = {
      Converb = "tað er" ++ ap.s ! Neuter ! Sg ! Nom ++ vp.Nonfinite ;
      Indicative = \\t,pol => "tað" ++ copula ! t ! PSg P3 ++ negStr pol ++ ap.s ! Neuter ! Sg ! Nom ++ vp.Nonfinite ;
      Nonfinite = "vera" ++ ap.s ! Neuter ! Sg ! Nom ++ vp.Nonfinite ;
      Participle = \\_ => "verið" ++ ap.s ! Neuter ! Sg ! Nom ++ vp.Nonfinite
    } ;
    AdjAsCN ap = mkCN (ap.s ! Masc ! Sg ! Nom) Masc ;
    AdjAsNP ap = mkNP (ap.s ! Masc ! Sg ! Nom) Masc Sg P3 ;
    PredIAdvVP iadv vp = {s = \\t,pol => iadv.s ++ vp.Nonfinite} ;
    EmbedSSlash ss = {s = ss.s} ;

    ReflPron = mkNP "seg" Masc Sg P3 ;
    ReflPoss num cn = mkNP (cn.s ! Def ! num.n ! Nom) cn.g num.n P3 ;
    PredetRNP pred rnp = rnp ** {s = \\c => pred.s ++ rnp.s ! c} ;
    AdvRNP np prep rnp = rnp ** {s = \\c => rnp.s ! c ++ prep.s ++ np.s ! prep.c} ;
    AdvRVP vp prep rnp = vp ** {
      Converb = vp.Converb ++ prep.s ++ rnp.s ! prep.c ;
      Indicative = \\t,pol,g,p => vp.Indicative ! t ! pol ! g ! p ++ prep.s ++ rnp.s ! prep.c ;
      Nonfinite = vp.Nonfinite ++ prep.s ++ rnp.s ! prep.c ;
      Participle = \\t => vp.Participle ! t ++ prep.s ++ rnp.s ! prep.c
    } ;
    AdvRAP ap prep rnp = {s = \\g,n,c => ap.s ! g ! n ! c ++ prep.s ++ rnp.s ! prep.c} ;
    PossPronRNP pron num cn rnp = mkNP (pron.s ! Gen ++ cn.s ! Def ! num.n ! Nom ++ rnp.s ! Gen) cn.g num.n P3 ;
    ConjRNP conj xs = mkNP (xs.s1 ! Nom ++ conj.s ++ xs.s2 ! Nom) xs.g Pl P3 ;
    Base_rr_RNP x y = {s1 = x.s ; s2 = y.s ; g = x.g ; n = Pl ; p = P3} ;
    Base_nr_RNP x y = {s1 = x.s ; s2 = y.s ; g = x.g ; n = Pl ; p = P3} ;
    Base_rn_RNP x y = {s1 = x.s ; s2 = y.s ; g = x.g ; n = Pl ; p = P3} ;
    Cons_rr_RNP x xs = {s1 = \\c => x.s ! c ++ "," ++ xs.s1 ! c ; s2 = xs.s2 ; g = xs.g ; n = Pl ; p = P3} ;
    Cons_nr_RNP x xs = {s1 = \\c => x.s ! c ++ "," ++ xs.s1 ! c ; s2 = xs.s2 ; g = xs.g ; n = Pl ; p = P3} ;
    ReflPossPron = {s = \\_,_,_,_ => "sítt" ; sp = Def} ;

    CompoundN n1 n2 = mkCN (n1.s ! Indef ! Sg ! Nom ++ n2.s ! Indef ! Sg ! Nom) n2.g ;
    CompoundAP n a = {s = \\g,num,c => n.s ! Indef ! Sg ! Nom ++ a.s ! g ! num ! c} ;
    GerundCN vp = mkCN vp.Nonfinite Neuter ;
    GerundNP vp = mkNP vp.Nonfinite Neuter Sg P3 ;
    GerundAdv vp = {s = vp.Nonfinite} ;
    WithoutVP vp = {s = "uttan at" ++ vp.Nonfinite} ;
    ByVP vp = {s = "við at" ++ vp.Nonfinite} ;
    InOrderToVP ant pol pron vp = {s = "fyri at" ++ negStr pol.p ++ vp.Nonfinite} ;
    ApposNP np app = np ** {s = \\c => np.s ! c ++ "," ++ app.s ! Nom} ;
    AdAdV ada adv = {s = ada.s ++ adv.s} ;
    UttAdV adv = adv ;
    PositAdVAdj a = {s = a.s ! Neuter ! Sg ! Nom} ;
    CompS s = {s = \\_,_ => s.s} ;
    CompQS qs = {s = \\_,_ => qs.s} ;
    CompVP ant pol pron vp = {s = \\_,_ => negStr pol.p ++ vp.Nonfinite} ;
    UttVP ant pol pron vp = {s = negStr pol.p ++ vp.Nonfinite} ;
    UttVPShort vp = {s = vp.Nonfinite} ;
    ComplSlashPartLast vps np = {
      Converb = vps.Converb ++ vps.c2.s ++ np.s ! vps.c2.c ++ vps.particle ++ vps.sc ;
      Indicative = \\t,pol,g,p => vps.Indicative ! t ! p ++ negStr pol ++ vps.c2.s ++ np.s ! vps.c2.c ++ vps.particle ++ vps.sc ;
      Nonfinite = vps.Nonfinite ++ vps.c2.s ++ np.s ! vps.c2.c ++ vps.particle ++ vps.sc ;
      Participle = \\t => vps.Participle ! t ++ vps.c2.s ++ np.s ! vps.c2.c ++ vps.particle ++ vps.sc
    } ;
    UseComp_estar comp = {
      Converb = copula ! Pres ! PPl ++ comp.s ! Masc ! Sg ;
      Indicative = \\t,pol,g,p => copula ! t ! p ++ negStr pol ++ comp.s ! g ! persNumNumber p ;
      Nonfinite = "vera" ++ comp.s ! Masc ! Sg ;
      Participle = \\_ => "verið" ++ comp.s ! Masc ! Sg
    } ;
    UseComp_ser comp = {
      Converb = copula ! Pres ! PPl ++ comp.s ! Masc ! Sg ;
      Indicative = \\t,pol,g,p => copula ! t ! p ++ negStr pol ++ comp.s ! g ! persNumNumber p ;
      Nonfinite = "vera" ++ comp.s ! Masc ! Sg ;
      Participle = \\_ => "verið" ++ comp.s ! Masc ! Sg
    } ;
    SubjRelNP np rs = np ** {s = \\c => np.s ! c ++ rs.s ! np.g ! persNum np.n np.p} ;
    theyNeutr_Pron = mkNP "tey" Neuter Pl P3 ;
    UttAccNP np = {s = np.s ! Acc} ;
    UttDatNP np = {s = np.s ! Dat} ;
    UttAccIP ip = {s = ip.s} ;
    UttDatIP ip = {s = ip.s} ;
    UseDAP dap = mkNP (dap.s ! Neuter ! Nom) Neuter dap.n P3 ;
    UseDAPMasc dap = mkNP (dap.s ! Masc ! Nom) Masc dap.n P3 ;
    UseDAPFem dap = mkNP (dap.s ! Fem ! Nom) Fem dap.n P3 ;
    CardCNCard card cn = {s = \\_,c => card.s ! cn.g ! c ++ cn.s ! Indef ! card.n ! c ; n = Pl} ;
    SubjunctRelCN cn rs = cn ** {s = \\sp,n,c => cn.s ! sp ! n ! c ++ rs.s ! cn.g ! persNum n P3} ;

    NumLess num = num ** {s = \\g,c => num.s ! g ! c ++ "minni"} ;
    NumMore num = num ** {s = \\g,c => num.s ! g ! c ++ "afturat"} ;
    UseACard acard = {s = \\_,_ => acard.s ; n = Pl} ;
    UseAdAACard ada acard = {s = \\_,_ => ada.s ++ acard.s ; n = Pl} ;
    RelNP np rs = np ** {s = \\c => np.s ! c ++ rs.s ! np.g ! persNum np.n np.p} ;
    ExtRelNP np rs = np ** {s = \\c => np.s ! c ++ "," ++ rs.s ! np.g ! persNum np.n np.p} ;
    ExtAdvAP ap adv = {s = \\g,n,c => ap.s ! g ! n ! c ++ "," ++ adv.s} ;
    BareN2 n2 = n2 ;
    ComparAdv pol cadv adv comp = {s = negStr pol.p ++ cadv.s ++ adv.s ++ cadv.p ++ comp.s ! Masc ! Sg} ;
    CAdvAP pol cadv ap comp = {s = \\g,n,c => negStr pol.p ++ cadv.s ++ ap.s ! g ! n ! c ++ cadv.p ++ comp.s ! g ! n} ;
    AdnCAdv pol cadv = {s = negStr pol.p ++ cadv.s} ;
    EnoughAP ap ant pol vp = {s = \\g,n,c => ap.s ! g ! n ! c ++ "nóg" ++ negStr pol.p ++ vp.Nonfinite} ;
    EnoughAdv adv = {s = adv.s ++ "nóg"} ;
    TimeNP np = {s = np.s ! Acc} ;
    AdvAdv a b = {s = a.s ++ b.s} ;
    whatSgFem_IP = {s = "hvat" ; n = Sg} ;
    whatSgNeut_IP = {s = "hvat" ; n = Sg} ;
    that_RP = {s = "sum"} ;
    EmbedVP ant pol pron vp = {s = negStr pol.p ++ vp.Nonfinite} ;
    ComplVV vv ant pol vp = {
      Converb = vv.Converb ++ vv.particle ++ negStr pol.p ++ vp.Nonfinite ;
      Indicative = \\t,p2,g,pn => vv.Indicative ! t ! pn ++ vv.particle ++ negStr p2 ++ negStr pol.p ++ vp.Nonfinite ;
      Nonfinite = vv.Nonfinite ++ vv.particle ++ negStr pol.p ++ vp.Nonfinite ;
      Participle = \\t => vv.Participle ! t ++ vv.particle ++ negStr pol.p ++ vp.Nonfinite
    } ;
    SlashVV vv ant pol vps = vps ** {
      Converb = vv.Converb ++ vv.particle ++ negStr pol.p ++ vps.Nonfinite ;
      Indicative = \\t,pn => vv.Indicative ! t ! pn ++ vv.particle ++ negStr pol.p ++ vps.Nonfinite ;
      Nonfinite = vv.Nonfinite ++ vv.particle ++ negStr pol.p ++ vps.Nonfinite ;
      Participle = \\t => vv.Participle ! t ++ vv.particle ++ negStr pol.p ++ vps.Nonfinite
    } ;
    SlashV2V v ant pol vp = v ** {
      c2 = v.c2 ;
      sc = negStr pol.p ++ vp.Nonfinite
    } ;
    SlashV2VNP v np ant pol vps = v ** {
      c2 = v.c2 ;
      sc = v.c3.s ++ np.s ! v.c3.c ++ negStr pol.p ++ vps.Nonfinite
    } ;
    RecipVPSlash vps = mkVP (vps.Nonfinite ++ "hvør annan") ;
    RecipVPSlashCN vps cn = mkVP (vps.Nonfinite ++ cn.s ! Def ! Sg ! Acc) ;
    FocusComp comp np = {
      Converb = comp.s ! np.g ! np.n ++ copula ! Pres ! persNum np.n np.p ++ np.s ! Nom ;
      Indicative = \\t,pol => comp.s ! np.g ! np.n ++ copula ! t ! persNum np.n np.p ++ negStr pol ++ np.s ! Nom ;
      Nonfinite = comp.s ! np.g ! np.n ++ "vera" ++ np.s ! Nom ;
      Participle = \\_ => comp.s ! np.g ! np.n ++ "verið" ++ np.s ! Nom
    } ;

}
