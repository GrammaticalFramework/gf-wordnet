resource Predef = open Parse in {

param Bool = True | False ;

oper Int    : Type = variants {} ;          -- the type of integers
oper Float  : Type = variants {} ;          -- the type of floats
oper Time   : Type = variants {} ;          -- the type of Wikidata time
oper Markup : Type = variants {} ;          -- the type of Wikidata time

oper entity : ({a} : Type) -> Str -> a = variants {} ;
oper expr   : ({a} : Type) -> Str -> a = variants {} ;

oper int2digits  : Int -> Digits = variants {} ;
oper int2decimal : Int -> Decimal = variants {} ;
oper float2decimal : Float -> Decimal = variants {} ;
oper int2numeral : Int -> Numeral = variants {} ;
oper time2adv : Time -> Adv = variants {} ;

oper lessInt : Int -> Int -> Bool = variants {} ;

}
