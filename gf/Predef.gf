resource Predef = open Parse in {

param Bool = True | False ;
param Ordering = LT | EQ | GT ;

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

oper lang : Str = variants {} ;

oper compareInt : Int -> Int -> Ordering = variants {} ;
oper plusInt : Int -> Int -> Int = variants {} ;
oper minusInt : Int -> Int -> Int = variants {} ;
oper mulInt : Int -> Int -> Int = variants {} ;
oper divInt : Int -> Int -> Int = variants {} ;
oper modInt : Int -> Int -> Int = variants {} ;
oper compareFloat : Float -> Float -> Ordering = variants {} ;
oper plusFloat : Float -> Float -> Float = variants {} ;
oper minusFloat : Float -> Float -> Float = variants {} ;
oper mulFloat : Float -> Float -> Float = variants {} ;
oper divFloat : Float -> Float -> Float = variants {} ;
oper round : Float -> Int -> Float = variants {} ;

}
