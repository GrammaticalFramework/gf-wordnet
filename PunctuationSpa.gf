concrete PunctuationSpa of Punctuation = {

lincat Mark = {s1,s2 : Str} ;
lindef Mark = \s -> {s1=""; s2=s} ;
linref Mark = \m -> m.s1++m.s2 ;

lin FullStop  = {s1="";  s2="."} ;
    ExclMark  = {s1="¡"; s2="!"} ;
    QuestMark = {s1="¿"; s2="?"} ;
    Ellipsis  = {s1="";  s2 = "..."} ;

lincat Bracket = {s1,s2 : Str} ;
lindef Bracket = \s -> {s1=s; s2=s} ;
linref Bracket = \b -> b.s1++b.s2 ;

lin CurlyBracket  = {s1="{"; s2="}"} ;
    SquareBracket = {s1="["; s2="]"} ;
    RoundBracket  = {s1="("; s2=")"} ;

lincat Quote = {s : Str} ;
lin DoubleQuote = {s="\""} ;
    SingleQuote = {s="'"} ;

}
