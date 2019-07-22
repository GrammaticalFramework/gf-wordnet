concrete PunctuationX of Punctuation = {

lincat Mark = {s : Str} ;
lin FullStop  = {s = "."} ;
    ExclMark  = {s = "!"} ;
    QuestMark = {s = "?"} ;
    Ellipsis  = {s = "..."} ;

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
