resource RGL = open Parse in {

oper mkCN = overload { --%

-- The simplest way of forming common noun phrases is from atomic nouns $N$.

      mkCN : N  -> CN            -- house  --:
      = UseN     ; --%

     } ;

}
