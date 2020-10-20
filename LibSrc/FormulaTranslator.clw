  MEMBER

  MAP
    MODULE('')
      OutputDebugString(*CSTRING), PASCAL, RAW, NAME('OutputDebugStringA')
    END
  END

  INCLUDE('gcCString.inc'),ONCE
  INCLUDE('FormulaTranslator.inc'),ONCE


! Init


FormulaTranslator.Construct PROCEDURE!, PROTECTED
  CODE
  SELF.Input &= NEW SymbolSTack

  
FormulaTranslator.Destruct  PROCEDURE!, PROTECTED, VIRTUAL
  CODE
  SELF.Destroy()


FormulaTranslator.Destroy   PROCEDURE()!, VIRTUAL
  CODE
  IF NOT SELF.Input &= NULL THEN
    FREE(SELF.Input)
    DISPOSE(SELF.Input)
  END

  
! Properties


FormulaTranslator.Evaluate  PROCEDURE(STRING pFormula)!, REAL
  CODE
  SELF.Parse(pFormula)
  RETURN 3.14159

  
!!! Parses infix notation formula into reverse polish notation with operator precedence. https://en.wikipedia.org/wiki/Shunting-yard_algorithm  
FormulaTranslator.Parse PROCEDURE(STRING pFormula)!, VIRTUAL
  CODE
  SELF.Parse(SELF.Input, pFormula)

!
! Parse End: [ 32 , + , 4 , * , (21 , - , 1, ) , >, =  ]  
  
  
FormulaTranslator.Parse PROCEDURE(SymbolStack pInput, STRING pFormula)!, VIRTUAL, PRIVATE
aSpace                    EQUATE(' ')
aQuote                    EQUATE('''')
aOperator                 EQUATE('&|%^+-/*')
aBracket                  EQUATE('(){{}[]')
aCompare                  EQUATE('~=<<>')
!aAny                      EQUATE(aQuote & aOperator & aBracket & aCompare)
Pos                       LONG(1)
PosEnd                    LONG(0) ! End of current term
State                     ITEMIZE(0), PRE(State)
None                        EQUATE
Space                       EQUATE
Quote                       EQUATE
Operator                    EQUATE
Bracket                     EQUATE
Compare                     EQUATE
Label                       EQUATE
                          END
Stat                      BYTE(State:None)
StatEnd                   BYTE(State:None)


  CODE
  IF pInput &= NULL THEN RETURN END
  IF LEN(pFormula) = 0 THEN RETURN END
  SELF.DebugOutput('Parse Begin: ''' & pFormula & '''')

  ! 32 + 4 * (21 - 1) >= 0
  ! 123456789012345678901234567890
  LOOP WHILE PosEnd < LEN(pFormula)
    PosEnd += 1
    
    IF INSTRING(pFormula[PosEnd], aSpace, 1, 1)
    ELSIF INSTRING(pFormula[PosEnd], aQuote, 1, 1)
      StatEnd = State:Quote
    ELSIF INSTRING(pFormula[PosEnd], aOperator, 1, 1)
      StatEnd = State:Operator
    ELSIF INSTRING(pFormula[PosEnd], aBracket, 1, 1)
      StatEnd = State:Bracket
    ELSIF INSTRING(pFormula[PosEnd], aCompare, 1)
      StatEnd = State:Compare
    ELSE
      StatEnd = State:Label
    END
    !SELF.DebugOutput('  Parse [' & PosEnd & '] ''' & pFormula[PosEnd] & ''' Stat=' & Stat & ' StatEnd=' & StatEnd)
    
    IF Stat <> StatEnd THEN
      IF Pos < PosEnd THEN
        SELF.Push(pInput, CLIP(SUB(pFormula, Pos, PosEnd - Pos)))
        Pos = PosEnd
      END
      Stat = StatEnd
!      SELF.DebugOutput('  Parse [' & PosEnd & '] = ''' & pFormula[PosEnd] & ''' IN Quote ''' & aQuote & '''')
    END
    IF PosEnd = LEN(pFormula) THEN
      IF Pos <= PosEnd THEN
        SELF.Push(pInput, CLIP(SUB(pFormula, Pos, PosEnd - Pos + 1)))
        Pos = PosEnd
      END
      Stat = StatEnd
    END
    
!    IF Pos = PosEnd AND INSTRING(pFormula[PosEnd], aSpace, 1, 1)
!      Pos = PosEnd
!      
!    ELSIF aState <> State:Quote AND INSTRING(pFormula[PosEnd], aQuote, 1, 1) THEN
!      IF Pos < PosEnd THEN
!        SELF.Push(pInput, SUB(pFormula, Pos, PosEnd - Pos))
!        Pos = PosEnd
!      END
!      Pos = PosEnd
!      aState = State:Quote
!      SELF.DebugOutput('  Parse [' & PosEnd & '] = ''' & pFormula[PosEnd] & ''' IN Quote ''' & aQuote & '''')
!    ELSIF aState = State:Quote THEN
!      IF INSTRING(pFormula[PosEnd], aSpace & aQuote, 1, 1)
!        SELF.Push(pInput, SUB(pFormula, Pos, PosEnd - Pos))
!        Pos = PosEnd
!        aState = State:None
!        SELF.DebugOutput('  Parse [' & PosEnd & '] = ''' & pFormula[PosEnd] & ''' NOT IN Quote ''' & aQuote & '''')
!      END
!      
!    ELSIF aState <> State:Operator AND INSTRING(pFormula[PosEnd], aOperator, 1, 1)
!      IF Pos < PosEnd THEN
!        SELF.Push(pInput, SUB(pFormula, Pos, PosEnd - Pos))
!        Pos = PosEnd
!      END
!      aState = State:Operator
!      SELF.DebugOutput('  Parse [' & PosEnd & '] = ''' & pFormula[PosEnd] & ''' IN Operator ''' & aOperator & '''')
!    ELSIF aState = State:Operator THEN
!      IF NOT INSTRING(pFormula[PosEnd], aSpace & aOperator, 1, 1)
!        SELF.Push(pInput, SUB(pFormula, Pos, PosEnd - Pos))
!        Pos = PosEnd
!        aState = State:None
!        SELF.DebugOutput('  Parse [' & PosEnd & '] = ''' & pFormula[PosEnd] & ''' NOT IN Operator ''' & aOperator & '''')
!      END
!      
!    ELSIF aState <> State:Bracket AND INSTRING(pFormula[PosEnd], aBracket, 1, 1)
!      IF Pos < PosEnd THEN
!        SELF.Push(pInput, SUB(pFormula, Pos, PosEnd - Pos))
!        Pos = PosEnd
!      END
!      aState = State:Bracket
!      SELF.DebugOutput('  Parse [' & PosEnd & '] = ''' & pFormula[PosEnd] & ''' IN Bracket ''' & aBracket & '''')
!    ELSIF aState = State:Bracket THEN
!      IF NOT INSTRING(pFormula[PosEnd], aSpace & aBracket, 1, 1)
!        SELF.Push(pInput, SUB(pFormula, Pos, PosEnd - Pos))
!        Pos = PosEnd
!        aState = State:None
!        SELF.DebugOutput('  Parse [' & PosEnd & '] = ''' & pFormula[PosEnd] & ''' NOT IN Bracket ''' & aBracket & '''')
!      END
!
!    ELSIF aState <> State:Compare AND INSTRING(pFormula[PosEnd], aCompare, 1, 1)
!      IF Pos < PosEnd THEN
!        SELF.Push(pInput, SUB(pFormula, Pos, PosEnd - Pos))
!        Pos = PosEnd
!      END
!      aState = State:Compare
!      SELF.DebugOutput('  Parse [' & PosEnd & '] = ''' & pFormula[PosEnd] & ''' IN Compare ''' & aCompare & '''')
!    ELSIF aState = State:Compare THEN
!      IF NOT INSTRING(pFormula[PosEnd], aSpace & aCompare, 1, 1)
!        SELF.Push(pInput, SUB(pFormula, Pos, PosEnd - Pos))
!        Pos = PosEnd
!        aState = State:None
!        SELF.DebugOutput('  Parse [' & PosEnd & '] = ''' & pFormula[PosEnd] & ''' NOT IN Compare ''' & aCompare & '''')
!      END
!
!    ELSIF aState = State:Label THEN
!      IF INSTRING(pFormula[PosEnd], aAny, 1, 1)
!        SELF.Push(pInput, SUB(pFormula, Pos, PosEnd - Pos))
!        Pos = PosEnd
!        aState = State:None
!        SELF.DebugOutput('  Parse [' & PosEnd & '] = ''' & pFormula[PosEnd] & ''' NOT IN Compare ''' & aCompare & '''')
!      END
!      
!    ELSE
!      IF Pos < PosEnd THEN
!        SELF.Push(pInput, SUB(pFormula, Pos, PosEnd - Pos))
!        Pos = PosEnd
!      END
!      aState = State:Label
!      SELF.DebugOutput('  Parse [' & PosEnd & '] = ''' & pFormula[PosEnd] & ''' IN ELSE')
!      
!!    ELSIF aState <> State:Token AND INSTRING(pFormula[PosEnd], aToken, 1, 1)
!!      IF Pos < PosEnd THEN
!!        SELF.Push(pInput, SUB(pFormula, Pos, PosEnd - Pos))
!!        Pos = PosEnd
!!      END
!!      aState = State:Quote
!!    ELSIF aState = State:Token THEN
!!      IF NOT INSTRING(pFormula[PosEnd], aToken + aSpace, 1, 1)
!!        SELF.Push(pInput, SUB(pFormula, Pos, PosEnd - Pos))
!!        Pos = PosEnd
!!        aState = State:None
!!      END
!      
!    END
    
  END ! LOOP PosEnd
  
!  LOOP WHILE Pos < LEN(pFormula)
!    Pos += 1
!    
!    LOOP WHILE Pos < LEN(pFormula) AND pFormula[Pos] = ' ' ! Skip whitespace
!      Pos += 1
!    END
!    PosEnd = Pos
!    
!    LOOP WHILE PosEnd < LEN(pFormula)
!      PosEnd += 1
!      
!      IF INSTRING(pFormula[PosEnd], aQuote, 1, 1) THEN
!        InsideQuote = True
!      IF NOT InsideToken AND INSTRING(pFormula[PosEnd], atoken, 1, 1) THEN
!        InsideToken = True
!      ELSIF InsideQuote THEN
!        IF INSTRING(pFormula[PosEnd], aQuote, 1, 1) THEN 
!          InsideQuote = False
!          SELF.Push(pInput, SUB(pFormula, Pos, PosEnd - Pos))
!          Pos = PosEnd
!          BREAK
!        END
!      ELSIF InsideToken THEN
!        IF INSTRING(pFormula[PosEnd], aQuote, 1, 1) THEN 
!          InsideQuote = False
!          SELF.Push(pInput, SUB(pFormula, Pos, PosEnd - Pos))
!          Pos = PosEnd
!          BREAK
!        END
!      ELSE ! NOT InsideStr
!        IF INSTRING(pFormula[PosEnd], aToken, 1, 1) THEN
!          SELF.Push(pInput, SUB(pFormula, Pos, PosEnd - Pos))
!          Pos = PosEnd
!          BREAK
!        ELSIF PosEnd = LEN(pFormula) THEN
!          SELF.Push(pInput, SUB(pFormula, Pos, PosEnd - Pos))
!          Pos = PosEnd
!          BREAK
!        END
!      END
!      
!    END ! LOOP PosEnd
!    
!  END ! LOOP Pos

  SELF.DebugOutput('Parse End: ' & SELF.ToString())


!! BUG: did not split '1)'
!!  Parse End: [ 3, +, 4, ×, (, 2, -, 1), >=, 0 ]  
!FormulaTranslator.Parse PROCEDURE(SymbolStack pInput, STRING pFormula)!, VIRTUAL, PRIVATE
!Pos                       LONG
!PosEnd                    LONG ! End of current term
!PosBegin                  LONG ! Begin of next term
!
!  CODE
!  IF pInput &= NULL THEN RETURN END
!  IF LEN(pFormula) = 0 THEN RETURN END
!  SELF.DebugOutput(ALL(' ', RECORDS(pInput) * 2) & 'Parse Begin: ''' & pFormula & '''')
!  
!  LOOP Pos = 1 TO LEN(pFormula)
!    IF pFormula[Pos] = ' ' THEN ! Term seperated by space
!      PosEnd   = pos - 1
!      LOOP WHILE pos < LEN(pFormula) AND pFormula[Pos] = ' ' ! Skip Spaces
!        pos += 1
!      END
!      PosBegin = Pos
!      SELF.DebugOutput(ALL(' ', RECORDS(pInput) * 2) & '  Space: PosEnd=' & PosEnd & ', PosBegin=' & posBegin)
!      BREAK
!    ELSIF INSTRING(pFormula[Pos], '=<>!') > 0 THEN ! Compare character
!      LOOP WHILE pos < LEN(pFormula) AND INSTRING(pFormula[Pos], '=<>!') > 0
!        pos += 1
!      END
!      PosEnd   = pos - 1
!      LOOP WHILE pos < LEN(pFormula) AND pFormula[Pos] = ' ' ! Skip Spaces
!        pos += 1
!      END
!      PosBegin = Pos
!      SELF.DebugOutput(ALL(' ', RECORDS(pInput) * 2) & '  Compare: PosEnd=' & PosEnd & ', PosBegin=' & posBegin)
!      BREAK
!    ELSIF INSTRING(pFormula[Pos], '+-*/()') > 0 THEN ! Formula character
!      LOOP WHILE pos < LEN(pFormula) AND INSTRING(pFormula[Pos], '+-*/()') > 0
!        pos += 1
!      END
!      PosEnd   = pos - 1
!      LOOP WHILE pos < LEN(pFormula) AND pFormula[Pos] = ' ' ! Skip Spaces
!        pos += 1
!      END
!      PosBegin = Pos
!      SELF.DebugOutput(ALL(' ', RECORDS(pInput) * 2) & '  Formula: PosEnd=' & PosEnd & ', PosBegin=' & posBegin)
!      BREAK
!    END ! IF
!  END ! LOOP Pos
!  IF PosEnd = 0 THEN ! No more terms found then go until end
!    PosEnd = LEN(pFormula)
!    PosBegin = 0
!  END
!  IF PosEnd > 0 THEN ! Found a term ?
!    SELF.Push(pInput, SUB(pFormula, 1, PosEnd))
!  END
!  IF PosBegin > 0 AND PosBegin <= LEN(pFormula) THEN ! Found more terms ?
!    SELF.Parse(pInput, SUB(pFormula, PosBegin, LEN(pFormula) - PosBegin + 1))
!  END
!  SELF.DebugOutput(ALL(' ', RECORDS(pInput) * 2) & 'Parse End: ' & SELF.ToString())


FormulaTranslator.Push  PROCEDURE(SymbolStack pStack, STRING pSymbol)!, VIRTUAL
  CODE
  IF pStack &= NULL THEN RETURN END
  IF LEN(pSymbol) = 0 THEN RETURN END
  SELF.DebugOutput('Push: ''' & pSymbol & '''')
  pStack.Symbol = pSymbol
  ADD(pStack, 1)
  
  
FormulaTranslator.Pop   PROCEDURE(SymbolStack pStack)!, STRING, VIRTUAL
aResult                   CSTRING(SymbolSize)
  CODE
  IF pStack &= NULL THEN RETURN '' END
  IF RECORDS(pStack) = 0 THEN RETURN '' END
  GET(pStack, 1)
  aResult = pStack.Symbol
  DELETE(pStack)
  SELF.DebugOutput('Pop ' & aResult)
  RETURN aResult
  
  
FormulaTranslator.ToString  PROCEDURE()!, STRING, VIRTUAL
I                             LONG
X                             gcCString
  CODE
  IF SELF.Input &= NULL THEN RETURN '' END
  IF RECORDS(SELF.Input) = 0 THEN RETURN '' END
  X.Init(1024 * 1024)
  LOOP I = 1 TO RECORDS(SELF.Input)
    GET(SELF.Input, I)
    IF I > 1 THEN
      X.Value = ', ' & X.Value
    END
    X.Value = SELF.Input.Symbol & X.Value
  END
  X.Value = '[ ' & X.Value & ' ]'
  RETURN X.Value

  
! Debug

        
FormulaTranslator.DebugOutput   PROCEDURE(STRING pMessage)!, VIRTUAL
Msg CSTRING(2048)
  CODE
  Msg = 'FormulaTranslator: ' & pMessage
  OutputDebugString(Msg)


