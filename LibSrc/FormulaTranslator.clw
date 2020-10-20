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

  
FormulaTranslator.Parse PROCEDURE(SymbolStack pInput, STRING pFormula)!, VIRTUAL, PRIVATE
aSpace                    EQUATE(' ')
aQuote                    EQUATE('''')
aOperator                 EQUATE('&|%^+-/*')
aBracket                  EQUATE('(){{}[]')
aCompare                  EQUATE('~=<<>')
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
    
  END ! LOOP PosEnd
  SELF.DebugOutput('Parse End: ' & SELF.ToString())


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


