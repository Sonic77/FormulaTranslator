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
  SELF.Input  &= NEW SymbolStack
  SELF.Output &= NEW SymbolStack

  
FormulaTranslator.Destruct  PROCEDURE!, PROTECTED, VIRTUAL
  CODE
  SELF.Destroy()


FormulaTranslator.Destroy   PROCEDURE()!, VIRTUAL
  CODE
  IF NOT SELF.Input &= NULL THEN
    FREE(SELF.Input)
    DISPOSE(SELF.Input)
  END
  IF NOT SELF.Output &= NULL THEN
    FREE(SELF.Output)
    DISPOSE(SELF.Output)
  END


FormulaTranslator.Reset PROCEDURE()!, VIRTUAL
  CODE
  IF NOT SELF.Input &= NULL THEN
    FREE(SELF.Input)
  END
  IF NOT SELF.Output &= NULL THEN
    FREE(SELF.Output)
  END

  
! Properties


FormulaTranslator.Evaluate  PROCEDURE(STRING pFormula)!, STRING
  CODE
  SELF.Parse(pFormula)
  SELF.InfixToPostFix()
  RETURN 3.14159

  
!!! Parses infix notation formula into reverse polish notation with operator precedence. https://en.wikipedia.org/wiki/Shunting-yard_algorithm  
FormulaTranslator.Parse PROCEDURE(STRING pFormula)!, VIRTUAL
  CODE
  SELF.Parse(SELF.Input, pFormula)


FormulaTranslator.Parse PROCEDURE(*SymbolStack pInput, STRING pFormula)!, VIRTUAL, PRIVATE
aSpace                    EQUATE(' ')
aQuote                    EQUATE('''')
aOperator                 EQUATE('&|%^+-/*')
aBracket                  EQUATE('(){{}[]')
aCompare                  EQUATE('~=<<>')
aSeperator                EQUATE(',;')
Pos                       LONG(1)
PosEnd                    LONG(0) ! End of current term
State                     ITEMIZE(0), PRE(State)
None                        EQUATE
Space                       EQUATE
Quote                       EQUATE
QuoteEnd                    EQUATE
Operator                    EQUATE
Bracket                     EQUATE
Compare                     EQUATE
Label                       EQUATE
Seperator                   EQUATE
                          END
Stat                      BYTE(State:None)
StatEnd                   BYTE(State:None)

  CODE
  IF pInput &= NULL THEN RETURN END
  IF LEN(pFormula) = 0 THEN RETURN END
  SELF.DebugOutput('Parse Begin: ''' & pFormula & '''')

  LOOP WHILE PosEnd < LEN(pFormula)
    PosEnd += 1

    IF StatEnd = State:Quote THEN
      ! No state change
    ELSIF INSTRING(pFormula[PosEnd], aQuote    , 1, 1) THEN
      IF StatEnd <> State:Quote THEN
        StatEnd = State:Quote
      ELSE
        StatEnd = State:QuoteEnd
      END
    ELSIF INSTRING(pFormula[PosEnd], aSpace    , 1, 1) THEN
      StatEnd = State:Space
    ELSIF INSTRING(pFormula[PosEnd], aOperator , 1, 1) THEN
      StatEnd = State:Operator
    ELSIF INSTRING(pFormula[PosEnd], aBracket  , 1, 1) THEN
      StatEnd = State:Bracket
    ELSIF INSTRING(pFormula[PosEnd], aCompare  , 1, 1) THEN
      StatEnd = State:Compare
    ELSIF INSTRING(pFormula[PosEnd], aSeperator, 1, 1) THEN
      StatEnd = State:Seperator
    ELSE
      StatEnd = State:Label
    END
    !SELF.DebugOutput('  Parse [' & PosEnd & '] ''' & pFormula[PosEnd] & ''' Stat=' & Stat & ' StatEnd=' & StatEnd)
    
    IF Stat <> StatEnd AND StatEnd <> State:QuoteEnd THEN
      IF Pos < PosEnd THEN
        SELF.PushSymbol(pInput, CLIP(SUB(pFormula, Pos, PosEnd - Pos)), SymbolType:Unknown, StackDirection:FIFO)
        Pos = PosEnd
      END
      Stat = StatEnd
!      SELF.DebugOutput('  Parse [' & PosEnd & '] = ''' & pFormula[PosEnd] & ''' IN Quote ''' & aQuote & '''')
    END
    IF PosEnd = LEN(pFormula) THEN
      IF Pos <= PosEnd THEN
        SELF.PushSymbol(pInput, CLIP(SUB(pFormula, Pos, PosEnd - Pos + 1)), SymbolType:Unknown, StackDirection:FIFO)
        Pos = PosEnd
      END
      Stat = StatEnd
    END
    !SELF.DebugOutput('Parse Loop: ' & SELF.ToString())
    
  END ! LOOP PosEnd
  SELF.CheckInFix(pInput)
  SELF.DebugOutput('Parse End: ' & SELF.ToString())


!!! Converts infix symbol stack to postfix (RPN/Reverse Polish Notation) symbol stack
FormulaTranslator.InFixToPostFix    PROCEDURE() !, VIRTUAL
  CODE
  IF NOT SELF.Output &= NULL THEN
    FREE(SELF.Output)
    DISPOSE(SELF.Output)
  END
  SELF.Output &= SELF.InFixToPostFix(SELF.Input)

  
FormulaTranslator.InFixToPostFix    PROCEDURE(*SymbolStack pInput) !, *SymbolStack, VIRTUAL, PRIVATE
aOperator                             EQUATE('&|%^+-/*')
aOperatorLevel                        EQUATE('44231122') ! 1=+-, 2=%/*, 3=^, 4=&|
aBracket                              EQUATE('(){{}[]')
aCompare                              EQUATE('~=<<>')
aSeperator                            EQUATE(',;')
aToken                                EQUATE(aOperator & aBracket & aCompare & aSeperator)
lOperator                             &SymbolStack ! Operator Stack
lOutput                               &SymbolStack ! Operator Stack
aSymbol                               LIKE(SymbolStackType)

  CODE ! https://en.wikipedia.org/wiki/Shunting-yard_algorithm
  IF pInput &= NULL THEN RETURN NULL END
  IF RECORDS(pInput) = 0 THEN RETURN NULL END
  !SELF.DebugOutput('InFixToPostFix Begin: ' & SELF.ToStringStack(pInput))
  
  lOperator &= NEW SymbolStack
  lOutput   &= NEW SymbolStack
  
  LOOP WHILE RECORDS(pInput) > 0
    SELF.PopSymbol(pInput, aSymbol, StackDirection:FIFO)
    IF LEN(aSymbol) = 0 THEN CYCLE END
    
    IF BAND(aSymbol.SymbolType, SymbolType:Value) <> 0 THEN ! Value
      SELF.PushSymbol(lOutput, aSymbol, StackDirection:LILO) ! Push Output
    ELSIF BAND(aSymbol.SymbolType, SymbolType:Label) <> 0 THEN ! Label
      SELF.PushSymbol(lOutput, aSymbol, StackDirection:LILO) ! Push Output
    ELSIF BAND(aSymbol.SymbolType, SymbolType:Functie) <> 0 THEN ! Function
      SELF.PushSymbol(lOperator, aSymbol, StackDirection:LILO) ! Push Operator
    ELSIF BAND(aSymbol.SymbolType, SymbolType:Operator) <> 0 THEN ! Operator
      SELF.PushSymbol(lOperator, aSymbol, StackDirection:LILO) ! Push Operator
      ! .... 
    ELSIF BAND(aSymbol.SymbolType, SymbolType:BracketOpen) <> 0 THEN ! Bracket Open
      SELF.PushSymbol(lOperator, aSymbol, StackDirection:LILO) ! Push Operator
    ELSIF BAND(aSymbol.SymbolType, SymbolType:BracketClose) <> 0 THEN ! Bracket Close
      SELF.PushSymbol(lOperator, aSymbol, StackDirection:LILO) ! Push Operator
      ! .... 
    ELSE
      ! Error unsupported symbol
    END
    ! If operators left move to output, if bracket then error

    !SELF.DebugOutput('InFixToPostFix Loop: ' & SELF.ToStringStack(pInput) & ' -> ' & SELF.ToStringStack(lOperator) & ' -> ' & SELF.ToStringStack(lOutput))
  END

  SELF.DebugOutput('InFixToPostFix End: ' & SELF.ToStringStack(pInput) & ' -> ' & SELF.ToStringStack(lOperator) & ' -> ' & SELF.ToStringStack(lOutput))
  DISPOSE(lOperator)
  RETURN lOutput


FormulaTranslator.CheckInfix    PROCEDURE(*SymbolStack pStack)!, VIRTUAL, PRIVATE
lNextSymbol                       LIKE(SymbolStack)
lPreviousSymbol                   LIKE(SymbolStack)
I                                 LONG
  CODE
  IF pStack &= NULL THEN RETURN END
  IF RECORDS(pStack) = 0 THEN RETURN END
  !SELF.DebugOutput('CheckInfix Begin: ' & SELF.ToStringStack(pStack, True))

  ! Check operators
  CLEAR(lPreviousSymbol)
  LOOP I = 1 TO RECORDS(pStack)
    GET(pStack, I)
    IF pStack.SymbolType <> SymbolType:Unknown THEN CYCLE END

    CASE UPPER(pStack.Symbol)
    OF '(' ! Parenthices
      pStack.SymbolType     = SymbolType:BracketOpen
      pStack.SymbolPriority = 1
    OF ')' ! Parenthices
      pStack.SymbolType     = SymbolType:BracketClose
      pStack.SymbolPriority = 1
    OF '^' OROF 'pow' ! Power
      pStack.SymbolType     = SymbolType:OperatorPower
      pStack.SymbolPriority = 4
    OF 'rt' OROF 'root' ! Root
      pStack.SymbolType     = SymbolType:OperatorRoot
      pStack.SymbolPriority = 4
    OF '*' OROF 'mul' ! Multiply
      pStack.SymbolType     = SymbolType:OperatorMultiply
      pStack.SymbolPriority = 5
    OF '/' OROF 'div' ! Divide
      pStack.SymbolType     = SymbolType:OperatorDivide
      pStack.SymbolPriority = 5
    OF '%' OROF 'mod'! Modulo
      pStack.SymbolType     = SymbolType:OperatorModulo
      pStack.SymbolPriority = 5
    OF '-' ! Sign / Minus
      IF lPreviousSymbol.SymbolType <> SymbolType:Unknown THEN ! Preceeded by an operator? then we can not subsctract anything
        pStack.SymbolType     = SymbolType:OperatorSign
        pStack.SymbolPriority = 2
      ELSE
        pStack.SymbolType     = SymbolType:OperatorSubstract
        pStack.SymbolPriority = 6
      END
    OF '+' ! Plus
      pStack.SymbolType     = SymbolType:OperatorAdd
      pStack.SymbolPriority = 6
    OF '&' ! Concat
      pStack.SymbolType     = SymbolType:Concatenate
      pStack.SymbolPriority = 7
    OF '=' OROF '==' 
      pStack.SymbolType     = SymbolType:CompareEqual
      pStack.SymbolPriority = 8
    OF '<>' OROF '!=' OROF '~='
      pStack.SymbolType     = SymbolType:CompareNot   + SymbolType:CompareEqual
      pStack.SymbolPriority = 8
    OF '<' 
      pStack.SymbolType     = SymbolType:CompareLess
      pStack.SymbolPriority = 8
    OF '<=' OROF '=<' 
      pStack.SymbolType     = SymbolType:CompareEqual + SymbolType:CompareLess
      pStack.SymbolPriority = 8
    OROF '~<' 
      pStack.SymbolType     = SymbolType:CompareNot   + SymbolType:CompareLess
      pStack.SymbolPriority = 8
    OF '>' 
      pStack.SymbolType     = SymbolType:CompareGreater
      pStack.SymbolPriority = 8
    OF '>=' OROF '=>' 
      pStack.SymbolType     = SymbolType:CompareEqual + SymbolType:CompareGreater
      pStack.SymbolPriority = 8
    OROF '~>' 
      pStack.SymbolType     = SymbolType:CompareNot   + SymbolType:CompareGreater
      pStack.SymbolPriority = 8
    OF 'AND' 
      pStack.SymbolType     = SymbolType:BinaryAnd
      pStack.SymbolPriority = 9
    OF 'OR' 
      pStack.SymbolType     = SymbolType:BinaryOr
      pStack.SymbolPriority = 9
    OF 'XOR' 
      pStack.SymbolType     = SymbolType:BinaryOr     + SymbolType:BinaryExclusive
      pStack.SymbolPriority = 9
    OF 'NAND' 
      pStack.SymbolType     = SymbolType:BinaryNot    + SymbolType:BinaryAnd
      pStack.SymbolPriority = 9
    OF 'NOR' 
      pStack.SymbolType     = SymbolType:BinaryNot    + SymbolType:BinaryOr
      pStack.SymbolPriority = 9
    OF 'NOT' OROF '~' OROF '!' ! Boolean
      pStack.SymbolType     = SymbolType:BinaryNot
      pStack.SymbolPriority = 9
    END ! CASE

    PUT(pStack)
    lPreviousSymbol = pStack
  END ! LOOP

  ! Check keywords
  LOOP I = 1 TO RECORDS(pStack)
    IF I < RECORDS(pStack) THEN
      GET(pStack, I + 1)
      lNextSymbol = pStack
    ELSE
      CLEAR(lNextSymbol)
    END
    GET(pStack, I)
    IF pStack.SymbolType <> SymbolType:Unknown THEN CYCLE END

    CASE UPPER(pStack.Symbol)
    OF 'NULL' ! Null
      pStack.SymbolType     = SymbolType:ObjectNull
      pStack.SymbolPriority = 0
    OF 'LAST' ! Function
      IF lNextSymbol.SymbolType = SymbolType:BracketOpen THEN
        pStack.SymbolType     = SymbolType:Functie
        pStack.SymbolPriority = 0
      END
    OF 'POSITION' ! Function
      IF lNextSymbol.SymbolType = SymbolType:BracketOpen THEN
        pStack.SymbolType     = SymbolType:Functie
        pStack.SymbolPriority = 0
      END
    END ! CASE

    PUT(pStack)
    lPreviousSymbol = pStack
  END ! LOOP

  ! Check symbols
  LOOP I = 1 TO RECORDS(pStack)
    IF I < RECORDS(pStack) THEN
      GET(pStack, I + 1)
      lNextSymbol = pStack
    ELSE
      CLEAR(lNextSymbol)
    END
    GET(pStack, I)
    IF pStack.SymbolType <> SymbolType:Unknown THEN CYCLE END

    IF SUB(pStack.Symbol, 1, 1) = '''' AND SUB(pStack.Symbol, -1, 1) = '''' THEN ! String
      pStack.SymbolType     = SymbolType:Text
      pStack.SymbolPriority = 0
    ELSIF SUB(pStack.Symbol, 1, 1) = '@' THEN ! Attribute
      pStack.SymbolType     = SymbolType:Attribute
      pStack.SymbolPriority = 0
    ELSIF NUMERIC(pStack.Symbol) THEN ! Numeric
      pStack.SymbolType     = SymbolType:Number
      pStack.SymbolPriority = 0
    ELSIF INSTRING(UPPER(SUB(pStack.Symbol, 1, 1)), '_ABCDEFGHIJKLMNOPQRSTUVWXYZ', 1, 1) THEN ! Variable
      IF lNextSymbol.SymbolType = SymbolType:BracketOpen THEN
        pStack.SymbolType     = SymbolType:Functie
        pStack.SymbolPriority = 0
      ELSE
        pStack.SymbolType     = SymbolType:Variable
        pStack.SymbolPriority = 0
      END
    END ! CASE

    PUT(pStack)
    lPreviousSymbol = pStack
  END ! LOOP

  SELF.DebugOutput('CheckInfix End: ' & SELF.ToStringStack(pStack, True))


FormulaTranslator.PushSymbol    PROCEDURE(*SymbolStack pStack, STRING pSymbol, BYTE pStackDirection = StackDirection:FIFO)!, VIRTUAL
aSymbol                           LIKE(SymbolStackType)
  CODE
  IF pStack &= NULL THEN RETURN END
  IF LEN(pSymbol) = 0 THEN RETURN END
  !SELF.DebugOutput('Push: ''' & pSymbol & '''')
  aSymbol.Symbol     = pSymbol
  SELF.PushSymbol(pStack, aSymbol, pStackDirection)


FormulaTranslator.PushSymbol    PROCEDURE(*SymbolStack pStack, STRING pSymbol, BYTE pSymbolType, BYTE pStackDirection = StackDirection:FIFO)!, VIRTUAL
aSymbol                           LIKE(SymbolStackType)
  CODE
  IF pStack &= NULL THEN RETURN END
  IF LEN(pSymbol) = 0 THEN RETURN END
  !SELF.DebugOutput('Push: ''' & pSymbol & ''', t:' & pSymbolType)
  aSymbol.Symbol     = pSymbol
  aSymbol.SymbolType = pSymbolType
  SELF.PushSymbol(pStack, aSymbol, pStackDirection)


FormulaTranslator.PushSymbol    PROCEDURE(*SymbolStack pStack, *SymbolStackType pSymbol, BYTE pStackDirection = StackDirection:FIFO)!, VIRTUAL
  CODE
  IF pStack &= NULL THEN RETURN END
  !IF pSymbol &= NULL THEN RETURN END ! Illegal reference assignment or equivalence
  IF LEN(pSymbol.Symbol) = 0 THEN RETURN END
  !SELF.DebugOutput('Push ' & SELF.ToStringSymbol(pSymbol))
  pStack = pSymbol
  CASE pStackDirection 
  OF StackDirection:FIFO OROF StackDirection:FILO ! First In
    ADD(pStack, 1)
  OF StackDirection:LIFO OROF StackDirection:LILO ! Last  In
    ADD(pStack, RECORDS(pStack) + 1)
  END

  
FormulaTranslator.PopSymbol PROCEDURE(*SymbolStack pStack, BYTE pStackDirection = StackDirection:FIFO)!, STRING, VIRTUAL
aSymbol                       LIKE(SymbolStackType)
  CODE
  IF pStack &= NULL THEN RETURN '' END
  IF RECORDS(pStack) = 0 THEN RETURN '' END
  SELF.PopSymbol(pStack, aSymbol, pStackDirection)
  !SELF.DebugOutput('Pop: ''' & aSymbol.Symbol & '''')
  RETURN aSymbol.Symbol
  

FormulaTranslator.PopSymbol PROCEDURE(*SymbolStack pStack, *SymbolStackType pSymbol, BYTE pStackDirection = StackDirection:FIFO)!, VIRTUAL
  CODE
  IF pStack &= NULL THEN RETURN END
  !IF pSymbol &= NULL THEN RETURN END ! Illegal reference assignment or equivalence
  IF RECORDS(pStack) = 0 THEN RETURN END
  CASE pStackDirection 
  OF StackDirection:FIFO OROF StackDirection:LIFO ! First Out
    GET(pStack, 1)
  OF StackDirection:FILO OROF StackDirection:LILO ! Last  Out
    GET(pStack, RECORDS(pStack))
  END
  pSymbol = pStack
  DELETE(pStack)
  !SELF.DebugOutput('Pop: ' & SELF.ToStringSymbol(pSymbol))
  RETURN

  
FormulaTranslator.PeekSymbol    PROCEDURE(*SymbolStack pStack, BYTE pStackDirection = StackDirection:FIFO)!, STRING, VIRTUAL
aSymbol                           LIKE(SymbolStackType)

  CODE
  IF pStack &= NULL THEN RETURN '' END
  IF RECORDS(pStack) = 0 THEN RETURN '' END
  SELF.PeekSymbol(pStack, aSymbol, pStackDirection)
  !SELF.DebugOutput('Peek: ''' & aSymbol.Symbol & '''')
  RETURN aSymbol.Symbol

  
FormulaTranslator.PeekSymbol    PROCEDURE(*SymbolStack pStack, *SymbolStackType pSymbol, BYTE pStackDirection = StackDirection:FIFO)!, VIRTUAL
CODE
  IF pStack &= NULL THEN RETURN END
  !IF pSymbol &= NULL THEN RETURN END ! Illegal reference assignment or equivalence
  IF RECORDS(pStack) = 0 THEN RETURN END
  CASE pStackDirection 
  OF StackDirection:FIFO OROF StackDirection:LIFO ! First Out
    GET(pStack, 1)
  OF StackDirection:FILO OROF StackDirection:LILO ! Last  Out
    GET(pStack, RECORDS(pStack))
  END
  pSymbol = pStack
  !SELF.DebugOutput('Peek: ' & SELF.ToStringSymbol(pSymbol))
  RETURN
  
  
FormulaTranslator.ToString  PROCEDURE()!, STRING, VIRTUAL
I                             LONG
X                             gcCString
  CODE
  RETURN SELF.ToStringStack(SELF.Input) & ' -> ' & SELF.ToStringStack(SELF.Output)


FormulaTranslator.ToStringStack PROCEDURE(*SymbolStack pStack, BYTE pFull=False)!, STRING, VIRTUAL
I                             LONG
X                             gcCString
  CODE
  IF pStack &= NULL THEN RETURN '' END
  IF RECORDS(pStack) = 0 THEN RETURN '[ ]' END
  X.Init(1024 * 1024)
  LOOP I = 1 TO RECORDS(pStack)
    GET(pStack, I)
    IF I > 1 THEN
      X.Value = ', ' & X.Value
    END
    IF pFull = True THEN
      X.Value = '( ' & SELF.ToStringSymbol(pStack) & ' )' & X.Value
    ELSE
      X.Value = pStack.Symbol & X.Value
    END
  END
  X.Value = '[ ' & X.Value & ' ]'
  RETURN X.Value

  
FormulaTranslator.ToStringSymbol    PROCEDURE(*SymbolStackType pSymbol)!, STRING, VIRTUAL
X                             gcCString
  CODE
  !IF pSymbol &= NULL THEN RETURN '' END ! Illegal reference assignment or equivalence
  X.Init(1024)
  X.Value = '''' & pSymbol.Symbol & ''', t:' & pSymbol.SymbolType & ', p:' & pSymbol.SymbolPriority
  RETURN X.Value
  

! Debug

        
FormulaTranslator.DebugOutput   PROCEDURE(STRING pMessage)!, VIRTUAL
Msg CSTRING(2048)
  CODE
  Msg = 'FormulaTranslator: ' & pMessage
  OutputDebugString(Msg)


