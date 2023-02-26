  MEMBER

! TODO: Test *Equate* functions, and Get*Label functions, and check unique symbols
! TODO: Add Get*Label for variable stack

  MAP
    MODULE('')
      OutputDebugString(*CSTRING), PASCAL, RAW, NAME('OutputDebugStringA')
    END
  END

  INCLUDE('gcCString.inc'),ONCE
  INCLUDE('FormulaTranslator.inc'),ONCE

! Functions with multiple parameters are not supported

! Init


FormulaTranslator.Construct PROCEDURE!, PROTECTED
  CODE
  SELF.Input   &= NEW SymbolStack
  SELF.Output  &= NEW SymbolStack
  SELF.Solve   &= NEW SymbolStack
  SELF.Equates &= NEW EquateList

  
FormulaTranslator.Destruct  PROCEDURE!, PROTECTED, VIRTUAL
  CODE
  SELF.Destroy()


FormulaTranslator.Destroy   PROCEDURE()!, VIRTUAL
  CODE
  IF NOT SELF.Input   &= NULL THEN
    FREE(SELF.Input)
    DISPOSE(SELF.Input)
  END
  IF NOT SELF.Output  &= NULL THEN
    FREE(SELF.Output)
    DISPOSE(SELF.Output)
  END
  IF NOT SELF.Solve   &= NULL THEN
    FREE(SELF.Solve)
    DISPOSE(SELF.Solve)
  END
  IF NOT SELF.Equates &= NULL THEN
    FREE(SELF.Equates)
    DISPOSE(SELF.Equates)
  END


FormulaTranslator.Reset PROCEDURE()!, VIRTUAL
  CODE
  IF NOT SELF.Input   &= NULL THEN
    FREE(SELF.Input)
  END
  IF NOT SELF.Output  &= NULL THEN
    FREE(SELF.Output)
  END
  IF NOT SELF.Solve   &= NULL THEN
    FREE(SELF.Solve)
  END
  IF NOT SELF.Equates &= NULL THEN
    FREE(SELF.Equates)
  END

  
! Properties


!!! Calls Parse then Resolve, this will not support variables. To support variables call: AddEquate(pSymbol, pValue) before Evaluate(pFormulat) or: Parse(pFormula),     ???????    , ResolvePostFix()
FormulaTranslator.Evaluate  PROCEDURE(STRING pFormula)!, STRING
  CODE
  SELF.DebugOutput('Evaluate Begin: ''' & pFormula & '''')
  SELF.Parse(pFormula)
  SELF.ResolvePostFix()
  SELF.DebugOutput('Evaluate End: ' & SELF.ToString())
  RETURN 3.14159

  
!!! Parses infix notation formula into reverse polish notation (with internal Input stack) with operator precedence. https://en.wikipedia.org/wiki/Shunting-yard_algorithm  
FormulaTranslator.Parse PROCEDURE(STRING pFormula)!, VIRTUAL
  CODE
  SELF.Parse(SELF.Input, SELF.Output, pFormula)


!!! Parses infix notation formula into reverse polish notation with operator precedence. https://en.wikipedia.org/wiki/Shunting-yard_algorithm  
FormulaTranslator.Parse PROCEDURE(*SymbolStack pInput, *SymbolStack pOutput, STRING pFormula)!, VIRTUAL, PRIVATE
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
  SELF.InFixToPostFix(pInput, pOutput)
  SELF.DebugOutput('Parse End: ' & SELF.ToString())


!!! Converts infix symbol stack to postfix (RPN/Reverse Polish Notation) (from internal Input to internal Output) symbol stack
FormulaTranslator.InFixToPostFix    PROCEDURE() !, VIRTUAL
  CODE
  IF NOT SELF.Output &= NULL THEN
    FREE(SELF.Output)
    !DISPOSE(SELF.Output)
  END
  SELF.InFixToPostFix(SELF.Input, SELF.Output)

  
!!! Converts infix symbol stack to postfix (RPN/Reverse Polish Notation) symbol stack
FormulaTranslator.InFixToPostFix    PROCEDURE(*SymbolStack pInput, *SymbolStack pOutput) !, VIRTUAL, PRIVATE
! The shunting yard algorithm can also be applied to produce prefix notation (also known as Polish notation). To do this one would simply start from the end of a string of tokens to be parsed and work backwards, reverse the output queue (therefore making the output queue an output stack), and flip the left and right parenthesis behavior (remembering that the now-left parenthesis behavior should pop until it finds a now-right parenthesis). And changing the associativity condition to right. 
! This implementation does not implement composite functions, functions with variable number of arguments, and unary operators.
lOperator                             &SymbolStack ! Operator Stack
aSymbol                               LIKE(SymbolStackType)
aOperator                             LIKE(SymbolStackType)
aUnaryOperator                        LIKE(SymbolStackType)

  CODE ! https://en.wikipedia.org/wiki/Shunting-yard_algorithm
  IF pInput &= NULL THEN RETURN END
  IF pOutput &= NULL THEN RETURN END
  IF RECORDS(pInput) = 0 THEN RETURN END
  !SELF.DebugOutput('InFixToPostFix Begin: ' & SELF.ToStringStack(pInput))
  
  lOperator &= NEW SymbolStack
  
  LOOP WHILE RECORDS(pInput) > 0
    !SELF.DebugOutput('InFixToPostFix   Loop Input')
    SELF.PopSymbol(pInput, aSymbol, StackDirection:FIFO) ! Get Input
    !SELF.DebugOutput('InFixToPostFix   Loop ' & SELF.ToStringSymbol(aSymbol))
    IF LEN(aSymbol) = 0 THEN CYCLE END
    
    IF BAND(aSymbol.SymbolType, SymbolType:Values) <> 0 THEN ! Is Value
      SELF.PushSymbol(pOutput, aSymbol, StackDirection:FIFO) ! Push Output
    ELSIF BAND(aSymbol.SymbolType, SymbolType:Labels) <> 0 THEN ! Is Label
      SELF.PushSymbol(pOutput, aSymbol, StackDirection:FIFO) ! Push Output
    ELSIF BAND(aSymbol.SymbolType, SymbolType:Functie) <> 0 THEN ! Is Function
      SELF.PushSymbol(lOperator, aSymbol, StackDirection:FILO) ! Push Operator
    !ELSIF BAND(aSymbol.SymbolType, SymbolType:UnaryOperations) <> 0 THEN ! Is Unary Operator
    !  SELF.PopSymbol(lOperator, aOperator, StackDirection:FILO) ! Get Operator
    !  SELF.PushSymbol(pOutput, aOperator, StackDirection:FIFO) ! Push Output
    ELSIF BAND(aSymbol.SymbolType, SymbolType:AllOperations) <> 0 THEN ! Is Operator

      LOOP WHILE RECORDS(lOperator) > 0
        SELF.PeekSymbol(lOperator, aOperator, StackDirection:FILO)
        !SELF.DebugOutput('InFixToPostFix   Loop Precedence Operators ' & SELF.ToStringSymbol(aOperator))
        
        IF BAND(aOperator.SymbolType, SymbolType:BracketOpen) <> 0 THEN 
          BREAK
        ELSIF aOperator.SymbolPriority < aSymbol.SymbolPriority THEN ! Is Higher Priority (1 higher than 2)
          SELF.PopSymbol(lOperator, aOperator, StackDirection:FILO) ! Get Operator
          SELF.PushSymbol(pOutput, aOperator, StackDirection:FIFO) ! Push Output
        ELSIF aOperator.SymbolPriority = aSymbol.SymbolPriority AND | ! Is Equal Priority
              BAND(aOperator.SymbolType, SymbolType:LeftAssociative) <> 0 THEN ! Is Left Associative
          SELF.PopSymbol(lOperator, aOperator, StackDirection:FILO) ! Get Operator
          SELF.PushSymbol(pOutput, aOperator, StackDirection:FIFO) ! Push Outout
        ELSE
          BREAK
        END ! IF
        
      END ! LOOP
      SELF.PushSymbol(lOperator, aSymbol, StackDirection:FILO) ! Push Operator

    ELSIF BAND(aSymbol.SymbolType, SymbolType:BracketOpen) <> 0 THEN ! Is Bracket Open
      SELF.PushSymbol(lOperator, aSymbol, StackDirection:FILO) ! Push Operator
    ELSIF BAND(aSymbol.SymbolType, SymbolType:BracketClose) <> 0 THEN ! Is Bracket Close

      LOOP WHILE RECORDS(lOperator) > 0
        SELF.PeekSymbol(lOperator, aOperator, StackDirection:FILO)
        !SELF.DebugOutput('InFixToPostFix   Loop BracketClose Operators ' & SELF.ToStringSymbol(aOperator))
        
        IF BAND(aOperator.SymbolType, SymbolType:BracketOpen) <> 0 THEN ! Is Bracket Open
          BREAK
        ELSE
          SELF.PopSymbol(lOperator, aOperator, StackDirection:FILO) ! Get Operator
          SELF.PushSymbol(pOutput, aOperator, StackDirection:FIFO) ! Push Output
        END
        ! If the stack runs out without finding a left parenthesis, then there are mismatched parentheses.
        
      END ! LOOP
      SELF.PeekSymbol(lOperator, aOperator, StackDirection:FILO)
      IF BAND(aOperator.SymbolType, SymbolType:BracketOpen) <> 0 THEN ! Is Bracket Open
        SELF.PopSymbol(lOperator, aOperator, StackDirection:FILO) ! Get Operator
        !!SELF.PushSymbol(pOutput, aOperator, StackDirection:FIFO) ! Push Output
        ! Discard
        !SELF.DebugOutput('InFixToPostFix   Loop BracketClose Operators Discard ' & SELF.ToStringSymbol(aOperator))
      END
      !!SELF.PushSymbol(pOutput, aSymbol, StackDirection:FIFO) ! Push Output
      ! Discard

    ELSE
      SELF.DebugOutput('InFixToPostFix   Loop Unsupported t:' & aSymbol.SymbolType & '/' & SymbolType:AllOperations & ' ' & SELF.ToStringSymbol(aSymbol))
      ! Error unsupported symbol
    END

    !SELF.DebugOutput('InFixToPostFix Loop: ' & SELF.ToStringStack(pInput) & ' -> ' & SELF.ToStringStack(lOperator) & ' -> ' & SELF.ToStringStack(pOutput))
  END ! LOOP

  LOOP WHILE RECORDS(lOperator) > 0
    SELF.PeekSymbol(lOperator, aOperator, StackDirection:FILO)
    !SELF.DebugOutput('InFixToPostFix   Loop Leftover Operators ' & SELF.ToStringSymbol(aOperator))
    
    IF BAND(aOperator.SymbolType, SymbolType:BracketOpen) <> 0 THEN ! Is Bracket Open
      ! If the operator token on the top of the stack is a parenthesis, then there are mismatched parentheses.
      SELF.PopSymbol(lOperator, aOperator, StackDirection:FILO) ! Get Operator
      !!SELF.PushSymbol(pOutput, aOperator, StackDirection:FIFO) ! Push Output
      ! Discard
    ELSIF BAND(aOperator.SymbolType, SymbolType:BracketClose) <> 0 THEN ! Is Bracket Close
      ! If the operator token on the top of the stack is a parenthesis, then there are mismatched parentheses.
      SELF.PopSymbol(lOperator, aOperator, StackDirection:FILO) ! Get Operator
      ! Discard
      !!SELF.PushSymbol(pOutput, aOperator, StackDirection:FIFO) ! Push Output
    ELSE
      SELF.PopSymbol(lOperator, aOperator, StackDirection:FILO) ! Get Operator
      SELF.PushSymbol(pOutput, aOperator, StackDirection:FIFO) ! Push Output
    END
    
    !SELF.DebugOutput('InFixToPostFix Loop2 : ' & SELF.ToStringStack(pInput) & ' -> ' & SELF.ToStringStack(lOperator) & ' -> ' & SELF.ToStringStack(pOutput))
  END ! LOOP
  
  SELF.DebugOutput('InFixToPostFix End: ' & SELF.ToStringStack(pInput) & ' -> ' & SELF.ToStringStack(lOperator) & ' -> ' & SELF.ToStringStack(pOutput))
  DISPOSE(lOperator)


!!! Checks infix (from internal) symbol stack and symbols to symbol-types (represented in a bitlist) based on context, these will be used while resolving
FormulaTranslator.CheckInFix    PROCEDURE()!, VIRTUAL, PRIVATE
  CODE
  IF SELF.Input &= NULL THEN RETURN END
  IF RECORDS(SELF.Input) = 0 THEN RETURN END
  
  SELF.CheckInFix(SELF.Input)
  

!!! Checks infix symbol stack and symbols to symbol-types (represented in a bitlist) based on context, these will be used while resolving
FormulaTranslator.CheckInfix    PROCEDURE(*SymbolStack pStack)!, VIRTUAL, PRIVATE
lNextSymbol                       LIKE(SymbolStack)
lPreviousSymbol                   LIKE(SymbolStack)
I                                 LONG
! https://en.wikipedia.org/wiki/Order_of_operations
! https://en.wikipedia.org/wiki/Operators_in_C_and_C%2B%2B#Operator_precedence
  CODE
  IF pStack &= NULL THEN RETURN END
  IF RECORDS(pStack) = 0 THEN RETURN END
  !SELF.DebugOutput('CheckInfix Begin: ' & SELF.ToStringStack(pStack, True))

  ! Check operators
  CLEAR(lPreviousSymbol)
  LOOP I = RECORDS(pStack) TO 1 BY -1
  !LOOP I = 1 TO RECORDS(pStack)
    GET(pStack, I)
    IF pStack.SymbolType <> SymbolType:Unknown THEN CYCLE END

    CASE UPPER(pStack.Symbol)
    OF '(' ! Parenthices
      pStack.SymbolType     = SymbolType:BracketOpen
      pStack.SymbolPriority = 1
    OF ')' ! Parenthices
      pStack.SymbolType     = SymbolType:BracketClose
      pStack.SymbolPriority = 1
    OF '?' ! Parenthices
      pStack.SymbolType     = SymbolType:ChoiceQuestion
      pStack.SymbolPriority = 1
    OF ':' ! Parenthices
      pStack.SymbolType     = SymbolType:ChoiceAnswer
      pStack.SymbolPriority = 1
    OF '++' 
      pStack.SymbolType     = SymbolType:UnaryIncrement
      pStack.SymbolPriority = 2
    OF '--' 
      pStack.SymbolType     = SymbolType:UnaryDecrement
      pStack.SymbolPriority = 2
    OF '^' OROF 'POW' ! Power
      pStack.SymbolType     = SymbolType:OperatorPower
      pStack.SymbolPriority = 4
    OF 'RT' OROF 'ROOT' ! Root
      pStack.SymbolType     = SymbolType:OperatorRoot
      pStack.SymbolPriority = 4
    OF '*' OROF 'MUL' ! Multiply
      pStack.SymbolType     = SymbolType:OperatorMultiply
      pStack.SymbolPriority = 5
    OF '/' OROF 'DIV' ! Divide
      pStack.SymbolType     = SymbolType:OperatorDivide
      pStack.SymbolPriority = 5
    OF '%' OROF 'MOD'! Modulo
      pStack.SymbolType     = SymbolType:OperatorModulo
      pStack.SymbolPriority = 5
    OF '-' ! Sign / Minus
      IF lPreviousSymbol.SymbolType <> SymbolType:Unknown THEN ! Preceeded by an operator? then we can not subsctract anything
        pStack.SymbolType     = SymbolType:UnaryNegative
        pStack.SymbolPriority = 2
      ELSE
        pStack.SymbolType     = SymbolType:OperatorSubstract
        pStack.SymbolPriority = 6
      END
    OF '+' ! Sign / Plus
      IF lPreviousSymbol.SymbolType <> SymbolType:Unknown THEN ! Preceeded by an operator? then we can not add anything
        pStack.SymbolType     = SymbolType:UnaryPositive
        pStack.SymbolPriority = 2
      ELSE
        pStack.SymbolType     = SymbolType:OperatorAdd
        pStack.SymbolPriority = 6
      END
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
    OF '&&' OROF 'AND' 
      pStack.SymbolType     = SymbolType:BinaryAnd
      pStack.SymbolPriority = 9
    OF '||' OROF 'OR' 
      pStack.SymbolType     = SymbolType:BinaryOr
      pStack.SymbolPriority = 9
    OF 'XOR' 
      pStack.SymbolType     = SymbolType:BinaryOr      + SymbolType:BinaryExclusive
      pStack.SymbolPriority = 9
    OF 'XNOR' 
      pStack.SymbolType     = SymbolType:BinaryOr      + SymbolType:BinaryExclusive + SymbolType:BinaryInverse
      pStack.SymbolPriority = 9
    OF 'NAND' 
      pStack.SymbolType     = SymbolType:BinaryInverse + SymbolType:BinaryAnd
      pStack.SymbolPriority = 9
    OF 'NOR' 
      pStack.SymbolType     = SymbolType:BinaryInverse + SymbolType:BinaryOr
      pStack.SymbolPriority = 9
    OF '>>' OROF 'SHR'
      pStack.SymbolType     = SymbolType:BinaryShiftRight
      pStack.SymbolPriority = 9
    OF '<<' OROF 'SHL'
      pStack.SymbolType     = SymbolType:BinaryShiftLeft
      pStack.SymbolPriority = 9
    OF 'NOT' OROF '~' OROF '!' ! Boolean
      pStack.SymbolType     = SymbolType:BinaryInverse
      pStack.SymbolPriority = 9
    ELSE
      !SELF.DebugOutput('CheckInfix operator case else ' & SELF.ToStringSymbol(pStack))
    END ! CASE

    PUT(pStack)
    lPreviousSymbol = pStack
    !SELF.DebugOutput('CheckInfix operator case: ' & SELF.ToStringSymbol(pStack) & ', next: '  & SELF.ToStringSymbol(lNextSymbol))
  END ! LOOP

  ! Check keywords
  LOOP I = RECORDS(pStack) TO 1 BY -1
  !LOOP I = 1 TO RECORDS(pStack)
    IF I > 1 THEN
      GET(pStack, I - 1)
    !IF I < RECORDS(pStack) THEN
    !  GET(pStack, I + 1)
      lNextSymbol = pStack
    ELSE
      CLEAR(lNextSymbol)
    END
    GET(pStack, I)
    IF pStack.SymbolType <> SymbolType:Unknown THEN CYCLE END

    CASE UPPER(pStack.Symbol)
    OF 'NULL' ! Null
      pStack.SymbolType     = SymbolType:Constant
      pStack.SymbolPriority = 0
    OF 'TRUE' OROF 'FALSE' ! Boolean
      pStack.SymbolType     = SymbolType:Constant
      pStack.SymbolPriority = 0
    OF 'PI' ! Pi = +- 3.1415926535898
      pStack.SymbolType     = SymbolType:Constant
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
    OF 'SQRT' ! Function
      IF lNextSymbol.SymbolType = SymbolType:BracketOpen THEN
        pStack.SymbolType     = SymbolType:Functie
        pStack.SymbolPriority = 0
      END
    OF 'SIN' OROF 'ASIN' OROF 'COS' OROF 'ACOS' OROF 'TAN' OROF 'ATAN' ! Function
      IF lNextSymbol.SymbolType = SymbolType:BracketOpen THEN
        pStack.SymbolType     = SymbolType:Functie
        pStack.SymbolPriority = 0
      END
    END ! CASE

    PUT(pStack)
    lPreviousSymbol = pStack
    !SELF.DebugOutput('CheckInfix keyword case: ' & SELF.ToStringSymbol(pStack) & ', next: '  & SELF.ToStringSymbol(lNextSymbol))
  END ! LOOP

  ! Check symbols
  LOOP I = RECORDS(pStack) TO 1 BY -1
  !LOOP I = 1 TO RECORDS(pStack)
    IF I > 1 THEN
      GET(pStack, I - 1)
    !IF I < RECORDS(pStack) THEN
    !  GET(pStack, I + 1)
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
    !SELF.DebugOutput('CheckInfix symbol case: ' & SELF.ToStringSymbol(pStack) & ', next: '  & SELF.ToStringSymbol(lNextSymbol))
  END ! LOOP

  SELF.DebugOutput('CheckInfix End: ' & SELF.ToStringStack(pStack, True))

  
FormulaTranslator.ResolvePostFix    PROCEDURE()!, VIRTUAL, PRIVATE
  CODE
  IF SELF.Output &= NULL THEN SELF.DebugOutput('ResolvePostFix Output=NULL'); RETURN END
  IF RECORDS(SELF.Output) = 0 THEN SELF.DebugOutput('ResolvePostFix Output=empty'); RETURN END
  IF SELF.Solve &= NULL THEN SELF.DebugOutput('ResolvePostFix Solve=NULL'); RETURN END
  
  SELF.ResolvePostFix(SELF.Output)
  
  
FormulaTranslator.ResolvePostFix    PROCEDURE(*SymbolStack pStack)!, VIRTUAL, PRIVATE
lSolve                                &SymbolStack ! Operator Stack
aSymbol                               LIKE(SymbolStackType)
aTerm1                                LIKE(SymbolStackType)
aTerm2                                LIKE(SymbolStackType)

  CODE
  IF pStack &= NULL THEN SELF.DebugOutput('ResolvePostFix pStack=NULL'); RETURN END
  IF RECORDS(pStack) = 0 THEN SELF.DebugOutput('ResolvePostFix pStack=empty'); RETURN END
  SELF.DebugOutput('ResolvePostFix Begin: ' & SELF.ToStringStack(pStack) & ' -> ' & SELF.ToStringStack(lSolve))
  
  lSolve &= NEW SymbolStack
  LOOP WHILE RECORDS(pStack) > 0
    SELF.PopSymbol(pStack, aSymbol, StackDirection:FIFO)
    SELF.DebugOutput('ResolvePostFix   Loop ' & SELF.ToStringSymbol(aSymbol))
    
    ! Single values
    IF BAND(aSymbol.SymbolType, SymbolType:Constant) <> 0 THEN ! Is a Constant
      SELF.ResolveSymbol(aSymbol)
      aTerm2 = aTerm1
      aTerm1 = aSymbol
    ELSIF BAND(aSymbol.SymbolType, SymbolType:Labels) <> 0 THEN ! Is a Label
      SELF.DebugOutput('ResolvePostFix   ' & 'Unknown Label ' & SELF.ToStringSymbol(aSymbol) & '. Variables and Attributes need to be replaced before Evaluation')
      ASSERT(False, 'Unknown Label ' & SELF.ToStringSymbol(aSymbol) & '. Variables and Attributes need to be replaced before Evaluation')
    ELSIF BAND(aSymbol.SymbolType, SymbolType:Values) <> 0 THEN ! Is a Value
      aTerm2 = aTerm1
      aTerm1 = aSymbol

    ! Operations with 1 term
    ELSIF BAND(aSymbol.SymbolType, SymbolType:UnaryOperations) <> 0 THEN ! Is a Unary Operator
      SELF.ResolveSymbol(aTerm1, aSymbol)
      
    ! Operations with 2 terms
    ELSIF BAND(aSymbol.SymbolType, SymbolType:BinaryOperations) <> 0 THEN ! Is a Binary Operator
      SELF.ResolveSymbol(aTerm1, aSymbol, aTerm2)

    ! Operations with * terms (i.e functions with 1 term)
    ELSIF BAND(aSymbol.SymbolType, SymbolType:Functie) <> 0 THEN ! Is a Function
      SELF.ResolveFunction(aTerm1, aSymbol)

    END
    SELF.DebugOutput('ResolvePostFix   Result ' & SELF.ToStringSymbol(aTerm1))
  END
  SELF.PushSymbol(lSolve, aTerm1, StackDirection:LILO) ! Push Solution Stack
  SELF.DebugOutput('ResolvePostFix End: ' & SELF.ToStringStack(pStack) & ' -> ' & SELF.ToStringStack(lSolve))

  
FormulaTranslator.ResolveFunction   PROCEDURE(*SymbolStackType pTermInOut, *SymbolStackType pSymbol)!, VIRTUAL, PRIVATE
aNumber                               REAL

  CODE
  SELF.DebugOutput('ResolveFunction Begin: ' & SELF.ToStringSymbol(pTermInOut) & ', ' & SELF.ToStringSymbol(pSymbol))
  IF BAND(pSymbol.SymbolType, SymbolType:Functie) <> 0 THEN ! Is a Function
    IF BAND(pTermInOut.SymbolType, SymbolType:Number) <> 0 THEN ! Is Number
      CASE pSymbol.Symbol
      OF 'SQRT' ! Square Root
        aNumber = pTermInOut.Symbol
        aNumber = sqrt(aNumber)
        pTermInOut.Symbol = aNumber
      ELSE
        SELF.DebugOutput('ResolveFunction Unknown function ' & SELF.ToStringSymbol(pTermInOut) & ', ' & SELF.ToStringSymbol(pSymbol))
        ASSERT(False, 'Unknown function ' & SELF.ToStringSymbol(pTermInOut) & ', ' & SELF.ToStringSymbol(pSymbol))
      END
    END
  END
  SELF.DebugOutput('ResolveFunction End: ' & SELF.ToStringSymbol(pTermInOut) & ', ' & SELF.ToStringSymbol(pSymbol))
  
  
FormulaTranslator.ResolveSymbol PROCEDURE(*SymbolStackType pTermInOut)!, VIRTUAL, PRIVATE
  CODE
  !SELF.DebugOutput('ResolveSymbol Begin: ' & SELF.ToStringSymbol(pTermInOut))
  IF BAND(pTermInOut.SymbolType, SymbolType:Constant) <> 0 THEN ! Is a Constant ?
    CASE pTermInOut.Symbol
    OF 'PI' ! Pi = +- 3.1415926535898
      pTermInOut.Symbol     = '3.1415926535898'
      pTermInOut.SymbolType = SymbolType:Number
    OF 'NULL'
      ! Unchanged
    OF 'NAN' ! Not A Number
      ! Unchanged
    OF 'TRUE'
      ! Unchanged
    OF 'FALSE'
      ! Unchanged
    ELSE
      SELF.DebugOutput('ResolveSymbol Unknown(0) ' & SELF.ToStringSymbol(pTermInOut))
      ASSERT(False, 'Unknown(0) ' & SELF.ToStringSymbol(pTermInOut))
    END
  ELSIF BAND(pTermInOut.SymbolType, SymbolType:Labels) <> 0 THEN ! Is a Variable or Attribute ?
    IF NOT SELF.Equates &= NULL THEN 

      IF SELF.GetEquate(pTermInOut.Symbol) THEN ! Always match without type
        pTermInOut.Symbol     = SELF.Equates.ReplaceSymbol
        pTermInOut.SymbolType = SELF.Equates.ReplaceSymbolType
      ELSE
        SELF.DebugOutput('ResolveSymbol Equate not found ' & SELF.ToStringSymbol(pTermInOut))
        ASSERT(False, 'Unknown(0) ' & SELF.ToStringSymbol(pTermInOut))
      END

    ELSE
      SELF.DebugOutput('ResolveSymbol No equates ' & SELF.ToStringSymbol(pTermInOut))
      ASSERT(False, 'Unknown(0) ' & SELF.ToStringSymbol(pTermInOut))
    END
  END
  SELF.DebugOutput('ResolveSymbol End: ' & SELF.ToStringSymbol(pTermInOut))


FormulaTranslator.ResolveSymbol PROCEDURE(*SymbolStackType pTermInOut, *SymbolStackType pSymbol)!, VIRTUAL, PRIVATE
aNumber                           REAL
isNumber                          BYTE(False)

  CODE
  SELF.DebugOutput('ResolveSymbol Begin: ' & SELF.ToStringSymbol(pTermInOut) & ', ' & SELF.ToStringSymbol(pSymbol))
  IF BAND(pTermInOut.SymbolType, SymbolType:Number) <> 0 THEN ! Is number
    isNumber = True
  END
    
  IF BAND(pSymbol.SymbolType, SymbolType:Functie) <> 0 THEN ! Is a Function
    SELF.ResolveFunction(pTermInOut, pSymbol)
  ELSIF BAND(pSymbol.SymbolType, SymbolType:UnaryOperations) <> 0 THEN ! Is Unary Operation

    IF isNumber THEN
      aNumber = pTermInOut.Symbol
      IF    BAND(pSymbol.SymbolType, SymbolType:UnaryDecrement) <> 0 THEN ! Is Unary Increment
        aNumber = aNumber - 1
      ELSIF BAND(pSymbol.SymbolType, SymbolType:UnaryIncrement) <> 0 THEN ! Is Unary Decrement
        aNumber = aNumber + 1
      ELSIF BAND(pSymbol.SymbolType, SymbolType:UnaryPositive) <> 0 THEN ! Is Unary Positive
        aNumber = +aNumber
      ELSIF BAND(pSymbol.SymbolType, SymbolType:UnaryNegative) <> 0 THEN ! Is Unary Negative
        aNumber = -aNumber
      ELSIF BAND(pSymbol.SymbolType, SymbolType:BinaryInverse) <> 0 THEN ! Is Binary NOT
        aNumber = BXOR(aNumber, 0FFFFFFFFh)
      ELSE
        SELF.DebugOutput('ResolveSymbol Unknown(1) ' & SELF.ToStringSymbol(pTermInOut) & ', ' & SELF.ToStringSymbol(pSymbol))
        ASSERT(False, 'Unknown(1) ' & SELF.ToStringSymbol(pTermInOut) & ', ' & SELF.ToStringSymbol(pSymbol))
      END
      pTermInOut.Symbol = aNumber
    END
  END
  SELF.DebugOutput('ResolveSymbol End: ' & SELF.ToStringSymbol(pTermInOut) & ', ' & SELF.ToStringSymbol(pSymbol))
    

FormulaTranslator.ResolveSymbol PROCEDURE(*SymbolStackType pTermInOut, *SymbolStackType pSymbol, *SymbolStackType pTerm2)!, VIRTUAL, PRIVATE
aNumber                           REAL
aNumber2                          REAL
aBit                              UNSIGNED
aBit2                             UNSIGNED
isNumber                          BYTE(False)
isNumber2                         BYTE(False)

  CODE
  SELF.DebugOutput('ResolveSymbol Begin: ' & SELF.ToStringSymbol(pTermInOut) & ', ' & SELF.ToStringSymbol(pSymbol) & ', ' & SELF.ToStringSymbol(pTerm2))
  IF BAND(pTermInOut.SymbolType, SymbolType:Number) <> 0 THEN ! Is number
    isNumber = True
  END
  IF BAND(pTerm2.SymbolType, SymbolType:Number) <> 0 THEN ! Is number
    isNumber2 = True
  END

  IF    BAND(pSymbol.SymbolType, SymbolType:Concatenate) <> 0 THEN ! Is Concatenate Operator
    pTermInOut.Symbol = pTermInOut.Symbol & pTerm2.Symbol

  ELSIF BAND(pSymbol.SymbolType, SymbolType:Operators) <> 0 THEN ! Is Operator
    IF isNumber AND isNumber2 THEN
      aNumber  = pTermInOut.Symbol
      aNumber2 = pTerm2.Symbol
      IF    BAND(pSymbol.SymbolType, SymbolType:OperatorAdd) <> 0 THEN ! Is Add Operator
        aNumber  = aNumber + aNumber2
      ELSIF BAND(pSymbol.SymbolType, SymbolType:OperatorSubstract) <> 0 THEN ! Is Substract Operator
        aNumber  = aNumber - aNumber2
      ELSIF BAND(pSymbol.SymbolType, SymbolType:OperatorMultiply) <> 0 THEN ! Is Multiply Operator
        aNumber  = aNumber * aNumber2
      ELSIF BAND(pSymbol.SymbolType, SymbolType:OperatorDivide) <> 0 THEN ! Is Divide Operator
        IF aNumber2 = 0 THEN
          pTermInOut.SymbolType = SymbolType:Constant
          pTermInOut.Symbol     = 'NaN'
        ELSE
          aNumber  = aNumber / aNumber2
        END
      ELSIF BAND(pSymbol.SymbolType, SymbolType:OperatorModulo) <> 0 THEN  ! Is Modulo Operator
        IF aNumber2 = 0 THEN
          pTermInOut.SymbolType = SymbolType:Constant
          pTermInOut.Symbol     = 'NaN'
        ELSE
          aNumber  = aNumber % aNumber2
        END
      ELSIF BAND(pSymbol.SymbolType, SymbolType:OperatorPower) <> 0 THEN ! Is Power Operator
        aNumber  = aNumber ^ aNumber2
      ELSIF BAND(pSymbol.SymbolType, SymbolType:OperatorRoot) <> 0 THEN ! Is Root Operator
        IF aNumber2 = 0 THEN
          pTermInOut.SymbolType = SymbolType:Constant
          pTermInOut.Symbol     = 'NaN'
        ELSE
          aNumber  = aNumber ^ (1 / aNumber2)
        END
      ELSE
        SELF.DebugOutput('ResolveSymbol Unknown operator ' & SELF.ToStringSymbol(pTermInOut) & ', ' & SELF.ToStringSymbol(pSymbol) & ', ' & SELF.ToStringSymbol(pTerm2))
        ASSERT(False, 'Unknown operator ' & SELF.ToStringSymbol(pTermInOut) & ', ' & SELF.ToStringSymbol(pSymbol) & ', ' & SELF.ToStringSymbol(pTerm2))
      END
      pTermInOut.Symbol = aNumber

    ELSE |
      SELF.DebugOutput('ResolveSymbol Not Implemented non-numeric operator ' & SELF.ToStringSymbol(pTermInOut) & ', ' & SELF.ToStringSymbol(pSymbol) & ', ' & SELF.ToStringSymbol(pTerm2))
      ASSERT(False, 'Not Implemented non-numeric operator ' & SELF.ToStringSymbol(pTermInOut) & ', ' & SELF.ToStringSymbol(pSymbol) & ', ' & SELF.ToStringSymbol(pTerm2))
    END

  ELSIF BAND(pSymbol.SymbolType, SymbolType:Binaries) <> 0 THEN ! Is Binary Operator
    IF isNumber AND isNumber2 THEN
      aBit  = pTermInOut.Symbol
      aBit2 = pTerm2.Symbol
      IF    BAND(pSymbol.SymbolType, SymbolType:BinaryShiftRight) <> 0 THEN ! Is Right-Shift Operator
        aBit  = BSHIFT(aBit, -aBit2)
      ELSIF BAND(pSymbol.SymbolType, SymbolType:BinaryShiftRight) <> 0 THEN ! Is Left-Shift Operator
        aBit  = BSHIFT(aBit, +aBit2)
      ELSIF BAND(pSymbol.SymbolType, SymbolType:BinaryAnd) <> 0 THEN ! Is AND Operator
        aBit  = BAND(aBit, aBit2)
      ELSIF BAND(pSymbol.SymbolType, SymbolType:BinaryExclusive + SymbolType:BinaryOr + SymbolType:BinaryInverse) <> 0 THEN ! Is XNOR Operator
        aBit  = BXOR(aBit, BXOR(aBit2, 0FFFFFFFFh))
      ELSIF BAND(pSymbol.SymbolType, SymbolType:BinaryExclusive + SymbolType:BinaryOr) <> 0 THEN ! Is XOR Operator
        aBit  = BXOR(aBit, aBit2)
      ELSIF BAND(pSymbol.SymbolType, SymbolType:BinaryOr) <> 0 THEN ! Is OR Operator
        aBit  = BOR(aBit, aBit2)
      ELSE |
        SELF.DebugOutput('ResolveSymbol Unknown binary ' & SELF.ToStringSymbol(pTermInOut) & ', ' & SELF.ToStringSymbol(pSymbol) & ', ' & SELF.ToStringSymbol(pTerm2))
        ASSERT(False, 'Unknown binary ' & SELF.ToStringSymbol(pTermInOut) & ', ' & SELF.ToStringSymbol(pSymbol) & ', ' & SELF.ToStringSymbol(pTerm2))
      END
      pTermInOut.Symbol = aBit
    ELSE |
      SELF.DebugOutput('ResolveSymbol Not Implemented non-numeric binary ' & SELF.ToStringSymbol(pTermInOut) & ', ' & SELF.ToStringSymbol(pSymbol) & ', ' & SELF.ToStringSymbol(pTerm2))
      ASSERT(False, 'Not Implemented non-numeric binary ' & SELF.ToStringSymbol(pTermInOut) & ', ' & SELF.ToStringSymbol(pSymbol) & ', ' & SELF.ToStringSymbol(pTerm2))
    END

  ELSIF BAND(pSymbol.SymbolType, SymbolType:Compares) <> 0 THEN | ! Is Compare Operator
    SELF.DebugOutput('ResolveSymbol Not Implemented operator ' & SELF.ToStringSymbol(pTermInOut) & ', ' & SELF.ToStringSymbol(pSymbol) & ', ' & SELF.ToStringSymbol(pTerm2))
    ASSERT(False, 'Not Implemented operator ' & SELF.ToStringSymbol(pTermInOut) & ', ' & SELF.ToStringSymbol(pSymbol) & ', ' & SELF.ToStringSymbol(pTerm2))

    IF isNumber AND isNumber2 THEN |
    ELSE
    END
    
  ELSIF BAND(pSymbol.SymbolType, SymbolType:Choices) <> 0 THEN | ! Is Choice Operator
    SELF.DebugOutput('ResolveSymbol Not Implemented choice ' & SELF.ToStringSymbol(pTermInOut) & ', ' & SELF.ToStringSymbol(pSymbol) & ', ' & SELF.ToStringSymbol(pTerm2))
    ASSERT(False, 'Not Implemented choice ' & SELF.ToStringSymbol(pTermInOut) & ', ' & SELF.ToStringSymbol(pSymbol) & ', ' & SELF.ToStringSymbol(pTerm2))

  ELSE |
    SELF.DebugOutput('ResolveSymbol Unknown(2) ' & SELF.ToStringSymbol(pSymbol))
    ASSERT(False, 'Unknown(2) ' & SELF.ToStringSymbol(pSymbol))
  END
  SELF.DebugOutput('ResolveSymbol End: ' & SELF.ToStringSymbol(pTermInOut) & ', ' & SELF.ToStringSymbol(pSymbol) & ', ' & SELF.ToStringSymbol(pTerm2))

  
FormulaTranslator.AddConstant   PROCEDURE(STRING pSymbol, STRING pReplaceSymbol)!, VIRTUAL
  CODE
  IF NUMERIC(pReplaceSymbol) THEN
    SELF.AddEquate(pSymbol, SymbolType:Constant, pReplaceSymbol, SymbolType:Number)
  ELSE
    SELF.AddEquate(pSymbol, SymbolType:Constant, pReplaceSymbol, SymbolType:Text)
  END
  
  
FormulaTranslator.AddVariable   PROCEDURE(STRING pSymbol, STRING pReplaceSymbol)!, VIRTUAL
  CODE
  IF NUMERIC(pReplaceSymbol) THEN
    SELF.AddEquate(pSymbol, SymbolType:Variable, pReplaceSymbol, SymbolType:Number)
  ELSE
    SELF.AddEquate(pSymbol, SymbolType:Variable, pReplaceSymbol, SymbolType:Text)
  END

  
FormulaTranslator.AddAttribute  PROCEDURE(STRING pSymbol, STRING pReplaceSymbol)!, VIRTUAL
  CODE
  IF NUMERIC(pReplaceSymbol) THEN
    SELF.AddEquate(pSymbol, SymbolType:Attribute, pReplaceSymbol, SymbolType:Number)
  ELSE
    SELF.AddEquate(pSymbol, SymbolType:Attribute, pReplaceSymbol, SymbolType:Text)
  END
  
  
FormulaTranslator.AddEquate PROCEDURE(STRING pSymbol, UNSIGNED pSymbolType, STRING pReplaceSymbol)!, VIRTUAL
  CODE
  IF NUMERIC(pReplaceSymbol) THEN
    SELF.AddEquate(pSymbol, pSymbolType, pReplaceSymbol, SymbolType:Number)
  ELSE
    SELF.AddEquate(pSymbol, pSymbolType, pReplaceSymbol, SymbolType:Text)
  END

  
FormulaTranslator.AddEquate PROCEDURE(STRING pSymbol, UNSIGNED pSymbolType, STRING pReplaceSymbol, UNSIGNED pReplaceSymbolType, BYTE pSymbolPriority=0)!, VIRTUAL
  CODE
  IF SELF.Equates &= NULL THEN RETURN END

  IF SELF.GetEquate(pSymbol) THEN

    SELF.Equates.Symbol            = pSymbol
    SELF.Equates.SymbolType        = pSymbolType
    SELF.Equates.SymbolPriority    = pSymbolPriority
    SELF.Equates.ReplaceSymbol     = pReplaceSymbol
    SELF.Equates.ReplaceSymbolType = pReplaceSymbolType
    PUT(SELF.Equates)
    
  ELSE
    
    CLEAR(SELF.Equates)
    SELF.Equates.Symbol            = pSymbol
    SELF.Equates.SymbolType        = pSymbolType
    SELF.Equates.SymbolPriority    = pSymbolPriority
    SELF.Equates.ReplaceSymbol     = pReplaceSymbol
    SELF.Equates.ReplaceSymbolType = pReplaceSymbolType
    ADD(SELF.Equates)
    
  END


FormulaTranslator.GetEquate PROCEDURE(STRING pSymbol, UNSIGNED pSymbolType)!, STRING, PROC, VIRTUAL
I   LONG
Found   BYTE(False)

  CODE
  IF SELF.Equates &= NULL THEN RETURN '' END
  IF NOT pSymbol THEN RETURN '' END

  IF pSymbolType = SymbolType:Unknown THEN
    
    SORT(SELF.Equates, SELF.Equates.Symbol, SELF.Equates.SymbolType, SELF.Equates.SymbolPriority)
    LOOP I = 1 TO RECORDS(SELF.Equates)
      GET(SELF.Equates, I)
      IF SELF.Equates.Symbol <> pSymbol THEN CYCLE END
      Found = True
    END ! LOOP
    IF NOT Found THEN
      CLEAR(SELF.Equates)
    END
    SELF.IndexGetLabel = POINTER(SELF.Equates)
    RETURN SELF.Equates.ReplaceSymbol
    
  ELSE  
    
    SELF.Equates.Symbol            = pSymbol
    SELF.Equates.SymbolType        = pSymbolType
    GET(SELF.Equates, SELF.Equates.Symbol, SELF.Equates.SymbolType)
    IF ERRORCODE() THEN 
      CLEAR(SELF.Equates)
      RETURN ''
    ELSE
      SELF.IndexGetLabel = POINTER(SELF.Equates)
      RETURN SELF.Equates.ReplaceSymbol
    END
    
  END ! IF 


FormulaTranslator.RemoveEquate  PROCEDURE(STRING pSymbol)!, STRING, PROC, VIRTUAL
  CODE
  IF SELF.Equates &= NULL THEN RETURN '' END
  IF NOT pSymbol THEN RETURN '' END
  
  IF SELF.GetEquate(pSymbol) THEN
    DELETE(SELF.Equates)
  END


FormulaTranslator.FreeEquates   PROCEDURE()!, VIRTUAL
  CODE
  IF SELF.Equates &= NULL THEN RETURN END
  
  FREE(SELF.Equates)


FormulaTranslator.GetFirstLabel PROCEDURE()!, STRING, VIRTUAL
  CODE
  IF 1 > RECORDS(SELF.Output) THEN RETURN '' END
  SELF.IndexGetLabel = 1
    
  GET(SELF.Output, SELF.IndexGetLabel)
  IF ERRORCODE() THEN 
    CLEAR(SELF.Output)
  END
  RETURN SELF.Output.Symbol


FormulaTranslator.GetNextLabel  PROCEDURE()!, STRING, VIRTUAL
  CODE
  IF SELF.IndexGetLabel + 1 > RECORDS(SELF.Output) THEN RETURN '' END
  SELF.IndexGetLabel += 1
    
  GET(SELF.Output, SELF.IndexGetLabel)
  IF ERRORCODE() THEN 
    CLEAR(SELF.Output)
  END
  RETURN SELF.Output.Symbol


FormulaTranslator.ReplaceLabel  PROCEDURE(STRING pReplaceSymbol)!, VIRTUAL
  CODE
  IF SELF.IndexGetLabel > RECORDS(SELF.Output) THEN RETURN END

  IF NUMERIC(pReplaceSymbol) THEN
    SELF.Output.Symbol     = pReplaceSymbol
    SELF.Output.SymbolType = SymbolType:Number
    PUT(SELF.Output)
  ELSE
    SELF.Output.Symbol     = pReplaceSymbol
    SELF.Output.SymbolType = SymbolType:Text
    PUT(SELF.Output)
  END

  
FormulaTranslator.ReplaceLabel  PROCEDURE(STRING pSymbol, STRING pReplaceSymbol)!, VIRTUAL
  CODE
  SELF.Output.Symbol = pSymbol
  GET(SELF.Output, SELF.Output.Symbol)
  IF ERRORCODE() THEN RETURN END
  SELF.ReplaceLabel(pReplaceSymbol)
  
  
FormulaTranslator.PushSymbol    PROCEDURE(*SymbolStack pStack, STRING pSymbol, BYTE pStackDirection = StackDirection:FIFO)!, VIRTUAL
aSymbol                           LIKE(SymbolStackType)
  CODE
  IF pStack &= NULL THEN RETURN END
  IF LEN(pSymbol) = 0 THEN RETURN END
  !SELF.DebugOutput('Push: ''' & pSymbol & '''')
  aSymbol.Symbol     = pSymbol
  SELF.PushSymbol(pStack, aSymbol, pStackDirection)


FormulaTranslator.PushSymbol    PROCEDURE(*SymbolStack pStack, STRING pSymbol, UNSIGNED pSymbolType, BYTE pStackDirection = StackDirection:FIFO)!, VIRTUAL
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
  !SELF.DebugOutput('Push: ' & SELF.ToStringSymbol(pSymbol))
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
    GET(pStack, RECORDS(pStack))
  OF StackDirection:FILO OROF StackDirection:LILO ! Last  Out
    GET(pStack, 1)
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
    GET(pStack, RECORDS(pStack))
  OF StackDirection:FILO OROF StackDirection:LILO ! Last  Out
    GET(pStack, 1)
  END
  pSymbol = pStack
  !SELF.DebugOutput('Peek: ' & SELF.ToStringSymbol(pSymbol))
  RETURN
  

FormulaTranslator.GetTypeName   PROCEDURE(UNSIGNED pSymbolType)!, STRING, VIRTUAL
X                                 gcCString
  CODE
  IF pSymbolType = SymbolType:Unknown THEN
    RETURN 'Unknown'
  END
  X.Init(1 * 1024)
  ! Values
  IF BAND(pSymbolType, SymbolType:Constant) <> 0 THEN
    X.Value = X.Value & 'Constant'
  END
  IF BAND(pSymbolType, SymbolType:Number) <> 0 THEN
    X.Value = X.Value & 'Number'
  END
  IF BAND(pSymbolType, SymbolType:Text) <> 0 THEN
    X.Value = X.Value & 'Text'
  END
  ! Labels
  IF BAND(pSymbolType, SymbolType:Variable) <> 0 THEN
    X.Value = X.Value & 'Variable'
  END
  IF BAND(pSymbolType, SymbolType:Attribute) <> 0 THEN
    X.Value = X.Value & 'Attribute'
  END
  !
  IF BAND(pSymbolType, SymbolType:Functie) <> 0 THEN
    X.Value = X.Value & 'Function'
  END
  IF BAND(pSymbolType, SymbolType:Concatenate) <> 0 THEN
    X.Value = X.Value & 'Concatenate'
  END
  ! Brackets
  IF BAND(pSymbolType, SymbolType:BracketOpen) <> 0 THEN
    X.Value = X.Value & 'Bracket'
  END
  IF BAND(pSymbolType, SymbolType:BracketClose) <> 0 THEN
    X.Value = X.Value & 'Bracket'
  END
  ! Operators
  IF BAND(pSymbolType, SymbolType:OperatorAdd) <> 0 THEN
    X.Value = X.Value & 'Add'
  END
  IF BAND(pSymbolType, SymbolType:OperatorSubstract) <> 0 THEN
    X.Value = X.Value & 'Substract'
  END
  IF BAND(pSymbolType, SymbolType:OperatorMultiply) <> 0 THEN
    X.Value = X.Value & 'Multiply'
  END
  IF BAND(pSymbolType, SymbolType:OperatorDivide) <> 0 THEN
    X.Value = X.Value & 'Divide'
  END
  IF BAND(pSymbolType, SymbolType:OperatorModulo) <> 0 THEN
    X.Value = X.Value & 'Modulo'
  END
  IF BAND(pSymbolType, SymbolType:OperatorPower) <> 0 THEN
    X.Value = X.Value & 'Power'
  END
  IF BAND(pSymbolType, SymbolType:OperatorRoot) <> 0 THEN
    X.Value = X.Value & 'Root'
  END
  ! Unaries
  IF BAND(pSymbolType, SymbolType:UnaryNegative) <> 0 THEN
    X.Value = X.Value & 'Negative'
  END
  IF BAND(pSymbolType, SymbolType:UnaryIncrement) <> 0 THEN
    X.Value = X.Value & 'Increment'
  END
  IF BAND(pSymbolType, SymbolType:UnaryPositive) <> 0 THEN
    X.Value = X.Value & 'Positive'
  END
  ! Compares
  IF BAND(pSymbolType, SymbolType:CompareEqual) <> 0 THEN
    X.Value = X.Value & 'Equal'
  END
  IF BAND(pSymbolType, SymbolType:CompareNot) <> 0 THEN
    X.Value = X.Value & 'Not'
  END
  IF BAND(pSymbolType, SymbolType:CompareLess) <> 0 THEN
    X.Value = X.Value & 'Less'
  END
  IF BAND(pSymbolType, SymbolType:CompareGreater) <> 0 THEN
    X.Value = X.Value & 'Greater'
  END
  ! Binaries
  IF BAND(pSymbolType, SymbolType:BinaryExclusive) <> 0 THEN
    X.Value = X.Value & 'Exclusive'
  END
  IF BAND(pSymbolType, SymbolType:BinaryInverse) <> 0 THEN
    X.Value = X.Value & 'Inverse'
  END
  IF BAND(pSymbolType, SymbolType:BinaryShiftLeft) <> 0 THEN
    X.Value = X.Value & 'Shift'
  END
  IF BAND(pSymbolType, SymbolType:BinaryAnd) <> 0 THEN
    X.Value = X.Value & 'And'
  END
  IF BAND(pSymbolType, SymbolType:BinaryOr) <> 0 THEN
    X.Value = X.Value & 'Or'
  END
  ! Choices
  IF BAND(pSymbolType, SymbolType:ChoiceQuestion) <> 0 THEN
    X.Value = X.Value & 'Question'
  END
  IF BAND(pSymbolType, SymbolType:ChoiceAnswer) <> 0 THEN
    X.Value = X.Value & 'Answer'
  END
  !
  IF LEN(X.Value) = 0 THEN
    X.Value = X.Value & pSymbolType
  END
  RETURN X.Value

  
FormulaTranslator.ToString  PROCEDURE()!, STRING, VIRTUAL
  CODE
  RETURN SELF.ToStringStack(SELF.Input) & ' -> ' & SELF.ToStringStack(SELF.Output) & ' -> ' & SELF.ToStringStack(SELF.Solve)


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
      !X.Value = '( ' & SELF.ToStringSymbol(pStack) & ' )' & X.Value
      X.Value = SELF.ToStringSymbol(pStack) & X.Value
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
  X.Value = SELF.GetTypeName(pSymbol.SymbolType) & ':''' & pSymbol.Symbol & ''' #' & pSymbol.SymbolPriority
  RETURN X.Value
  

! Debug

        
FormulaTranslator.DebugOutput   PROCEDURE(STRING pMessage)!, VIRTUAL
Msg CSTRING(2048)
  CODE
  Msg = 'FormulaTranslator: ' & pMessage
  OutputDebugString(Msg)


