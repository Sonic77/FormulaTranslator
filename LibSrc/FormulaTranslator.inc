!ABCIncludeFile

! TODO: Use Equates list for Constands/Variables/Attributes


  OMIT('_EndOfInclude_', _FormulaTranslator_)


  INCLUDE('FormulaTranslator.inc'),ONCE

_FormulaTranslator_ EQUATE(1)

SymbolSize          EQUATE(33)

SymbolStackType     GROUP, TYPE
Symbol                CSTRING(SymbolSize)
SymbolType            UNSIGNED
SymbolPriority        BYTE
                    END

SymbolStack         QUEUE(SymbolStackType)
                    END

EquateListType      GROUP(SymbolStackType)
ReplaceSymbol         CSTRING(SymbolSize)
ReplaceSymbolType     UNSIGNED
                    END

EquateList          QUEUE(EquateListType)
                    END


StackDirection      ITEMIZE(0), PRE(StackDirection)
None                  EQUATE
FIFO                  EQUATE ! First In First Out (Adds in front, Removes from the end)
LIFO                  EQUATE ! Last  In First Out (Adds at the end, Removes from the end)
FILO                  EQUATE ! First In Last  Out (Adds in front, Removes from the front)
LILO                  EQUATE ! Last  In Last  Out (Adds at the end, Removes from the front)
                    END

SymbolType          ITEMIZE, PRE(SymbolType)
! Bit x10                     3         2         1
! Bit x1                     10987654321098765432109876543210
Unknown               EQUATE(00000000000000000000000000000000B) ! 0
Constant              EQUATE(00000000000000000000000000000001B) ! 1
Number                EQUATE(00000000000000000000000000000010B) ! 2
Text                  EQUATE(00000000000000000000000000000100B) ! 4
Values                EQUATE(SymbolType:Constant + SymbolType:Number + SymbolType:Text)
Variable              EQUATE(00000000000000000000000000001000B) ! 8
Attribute             EQUATE(00000000000000000000000000010000B) ! 16
Labels                EQUATE(SymbolType:Variable + SymbolType:Attribute)
!                      EQUATE(00000000000000000000000000100000B) ! 32
Concatenate           EQUATE(00000000000000000000000001000000B) ! 256
BracketOpen           EQUATE(00000000000000000000000010000000B) ! 64
BracketClose          EQUATE(00000000000000000000000100000000B) ! 128
Brackets              EQUATE(SymbolType:BracketOpen + SymbolType:BracketClose)
OperatorAdd           EQUATE(00000000000000000000001000000000B) ! 512
OperatorSubstract     EQUATE(00000000000000000000010000000000B) ! 1024
OperatorMultiply      EQUATE(00000000000000000000100000000000B) ! 2048
OperatorDivide        EQUATE(00000000000000000001000000000000B) ! 4096
OperatorModulo        EQUATE(00000000000000000010000000000000B) ! 8192
OperatorPower         EQUATE(00000000000000000100000000000000B) ! 16384
OperatorRoot          EQUATE(00000000000000001000000000000000B) ! 32768
RightAssociative      EQUATE(SymbolType:OperatorPower + SymbolType:OperatorRoot)
LeftAssociative       EQUATE(SymbolType:OperatorAdd + SymbolType:OperatorSubstract + SymbolType:OperatorMultiply + SymbolType:OperatorDivide + SymbolType:OperatorModulo)
Operators             EQUATE(SymbolType:OperatorAdd + SymbolType:OperatorSubstract + SymbolType:OperatorMultiply + SymbolType:OperatorDivide + SymbolType:OperatorModulo + SymbolType:OperatorPower + SymbolType:OperatorRoot)
UnaryPositive         EQUATE(00000000000000010000000000000000B) ! 65536
UnaryNegative         EQUATE(00000000000000100000000000000000B) ! 131072
UnaryIncDec           EQUATE(00000000000001000000000000000000B) ! 262144
UnaryIncrement        EQUATE(SymbolType:UnaryIncDec + SymbolType:UnaryPositive)
UnaryDecrement        EQUATE(SymbolType:UnaryIncDec + SymbolType:UnaryNegative)
Unaries               EQUATE(SymbolType:UnaryPositive + SymbolType:UnaryNegative + SymbolType:UnaryIncDec)
CompareEqual          EQUATE(00000000000010000000000000000000B) ! 524288
CompareNot            EQUATE(00000000000100000000000000000000B) ! 1048576
CompareLess           EQUATE(00000000001000000000000000000000B) ! 2097152
CompareGreater        EQUATE(00000000010000000000000000000000B) ! 4194304
Compares              EQUATE(SymbolType:CompareEqual + SymbolType:CompareNot + SymbolType:CompareLess + SymbolType:CompareGreater)
BinaryAnd             EQUATE(00000000100000000000000000000000B) ! 8388608
BinaryOr              EQUATE(00000001000000000000000000000000B) ! 16777216
BinaryExclusive       EQUATE(00000010000000000000000000000000B) ! 33554432
BinaryInverse         EQUATE(00000100000000000000000000000000B) ! 67108864
BinaryShiftLeft       EQUATE(00001000000000000000000000000000B) ! 134217728
BinaryShiftRight      EQUATE(SymbolType:BinaryShiftLeft + SymbolType:BinaryInverse)
Binaries              EQUATE(SymbolType:BinaryAnd + SymbolType:BinaryOr + SymbolType:BinaryExclusive + SymbolType:BinaryInverse + SymbolType:BinaryShiftLeft)
ChoiceQuestion        EQUATE(00010000000000000000000000000000B) ! 268435456
ChoiceAnswer          EQUATE(00100000000000000000000000000000B) ! 536870912
Choices               EQUATE(SymbolType:ChoiceQuestion + SymbolType:ChoiceAnswer)
Functie               EQUATE(01000000000000000000000000000000B) ! 1073741824
FunctieParameter      EQUATE(10000000000000000000000000000000B) ! 2147483648 (UNUSED)
UnaryOperations       EQUATE(SymbolType:Unaries + SymbolType:BinaryInverse)
BinaryOperations      EQUATE(SymbolType:Operators + SymbolType:Compares + (SymbolType:Binaries - SymbolType:BinaryInverse) + SymbolType:Choices)
AllOperations         EQUATE(SymbolType:UnaryOperations + SymbolType:BinaryOperations)
                    END


FormulaTranslator   CLASS, TYPE, MODULE('FormulaTranslator.CLW'), LINK('FormulaTranslator.CLW', _ABCLinkMode_), DLL(_ABCDLLMode_)

! Data
Input                 &SymbolStack ! InFix symbol stack, filled after Parse()
Output                &SymbolStack ! PostFix/RPN symbol stack, filled after Parse() and InFixToPostFix() 
Solve                 &SymbolStack ! Evaluate/Resolve Solution stack, filled after ResolvePostFix()
Equates               &EquateList  ! List of constants/varibles/attributes
IndexGetLabel         LONG, PRIVATE ! Internal use for GetFirstLabel(), GetNextLabel()

! Init
Construct             PROCEDURE, PROTECTED
Destruct              PROCEDURE, PROTECTED, VIRTUAL
Destroy               PROCEDURE(), VIRTUAL
Reset                 PROCEDURE(), VIRTUAL

! Properties
Evaluate              PROCEDURE(STRING pFormula), STRING, VIRTUAL
Parse                 PROCEDURE(STRING pFormula), VIRTUAL
Parse                 PROCEDURE(*SymbolStack pInput, *SymbolStack pOutput, STRING pFormula), VIRTUAL, PRIVATE
InFixToPostFix        PROCEDURE(), VIRTUAL
InFixToPostFix        PROCEDURE(*SymbolStack pInput, *SymbolStack pOutput), VIRTUAL, PRIVATE
CheckInFix            PROCEDURE(), VIRTUAL, PRIVATE
CheckInFix            PROCEDURE(*SymbolStack pStack), VIRTUAL, PRIVATE
ResolvePostFix        PROCEDURE(), VIRTUAL, PRIVATE
ResolvePostFix        PROCEDURE(*SymbolStack pStack), VIRTUAL, PRIVATE
ResolveFunction       PROCEDURE(*SymbolStackType pTermInOut, *SymbolStackType pSymbol), VIRTUAL, PRIVATE
ResolveSymbol         PROCEDURE(*SymbolStackType pTermInOut), VIRTUAL, PRIVATE
ResolveSymbol         PROCEDURE(*SymbolStackType pTermInOut, *SymbolStackType pSymbol), VIRTUAL, PRIVATE
ResolveSymbol         PROCEDURE(*SymbolStackType pTermInOut, *SymbolStackType pSymbol, *SymbolStackType pTerm2), VIRTUAL, PRIVATE

! Equate functions before parse
AddConstant           PROCEDURE(STRING pSymbol, STRING pReplaceSymbol), VIRTUAL
AddVariable           PROCEDURE(STRING pSymbol, STRING pReplaceSymbol), VIRTUAL
AddAttribute          PROCEDURE(STRING pSymbol, STRING pReplaceSymbol), VIRTUAL
AddEquate             PROCEDURE(STRING pSymbol, UNSIGNED pSymbolType, STRING pReplaceSymbol), VIRTUAL
AddEquate             PROCEDURE(STRING pSymbol, UNSIGNED pSymbolType, STRING pReplaceSymbol, UNSIGNED pReplaceSymbolType, BYTE pSymbolPriority=0), VIRTUAL
GetEquate             PROCEDURE(STRING pSymbol, UNSIGNED pSymbolType=SymbolType:Unknown), STRING, PROC, VIRTUAL
RemoveEquate          PROCEDURE(STRING pSymbol), STRING, PROC, VIRTUAL
FreeEquates           PROCEDURE(), VIRTUAL

! Equate functions after parse before resolve
GetFirstLabel         PROCEDURE(), STRING, VIRTUAL
GetNextLabel          PROCEDURE(), STRING, VIRTUAL
ReplaceLabel          PROCEDURE(STRING pReplaceSymbol), VIRTUAL
ReplaceLabel          PROCEDURE(STRING pSymbol, STRING pReplaceSymbol), VIRTUAL

! Stack functions
PushSymbol            PROCEDURE(*SymbolStack pStack, STRING pSymbol, BYTE pStackDirection = StackDirection:FIFO), VIRTUAL
PushSymbol            PROCEDURE(*SymbolStack pStack, STRING pSymbol, UNSIGNED pSymbolType, BYTE pStackDirection = StackDirection:FIFO), VIRTUAL
PushSymbol            PROCEDURE(*SymbolStack pStack, *SymbolStackType pSymbol, BYTE pStackDirection = StackDirection:FIFO), VIRTUAL
PopSymbol             PROCEDURE(*SymbolStack pStack, BYTE pStackDirection = StackDirection:FIFO), STRING, VIRTUAL
PopSymbol             PROCEDURE(*SymbolStack pStack, *SymbolStackType pSymbol, BYTE pStackDirection = StackDirection:FIFO), VIRTUAL
PeekSymbol            PROCEDURE(*SymbolStack pStack, BYTE pStackDirection = StackDirection:FIFO), STRING, VIRTUAL
PeekSymbol            PROCEDURE(*SymbolStack pStack, *SymbolStackType pSymbol, BYTE pStackDirection = StackDirection:FIFO), VIRTUAL

! Output functions
GetTypeName           PROCEDURE(UNSIGNED pSymbolType), STRING, VIRTUAL
ToString              PROCEDURE(), STRING, VIRTUAL
ToStringStack         PROCEDURE(*SymbolStack pStack, BYTE pFull=False), STRING, VIRTUAL
ToStringSymbol        PROCEDURE(*SymbolStackType pSymbol), STRING, VIRTUAL

! Debug
DebugOutput           PROCEDURE(STRING pMessage), VIRTUAL
                    END

  _EndOfInclude_
