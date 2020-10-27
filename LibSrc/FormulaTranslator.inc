!ABCIncludeFile


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

StackDirection      ITEMIZE(0), PRE(StackDirection)
None                  EQUATE
FIFO                  EQUATE ! First In First Out
LIFO                  EQUATE ! Last  In First Out
FILO                  EQUATE ! First In Last  Out
LILO                  EQUATE ! Last  In Last  Out
                    END

SymbolType          ITEMIZE, PRE(SymbolType)
! Bit x10                     3         2         1
! Bit x1                     10987654321098765432109876543210
Unknown               EQUATE(00000000000000000000000000000000B) ! 0
ObjectNull            EQUATE(00000000000000000000000000000001B) ! 1
Variable              EQUATE(00000000000000000000000000000010B) ! 2
Attribute             EQUATE(00000000000000000000000000000100B) ! 4
Number                EQUATE(00000000000000000000000000001000B) ! 8
Functie               EQUATE(00000000000000000000000000010000B) ! 16
Text                  EQUATE(00000000000000000000000000100000B) ! 32
BracketOpen           EQUATE(00000000000000000000000001000000B) ! 64
BracketClose          EQUATE(00000000000000000000000010000000B) ! 128
Concatenate           EQUATE(00000000000000000000000100000000B) ! 256
OperatorSign          EQUATE(00000000000000000000001000000000B) ! 512
OperatorAdd           EQUATE(00000000000000000000010000000000B) ! 1024
OperatorSubstract     EQUATE(00000000000000000000100000000000B) ! 2048
OperatorMultiply      EQUATE(00000000000000000001000000000000B) ! 4096
OperatorDivide        EQUATE(00000000000000000010000000000000B) ! 8192
OperatorModulo        EQUATE(00000000000000000100000000000000B) ! 16384
OperatorPower         EQUATE(00000000000000001000000000000000B) ! 32768
OperatorRoot          EQUATE(00000000000000010000000000000000B) ! 65536
CompareEqual          EQUATE(00000000000000100000000000000000B) ! 131072
CompareNot            EQUATE(00000000000001000000000000000000B) ! 262144
CompareLess           EQUATE(00000000000010000000000000000000B) ! 524288
CompareGreater        EQUATE(00000000000100000000000000000000B) ! 1048576
BinaryAnd             EQUATE(00000000001000000000000000000000B) ! 2097152
BinaryOr              EQUATE(00000000010000000000000000000000B) ! 4194304
BinaryExclusive       EQUATE(00000000100000000000000000000000B) ! 8388608
BinaryNot             EQUATE(00000001000000000000000000000000B) ! 16777216
Label                 EQUATE(SymbolType:Attribute + SymbolType:Variable)
Value                 EQUATE(SymbolType:Number + SymbolType:ObjectNull + SymbolType:Text)
Bracket               EQUATE(SymbolType:BracketOpen + SymbolType:BracketClose)
Operator              EQUATE(SymbolType:OperatorAdd + SymbolType:OperatorSubstract + SymbolType:OperatorMultiply + SymbolType:OperatorDivide + SymbolType:OperatorModulo + SymbolType:OperatorPower + SymbolType:OperatorRoot)
Compare               EQUATE(SymbolType:CompareEqual + SymbolType:CompareNot + SymbolType:CompareLess + SymbolType:CompareGreater)
Binary                EQUATE(SymbolType:BinaryAnd + SymbolType:BinaryOr + SymbolType:BinaryExclusive + SymbolType:BinaryNot)
                    END


FormulaTranslator   CLASS, TYPE, MODULE('FormulaTranslator.CLW'), LINK('FormulaTranslator.CLW', _ABCLinkMode_), DLL(_ABCDLLMode_)

! Data
Input                 &SymbolStack
Output                &SymbolStack

! Init
Construct             PROCEDURE, PROTECTED
Destruct              PROCEDURE, PROTECTED, VIRTUAL
Destroy               PROCEDURE(), VIRTUAL
Reset                 PROCEDURE(), VIRTUAL

! Properties
Evaluate              PROCEDURE(STRING pFormula), STRING, VIRTUAL
Parse                 PROCEDURE(STRING pFormula), VIRTUAL
Parse                 PROCEDURE(*SymbolStack pInput, STRING pFormula), VIRTUAL, PRIVATE
InFixToPostFix        PROCEDURE(), VIRTUAL
InFixToPostFix        PROCEDURE(*SymbolStack pInput), *SymbolStack, VIRTUAL, PRIVATE
CheckInFix            PROCEDURE(*SymbolStack pStack), VIRTUAL, PRIVATE
PushSymbol            PROCEDURE(*SymbolStack pStack, STRING pSymbol, BYTE pStackDirection = StackDirection:FIFO), VIRTUAL
PushSymbol            PROCEDURE(*SymbolStack pStack, STRING pSymbol, BYTE pSymbolType, BYTE pStackDirection = StackDirection:FIFO), VIRTUAL
PushSymbol            PROCEDURE(*SymbolStack pStack, *SymbolStackType pSymbol, BYTE pStackDirection = StackDirection:FIFO), VIRTUAL
PopSymbol             PROCEDURE(*SymbolStack pStack, BYTE pStackDirection = StackDirection:FIFO), STRING, VIRTUAL
PopSymbol             PROCEDURE(*SymbolStack pStack, *SymbolStackType pSymbol, BYTE pStackDirection = StackDirection:FIFO), VIRTUAL
PeekSymbol            PROCEDURE(*SymbolStack pStack, BYTE pStackDirection = StackDirection:FIFO), STRING, VIRTUAL
PeekSymbol            PROCEDURE(*SymbolStack pStack, *SymbolStackType pSymbol, BYTE pStackDirection = StackDirection:FIFO), VIRTUAL
ToString              PROCEDURE(), STRING, VIRTUAL
ToStringStack         PROCEDURE(*SymbolStack pStack, BYTE pFull=False), STRING, VIRTUAL
ToStringSymbol        PROCEDURE(*SymbolStackType pSymbol), STRING, VIRTUAL

! Debug
DebugOutput           PROCEDURE(STRING pMessage), VIRTUAL
                    END

  _EndOfInclude_
