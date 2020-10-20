!ABCIncludeFile


  OMIT('_EndOfInclude_', _FormulaTranslator_)


  INCLUDE('FormulaTranslator.inc'),ONCE

_FormulaTranslator_ EQUATE(1)

SymbolSize          EQUATE(33)

SymbolStack         QUEUE, TYPE
Symbol                CSTRING(SymbolSize)
                    END


FormulaTranslator   CLASS, TYPE, MODULE('FormulaTranslator.CLW'), LINK('FormulaTranslator.CLW', _ABCLinkMode_), DLL(_ABCDLLMode_)

! Data
Input                 &SymbolStack

! Init
Construct             PROCEDURE, PROTECTED
Destruct              PROCEDURE, PROTECTED, VIRTUAL
Destroy               PROCEDURE(), VIRTUAL

! Properties
Evaluate              PROCEDURE(STRING pFormula), REAL, VIRTUAL
Parse                 PROCEDURE(STRING pFormula), VIRTUAL
Parse                 PROCEDURE(SymbolStack pInput, STRING pFormula), VIRTUAL, PRIVATE
Push                  PROCEDURE(SymbolStack pStack, STRING pSymbol), VIRTUAL
Pop                   PROCEDURE(SymbolStack pStack), STRING, VIRTUAL
ToString              PROCEDURE(), STRING, VIRTUAL

! Debug
DebugOutput           PROCEDURE(STRING pMessage), VIRTUAL
                    END

  _EndOfInclude_
