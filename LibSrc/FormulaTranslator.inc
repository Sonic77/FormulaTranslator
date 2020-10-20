!ABCIncludeFile


  OMIT('_EndOfInclude_', _gcCString_)


  INCLUDE('gcObject.inc'),ONCE

_gcCString_          EQUATE(1)

! This is a cstring garbage collector, you can use this to reserve memory inside a faction then return this value and memory will be release when this object's scope ends
!gcCString             CLASS(gcObject), TYPE, MODULE('gcCString.CLW'), LINK('gcCString.CLW', _ABCLinkMode_), DLL(_ABCDLLMode_)
gcCString             CLASS, TYPE, MODULE('gcCString.CLW'), LINK('gcCString.CLW', _ABCLinkMode_), DLL(_ABCDLLMode_)

! Data
Value                 &CSTRING

! Init
Construct             PROCEDURE, PROTECTED
Destruct              PROCEDURE, PROTECTED, VIRTUAL
Destroy               PROCEDURE(), VIRTUAL
_New                  PROCEDURE(LONG pSize), *gcCString, VIRTUAL !, STATIC ! Causes GPF when calling as static

! Properties
Resize                PROCEDURE(LONG pSize), VIRTUAL

! Debug
DebugOutput           PROCEDURE(STRING pMessage), VIRTUAL
                    END

  _EndOfInclude_
