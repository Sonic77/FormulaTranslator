  MEMBER

  MAP
    MODULE('')
      OutputDebugString(*CSTRING), PASCAL, RAW, NAME('OutputDebugStringA')
    END
  END

!  INCLUDE('gcObject.inc'),ONCE
  INCLUDE('gcCString.inc'),ONCE


! Init


gcCString.Construct    PROCEDURE!, PROTECTED
  CODE

  
gcCString.Destruct     PROCEDURE!, PROTECTED, VIRTUAL
  CODE
  SELF.Destroy()


gcCString.Destroy    PROCEDURE()!, VIRTUAL
  CODE
  DISPOSE(SELF.Value)

  
gcCString._New        PROCEDURE(LONG pSize) !, STATIC ! Causes GPF when calling as static
sString               &gcCString
  CODE
  IF pSize <= 1 THEN pSize = 1 END
  SELF.DebugOutput('_New(' & pSize & ')')
  sString &= NEW gcCString
  sString.Value[1] = '<0>'
  RETURN sString

  
! Properties


gcCString.Resize      PROCEDURE(LONG pSize)!, VIRTUAL
sValue                &CSTRING
  CODE
  sValue &= NEW CSTRING(pSize)
  !SELF.Destroy()
  SELF.Value &= sValue

  
! Debug

        
gcCString.DebugOutput  PROCEDURE(STRING pMessage)!, VIRTUAL
Msg CSTRING(2048)
  CODE
  Msg = 'gcCString: ' & pMessage
  OutputDebugString(Msg)


