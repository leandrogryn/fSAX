! This is a template source code to modify and use by an application that runs
! fSAX.

! Language: Fortran 90/95

! Content:
! --------
!
! Content Handling
! ----------------
! MODULE ContentHandler_m
!   SUBROUTINE characters(ch, start, length)
!   SUBROUTINE endDocument()
!   SUBROUTINE endElement(uri, localName, qName)
!   SUBROUTINE endPrefixMapping(prefix)
!   SUBROUTINE ignorableWhitespace(ch, start, length)
!   SUBROUTINE processingInstruction(insTarget, insData)
!   SUBROUTINE setDocumentLocator(locator_p)
!   SUBROUTINE skippedEntity(entityName)
!   SUBROUTINE startDocument()
!   SUBROUTINE startElement(uri, localName, qName, attr)
!   SUBROUTINE startPrefixMapping(prefix, uri)
!
! Error Handling
! --------------
! MODULE ErrorHandler_m
!   SUBROUTINE error(exception)
!   SUBROUTINE fatalError(exception)
!   SUBROUTINE warning(exception)

!-----------------------------------------------------------------------------!
! Content Handling:
!-----------------------------------------------------------------------------!
MODULE ContentHandler_m
! This module stores the information required from the XML file.
  USE fSAX_m, ONLY: Locator_t
  IMPLICIT NONE
  PUBLIC
  SAVE

  ! Internal variables
  LOGICAL :: docOpen = .FALSE.
  CHARACTER(LEN=32) :: tagName
  TYPE(Locator_t), POINTER :: locator=>NULL()

  ! Add user code here:


CONTAINS
!-----------------------------------------------------------------------------!
  SUBROUTINE characters(ch, start, length)
    !USE UserDef_m
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: ch
    INTEGER, INTENT(IN) :: start, length

    ! Add user code here:


  END SUBROUTINE characters

!-----------------------------------------------------------------------------!
  SUBROUTINE endDocument()
    !USE UserDef_m
    IMPLICIT NONE

    docOpen = .FALSE.

    ! Add user code here:


  END SUBROUTINE endDocument

!-----------------------------------------------------------------------------!
  SUBROUTINE endElement(uri, localName, qName)
    !USE UserDef_m
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: uri, localName, qName

    tagName = ""

    ! Add user code here:


  END SUBROUTINE endElement

!-----------------------------------------------------------------------------!
  SUBROUTINE endPrefixMapping(prefix)
    !USE UserDef_m
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: prefix

    ! Add user code here:


  END SUBROUTINE endPrefixMapping

!-----------------------------------------------------------------------------!
  SUBROUTINE ignorableWhitespace(ch, start, length)
    !USE UserDef_m
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: ch
    INTEGER, INTENT(IN) :: start, length

    ! Add user code here:


  END SUBROUTINE ignorableWhitespace

!-----------------------------------------------------------------------------!
  SUBROUTINE processingInstruction(insTarget, insData)
    !USE UserDef_m
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: insTarget, insData

    ! Add user code here:


  END SUBROUTINE processingInstruction

!-----------------------------------------------------------------------------!
  SUBROUTINE setDocumentLocator(locator_p)
    USE fSAX_m, ONLY: Locator_t
    !USE UserDef_m
    IMPLICIT NONE
    TYPE(Locator_t), POINTER :: locator_p

    locator => locator_p

    ! Add user code here:


  END SUBROUTINE setDocumentLocator

!-----------------------------------------------------------------------------!
  SUBROUTINE skippedEntity(entityName)
    !USE UserDef_m
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: entityName

    ! Add user code here:


  END SUBROUTINE skippedEntity

!-----------------------------------------------------------------------------!
  SUBROUTINE startDocument()
    !USE UserDef_m
    IMPLICIT NONE

    docOpen = .TRUE.

    ! Add user code here:


  END SUBROUTINE startDocument

!-----------------------------------------------------------------------------!
  SUBROUTINE startElement(uri, localName, qName, attr)
    USE fSAX_m, ONLY: Attributes_t, getValue, ASSIGNMENT(=)
    !USE UserDef_m
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: uri, localName, qName
    TYPE(Attributes_t), INTENT(IN) :: attr

    tagName = localName

    ! Add user code here:


  END SUBROUTINE startElement

!-----------------------------------------------------------------------------!
  SUBROUTINE startPrefixMapping(prefix, uri)
    !USE UserDef_m
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: prefix, uri

    ! Add user code here:


  END SUBROUTINE startPrefixMapping

END MODULE ContentHandler_m

!-----------------------------------------------------------------------------!
! Error Handling:
!-----------------------------------------------------------------------------!
MODULE ErrorHandler_m
  IMPLICIT NONE
  PUBLIC

CONTAINS

!-----------------------------------------------------------------------------!
  SUBROUTINE error(exception)
    USE SAXParseException_m, ONLY: SAXParseException_t
    IMPLICIT NONE
    TYPE(SAXParseException_t), INTENT(IN) :: exception

    ! Add user code here:


  END SUBROUTINE error

!-----------------------------------------------------------------------------!
  SUBROUTINE fatalError(exception)
    USE SAXParseException_m, ONLY: SAXParseException_t
    IMPLICIT NONE
    TYPE(SAXParseException_t), INTENT(IN) :: exception

    ! Add user code here:


  END SUBROUTINE fatalError

!-----------------------------------------------------------------------------!
  SUBROUTINE warning(exception)
    USE SAXParseException_m, ONLY: SAXParseException_t
    IMPLICIT NONE
    TYPE(SAXParseException_t), INTENT(IN) :: exception

    ! Add user code here:


  END SUBROUTINE warning

END MODULE ErrorHandler_m
