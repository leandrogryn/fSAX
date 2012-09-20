!-----------------------------------------------------------------------------!
! fSAX: Fortran implementation of the Simple API for XML (SAX)                !
! Copyright (C) 2010-2012 Leandro Gryngarten                                  !
!                                                                             !
! This file is part of fSAX.                                                  !
!                                                                             !
! fSAX is free software: you can redistribute it and/or modify                !
! it under the terms of the GNU Lesser General Public License as published by !
! the Free Software Foundation, either version 3 of the License, or           !
! (at your option) any later version.                                         !
!                                                                             !
! fSAX is distributed in the hope that it will be useful,                     !
! but WITHOUT ANY WARRANTY; without even the implied warranty of              !
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               !
! GNU Lesser General Public License for more details.                         !
!                                                                             !
! You should have received a copy of the GNU Lesser General Public License    !
! along with fSAX.  If not, see <http://www.gnu.org/licenses/>.               !
!-----------------------------------------------------------------------------!


MODULE ContentHandlerAttr_m
! This module stores the information required from the XML file.
  USE fSAX_m, ONLY: Locator_t
  IMPLICIT NONE
  PUBLIC
  SAVE

  ! Information needed from the XML file:
  CHARACTER(LEN=80) :: title, typeOfBody, author

  INTEGER :: nElements = 0

  ! Internal variables
  LOGICAL :: docOpen = .FALSE.
  CHARACTER(LEN=10) :: tagName
  TYPE(Locator_t), POINTER :: locator

  LOGICAL, PARAMETER :: prints = .TRUE.
CONTAINS

  SUBROUTINE characters(ch, start, length)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: ch
    INTEGER, INTENT(IN) :: start, length

    IF(prints) WRITE(*,'(A,A,A)') "Character_",ch,"_"
  END SUBROUTINE characters


  SUBROUTINE endDocument()
    IMPLICIT NONE

    docOpen = .FALSE.
    IF(prints) WRITE(*,'(A)') "endDocument"
  END SUBROUTINE endDocument


  SUBROUTINE endElement(uri, localName, qName)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: uri, localName, qName

    IF(prints) WRITE(*,'(A,A,A)') "endElement_",TRIM(tagName),"_",localName
    tagName = ""
    nElements = nElements + 1
  END SUBROUTINE endElement


  SUBROUTINE endPrefixMapping(prefix)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: prefix !< prefix of the scope being ended
  END SUBROUTINE endPrefixMapping


  SUBROUTINE ignorableWhitespace(ch, start, length)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: ch !< Received string of whitespaces
    INTEGER, INTENT(IN) :: start !< Index of the first character (always 1).
    INTEGER, INTENT(IN) :: length !< Length of the string
  END SUBROUTINE ignorableWhitespace


  SUBROUTINE processingInstruction(insTarget, insData)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: insTarget !< target to process
    CHARACTER(LEN=*), INTENT(IN) :: insData !< data required (if any).
  END SUBROUTINE processingInstruction


  SUBROUTINE setDocumentLocator(locator_p)
    USE fSAX_m, ONLY: Locator_t
    IMPLICIT NONE
    TYPE(Locator_t), POINTER :: locator_p

    locator => locator_p
  END SUBROUTINE setDocumentLocator


  SUBROUTINE skippedEntity(entityName)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: entityName !< name of the entity skipped
  END SUBROUTINE skippedEntity


  SUBROUTINE startDocument()
    IMPLICIT NONE

    docOpen = .TRUE.
    IF(prints) WRITE(*,'(A)') "startDocument"
  END SUBROUTINE startDocument


  SUBROUTINE startElement(uri, localName, qName, attr)
    USE fSAX_m, ONLY: Attributes_t, getValue, ASSIGNMENT(=), getLocalName, &
      getLength
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: uri, localName, qName
    TYPE(Attributes_t), INTENT(IN) :: attr

    INTEGER :: i
    CHARACTER(LEN=20) :: text1, text2

    IF(prints) WRITE(*,'(A,A,A)') "startElement_",localName,"_"
    DO i=1,getLength(attr)
      text1 = getLocalName(attr,i)
      text2 = getValue(attr,i)
      IF(prints) WRITE(*,*) "attr_",i,"_",TRIM(text1),"=",TRIM(text2),"_"
    END DO
    tagName = localName
  END SUBROUTINE startElement


  SUBROUTINE startPrefixMapping(prefix, uri)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: prefix !< prefix of the scope that started
    CHARACTER(LEN=*), INTENT(IN) :: uri !< URI of the scope that started
  END SUBROUTINE startPrefixMapping

END MODULE ContentHandlerAttr_m


MODULE ErrorHandlerAttr_m
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

END MODULE ErrorHandlerAttr_m


PROGRAM ReadXML
! This program shows how to read an XML file.
  USE ContentHandlerAttr_m
  USE ErrorHandlerAttr_m
  USE fSAX_m
  IMPLICIT NONE

  CHARACTER(LEN=80) :: filename
  TYPE(XMLReader_t) :: reader
  CHARACTER(LEN=80) :: version

  IF(COMMAND_ARGUMENT_COUNT()==1) THEN
    CALL GET_COMMAND_ARGUMENT(1, filename)

    ! Print parser version
    version = getProperty(reader, 'version')
    WRITE(*,'(A,A)') 'Parser version: ',TRIM(version)

    CALL parse(reader, "file:/./"//filename, &
        characters, endDocument, endElement, endPrefixMapping, &
        ignorableWhitespace, processingInstruction, setDocumentLocator, &
        skippedEntity, startDocument, startElement, startPrefixMapping, &
        error, fatalError, warning)

    WRITE(*,'(A,I6)') 'Number of elements= ', nElements
  END IF
END PROGRAM ReadXML

