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


MODULE ContentHandlerEntity_m
! This module stores the information required from the XML file.
  USE fSAX_m, ONLY: Locator_t
  USE iso_varying_string
  IMPLICIT NONE
  PUBLIC
  SAVE

  INTEGER :: nErr = 0
  LOGICAL :: bodyFound = .FALSE.
  LOGICAL :: titleFound = .FALSE.

  ! Internal variables
  LOGICAL :: docOpen = .FALSE.
  CHARACTER(LEN=10) :: tagName
  TYPE(Locator_t), POINTER :: locator
  TYPE(varying_string) :: buffer

  LOGICAL, PARAMETER :: prints = .TRUE.
CONTAINS

  SUBROUTINE characters(ch, start, length)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: ch
    INTEGER, INTENT(IN) :: start, length

    buffer = buffer // ch(start:start+length-1)
  END SUBROUTINE characters


  SUBROUTINE endDocument()
    IMPLICIT NONE

    docOpen = .FALSE.
  END SUBROUTINE endDocument


  SUBROUTINE endElement(uri, localName, qName)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: uri, localName, qName

    IF(localName == 'title') THEN
      titleFound = .TRUE.
      IF(buffer /= 'XML & Document') THEN
        WRITE(*,'(A)') 'Wrong title (entity?).'
        nErr = nErr + 1
      END IF
    END IF
    tagName = ""
    buffer = ""
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

    buffer = ""
    docOpen = .TRUE.
  END SUBROUTINE startDocument


  SUBROUTINE startElement(uri, localName, qName, attr)
    USE fSAX_m, ONLY: Attributes_t, getValue, ASSIGNMENT(=), getLocalName, &
      getLength
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: uri, localName, qName
    TYPE(Attributes_t), INTENT(IN) :: attr

    INTEGER :: i
    CHARACTER(LEN=20) :: text1, text2

    IF(localName == 'body') THEN
      bodyFound = .TRUE.
      IF(getLength(attr) /= 2) THEN
        WRITE(*,*) 'Wrong number of attributes.'
        nErr = nErr + 1
      END IF
      DO i=1,getLength(attr)
        text1 = getLocalName(attr,i)
        text2 = getValue(attr,i)
        SELECT CASE(text1)
        CASE('type')
          IF(TRIM(text2) /= 'n>ormal<') THEN
            WRITE(*,*) 'Wrong attribute value (1).'
            nErr = nErr + 1
          END IF
        CASE('attr2')
          IF(TRIM(text2) /= 'val2') THEN
            WRITE(*,*) 'Wrong attribute value (2).'
            nErr = nErr + 1
          END IF
        CASE DEFAULT
          WRITE(*,*) 'Wrong attribute name.'
          nErr = nErr + 1
        END SELECT
      END DO
    END IF
    tagName = localName
  END SUBROUTINE startElement


  SUBROUTINE startPrefixMapping(prefix, uri)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: prefix !< prefix of the scope that started
    CHARACTER(LEN=*), INTENT(IN) :: uri !< URI of the scope that started
  END SUBROUTINE startPrefixMapping

END MODULE ContentHandlerEntity_m


MODULE ErrorHandlerEntity_m
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

END MODULE ErrorHandlerEntity_m

!> /brief Test the entities
PROGRAM ReadXML
! This program shows how to read an XML file.
  USE ContentHandlerEntity_m
  USE ErrorHandlerEntity_m
  USE fSAX_m
  IMPLICIT NONE

  CHARACTER(LEN=*), PARAMETER :: filename = 'tests/test.xml'
  TYPE(XMLReader_t) :: reader
  CHARACTER(LEN=80) :: version

  CALL parse(reader, "file:/./"//filename, &
      characters, endDocument, endElement, endPrefixMapping, &
      ignorableWhitespace, processingInstruction, setDocumentLocator, &
      skippedEntity, startDocument, startElement, startPrefixMapping, &
      error, fatalError, warning)

  IF(.NOT.bodyFound) THEN
    WRITE(*,'(A)') 'Element "body" was not found.'
    nErr = nErr + 1
  END IF
  IF(.NOT.titleFound) THEN
    WRITE(*,'(A)') 'Element "title" was not found.'
    nErr = nErr + 1
  END IF
  IF(nErr == 0) WRITE(*,'(A)') 'TestPassed'
END PROGRAM ReadXML

