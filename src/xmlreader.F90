!-----------------------------------------------------------------------------!
! fSAX: Fortran implementation of the Simple API for XML (SAX)                !
! Copyright (C) 2010-2012 Leandro D. Gryngarten                               !
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

!> Define the interface to read an XML file.

MODULE XMLReader_m
  USE iso_varying_string, ONLY: varying_string
  USE Locator_m, ONLY: Locator_t
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: &
    !getContentHandler, &
    !getDTDHandler, &
    !getEntityResolver, &
    !getErrorHandler, &
    getFeature, &
    getProperty, &
    parse, &
    !setContentHandler, &
    !setDTDHandler, &
    !setEntityResolver, &
    !setErrorHandler, &
    setFeature, &
    setProperty, &
    XMLReader_t
  SAVE
  INCLUDE 'config.h'

  !> Reader object.
  TYPE :: XMLReader_t
    PRIVATE
    TYPE(Locator_t) :: locator

    ! Error hanling:
    !LOGICAL :: errorHandlerSet = .FALSE.
    !INTEGER :: errorHandler

    !TYPE(ContentHandler_t), POINTER :: contentHandler => NULL()
    !TYPE(DTDHandler_t), POINTER :: dtdHandler => NULL()
    !TYPE(EntityResolver_t), POINTER :: entityResolver => NULL()

    ! Features:
    LOGICAL :: namespaces = .TRUE.
    LOGICAL :: validation = .FALSE.
  END TYPE XMLReader_t

  !> Call the parser to start reading an XML file.
  INTERFACE parse
    MODULE PROCEDURE parse_input
    MODULE PROCEDURE parse_systemId
  END INTERFACE parse

  !> Get the local name from a QName
  INTERFACE getLocalNameFromQName
    MODULE PROCEDURE getLocalNameFromQName_1
    MODULE PROCEDURE getLocalNameFromQName_2
  END INTERFACE getLocalNameFromQName

  !> Get the URI from a QName
  INTERFACE getUriFromQName
    MODULE PROCEDURE getUriFromQName_1
    MODULE PROCEDURE getUriFromQName_2
  END INTERFACE getUriFromQName

CONTAINS

  !> Check if the reader has a given feature.
  !> @param[in] reader Reader object.
  !> @param[in] featureName name of the feature to be verified.
  LOGICAL FUNCTION getFeature(reader, featureName)
    IMPLICIT NONE
    TYPE(XMLReader_t), INTENT(IN) :: reader
    CHARACTER(LEN=*), INTENT(IN) :: featureName

    SELECT CASE(featureName)
    CASE("http://www.xml.org/sax/features/namespaces")
      getFeature = reader%namespaces
    CASE("http://www.xml.org/sax/features/validation")
      getFeature = reader%validation
    CASE DEFAULT
      getFeature = .FALSE.
    END SELECT
  END FUNCTION getFeature

  !> Specify the value of a feature.
  !> @param[in,out] reader Reader object.
  !> @param[in] featureName name of the feature to be specified.
  !> @param[in] value Boolean value of the feature.
  SUBROUTINE setFeature(reader, featureName, value)
    IMPLICIT NONE
    TYPE(XMLReader_t), INTENT(INOUT) :: reader
    CHARACTER(LEN=*), INTENT(IN) :: featureName
    LOGICAL, INTENT(IN) :: value

    SELECT CASE(featureName)
    CASE("http://www.xml.org/sax/features/namespaces")
      reader%namespaces = value
    CASE("http://www.xml.org/sax/features/validation")
      reader%validation = value
    END SELECT
  END SUBROUTINE setFeature

  !> Get the value of a property of the reader.
  !> @param[in] reader Reader object.
  !> @param[in] propertyName Name of the property. Possible values: 'version'.
  TYPE(varying_string) FUNCTION getProperty(reader, propertyName)
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE
    TYPE(XMLReader_t), INTENT(IN) :: reader
    CHARACTER(LEN=*), INTENT(IN) :: propertyName

    SELECT CASE(propertyName)
    CASE('version')
      getProperty = version
    CASE DEFAULT
      getProperty = ''
    END SELECT
  END FUNCTION getProperty

  !> Set the value of a property of the reader.
  !> @param[in] reader Reader object.
  !> @param[in] propertyName Name of the property.
  !> @param[in] value Value to specify.
  SUBROUTINE setProperty(reader, propertyName, value)
    IMPLICIT NONE
    TYPE(XMLReader_t), INTENT(INOUT) :: reader
    CHARACTER(LEN=*), INTENT(IN) :: propertyName
    CHARACTER(LEN=*), INTENT(IN) :: value

  END SUBROUTINE setProperty

!  FUNCTION getDTDHandler(reader) RESULT(dtdHandler)
!    IMPLICIT NONE
!    TYPE(XMLReader_t), INTENT(IN), TARGET :: reader
!    TYPE(DTDHandler_t), POINTER :: dtdHandler
!
!    dtdHandler => reader%dtdHandler
!  END FUNCTION getDTDHandler
!
!  SUBROUTINE setDTDHandler(reader, dtdHandler)
!    IMPLICIT NONE
!    TYPE(XMLReader_t), INTENT(INOUT), TARGET :: reader
!    TYPE(DTDHandler_t), POINTER :: dtdHandler
!
!    reader%dtdHandler => dtdHandler
!  END SUBROUTINE setDTDHandler

!  FUNCTION getEntityResolver(reader) RESULT(entityResolver)
!    IMPLICIT NONE
!    TYPE(XMLReader_t), INTENT(IN), TARGET :: reader
!    TYPE(ErrorHandler_t), POINTER :: entityResolver
!
!    entityResolver => reader%entityResolver
!  END FUNCTION getEntityResolver
!
!  SUBROUTINE setEntityResolver(reader, entityResolver)
!    IMPLICIT NONE
!    TYPE(XMLReader_t), INTENT(INOUT), TARGET :: reader
!    TYPE(ErrorHandler_t), POINTER :: entityResolver
!
!    reader%entityResolver => entityResolver
!  END SUBROUTINE setEntityResolver

!  FUNCTION getErrorHandler(reader) RESULT(errorHandler)
!    IMPLICIT NONE
!    TYPE(XMLReader_t), INTENT(IN) :: reader
!    INTEGER :: errorHandler
!
!    errorHandler = reader%errorHandler
!  END FUNCTION getErrorHandler
!
!  SUBROUTINE setErrorHandler(reader, errorHandler)
!    IMPLICIT NONE
!    TYPE(XMLReader_t), INTENT(INOUT), TARGET :: reader
!    INTEGER, INTENT(IN) :: errorHandler
!
!    reader%errorHandlerSet = .TRUE.
!    reader%errorHandler = errorHandler
!  END SUBROUTINE setErrorHandler

  SUBROUTINE parse_input(reader, inputSource, &
      characters, endDocument, endElement, endPrefixMapping, &
      ignorableWhitespace, processingInstruction, setDocumentLocator, &
      skippedEntity, startDocument, startElement, startPrefixMapping, &
      error, fatalError, warning)
    USE InputSource_m, ONLY: InputSource_t, getSourceType, iSystemId, &
        getSystemId
    USE iso_varying_string, ONLY: char
    IMPLICIT NONE
    TYPE(XMLReader_t), INTENT(IN) :: reader
    TYPE(InputSource_t), INTENT(IN) :: inputSource
    EXTERNAL :: characters, endDocument, endElement, endPrefixMapping, &
      ignorableWhitespace, processingInstruction, setDocumentLocator, &
      skippedEntity, startDocument, startElement, startPrefixMapping, &
      error, fatalError, warning

    SELECT CASE(getSourceType(inputSource))
    CASE(iSystemId)
      CALL parse_systemId(reader, char(getSystemId(inputSource)), &
          characters, endDocument, endElement, endPrefixMapping, &
          ignorableWhitespace, processingInstruction, setDocumentLocator, &
          skippedEntity, startDocument, startElement, startPrefixMapping, &
          error, fatalError, warning)
    CASE DEFAULT
      STOP "Type of inputSource not implemented."
    END SELECT
  END SUBROUTINE parse_input

  !> Start parsing an XML file.
  !> @param[in] reader Reader object.
  !> @param[in] systemId Specify the file. For example: 'file:/../test.xml'.
  !> @param characters External procedure (Content Handler).
  !> @param endDocument External procedure (Content Handler).
  !> @param endElement External procedure (Content Handler).
  !> @param endPrefixMapping External procedure (Content Handler).
  !> @param ignorableWhitespace External procedure (Content Handler).
  !> @param processingInstruction External procedure (Content Handler).
  !> @param setDocumentLocator External procedure (Content Handler).
  !> @param skippedEntity External procedure (Content Handler).
  !> @param startDocument External procedure (Content Handler).
  !> @param startElement External procedure (Content Handler).
  !> @param startPrefixMapping External procedure (Content Handler).
  !> @param error External procedure (Error Handler).
  !> @param fatalError External procedure (Error Handler).
  !> @param warning External procedure (Error Handler).
  SUBROUTINE parse_systemId(reader, systemId, &
      characters, endDocument, endElement, endPrefixMapping, &
      ignorableWhitespace, processingInstruction, setDocumentLocator, &
      skippedEntity, startDocument, startElement, startPrefixMapping, &
      error, fatalError, warning)
    IMPLICIT NONE
    TYPE(XMLReader_t), INTENT(IN) :: reader
    CHARACTER(LEN=*), INTENT(IN) :: systemId
    EXTERNAL :: characters, endDocument, endElement, endPrefixMapping, &
      ignorableWhitespace, processingInstruction, setDocumentLocator, &
      skippedEntity, startDocument, startElement, startPrefixMapping, &
      error, fatalError, warning

    IF(LEN(systemId) > 5) THEN
      IF(systemId(1:6) == "file:/") THEN
        CALL parse_file(reader, systemId(7:LEN(systemId)), &
            characters, endDocument, endElement, endPrefixMapping, &
            ignorableWhitespace, processingInstruction, setDocumentLocator, &
            skippedEntity, startDocument, startElement, startPrefixMapping, &
            error, fatalError, warning)
      ELSE
        STOP "Type of systemId not implemented."
      END IF
    ELSE
      STOP "Wrong systemId."
    END IF
  END SUBROUTINE parse_systemId

  !> Start parsing an XML file.
  !> @param[in] reader Reader object.
  !> @param[in] filename Specify the file to parse. For example: 'test.xml'.
  !> @param characters External procedure (Content Handler).
  !> @param endDocument External procedure (Content Handler).
  !> @param endElement External procedure (Content Handler).
  !> @param endPrefixMapping External procedure (Content Handler).
  !> @param ignorableWhitespace External procedure (Content Handler).
  !> @param processingInstruction External procedure (Content Handler).
  !> @param setDocumentLocator External procedure (Content Handler).
  !> @param skippedEntity External procedure (Content Handler).
  !> @param startDocument External procedure (Content Handler).
  !> @param startElement External procedure (Content Handler).
  !> @param startPrefixMapping External procedure (Content Handler).
  !> @param error External procedure (Error Handler).
  !> @param fatalError External procedure (Error Handler).
  !> @param warning External procedure (Error Handler).
  SUBROUTINE parse_file(reader, filename, &
      characters, endDocument, endElement, endPrefixMapping, &
      ignorableWhitespace, processingInstruction, setDocumentLocator, &
      skippedEntity, startDocument, startElement, startPrefixMapping, &
      error, fatalError, warning)
    USE entities_m, ONLY: addPredefinedEntities, clear
    USE iso_varying_string, ONLY: var_str, INDEX, REMOVE, OPERATOR(//), &
        ASSIGNMENT(=), CHAR, EXTRACT, GET
    USE Locator_m, ONLY: Locator_t, setLocation, setPublicId, setSystemId
    USE SAXIo_m, ONLY: getUnit
    IMPLICIT NONE
    TYPE(XMLReader_t), INTENT(IN), TARGET :: reader
    CHARACTER(LEN=*), INTENT(IN) :: filename
    EXTERNAL :: characters, endDocument, endElement, endPrefixMapping, &
      ignorableWhitespace, processingInstruction, setDocumentLocator, &
      skippedEntity, startDocument, startElement, startPrefixMapping, &
      error, fatalError, warning

    INTEGER :: iUnit, istat, column, line, startCol, endCol
    CHARACTER(LEN=1) :: ch
    TYPE(varying_string) :: buffer, lineRead
    CHARACTER(LEN=5) :: firstLine
    CHARACTER(LEN=10) :: doctypeLine
    LOGICAL :: insideTag, insideComment
    TYPE(Locator_t), POINTER :: locator_p=>NULL()

    locator_p=>reader%locator
    CALL setDocumentLocator(locator_p)
    CALL setSystemId(locator_p, var_str("file:"//TRIM(filename)))
    CALL setPublicId(locator_p, var_str(TRIM(filename)))

    CALL AddPredefinedEntities

    ! Pick an available unit
    iUnit = getUnit()
    OPEN(UNIT = iUnit, FILE = filename, FORM = 'FORMATTED', ACTION = 'READ', &
      STATUS = 'OLD')

    CALL startDocument()

    line = 1
    column = 1
    buffer = ''
    insideTag = .FALSE.
    insideComment = .FALSE.
    DO
      CALL get(iUnit, lineRead, iostat = istat)
      IF (istat == -1) THEN
        EXIT
      END IF
      line = line + 1
      column = 1
      buffer = buffer // lineRead
      DO
        IF(.NOT.insideTag) THEN
          startCol = INDEX(buffer, '<')
          IF(startCol > 0) THEN
            CALL analyzeText(CHAR(EXTRACT(buffer, 1, startCol - 1)), &
                characters, skippedEntity)
            buffer = REMOVE(buffer, 1, startCol - 1)
            column = column + startCol
            insideTag = .TRUE.
          ELSE IF (startCol == 0) THEN
            EXIT
          END IF
        ELSE
          IF(.NOT.insideComment) THEN
            endCol = INDEX(buffer, '>')
          ELSE
            endCol = INDEX(buffer, '-->')
          END IF
          IF (endCol > 0) THEN
            IF(insideComment) endCol = endCol + 2
            CALL analyzeTag(CHAR(EXTRACT(buffer, 1, endCol)), &
                endElement, startElement, skippedEntity, insideComment)
            IF(.NOT.insideComment) THEN
              buffer = REMOVE(buffer, 1, endCol)
              insideTag = .FALSE.
            ELSE
              EXIT
            END IF
            column = column + endCol + 1
          ELSE
            EXIT
          END IF
        END IF
      END DO
    END DO

    CLOSE(iUnit)
    CALL endDocument()
    CALL clear

  CONTAINS

    SUBROUTINE analyzeFirstLine(buffer)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: buffer

      INTEGER :: start, finish, pos, length_2
      CHARACTER(LEN=LEN(buffer)) :: attrQName, attrValue
      LOGICAL :: starting, ended, error
      CHARACTER(LEN=1) :: ch

      error = (LEN(buffer) < 7)
      error = error .OR. (buffer(1:5) /= '<?xml')
      error = error .OR. (buffer(LEN(buffer) - 1 : LEN(buffer)) /= '?>')

      IF (.NOT.error) THEN
        pos = 6
        length_2 = LEN(buffer) - 2
        DO WHILE(pos < length_2)
          DO start = pos, length_2
            ch = buffer(start:start)
            IF(ch /= ' ') EXIT
          END DO
          DO finish = start + 1, length_2
            ch = buffer(finish:finish)
            IF(ch == ' ' .OR. ch == '=') EXIT
          END DO
          finish = finish - 1
          attrQName = buffer(start:finish)
          pos = finish + 1
          DO finish = pos, length_2
            ch = buffer(finish:finish)
            IF(ch == '=') EXIT
          END DO
          pos = finish + 1
          DO start = pos, length_2
            ch = buffer(start:start)
            IF(ch == '"') EXIT
          END DO
          start = start + 1
          DO finish = start + 1, length_2
            ch = buffer(finish:finish)
            IF(ch == '"') EXIT
          END DO
          finish = finish - 1
          pos = finish + 1
          attrValue = buffer(start:finish)

          SELECT CASE(attrQName)
          CASE('version')
            IF(attrValue /= '1.0' .AND. attrValue /= '1.1') THEN
              STOP "Version not compatible."
            END IF
          END SELECT
        END DO
      END IF
    END SUBROUTINE analyzeFirstLine

    SUBROUTINE analyzeTag(buffer, endElement, startElement, skippedEntity, &
        insideComment)
      USE Attr_m, ONLY: Attr_t, setAttr
      USE Attributes_m, ONLY: Attributes_t, appendAttribute
      USE Dtd_m, ONLY: analyzeDTDLine
      USE iso_varying_string, ONLY: ASSIGNMENT(=), INDEX, LEN, char
      USE entities_m, ONLY: ReplaceEntities
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: buffer
      EXTERNAL :: endElement, startElement, skippedEntity
      LOGICAL, INTENT(OUT) :: insideComment

      INTEGER :: start, finish, pos, length_1, i, tagQNameCLength, &
          attrQNameCLength
      TYPE(varying_string) :: attrValue
      LOGICAL :: starting, ended, ignore
      CHARACTER(LEN=1) :: ch
      TYPE(Attributes_t) :: attributes
      CHARACTER(LEN=LEN(buffer)) :: tagQNameC, buffer2, attrQNameC

      insideComment = .FALSE.
      SELECT CASE(buffer(2:2))
      CASE('?')
        ! This is an XML document definition, i.e. '<? ... ?>'
        CALL analyzeFirstLine(buffer)
      CASE('!')
        IF (LEN(buffer) >= 7) THEN
          IF (buffer(3:4) == '--') THEN
            IF(buffer(LEN(buffer) - 2 : LEN(buffer)) /= '-->') THEN
              ! This is a comment but it's incomplete
              insideComment = .TRUE.
            END IF
          ELSE
            ! This is a DTD definition, i.e. '<! ... >'
            CALL analyzeDTDLine(buffer)
          END IF
        ELSE
          ! This is a DTD definition, i.e. '<! ... >'
          CALL analyzeDTDLine(buffer)
        END IF
      CASE('/')
        ! This is an ending tag, i.e. '</ ... >'
        tagQNameC = TRIM(ADJUSTL(buffer(3 : LEN(buffer) - 1)))
        pos = INDEX(tagQNameC, ' ') - 1
        IF (pos == -1) pos = LEN(tagQNameC)
        CALL endElement(TRIM(getUriFromQName(tagQNameC(1:pos))), &
            TRIM(getLocalNameFromQName(tagQNameC(1:pos))), &
            tagQNameC(1:pos))
      CASE DEFAULT
        ! This is a starting tag, i.e. '< ... >' or '< ... />'
        length_1 = LEN(buffer) - 1
        IF(buffer(length_1:length_1) == '/') THEN
          ! This tag is ended with '/>'
          ended = .TRUE.
          finish = length_1 - 1
        ELSE
          finish = length_1
          ended = .FALSE.
        END IF
        buffer2 = TRIM(ADJUSTL(buffer(2:finish)))
        finish = INDEX(buffer2, ' ')
        IF(finish == 0) finish = LEN(buffer2)
        tagQNameC = buffer2(:finish)
        tagQNameCLength = finish

        buffer2 = buffer2(finish + 1:)
        DO
          finish = INDEX(buffer2, '=')
          IF(finish == 0) EXIT
          attrQNameC = TRIM(ADJUSTL(buffer2(:finish - 1)))
          attrQNameCLength = LEN(TRIM(attrQNameC))
          finish = INDEX(buffer2, '"')
          IF(finish == 0) THEN
            stop "Error: XML not valid"
          END IF
          buffer2 = buffer2(finish + 1:)
          finish = INDEX(buffer2, '"')
          IF(finish == 0) THEN
            stop "Error: XML not valid"
          END IF
          attrValue = buffer2(:finish - 1)
          CALL ReplaceEntities(attrValue, skippedEntity)
          buffer2 = buffer2(finish + 1:)
          CALL appendAttribute(attributes, &
              setAttr(TRIM(getUriFromQName(attrQNameC(:attrQNameCLength))), &
              TRIM(getLocalNameFromQName(attrQNameC(:attrQNameCLength))), &
              char(attrValue)))
        END DO

        CALL startElement(TRIM(getUriFromQName(tagQNameC(1:tagQNameCLength))),&
            TRIM(getLocalNameFromQName(tagQNameC(1:tagQNameCLength))), &
            tagQNameC(1:tagQNameCLength), attributes)
        IF(ended) CALL endElement(&
            TRIM(getUriFromQName(tagQNameC(1:tagQNameCLength))), &
            TRIM(getLocalNameFromQName(tagQNameC(1:tagQNameCLength))), &
            tagQNameC(1:tagQNameCLength))
      END SELECT
    END SUBROUTINE analyzeTag

    SUBROUTINE analyzeText(buffer, characters, skippedEntity)
      USE entities_m, ONLY: ReplaceEntities
      USE iso_varying_string, ONLY: ASSIGNMENT(=), CHAR, LEN
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: buffer
      EXTERNAL :: characters, skippedEntity
      
      TYPE(varying_string) :: copy

      IF(LEN(buffer) > 0) THEN
        copy = buffer
        CALL ReplaceEntities(copy, skippedEntity)
        CALL characters(CHAR(copy), 1, LEN(copy))
      END IF
    END SUBROUTINE analyzeText

    SUBROUTINE append(vec, s)
      IMPLICIT NONE
      INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: vec
      INTEGER, INTENT(IN) :: s

      INTEGER, DIMENSION(:), ALLOCATABLE :: dummy

      IF(.NOT.ALLOCATED(vec)) THEN
        ALLOCATE(vec(1))
        vec(1)=s
      ELSE
        ALLOCATE(dummy(SIZE(vec)))
        dummy=vec
        DEALLOCATE(vec)
        ALLOCATE(vec(SIZE(dummy)+1))
        vec(1:SIZE(dummy))=dummy
        vec(SIZE(vec))=s
      END IF
    END SUBROUTINE append

  END SUBROUTINE parse_file

  !> Extract the local name from a QName
  !> @param[in] qName QName
  FUNCTION getLocalNameFromQName_1(qName) RESULT(localName)
    USE iso_varying_string, ONLY: ASSIGNMENT(=), LEN, INDEX, EXTRACT
    IMPLICIT NONE
    TYPE(varying_string), INTENT(IN) :: qName
    TYPE(varying_string) :: localName

    INTEGER :: pos

    pos = INDEX(qName, ':')
    IF(pos < 1) THEN
      localName = qName
    ELSE
      localName = EXTRACT(qName, start = pos + 1)
    END IF
  END FUNCTION getLocalNameFromQName_1

  !> Extract the local name from a QName
  !> @param[in] qName QName
  FUNCTION getLocalNameFromQName_2(qName) RESULT(localName)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: qName
    CHARACTER(LEN=LEN(qName)) :: localName

    INTEGER :: pos

    pos = INDEX(qName, ':')
    IF(pos < 1) THEN
      localName = qName
    ELSE
      localName = qName(pos + 1 :)
    END IF
  END FUNCTION getLocalNameFromQName_2

  !> Extract the URI from a QName
  !> @param[in] qName QName
  FUNCTION getUriFromQName_1(qName) RESULT(uri)
    USE iso_varying_string, ONLY: ASSIGNMENT(=), LEN, INDEX, EXTRACT
    IMPLICIT NONE
    TYPE(varying_string), INTENT(IN) :: qName
    TYPE(varying_string) :: uri

    INTEGER :: pos

    pos = INDEX(qName, ':')
    IF(pos > LEN(qName) .OR. pos < 1) THEN
      uri = ""
    ELSE
      uri = EXTRACT(qName, finish = pos - 1)
    END IF
  END FUNCTION getUriFromQName_1

  !> Extract the URI from a QName
  !> @param[in] qName QName
  FUNCTION getUriFromQName_2(qName) RESULT(uri)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: qName
    CHARACTER(LEN=LEN(qName)) :: uri

    INTEGER :: pos

    pos = INDEX(qName, ':')
    IF(pos > LEN(qName) .OR. pos < 1) THEN
      uri = ""
    ELSE
      uri = qName(:pos-1)
    END IF
  END FUNCTION getUriFromQName_2

END MODULE XMLReader_m
