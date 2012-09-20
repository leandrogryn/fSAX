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

!> Defines the interface for the content-handling procedures.

MODULE ContentHandler_m
  IMPLICIT NONE
  PUBLIC

  INTERFACE
    !> Receives character data.
    !> @param[in] ch Received string
    !> @param[in] start Index of the first character (most likely equal to 1).
    !> @param[in] length Length of the string
    SUBROUTINE characters(ch, start, length)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: ch
      INTEGER, INTENT(IN) :: start
      INTEGER, INTENT(IN) :: length
    END SUBROUTINE characters

    !> Receive notification that the parser finished reading the file.
    SUBROUTINE endDocument()
      IMPLICIT NONE
    END SUBROUTINE endDocument

    !> Receive notification that the end of an element was found.
    !> @param[in] uri URI of the tag (namespace)
    !> @param[in] localName Local name of the tag
    !> @param[in] qName Full name (uri:localname)
    SUBROUTINE endElement(uri, localName, qName)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: uri
      CHARACTER(LEN=*), INTENT(IN) :: localName
      CHARACTER(LEN=*), INTENT(IN) :: qName
    END SUBROUTINE endElement

    !> Receive notification that a namespace prefix mapping is going out of scope.
    !> @param[in] prefix Prefix being notified.
    SUBROUTINE endPrefixMapping(prefix)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: prefix
    END SUBROUTINE endPrefixMapping

    !> Receive whitespaces that were found that can be ignored.
    !> @param[in] ch Received string
    !> @param[in] start Index of the first character (most likely equal to 1).
    !> @param[in] length Length of the string
    SUBROUTINE ignorableWhitespace(ch, start, length)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: ch
      INTEGER, INTENT(IN) :: start
      INTEGER, INTENT(IN) :: length
    END SUBROUTINE ignorableWhitespace

    !> Receive notification that an instruction is being processed.
    !> @param[in] insTarget target to process
    !> @param[in] insData data required (if any).
    SUBROUTINE processingInstruction(insTarget, insData)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: insTarget
      CHARACTER(LEN=*), INTENT(IN) :: insData
    END SUBROUTINE processingInstruction

    !> Specify what locator to use.
    !> @param locator_p Pointer to the parser's locator. This is an output.
    SUBROUTINE setDocumentLocator(locator_p)
      USE Locator_m, ONLY: Locator_t
      IMPLICIT NONE
      TYPE(Locator_t), POINTER :: locator_p
    END SUBROUTINE setDocumentLocator

    !> Receive notification an entity that was skipped.
    !> @param entityName Name of the entity skipped.
    SUBROUTINE skippedEntity(entityName)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: entityName
    END SUBROUTINE skippedEntity

    !> Receive notification that the parser started reading the document.
    SUBROUTINE startDocument()
      IMPLICIT NONE
    END SUBROUTINE startDocument

    !> Receive notification that the beginning of an element was found.
    !> @param[in] uri URI of the tag (namespace).
    !> @param[in] localName Local name of the tag.
    !> @param[in] qName Full name (uri:localname).
    !> @param[in] attr Attributes found (if any).
    SUBROUTINE startElement(uri, localName, qName, attr)
      USE Attributes_m, ONLY: Attributes_t
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: uri
      CHARACTER(LEN=*), INTENT(IN) :: localName
      CHARACTER(LEN=*), INTENT(IN) :: qName
      TYPE(Attributes_t), INTENT(IN) :: attr
    END SUBROUTINE startElement

    !> Receive notification that a namespace prefix mapping is coming into scope.
    !> @param[in] prefix Prefix being notified.
    !> @param[in] uri Namespace URI of the scope that started.
    SUBROUTINE startPrefixMapping(prefix, uri)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: prefix
      CHARACTER(LEN=*), INTENT(IN) :: uri
    END SUBROUTINE startPrefixMapping

  END INTERFACE
END MODULE ContentHandler_m
