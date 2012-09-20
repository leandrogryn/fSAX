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


MODULE SAXParseException_m
  USE iso_varying_string, ONLY: varying_string
  USE Locator_m, ONLY: Locator_t
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: &
    getColumnNumber, &
    getLineNumber, &
    getMessage, &
    getPublicId, &
    getSystemId, &
    SAXParseException_t, &
    SAXParseException, &
    toString

  TYPE :: SAXParseException_t
    PRIVATE
    TYPE(varying_string) :: message
    TYPE(Locator_t) :: locator
  END TYPE SAXParseException_t

  INTERFACE SAXParseException
    MODULE PROCEDURE SAXParseException_ml
    MODULE PROCEDURE SAXParseException_mpslc
  END INTERFACE SAXParseException

  INTERFACE getColumnNumber
    MODULE PROCEDURE getColumnNumber_spe
  END INTERFACE getColumnNumber

  INTERFACE getMessage
    MODULE PROCEDURE getMessage_spe
  END INTERFACE getMessage

  INTERFACE getLineNumber
    MODULE PROCEDURE getLineNumber_spe
  END INTERFACE getLineNumber

  INTERFACE getPublicId
    MODULE PROCEDURE getPublicId_spe
  END INTERFACE getPublicId

  INTERFACE getSystemId
    MODULE PROCEDURE getSystemId_spe
  END INTERFACE getSystemId

  INTERFACE toString
    MODULE PROCEDURE toString_spe
  END INTERFACE toString

CONTAINS

  INTEGER FUNCTION getColumnNumber_spe(SAXParseException)
    USE Locator_m, ONLY: getColumnNumber
    IMPLICIT NONE
    TYPE(SAXParseException_t), INTENT(IN) :: SAXParseException

    getColumnNumber_spe = getColumnNumber(SAXParseException%locator)
  END FUNCTION getColumnNumber_spe

  INTEGER FUNCTION getLineNumber_spe(SAXParseException)
    USE Locator_m, ONLY: getLineNumber
    IMPLICIT NONE
    TYPE(SAXParseException_t), INTENT(IN) :: SAXParseException

    getLineNumber_spe = getLineNumber(SAXParseException%locator)
  END FUNCTION getLineNumber_spe

  TYPE(varying_string) FUNCTION getMessage_spe(SAXParseException)
    IMPLICIT NONE
    TYPE(SAXParseException_t), INTENT(IN) :: SAXParseException

    getMessage_spe = SAXParseException%message
  END FUNCTION getMessage_spe

  TYPE(varying_string) FUNCTION getPublicId_spe(SAXParseException)
    USE Locator_m, ONLY: getPublicId
    IMPLICIT NONE
    TYPE(SAXParseException_t), INTENT(IN) :: SAXParseException

    getPublicId_spe = getPublicId(SAXParseException%locator)
  END FUNCTION getPublicId_spe

  TYPE(varying_string) FUNCTION getSystemId_spe(SAXParseException)
    USE Locator_m, ONLY: getSystemId
    IMPLICIT NONE
    TYPE(SAXParseException_t), INTENT(IN) :: SAXParseException

    getSystemId_spe = getSystemId(SAXParseException%locator)
  END FUNCTION getSystemId_spe

  TYPE(SAXParseException_t) FUNCTION SAXParseException_ml(message, locator)
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    USE Locator_m, ONLY: Locator_t
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: message
    TYPE(Locator_t), INTENT(IN) :: locator

    SAXParseException_ml%message = message
    SAXParseException_ml%locator = locator
  END FUNCTION SAXParseException_ml

  TYPE(SAXParseException_t) FUNCTION SAXParseException_mpslc(message, &
      publicId, systemId, lineNumber, columnNumber)
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    USE Locator_m, ONLY: setLocation, setPublicId, setSystemId
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: message
    TYPE(varying_string), INTENT(IN) :: publicId, systemId
    INTEGER, INTENT(IN) :: lineNumber, columnNumber

    SAXParseException_mpslc%message = message
    CALL setLocation(SAXParseException_mpslc%locator, columnNumber, lineNumber)
    CALL setPublicId(SAXParseException_mpslc%locator, publicId)
    CALL setSystemId(SAXParseException_mpslc%locator, systemId)
  END FUNCTION SAXParseException_mpslc

  TYPE(varying_string) FUNCTION toString_spe(e)
    IMPLICIT NONE
    TYPE(SAXParseException_t), INTENT(IN) :: e

    toString_spe = e%message
  END FUNCTION toString_spe

END MODULE SAXParseException_m
