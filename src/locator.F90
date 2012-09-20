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

!> The Locator provides information about the location (line and column) in the
!> file where the parser is reading, i.e. where the handler was called.

MODULE Locator_m
  USE iso_varying_string, ONLY: varying_string
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: &
    getColumnNumber, &
    getLineNumber, &
    getPublicId, &
    getSystemId, &
    Locator_t, &
    setLocation, &
    setPublicId, &
    setSystemId

  !> Locator object.
  TYPE :: Locator_t
    PRIVATE
    INTEGER :: column = 0, line = 0
    TYPE(varying_string) :: publicId, systemId
  END TYPE Locator_t

  !> Get column number.
  INTERFACE getColumnNumber
    MODULE PROCEDURE getColumnNumber_l
  END INTERFACE getColumnNumber

  !> Get line number.
  INTERFACE getLineNumber
    MODULE PROCEDURE getLineNumber_l
  END INTERFACE getLineNumber

  !> Get Public ID
  INTERFACE getPublicId
    MODULE PROCEDURE getPublicId_l
  END INTERFACE getPublicId

  !> Get System ID
  INTERFACE getSystemId
    MODULE PROCEDURE getSystemId_l
  END INTERFACE getSystemId

CONTAINS

  !> Get column number.
  !> @param[in] locator Locator object.
  !> @return Column number.
  INTEGER FUNCTION getColumnNumber_l(locator)
    IMPLICIT NONE
    TYPE(Locator_t), INTENT(IN) :: locator

    getColumnNumber_l = locator%column
  END FUNCTION getColumnNumber_l
  
  !> Get line number.
  !> @param[in] locator Locator object.
  !> @return Line number.
  INTEGER FUNCTION getLineNumber_l(locator)
    IMPLICIT NONE
    TYPE(Locator_t), INTENT(IN) :: locator

    getLineNumber_l = locator%line
  END FUNCTION getLineNumber_l
  
  !> Get Public ID.
  !> @param[in] locator Locator object.
  !> @return Public ID.
  TYPE(varying_string) FUNCTION getPublicId_l(locator)
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE
    TYPE(Locator_t), INTENT(IN) :: locator

    getPublicId_l = locator%publicId
  END FUNCTION getPublicId_l
  
  !> Get System ID.
  !> @param[in] locator Locator object.
  !> @return System ID.
  TYPE(varying_string) FUNCTION getSystemId_l(locator)
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE
    TYPE(Locator_t), INTENT(IN) :: locator

    getSystemId_l = locator%SystemId
  END FUNCTION getSystemId_l
  
  !> Set location.
  !> @param[in] locator Locator object.
  !> @param[in] column Column number.
  !> @param[in] line Line number.
  SUBROUTINE setLocation(locator, column, line)
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE
    TYPE(Locator_t), INTENT(INOUT) :: locator
    INTEGER, INTENT(IN) :: column, line

    locator%column = column
    locator%line = line
  END SUBROUTINE setLocation

  !> Set Public ID.
  !> @param[in] locator Locator object.
  !> @param[in] publicId Public ID.
  SUBROUTINE setPublicId(locator, publicId)
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE
    TYPE(Locator_t), INTENT(INOUT) :: locator
    TYPE(varying_string), INTENT(IN) :: publicId

    locator%publicId = publicId
  END SUBROUTINE setPublicId

  !> Set System ID.
  !> @param[in] locator Locator object.
  !> @param[in] systemId System ID.
  SUBROUTINE setSystemId(locator, systemId)
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE
    TYPE(Locator_t), INTENT(INOUT) :: locator
    TYPE(varying_string), INTENT(IN) :: systemId

    locator%systemId = systemId
  END SUBROUTINE setSystemId

END MODULE Locator_m
