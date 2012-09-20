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


MODULE SAXException_m
  USE iso_varying_string, ONLY: varying_string
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: &
    getMessage, &
    SAXException_t, &
    SAXException, &
    toString

  TYPE :: SAXException_t
    PRIVATE
    LOGICAL :: errorFound = .FALSE.
    TYPE(varying_string) :: message
  END TYPE SAXException_t

  INTERFACE SAXException
    MODULE PROCEDURE SAXException_
    MODULE PROCEDURE SAXException_message
  END INTERFACE SAXException

  INTERFACE getMessage
    MODULE PROCEDURE getMessage_se
  END INTERFACE getMessage

  INTERFACE toString
    MODULE PROCEDURE toString_se
  END INTERFACE toString

CONTAINS

  TYPE(varying_string) FUNCTION getMessage_se(e)
    IMPLICIT NONE
    TYPE(SAXException_t), INTENT(IN) :: e

    getMessage_se = e%message
  END FUNCTION getMessage_se

  TYPE(SAXException_t) FUNCTION SAXException_()
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE

    SAXException_%message = ""
  END FUNCTION SAXException_

  TYPE(SAXException_t) FUNCTION SAXException_message(message)
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: message

    SAXException_message%message = message
  END FUNCTION SAXException_message

  TYPE(varying_string) FUNCTION toString_se(e)
    IMPLICIT NONE
    TYPE(SAXException_t), INTENT(IN) :: e

    toString_se = e%message
  END FUNCTION toString_se

END MODULE SAXException_m
