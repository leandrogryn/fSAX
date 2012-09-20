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

!> Define generic I/O procedures.

MODULE SAXIo_m
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: &
    getUnit

CONTAINS

  !> Return an available unit for I/O.
  !> @return Abailable Unit.
  FUNCTION getUnit() RESULT(i)
    IMPLICIT NONE
    INTEGER :: i

    LOGICAL :: opend

    DO i = 10, 99
      INQUIRE(UNIT = i, OPENED = opend )
      IF (.NOT. opend) EXIT
    END DO
    IF(opend) i = -1
  END FUNCTION getUnit

END MODULE SAXIo_m
