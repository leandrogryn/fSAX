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


MODULE Dtd_m
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: analyzeDTDLine

CONTAINS

  SUBROUTINE analyzeDTDLine(buffer)
    USE Entities_m, ONLY: addEntity
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: buffer

    INTEGER :: i1, i2, i3, i4, i5, i6

    IF(LEN(buffer) >= 8) THEN
      IF(buffer(3:8) == 'ENTITY') THEN
        ! Search for the beginning of the name
        DO i1 = 10, LEN(buffer)
          IF (buffer(i1:i1) /= ' ') EXIT
        END DO
        ! Search for the ending of the name
        i2 = INDEX(buffer(i1:LEN(buffer)), ' ')
        i2 = i2 + i1 - 1

        ! Search for the beginning of the next word
        DO i3 = i2 + 1, LEN(buffer)
          IF (buffer(i3:i3) /= ' ') EXIT
        END DO
        IF (buffer(i3:i3) == '"') THEN
          ! Internal entity
          i3 = i3 + 1
          i4 = INDEX(buffer(i3 : LEN(buffer)), '"', back = .TRUE.)
          i4 = i4 + i3 - 1
          
          CALL addEntity(buffer(i1 : i2 - 1), buffer(i3 : i4 - 1))

        ELSE IF (buffer(i3:i3) == 'S') THEN
          i4 = INDEX(buffer(i3 : LEN(buffer)), ' ')
          IF (buffer(i3 : i4 - 1) == 'SYSTEM') THEN
            ! External entity
            i5 = INDEX(buffer(i4 : LEN(buffer)), '"')
            i5 = i5 + i4 - 1
            i6 = INDEX(buffer(i4 : LEN(buffer)), '"', back = .TRUE.)
            i6 = i6 + i4 - 1

            ! Continue the implementation here

          END IF  
        END IF  
      END IF
    END IF
  END SUBROUTINE analyzeDTDLine

END MODULE Dtd_m
