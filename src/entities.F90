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


MODULE Entities_m
  USE iso_varying_string, ONLY: varying_string
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: &
    addEntity, &
    addPredefinedEntities, &
    clear, &
    replaceEntities
  SAVE

  TYPE :: entity_t
    TYPE(varying_string) :: name, text
    TYPE(entity_t), POINTER :: next=>NULL()
  END TYPE entity_t

  TYPE(entity_t), POINTER :: firstEntity=>NULL(), lastEntity=>NULL()
  INTEGER :: numberOfEntities=0

CONTAINS

  SUBROUTINE addEntity(name, text)
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: name, text

    IF(numberOfEntities==0) THEN
      ALLOCATE(firstEntity)
      lastEntity=>firstEntity
    ELSE
      ALLOCATE(lastEntity%next)
      lastEntity=>lastEntity%next
    END IF
    numberOfEntities=numberOfEntities+1
    lastEntity%name=name
    lastEntity%text=text
  END SUBROUTINE addEntity

  SUBROUTINE addPredefinedEntities
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE

    CALL addEntity("amp", "&")
    CALL addEntity("apos", "'")
    CALL addEntity("gt", ">")
    CALL addEntity("lt", "<")
    CALL addEntity("quot", '"')
  END SUBROUTINE addPredefinedEntities

  SUBROUTINE clear
    IMPLICIT NONE

    INTEGER :: n, i
    
    n = numberOfEntities
    DO i = 1, n
      CALL popFront
    END DO
    !numberOfEntities = 0

  CONTAINS

    SUBROUTINE popFront
      USE iso_varying_string, ONLY: ASSIGNMENT(=)
      IMPLICIT NONE

      TYPE(entity_t), POINTER :: obsolete=>NULL()

      !IF(numberOfEntities > 0) THEN
        obsolete => firstEntity
        IF(numberOfEntities == 1) THEN
          NULLIFY(firstEntity, lastEntity)
        ELSE
          firstEntity => firstEntity%next
        END IF
        NULLIFY(obsolete%next)
        obsolete%name=''
        obsolete%text=''
        DEALLOCATE(obsolete)
        numberOfEntities = numberOfEntities - 1
      !END IF
    END SUBROUTINE popFront

  END SUBROUTINE clear

  FUNCTION find(name) RESULT(text)
    USE iso_varying_string, ONLY: OPERATOR(==), ASSIGNMENT(=), char
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: name

    TYPE(varying_string) :: text
    TYPE(entity_t), POINTER :: entity=>NULL()
    INTEGER :: i

    text = ""
    entity => firstEntity
    DO i = 1, numberOfEntities
      IF (entity%name == name) THEN
        text = entity%text
        EXIT
      END IF
      entity => entity%next
    END DO
  END FUNCTION find

  SUBROUTINE replaceEntities(text, skippedEntity)
    USE iso_varying_string, ONLY: OPERATOR(==), EXTRACT, CHAR, LEN, &
      OPERATOR(//), ASSIGNMENT(=), INDEX
    IMPLICIT NONE
    TYPE(varying_string), INTENT(INOUT) :: text
    EXTERNAL :: skippedEntity

    INTEGER :: pos, i1, i2
    TYPE(varying_string) :: name, entity

    pos=1
    DO
      i1 = INDEX(EXTRACT(text, start = pos), "&")
      IF (i1 == 0) THEN
        EXIT
      ELSE
        i2 = INDEX(EXTRACT(text, start = pos + i1), ";")
        IF (i2 == 0) THEN
          EXIT
        ELSE
          name=EXTRACT(text, pos + i1, pos + i1 + i2 - 2)
          entity = find(CHAR(name))
          IF(LEN(entity) == 0) THEN
            CALL skippedEntity(CHAR(name))
          END IF
          text=EXTRACT(text, 1, pos + i1 - 2) // entity // &
              EXTRACT(text, start = pos + i1 + i2)
          pos = pos + LEN(entity)
        END IF
      END IF
    END DO
  END SUBROUTINE replaceEntities

END MODULE Entities_m
