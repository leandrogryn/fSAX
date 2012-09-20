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


MODULE InputSource_m
  USE iso_varying_string, ONLY: varying_string
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: &
    getCharacterStream, &
    getEncoding, &
    getPublicId, &
    getSourceType, &
    getSystemId, &
    iCharacterStream, &
    iPublicId, &
    iSystemId, &
    InputSource_t, &
    setCharacterStream, &
    setEncoding, &
    setPublicId, &
    setSystemId
  SAVE

  INTEGER, PARAMETER :: &
    iCharacterStream = 1, &
    iPublicId = 2, &
    iSystemId = 3

  TYPE :: InputSource_t
    PRIVATE
    INTEGER :: sourceType = 0
    TYPE(varying_string) :: characterStream, encoding, publicId, systemId
  END TYPE InputSource_t

  INTERFACE getPublicId
    MODULE PROCEDURE getPublicId_is
  END INTERFACE getPublicId

  INTERFACE getSystemId
    MODULE PROCEDURE getSystemId_is
  END INTERFACE getSystemId

  INTERFACE setPublicId
    MODULE PROCEDURE setPublicId_is
  END INTERFACE setPublicId

  INTERFACE setSystemId
    MODULE PROCEDURE setSystemId_is
  END INTERFACE setSystemId

CONTAINS

  TYPE(varying_string) FUNCTION getCharacterStream(inputSource)
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE
    TYPE(InputSource_t), INTENT(IN) :: inputSource

    getCharacterStream = inputSource%characterStream
  END FUNCTION getCharacterStream

  TYPE(varying_string) FUNCTION getEncoding(inputSource)
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE
    TYPE(InputSource_t), INTENT(IN) :: inputSource

    getEncoding = inputSource%encoding
  END FUNCTION getEncoding

  TYPE(varying_string) FUNCTION getPublicId_is(inputSource)
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE
    TYPE(InputSource_t), INTENT(IN) :: inputSource

    getPublicId_is = inputSource%publicId
  END FUNCTION getPublicId_is

  TYPE(varying_string) FUNCTION getSystemId_is(inputSource)
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE
    TYPE(InputSource_t), INTENT(IN) :: inputSource

    getSystemId_is = inputSource%systemId
  END FUNCTION getSystemId_is

  INTEGER FUNCTION getSourceType(inputSource)
    IMPLICIT NONE
    TYPE(InputSource_t), INTENT(IN) :: inputSource

    getSourceType = inputSource%sourceType
  END FUNCTION getSourceType

  SUBROUTINE setEncoding(inputSource, encoding)
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE
    TYPE(InputSource_t), INTENT(INOUT) :: inputSource
    CHARACTER(LEN=*), INTENT(IN) :: encoding

    inputSource%encoding = encoding
  END SUBROUTINE setEncoding

  SUBROUTINE setCharacterStream(inputSource, characterStream)
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE
    TYPE(InputSource_t), INTENT(INOUT) :: inputSource
    CHARACTER(LEN=*), INTENT(IN) :: characterStream

    inputSource%sourceType = iCharacterStream
    inputSource%characterStream = characterStream
  END SUBROUTINE setCharacterStream

  SUBROUTINE setPublicId_is(inputSource, publicId)
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE
    TYPE(InputSource_t), INTENT(INOUT) :: inputSource
    CHARACTER(LEN=*), INTENT(IN) :: publicId

    inputSource%sourceType = iPublicId
    inputSource%publicId = publicId
  END SUBROUTINE setPublicId_is

  SUBROUTINE setSystemId_is(inputSource, systemId)
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE
    TYPE(InputSource_t), INTENT(INOUT) :: inputSource
    CHARACTER(LEN=*), INTENT(IN) :: systemId

    inputSource%sourceType = iSystemId
    inputSource%systemId = systemId
  END SUBROUTINE setSystemId_is

END MODULE InputSource_m
