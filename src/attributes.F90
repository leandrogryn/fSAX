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

!> Manage element attributes.

!> The user is allowed to access most of the procedures defined here to retrive
!> information of each attribute of an element.

MODULE Attributes_m
  USE Attr_m, ONLY: Attr_t
  USE iso_varying_string, ONLY: varying_string
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: &
    appendAttribute, &
    Attributes_t, &
    getIndex, &
    getLength, &
    getLocalName, &
    getQName, &
    getType, &
    getURI, &
    getValue

  !> Contains all the attributes that belong to an element.
  TYPE :: Attributes_t
    PRIVATE
    !> Vector with all the attributes.
    TYPE(Attr_t), DIMENSION(:), ALLOCATABLE :: attr
  END TYPE Attributes_t

  !> Search an attribute and return its index.
  INTERFACE getIndex
    MODULE PROCEDURE getIndex_cq
    MODULE PROCEDURE getIndex_vq
    MODULE PROCEDURE getIndex_qul
    MODULE PROCEDURE getIndex_vul
  END INTERFACE getIndex

  !> Get the type of an attribute.
  INTERFACE getType
    MODULE PROCEDURE getType_n
    MODULE PROCEDURE getType_cq
    MODULE PROCEDURE getType_vq
    MODULE PROCEDURE getType_cul
    MODULE PROCEDURE getType_vul
  END INTERFACE getType

  !> Get the value of an attribute.
  INTERFACE getValue
    MODULE PROCEDURE getValue_1
    MODULE PROCEDURE getValue_cq
    MODULE PROCEDURE getValue_vq
    MODULE PROCEDURE getValue_cul
    MODULE PROCEDURE getValue_vul
  END INTERFACE getValue

CONTAINS

  !> Return the index of an attribute that matches the QName.
  !> @param[in] attributes Attributes object.
  !> @param[in] qName QName being searched.
  !> @return Index.
  INTEGER FUNCTION getIndex_cq(attributes, qName)
    USE Attr_m, ONLY: getQName_attr
    USE iso_varying_string, ONLY: OPERATOR(==), char
    IMPLICIT NONE
    TYPE(Attributes_t), INTENT(IN) :: attributes
    CHARACTER(LEN=*), INTENT(IN) :: qName

    INTEGER :: i

    getIndex_cq = 0
    DO i = 1, getLength(attributes)
      IF(char(getQName_attr(attributes%attr(i))) == qName) THEN
        getIndex_cq = i
        EXIT
      END IF
    END DO
  END FUNCTION getIndex_cq

  !> Return the index of an attribute that matches the QName.
  !> @param[in] attributes Attributes object.
  !> @param[in] qName QName being searched.
  !> @return Index.
  INTEGER FUNCTION getIndex_vq(attributes, qName)
    USE Attr_m, ONLY: getQName_attr
    USE iso_varying_string, ONLY: char
    IMPLICIT NONE
    TYPE(Attributes_t), INTENT(IN) :: attributes
    TYPE(varying_string), INTENT(IN) :: qName

    getIndex_vq = getIndex_cq(attributes, char(qName))
  END FUNCTION getIndex_vq

  !> Return the index of an attribute that matches the URI-Local Name pair.
  !> @param[in] attributes Attributes object.
  !> @param[in] uri URI of the attribute.
  !> @param[in] localName Local Name of the attribute.
  !> @return Index.
  INTEGER FUNCTION getIndex_qul(attributes, uri, localName)
    USE Attr_m, ONLY: getURI_attr, getLocalName_attr
    USE iso_varying_string, ONLY: varying_string, OPERATOR(==), char
    IMPLICIT NONE
    TYPE(Attributes_t), INTENT(IN) :: attributes
    CHARACTER(LEN=*), INTENT(IN) :: uri, localName

    INTEGER :: i

    getIndex_qul = 0
    DO i = 1, getLength(attributes)
      IF(char(getURI_attr(attributes%attr(i))) == uri .AND. &
        char(getLocalName_attr(attributes%attr(i))) == localName) THEN
        EXIT
      END IF
    END DO
  END FUNCTION getIndex_qul

  !> Return the index of an attribute that matches the URI-Local Name pair.
  !> @param[in] attributes Attributes object.
  !> @param[in] uri URI of the attribute.
  !> @param[in] localName Local Name of the attribute.
  !> @return Index.
  INTEGER FUNCTION getIndex_vul(attributes, uri, localName)
    USE Attr_m, ONLY: getQName_attr
    USE iso_varying_string, ONLY: char
    IMPLICIT NONE
    TYPE(Attributes_t), INTENT(IN) :: attributes
    TYPE(varying_string), INTENT(IN) :: uri, localName

    getIndex_vul = getIndex_qul(attributes, char(uri), char(localName))
  END FUNCTION getIndex_vul

  !> Return the number of attributes found in an element.
  !> @param[in] attributes Attributes object.
  !> @return Number of attributes.
  INTEGER FUNCTION getLength(attributes)
    IMPLICIT NONE
    TYPE(Attributes_t), INTENT(IN) :: attributes

    IF(ALLOCATED(attributes%attr)) THEN
      getLength = SIZE(attributes%attr)
    ELSE
      getLength = 0
    END IF
  END FUNCTION getLength

  !> Return the Local Name of an attribute.
  !> @param[in] attributes Attributes object.
  !> @param[in] n Index of the attribute requested.
  !> @return Local Name.
  TYPE(varying_string) FUNCTION getLocalName(attributes, n)
    USE Attr_m, ONLY: getLocalName_attr
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE
    TYPE(Attributes_t), INTENT(IN) :: attributes
    INTEGER, INTENT(IN) :: n

    IF(n > 0 .AND. n <= getLength(attributes)) THEN
      getLocalName = getLocalName_attr(attributes%attr(n))
    ELSE
      getLocalName = ""
    END IF
  END FUNCTION getLocalName

  !> Return the QName of an attribute.
  !> @param[in] attributes Attributes object.
  !> @param[in] n Index of the attribute requested.
  !> @return QName.
  TYPE(varying_string) FUNCTION getQName(attributes, n)
    USE Attr_m, ONLY: getQName_attr
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE
    TYPE(Attributes_t), INTENT(IN) :: attributes
    INTEGER, INTENT(IN) :: n

    IF(n > 0 .AND. n <= getLength(attributes)) THEN
      getQName = getQName_attr(attributes%attr(n))
    ELSE
      getQName = ""
    END IF
  END FUNCTION getQName

  !> Return the type of an attribute.
  !> @param[in] attributes Attributes object.
  !> @param[in] n Index of the attribute requested.
  !> @return Type.
  TYPE(varying_string) FUNCTION getType_n(attributes, n)
    USE Attr_m, ONLY: getType_attr
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE
    TYPE(Attributes_t), INTENT(IN) :: attributes
    INTEGER, INTENT(IN) :: n

    IF(n > 0 .AND. n <= getLength(attributes)) THEN
      getType_n = getType_attr(attributes%attr(n))
    ELSE
      getType_n = ""
    END IF
  END FUNCTION getType_n

  !> Return the type of an attribute.
  !> @param[in] attributes Attributes object.
  !> @param[in] qName QName of the attribute to search.
  !> @return Type.
  TYPE(varying_string) FUNCTION getType_cq(attributes, qName)
    USE Attr_m, ONLY: getType_attr
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE
    TYPE(Attributes_t), INTENT(IN) :: attributes
    CHARACTER(LEN=*), INTENT(IN) :: qName

    INTEGER :: n

    n = getIndex(attributes, qName)
    IF(n > 0 .AND. n <= getLength(attributes)) THEN
      getType_cq = getType_attr(attributes%attr(n))
    ELSE
      getType_cq = ""
    END IF
  END FUNCTION getType_cq

  !> Return the type of an attribute.
  !> @param[in] attributes Attributes object.
  !> @param[in] qName QName of the attribute to search.
  !> @return Type.
  TYPE(varying_string) FUNCTION getType_vq(attributes, qName)
    USE Attr_m, ONLY: getType_attr
    USE iso_varying_string, ONLY: char
    IMPLICIT NONE
    TYPE(Attributes_t), INTENT(IN) :: attributes
    TYPE(varying_string), INTENT(IN) :: qName

    getType_vq = getType_cq(attributes, char(qName))
  END FUNCTION getType_vq

  !> Return the type of an attribute.
  !> @param[in] attributes Attributes object.
  !> @param[in] uri URI of the attribute to search.
  !> @param[in] localName Local Name of the attribute to search.
  !> @return Type.
  TYPE(varying_string) FUNCTION getType_cul(attributes, uri, localName)
    USE Attr_m, ONLY: getType_attr
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE
    TYPE(Attributes_t), INTENT(IN) :: attributes
    CHARACTER(LEN=*), INTENT(IN) :: uri, localName

    INTEGER :: n

    n = getIndex(attributes, uri, localName)
    IF(n > 0 .AND. n <= getLength(attributes)) THEN
      getType_cul = getType_attr(attributes%attr(n))
    ELSE
      getType_cul = ""
    END IF
  END FUNCTION getType_cul

  !> Return the type of an attribute.
  !> @param[in] attributes Attributes object.
  !> @param[in] uri URI of the attribute to search.
  !> @param[in] localName Local Name of the attribute to search.
  !> @return Type.
  TYPE(varying_string) FUNCTION getType_vul(attributes, uri, localName)
    USE Attr_m, ONLY: getType_attr
    USE iso_varying_string, ONLY: char
    IMPLICIT NONE
    TYPE(Attributes_t), INTENT(IN) :: attributes
    TYPE(varying_string), INTENT(IN) :: uri, localName

    getType_vul = getType_cul(attributes, char(uri), char(localName))
  END FUNCTION getType_vul

  !> Return the value of an attribute.
  !> @param[in] attributes Attributes object.
  !> @param[in] n Index of the requested attribute.
  !> @return Value.
  TYPE(varying_string) FUNCTION getValue_1(attributes, n)
    USE Attr_m, ONLY: getValue_attr
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE
    TYPE(Attributes_t), INTENT(IN) :: attributes
    INTEGER, INTENT(IN) :: n

    IF(n > 0 .AND. n <= getLength(attributes)) THEN
      getValue_1 = getValue_attr(attributes%attr(n))
    ELSE
      getValue_1 = ""
    END IF
  END FUNCTION getValue_1

  !> Return the value of an attribute.
  !> @param[in] attributes Attributes object.
  !> @param[in] qName QName of the attribute to search.
  !> @return Value.
  TYPE(varying_string) FUNCTION getValue_cq(attributes, qName)
    USE Attr_m, ONLY: getValue_attr
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE
    TYPE(Attributes_t), INTENT(IN) :: attributes
    CHARACTER(LEN=*), INTENT(IN) :: qName

    INTEGER :: n

    n = getIndex(attributes, qName)
    IF(n > 0 .AND. n <= getLength(attributes)) THEN
      getValue_cq = getValue_attr(attributes%attr(n))
    ELSE
      getValue_cq = ""
    END IF
  END FUNCTION getValue_cq

  !> Return the value of an attribute.
  !> @param[in] attributes Attributes object.
  !> @param[in] qName QName of the attribute to search.
  !> @return Value.
  TYPE(varying_string) FUNCTION getValue_vq(attributes, qName)
    USE Attr_m, ONLY: getValue_attr
    USE iso_varying_string, ONLY: char
    IMPLICIT NONE
    TYPE(Attributes_t), INTENT(IN) :: attributes
    TYPE(varying_string), INTENT(IN) :: qName

    getValue_vq = getValue_cq(attributes, char(qName))
  END FUNCTION getValue_vq

  !> Return the value of an attribute.
  !> @param[in] attributes Attributes object.
  !> @param[in] uri URI of the attribute to search.
  !> @param[in] localName Local Name of the attribute to search.
  !> @return Value.
  TYPE(varying_string) FUNCTION getValue_cul(attributes, uri, localName)
    USE Attr_m, ONLY: getValue_attr
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE
    TYPE(Attributes_t), INTENT(IN) :: attributes
    CHARACTER(LEN=*), INTENT(IN) :: uri, localName

    INTEGER :: n

    n = getIndex(attributes, uri, localName)
    IF(n > 0 .AND. n <= getLength(attributes)) THEN
      getValue_cul = getValue_attr(attributes%attr(n))
    ELSE
      getValue_cul = ""
    END IF
  END FUNCTION getValue_cul

  !> Return the value of an attribute.
  !> @param[in] attributes Attributes object.
  !> @param[in] uri URI of the attribute to search.
  !> @param[in] localName Local Name of the attribute to search.
  !> @return Value.
  TYPE(varying_string) FUNCTION getValue_vul(attributes, uri, localName)
    USE Attr_m, ONLY: getValue_attr
    USE iso_varying_string, ONLY: char
    IMPLICIT NONE
    TYPE(Attributes_t), INTENT(IN) :: attributes
    TYPE(varying_string), INTENT(IN) :: uri, localName

    getValue_vul = getValue_cul(attributes, char(uri), char(localName))
  END FUNCTION getValue_vul

  !> Return the URI of an attribute.
  !> @param[in] attributes Attributes object.
  !> @param[in] n Index of the requested attribute.
  !> @return URI.
  TYPE(varying_string) FUNCTION getURI(attributes, n)
    USE Attr_m, ONLY: getURI_attr
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE
    TYPE(Attributes_t), INTENT(IN) :: attributes
    INTEGER, INTENT(IN) :: n

    IF(n > 0 .AND. n <= getLength(attributes)) THEN
      getURI = getURI_attr(attributes%attr(n))
    ELSE
      getURI = ""
    END IF
  END FUNCTION getURI

  !> Append an attribute to vector of attributes.
  !> @param[in,out] attributes Attributes object.
  !> @param[in] attr Attribute to append.
  SUBROUTINE appendAttribute(attributes, attr)
    USE iso_varying_string, ONLY: varying_string
    IMPLICIT NONE
    TYPE(Attributes_t), INTENT(INOUT) :: attributes
    TYPE(Attr_t), INTENT(IN) :: attr

    TYPE(Attr_t), DIMENSION(:), ALLOCATABLE :: dummy
    INTEGER :: n
    
    n = getLength(attributes)
    IF(n > 0) THEN
      ALLOCATE(dummy(n))
      dummy = attributes%attr
      DEALLOCATE(attributes%attr)
      ALLOCATE(attributes%attr(n + 1))
      attributes%attr(1 : n) = dummy
      attributes%attr(n + 1) = attr
    ELSE
      ALLOCATE(attributes%attr(1))
      attributes%attr(1) = attr
    END IF
  END SUBROUTINE appendAttribute

END MODULE Attributes_m
