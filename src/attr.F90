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

!> Module to handle each individual attribute (name and value).

MODULE Attr_m
  USE iso_varying_string, ONLY: varying_string
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: &
    Attr_t, &
    getLocalName_attr, &
    getURI_attr, &
    getQName_attr, &
    getType_attr, &
    getValue_attr, &
    setAttr

  TYPE :: Attr_t
    PRIVATE
    !> uri is the namespace
    !> This would be the structure in XML:
    !> [uri:]localName="value"
    !> Note that the URI with ':' is optional.
    TYPE(varying_string) :: uri, localName, value
  END TYPE Attr_t

CONTAINS

  !> Returns the local name of an attribute.
  !> @param[in] attr Attribute object.
  !> @return Local Name.
  TYPE(varying_string) FUNCTION getLocalName_attr(attr)
    IMPLICIT NONE
    TYPE(Attr_t), INTENT(IN) :: attr

    getLocalName_attr = attr%localName
  END FUNCTION getLocalName_attr

  !> Returns the URI of an attribute.
  !> @param[in] attr Attribute object.
  !> @return URI.
  TYPE(varying_string) FUNCTION getURI_attr(attr)
    IMPLICIT NONE
    TYPE(Attr_t), INTENT(IN) :: attr

    getURI_attr = attr%uri
  END FUNCTION getURI_attr

  !> Returns the QName of an attribute.
  !> @param[in] attr Attribute object.
  !> @return QName.
  TYPE(varying_string) FUNCTION getQName_attr(attr)
    USE iso_varying_string, ONLY: OPERATOR(//), OPERATOR(==)
    IMPLICIT NONE
    TYPE(Attr_t), INTENT(IN) :: attr

    IF(attr%uri == "") THEN
      getQName_attr = attr%localName
    ELSE
      getQName_attr = attr%uri//":"//attr%localName
    END IF
  END FUNCTION getQName_attr

  !> Returns the value of an attribute.
  !> @param[in] attr Attribute object.
  !> @return Value.
  TYPE(varying_string) FUNCTION getValue_attr(attr)
    IMPLICIT NONE
    TYPE(Attr_t), INTENT(IN) :: attr

    getValue_attr = attr%value
  END FUNCTION getValue_attr

  !> Returns the type of an attribute (always "CDATA").
  !> @param[in] attr Attribute object.
  !> @return Type.
  TYPE(varying_string) FUNCTION getType_attr(attr)
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE
    TYPE(Attr_t), INTENT(IN) :: attr

    getType_attr = "CDATA"
  END FUNCTION getType_attr

  !> Create an individual attribute.
  !> @param[in] uri URI of the new attribute.
  !> @param[in] localName Local name of the new attribute.
  !> @param[in] value Value of the new attribute.
  !> @return New attribute.
  FUNCTION setAttr(uri, localName, value) RESULT(attr)
    USE iso_varying_string, ONLY: ASSIGNMENT(=)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: uri, localName, value
    TYPE(Attr_t) :: attr

    attr%uri = uri
    attr%localName = localName
    attr%value = value
  END FUNCTION setAttr

END MODULE Attr_m
