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


MODULE ErrorHandler_m
  IMPLICIT NONE
  PUBLIC

  INTERFACE

    !> Receive notification of a recoverable error.
    !> @param[in] exception Description of the exception.
    SUBROUTINE error(exception)
      USE SAXParseException_m, ONLY: SAXParseException_t
      IMPLICIT NONE
      TYPE(SAXParseException_t), INTENT(IN) :: exception
    END SUBROUTINE error

    !> Receive notification of a non-recoverable error.
    !> @param[in] exception Description of the exception.
    SUBROUTINE fatalError(exception)
      USE SAXParseException_m, ONLY: SAXParseException_t
      IMPLICIT NONE
      TYPE(SAXParseException_t), INTENT(IN) :: exception
    END SUBROUTINE fatalError

    !> Receive notification of a warning.
    !> @param[in] exception Description of the exception.
    SUBROUTINE warning(exception)
      USE SAXParseException_m, ONLY: SAXParseException_t
      IMPLICIT NONE
      TYPE(SAXParseException_t), INTENT(IN) :: exception
    END SUBROUTINE warning

  END INTERFACE

END MODULE ErrorHandler_m
