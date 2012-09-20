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

!> \mainpage
!>
!> \section Description
!>
!> fSAX is a library to read XML files written completely in Fortran.
!> The implementation tries to follow closely the original Simple API for XML
!> version 2 (SAX2) written in Java, however, some modifications had to be done
!> to write the code in Fortran and there some features that still need to be
!> implemented.
!> The library is mostly developed and maintained by Leandro D. Gryngarten at
!> the Computational Combustion Laboratory (CCL), directed by Prof. Suresh
!> Menon, in the Aerospace Engineering department of the Georgia Institute of
!> Technology.
!>
!> \section Language
!>
!> Especially for high-performance scientific codes written in Fortran that need
!> to run on a wide range of platforms, it is nice to have a library fully
!> written in Fortran that get the job done fast and right.
!> This library is written in Fortran 95 (F95). In addition, it has some widely
!> useful features from Fortran 2003 (F03), like allocatable arrays inside
!> derived types. However, the usage of F03 was limited to maximize
!> compatibility. The criteria to limit the utilization of F03 is that the
!> feature has to be implemeted in gFortran version 4.4.3 (the GNU Fortran
!> compiler), although this may change in the future.
!>
!> \section Documentation
!>
!> Additional introductory documentation can be found in <code>docs/html</code>.
!>
!> \section Introduction
!>
!> In order to minimize the duplication of data in memory and increase
!> performance, fSAX works based on event calls. This means that your
!> application calls the XML parser in the library, and the library will make
!> calls to subroutines implemented in your application based on the content of
!> the XML document. There are two types of subroutines needed in your
!> application, content handlers to process the information in the document,
!> and error handlers to process errors that the parser may potentially find in
!> the document. To make the usage of fSAX easier, a template source file,
!> <code>template.F90</code>, is located under the directory
!> <code>src/tests</code>. The figure below clarifies the idea (if it's not
!> clear yet, don't give up, it will be clear soon, just keep reading). You
!> should copy this template file to where your source code is and modify it
!> accordingly. Do not delete any subroutine defined in there, even if you
!> think that you don't need it.

!> This module defines all the procedures and derived types the user should be
!> able to access.

!> This module has to be included in a USE statement by the user.
!> No other module in fSAX needs to be included in a USE statement.

MODULE fSAX_m
  USE Attributes_m, ONLY: &
    Attributes_t, &
    getIndex, &
    getLength, &
    getLocalName, &
    getQName, &
    getType, &
    getURI, &
    getValue

  USE InputSource_m, ONLY: &
    getCharacterStream, &
    getEncoding, &
    getPublicId, &
    getSystemId, &
    InputSource_t, &
    setCharacterStream, &
    setEncoding, &
    setPublicId, &
    setSystemId

  USE iso_varying_string, ONLY: &
    ASSIGNMENT(=), &
    char

  USE Locator_m, ONLY: &
    Locator_t, &
    getColumnNumber, &
    getLineNumber, &
    getPublicId, &
    getSystemId

!  USE SAXException_m, ONLY: &
!    getMessage, &
!    SAXException_t, &
!    SAXException, &
!    toString
!
  USE SAXParseException_m, ONLY: &
    getColumnNumber, &
    getLineNumber, &
    getMessage, &
    getPublicId, &
    getSystemId, &
    SAXParseException_t, &
    SAXParseException, &
    toString

  USE XMLReader_m, ONLY: &
    !getContentHandler, &
    !getDTDHandler, &
    !getEntityResolver, &
    !getErrorHandler, &
    getFeature, &
    getProperty, &
    parse, &
    !setContentHandler, &
    !setDTDHandler, &
    !setEntityResolver, &
    !setErrorHandler, &
    setFeature, &
    setProperty, &
    XMLReader_t

  IMPLICIT NONE
  PUBLIC
END MODULE fSAX_m
