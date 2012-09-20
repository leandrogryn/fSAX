cmake_minimum_required (VERSION 2.6)

project (fsax)

set(FSAX_MAJOR_VERSION 0)
set(FSAX_MINOR_VERSION 15)
set(FSAX_PATCH_VERSION 0)
set(FSAX_VERSION ${FSAX_MAJOR_VERSION}.${FSAX_MINOR_VERSION}.${FSAX_PATCH_VERSION})

###############################################################################
enable_language (Fortran)
include(CMakeDependentOption)

###############################################################################
# make sure that the default is a RELEASE
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are: NONE DEBUG RELEASE."
      FORCE)
endif (NOT CMAKE_BUILD_TYPE)

# default installation
get_filename_component (default_prefix ${CMAKE_BINARY_DIR} ABSOLUTE)
if(NOT CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX ${default_prefix} CACHE STRING
      "Path to the installation directory" FORCE)
endif(NOT CMAKE_INSTALL_PREFIX)

option (BUILD_DOCUMENTATION "Compile documentation." OFF)
option (BUILD_TESTING "Compile tests." OFF)

###############################################################################
include (compilerOptions.cmake)

###############################################################################
set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include")

configure_file(config.h.in "${CMAKE_BINARY_DIR}/config.h" @ONLY)
include_directories (${CMAKE_BINARY_DIR})

set (SRC_FILES attr.F90 attributes.F90 contenthandler.F90 dtd.F90 entities.F90 errorhandler.F90 fsax.F90 inputsource.F90 iso_varying_string.F90 locator.F90 saxexception.F90 saxio.F90 saxparseexception.F90 xmlreader.F90)
add_library (fsax ${SRC_FILES})

set (MOD_FILES ${CMAKE_BINARY_DIR}/include/attributes_m.mod
  ${CMAKE_BINARY_DIR}/include/attr_m.mod
  ${CMAKE_BINARY_DIR}/include/contenthandler_m.mod
  ${CMAKE_BINARY_DIR}/include/dtd_m.mod             
  ${CMAKE_BINARY_DIR}/include/entities_m.mod        
  ${CMAKE_BINARY_DIR}/include/errorhandler_m.mod
  ${CMAKE_BINARY_DIR}/include/fsax_m.mod
  ${CMAKE_BINARY_DIR}/include/inputsource_m.mod
  ${CMAKE_BINARY_DIR}/include/iso_varying_string.mod
  ${CMAKE_BINARY_DIR}/include/locator_m.mod
  ${CMAKE_BINARY_DIR}/include/saxexception_m.mod
  ${CMAKE_BINARY_DIR}/include/saxio_m.mod
  ${CMAKE_BINARY_DIR}/include/saxparseexception_m.mod
  ${CMAKE_BINARY_DIR}/include/xmlreader_m.mod)

install (TARGETS fsax ARCHIVE DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)
install (FILES ${MOD_FILES} DESTINATION ${CMAKE_INSTALL_PREFIX}/include)

###############################################################################
set(INSTALL_INCLUDE_DIRS "${CMAKE_INSTALL_PREFIX}/include")
set(INSTALL_LIBRARY_DIRS "${CMAKE_INSTALL_PREFIX}/lib")

configure_file(fsaxConfig.cmake.in 
    "${CMAKE_CURRENT_BINARY_DIR}/fsaxConfig.cmake" @ONLY)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/fsaxConfig.cmake"
    DESTINATION ${CMAKE_INSTALL_PREFIX})

configure_file(fsaxVersion.cmake.in 
    "${CMAKE_CURRENT_BINARY_DIR}/fsaxVersion.cmake" @ONLY)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/fsaxVersion.cmake"
    DESTINATION ${CMAKE_INSTALL_PREFIX})

###############################################################################
if (BUILD_TESTING)
  include_directories (tests)
  add_subdirectory (tests)

  enable_testing()

  set (CTEST_START_WITH_EMPTY_BINARY_DIRECTORY_ONCE TRUE)

  add_test(TestAttr1 tests/testattr1.x)
  set_tests_properties(TestAttr1
      PROPERTIES PASS_REGULAR_EXPRESSION "TestPassed")
  add_test(TestEntity1 tests/testentity1.x)
  set_tests_properties(TestEntity1
      PROPERTIES PASS_REGULAR_EXPRESSION "TestPassed")
endif (BUILD_TESTING)

###############################################################################

if (BUILD_DOCUMENTATION)
  include (FindDoxygen)
  if (DOXYGEN)
    set(DOXYGEN_SOURCE_DIR ${CMAKE_SOURCE_DIR})
    set(DOXYGEN_OUTPUT_DIR ${CMAKE_BINARY_DIR}/documents)
    string(REGEX REPLACE ";" " " DOXYGEN_INPUT_LIST "${DOXYGEN_SOURCE_DIR}")
    configure_file(doxygen.conf.in ${CMAKE_CURRENT_BINARY_DIR}/doxygen.conf)
    set(HTML_TARGET "html")
    add_custom_target(${HTML_TARGET} ALL
      doxygen ${CMAKE_CURRENT_BINARY_DIR}/doxygen.conf
      DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/doxygen.conf)
  else (DOXYGEN)
    message(STATUS "WARNING: Doxygen not found - Reference manual will not be created")
  endif (DOXYGEN)
endif (BUILD_DOCUMENTATION)
