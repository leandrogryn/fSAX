add_executable (testattr1.x attr1.F90)
target_link_libraries (testattr1.x fsax)

add_executable (testentity1.x entity1.F90)
target_link_libraries (testentity1.x fsax)

add_executable (testparse.x parse.F90)
target_link_libraries (testparse.x fsax)

add_executable (testread.x read.F90)
target_link_libraries (testread.x fsax)

install (FILES test.xml DESTINATION "${PROJECT_BINARY_DIR}/tests")
