set(SRC_FILES
  preprocessor/macro_dict.c
  preprocessor/macro_dict.h
  array.h
  assert.h
  chars.h
  config.h
  tokenize.c
  tokenize.h
  error.c
  error.h
)

add_executable(fox
  ${SRC_FILES}
  main.c
)

set_property(TARGET fox PROPERTY C_STANDARD 11)
target_include_directories(fox PRIVATE ${CMAKE_CURRENT_LIST_DIR})
target_compile_definitions(fox PRIVATE _CRT_SECURE_NO_WARNINGS)

add_executable(fox_test
  ${SRC_FILES}
  preprocessor/macro_dict_test.h
  main_test.c
)

set_property(TARGET fox_test PROPERTY C_STANDARD 11)
target_include_directories(fox_test PRIVATE ${CMAKE_CURRENT_LIST_DIR})
target_compile_definitions(fox_test PRIVATE _CRT_SECURE_NO_WARNINGS)
