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

add_executable(microgd
  ${SRC_FILES}
  main.c
)

set_property(TARGET microgd PROPERTY C_STANDARD 11)
target_include_directories(microgd PRIVATE ${CMAKE_CURRENT_LIST_DIR})
target_compile_definitions(microgd PRIVATE _CRT_SECURE_NO_WARNINGS)

add_executable(microgd_test
  ${SRC_FILES}
  preprocessor/macro_dict_test.h
  main_test.c
)

set_property(TARGET microgd_test PROPERTY C_STANDARD 11)
target_include_directories(microgd_test PRIVATE ${CMAKE_CURRENT_LIST_DIR})
target_compile_definitions(microgd_test PRIVATE _CRT_SECURE_NO_WARNINGS)
