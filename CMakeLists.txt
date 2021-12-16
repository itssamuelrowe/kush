# Friday, November 03, 2017

cmake_minimum_required (VERSION 3.4.1)

# Project Name
project (Kush)

# Packages
include(FindPkgConfig)
pkg_search_module(JTK REQUIRED jtk)

find_package(LLVM REQUIRED CONFIG)
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
llvm_map_components_to_libnames(LLVM_LIBRARIES support core target)

# Project Version Number
set (KUSH_PROJECT_VERSION_MAJOR 0)
set (KUSH_PROJECT_VERSION_MINOR 1)
set (KUSH_PROJECT_VERSION_PATCH 0)
set (KUSH_PROJECT_VERSION_TWEAK 0)
set (KUSH_PROJECT_VERSION "${KUSH_PROJECT_VERSION_MAJOR}.${KUSH_PROJECT_VERSION_MINOR}.${KUSH_PROJECT_VERSION_PATCH}.${KUSH_PROJECT_VERSION_TWEAK}")

# Source
include_directories ("${PROJECT_SOURCE_DIR}/include")

set (
    KUSH_COMPILER_SOURCE

    ${PROJECT_SOURCE_DIR}/source/analyzer.c
    ${PROJECT_SOURCE_DIR}/source/compiler.c
    ${PROJECT_SOURCE_DIR}/source/configuration.c
    ${PROJECT_SOURCE_DIR}/source/context.c
    ${PROJECT_SOURCE_DIR}/source/error-handler.c
    ${PROJECT_SOURCE_DIR}/source/generator.c
    ${PROJECT_SOURCE_DIR}/source/kush.c
    ${PROJECT_SOURCE_DIR}/source/lexer.c
    ${PROJECT_SOURCE_DIR}/source/parser.c
    ${PROJECT_SOURCE_DIR}/source/scope.c
    # ${PROJECT_SOURCE_DIR}/source/symbol-loader.c
    ${PROJECT_SOURCE_DIR}/source/token.c
    ${PROJECT_SOURCE_DIR}/source/token-stream.c
)

# Compile

set(CMAKE_VERBOSE_MAKEFILE off)
link_directories(${JTK_LIBRARY_DIRS})
add_executable(kush ${KUSH_COMPILER_SOURCE})
target_link_libraries(kush ${JTK_LIBRARIES} ${LLVM_LIBRARIES} m)
target_include_directories(kush SYSTEM PUBLIC ${JTK_INCLUDE_DIRS})
# target_compile_options(kush PUBLIC -Wall -Wswitch)
target_compile_options(kush PUBLIC -g -Werror=return-type -Wall -Wno-switch ${JTK_CFLAGS} ${JTK_CFLAGS_OTHER})