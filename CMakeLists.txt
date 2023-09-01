cmake_minimum_required(VERSION 3.5)
project(swipl-sweep)

include("../cmake/PrologPackage.cmake")

find_path(
  EMACS_INCLUDE_DIR
  NAMES "emacs-module.h"
  PATHS /Applications/Emacs.app/Contents/Resources/include
	${MINGW_ROOT}/include
)

if(NOT EMACS_INCLUDE_DIR)
  message(WARNING "Could not find emacs-module.h in system headers.  Using our local copy from ${CMAKE_CURRENT_SOURCE_DIR}")
  set(EMACS_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
endif()

swipl_plugin(
  sweep-module
  C_SOURCES sweep.c
  C_INCLUDE_DIR ${EMACS_INCLUDE_DIR}
  PL_LIBS sweep_link.pl)

pkg_doc(
  sweep
  SECTION
  SOURCES sweep.doc)

