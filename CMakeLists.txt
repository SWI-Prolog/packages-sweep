cmake_minimum_required(VERSION 3.5)
project(swipl-sweep)

include("../cmake/PrologPackage.cmake")

find_program(EMACS emacs Emacs)

find_path(
  EMACS_INCLUDE_DIR
  NAMES "emacs-module.h"
  PATHS /Applications/Emacs.app/Contents/Resources/include
)

if(EMACS_INCLUDE_DIR)

  swipl_plugin(
    sweep-module
    C_SOURCES sweep.c
    C_INCLUDE_DIR ${EMACS_INCLUDE_DIR}
    PL_LIBS sweep.pl)

  pkg_doc(
    sweep
    SECTION
    SOURCES sweep.doc)


endif(EMACS_INCLUDE_DIR)
