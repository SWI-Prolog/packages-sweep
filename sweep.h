#ifndef _SWEEP_H
#define _SWEEP_H

#include "emacs-module.h"
#include <SWI-Prolog.h>
#include <SWI-Stream.h>

static int         value_to_term(emacs_env*, emacs_value, term_t);
static emacs_value term_to_value(emacs_env*, term_t);
static char *      estring_to_cstring(emacs_env*, emacs_value, ptrdiff_t*);
static void        ethrow(emacs_env*, const char*);
/* static int         estring_to_atom(emacs_env*, emacs_value, term_t); */
/* static IOSTREAM *  estring_to_stream(emacs_env*, emacs_value); */

#endif /*_SWEEP_H*/
