/*
  tre_regerror.c - POSIX tre_regerror() implementation for TRE.

  This software is released under a BSD-style license.
  See the file LICENSE for details and copyright.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <string.h>
#ifdef HAVE_WCHAR_H
#include <wchar.h>
#endif /* HAVE_WCHAR_H */
#ifdef HAVE_WCTYPE_H
#include <wctype.h>
#endif /* HAVE_WCTYPE_H */

#include "tre-internal.h"
#include "tre.h"

#include <Localization.h>

/* Error message strings for error codes listed in `tre.h'.  This list
   needs to be in sync with the codes listed there, naturally. */
static const char *tre_error_messages[] =
  { N_("No error"),				 /* REG_OK */
    N_("No match"),				 /* REG_NOMATCH */
    N_("Invalid regexp"),			 /* REG_BADPAT */
    N_("Unknown collating element"),		 /* REG_ECOLLATE */
    N_("Unknown character class name"),	 /* REG_ECTYPE */
    N_("Trailing backslash"),			 /* REG_EESCAPE */
    N_("Invalid back reference"),		 /* REG_ESUBREG */
    N_("Missing ']'"),			 /* REG_EBRACK */
    N_("Missing ')'"),			 /* REG_EPAREN */
    N_("Missing '}'"),			 /* REG_EBRACE */
    N_("Invalid contents of {}"),		 /* REG_BADBR */
    N_("Invalid character range"),		 /* REG_ERANGE */
    N_("Out of memory"),			 /* REG_ESPACE */
    N_("Invalid use of repetition operators")	 /* REG_BADRPT */
  };

size_t
tre_regerror(int errcode, const regex_t *preg, char *errbuf, size_t errbuf_size)
{
  const char *err;
  size_t err_len;

  /*LINTED*/(void)&preg;
  if (errcode >= 0
      && errcode < (int)(sizeof(tre_error_messages)
			 / sizeof(*tre_error_messages)))
    err = _(tre_error_messages[errcode]);
  else
    err = _("Unknown error");

  err_len = strlen(err) + 1;
  if (errbuf_size > 0 && errbuf != NULL)
    {
      if (err_len > errbuf_size)
	{
	  strncpy(errbuf, err, errbuf_size - 1);
	  errbuf[errbuf_size - 1] = '\0';
	}
      else
	{
	  strcpy(errbuf, err);
	}
    }
  return err_len;
}

/* EOF */
