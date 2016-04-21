/**
 * This file is generated by /users/protheo/vinju/software/bin/tifstoc. Do not edit!
 * Generated from tifs for tool 'sglr' (prefix='')
 * Implementation generated at Sat Nov 16 11:53:18 2002
 */

#include "sglr.tif.h"

#define NR_SIG_ENTRIES	7

static char *signature[NR_SIG_ENTRIES] = {
  "rec-eval(<sglr>,parse-string(<term>,<str>,<str>))",
  "rec-eval(<sglr>,parse-file(<term>,<str>,<str>))",
  "rec-eval(<sglr>,parse-string-as-asfix2me(<term>,<str>,<str>))",
  "rec-eval(<sglr>,open-language-from-term(<term>,<term>))",
  "rec-eval(<sglr>,open-language(<term>,<str>))",
  "rec-eval(<sglr>,close-language(<term>))",
  "rec-terminate(<sglr>,<term>)",
};

/* Event handler for tool 'sglr' */
ATerm sglr_handler(int conn, ATerm term)
{
  ATerm in, out;
  /* We need some temporary variables during matching */
  char *s0, *s1;
  ATerm t0, t1;

  if(ATmatch(term, "rec-eval(parse-string-as-asfix2me(<term>,<str>,<str>))", &t0, &s0, &s1)) {
    return parse_string_as_asfix2me(conn, t0, s0, s1);
  }
  if(ATmatch(term, "rec-eval(open-language-from-term(<term>,<term>))", &t0, &t1)) {
    return open_language_from_term(conn, t0, t1);
  }
  if(ATmatch(term, "rec-eval(parse-file(<term>,<str>,<str>))", &t0, &s0, &s1)) {
    return parse_file(conn, t0, s0, s1);
  }
  if(ATmatch(term, "rec-eval(open-language(<term>,<str>))", &t0, &s0)) {
    return open_language(conn, t0, s0);
  }
  if(ATmatch(term, "rec-eval(parse-string(<term>,<str>,<str>))", &t0, &s0, &s1)) {
    return parse_string(conn, t0, s0, s1);
  }
  if(ATmatch(term, "rec-eval(close-language(<term>))", &t0)) {
    return close_language(conn, t0);
  }
  if(ATmatch(term, "rec-terminate(<term>)", &t0)) {
    rec_terminate(conn, t0);
    return NULL;
  }
  if(ATmatch(term, "rec-do(signature(<term>,<term>))", &in, &out)) {
    ATerm result = sglr_checker(conn, in);
    if(!ATmatch(result, "[]"))
      ATfprintf(stderr, "warning: not in input signature:\n\t%\n\tl\n", result);
    return NULL;
  }

  ATerror("tool sglr cannot handle term %t", term);
  return NULL; /* Silence the compiler */
}

/* Check the signature of the tool 'sglr' */
ATerm sglr_checker(int conn, ATerm siglist)
{
  return ATBcheckSignature(siglist, signature, NR_SIG_ENTRIES);
}

