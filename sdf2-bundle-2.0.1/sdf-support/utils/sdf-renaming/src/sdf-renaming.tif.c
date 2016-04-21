/**
 * This file is generated by tifstoc. Do not edit!
 * Generated from tifs for tool 'sdf-renaming' (prefix='')
 * Implementation generated at Mon May 26 13:06:43 2003
 */

#include "sdf-renaming.tif.h"

#define NR_SIG_ENTRIES	2

static char *signature[NR_SIG_ENTRIES] = {
  "rec-eval(<sdf-renaming>,rename-module(<term>,<str>,<term>))",
  "rec-terminate(<sdf-renaming>,<term>)",
};

/* Event handler for tool 'sdf-renaming' */
ATerm sdf_renaming_handler(int conn, ATerm term)
{
  ATerm in, out;
  /* We need some temporary variables during matching */
  char *s0;
  ATerm t0, t1;

  if(ATmatch(term, "rec-eval(rename-module(<term>,<str>,<term>))", &t0, &s0, &t1)) {
    return rename_module(conn, t0, s0, t1);
  }
  if(ATmatch(term, "rec-terminate(<term>)", &t0)) {
    rec_terminate(conn, t0);
    return NULL;
  }
  if(ATmatch(term, "rec-do(signature(<term>,<term>))", &in, &out)) {
    ATerm result = sdf_renaming_checker(conn, in);
    if(!ATmatch(result, "[]"))
      ATfprintf(stderr, "warning: not in input signature:\n\t%\n\tl\n", result);
    return NULL;
  }

  ATerror("tool sdf-renaming cannot handle term %t", term);
  return NULL; /* Silence the compiler */
}

/* Check the signature of the tool 'sdf-renaming' */
ATerm sdf_renaming_checker(int conn, ATerm siglist)
{
  return ATBcheckSignature(siglist, signature, NR_SIG_ENTRIES);
}
