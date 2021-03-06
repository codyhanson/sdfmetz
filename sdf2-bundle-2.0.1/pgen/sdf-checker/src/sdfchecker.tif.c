/**
 * This file is generated by tifstoc. Do not edit!
 * Generated from tifs for tool 'sdfchecker' (prefix='')
 * Implementation generated at Mon Sep  9 17:13:19 2002
 */

#include "sdfchecker.tif.h"

#define NR_SIG_ENTRIES	2

static char *signature[NR_SIG_ENTRIES] = {
  "rec-eval(<sdfchecker>,check-sdf(<term>))",
  "rec-terminate(<sdfchecker>,<term>)",
};

/* Event handler for tool 'sdfchecker' */
ATerm sdfchecker_handler(int conn, ATerm term)
{
  ATerm in, out;
  /* We need some temporary variables during matching */
  ATerm t0;

  if(ATmatch(term, "rec-eval(check-sdf(<term>))", &t0)) {
    return check_sdf(conn, t0);
  }
  if(ATmatch(term, "rec-terminate(<term>)", &t0)) {
    rec_terminate(conn, t0);
    return NULL;
  }
  if(ATmatch(term, "rec-do(signature(<term>,<term>))", &in, &out)) {
    ATerm result = sdfchecker_checker(conn, in);
    if(!ATmatch(result, "[]"))
      ATfprintf(stderr, "warning: not in input signature:\n\t%\n\tl\n", result);
    return NULL;
  }

  ATerror("tool sdfchecker cannot handle term %t", term);
  return NULL; /* Silence the compiler */
}

/* Check the signature of the tool 'sdfchecker' */
ATerm sdfchecker_checker(int conn, ATerm siglist)
{
  return ATBcheckSignature(siglist, signature, NR_SIG_ENTRIES);
}

