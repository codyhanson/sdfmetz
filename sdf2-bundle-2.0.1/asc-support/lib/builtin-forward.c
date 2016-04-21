#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <MEPT-utils.h>
#include <PTMEPT.h>
#include <aterm2.h>
#include "asc-builtins.h"
#include "Library.h"

/* DO NOT EDIT: This file is generated */

static ATbool initialized = ATfalse;

void initBuiltins(void)
{
  CO_initLibraryApi();
  PTPT_initPTMEPTApi();
  initialized = ATtrue;

  return;
}

/* Naive implementation: just do strcmp's until we find the correct
 * function. Idea: use asc-support or similar hashtable implementation.
 */
PT_Tree forwardBuiltin(ATerm builtin, PT_Tree input)
{
  AFun afun;
  char *name = NULL;
  PT_Tree result = input;

  assert(initialized && "builtins are not initialized");
  assert(ATgetType(builtin) == AT_APPL && "builtins should be ATermAppls");

  afun = ATgetAFun(builtin);
  name = ATgetName(afun);

  if (!strcmp(name, "set-anno")) {
    result = ASFE_set_anno(input);
  }
  if (!strcmp(name, "get-anno")) {
    result = ASFE_get_anno(input);
  }
  if (!strcmp(name, "get-term-anno")) {
    result = ASFE_get_term_anno(input);
  }
  if (!strcmp(name, "read-term-from-file")) {
    result = ASFE_read_term_from_file(input);
  }
  if (!strcmp(name, "write-term-to-file")) {
    result = ASFE_write_term_to_file(input);
  }
  if (!strcmp(name, "shell")) {
    result = ASFE_shell(input);
  }
  if (!strcmp(name, "term-compare")) {
    result = ASFE_term_compare(input);
  }
  if (!strcmp(name, "lift-to-tree")) {
    result = ASFE_lift_to_tree(input);
  }
  if (!strcmp(name, "lift-to-term")) {
    result = ASFE_lift_to_term(input);
  }
  if (!strcmp(name, "unparse")) {
    result = ASFE_unparse(input);
  }

  return result; 
}
