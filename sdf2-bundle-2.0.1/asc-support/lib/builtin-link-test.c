#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <MEPT-utils.h>
#include <aterm2.h>
#include <asc-support2-me.h>

/* DO NOT EDIT: This file is generated */

PT_Tree  ASFE_set_anno(PT_Tree input);
PT_Tree  ASC_set_anno(ATerm arg3, ATerm arg2, ATerm arg1);

PT_Tree  ASFE_get_anno(PT_Tree input);
PT_Tree  ASC_get_anno(ATerm arg2, ATerm arg1);

PT_Tree  ASFE_get_term_anno(PT_Tree input);
PT_Tree  ASC_get_term_anno(ATerm arg2, ATerm arg1);

PT_Tree  ASFE_read_term_from_file(PT_Tree input);
PT_Tree  ASC_read_term_from_file(ATerm arg1);

PT_Tree  ASFE_write_term_to_file(PT_Tree input);
PT_Tree  ASC_write_term_to_file(ATerm arg2, ATerm arg1);

PT_Tree  ASFE_shell(PT_Tree input);
PT_Tree  ASC_shell(ATerm arg1);

PT_Tree  ASFE_term_compare(PT_Tree input);
PT_Tree  ASC_term_compare(ATerm arg2, ATerm arg1);

PT_Tree  ASFE_lift_to_tree(PT_Tree input);
PT_Tree  ASC_lift_to_tree(ATerm arg1);

PT_Tree  ASFE_lift_to_term(PT_Tree input);
PT_Tree  ASC_lift_to_term(ATerm arg1);

PT_Tree  ASFE_unparse(PT_Tree input);
PT_Tree  ASC_unparse(ATerm arg1);

/* This code is not meant for execution, 
 * it is used to check if there is an implementation for every built-in
 */
int main(void)
{
  if (fprintf(stderr, "This program does nothing\n") == 0) {
    ASFE_set_anno(NULL);
    ASC_set_anno(NULL, NULL, NULL);

    ASFE_get_anno(NULL);
    ASC_get_anno(NULL, NULL);

    ASFE_get_term_anno(NULL);
    ASC_get_term_anno(NULL, NULL);

    ASFE_read_term_from_file(NULL);
    ASC_read_term_from_file(NULL);

    ASFE_write_term_to_file(NULL);
    ASC_write_term_to_file(NULL, NULL);

    ASFE_shell(NULL);
    ASC_shell(NULL);

    ASFE_term_compare(NULL);
    ASC_term_compare(NULL, NULL);

    ASFE_lift_to_tree(NULL);
    ASC_lift_to_tree(NULL);

    ASFE_lift_to_term(NULL);
    ASC_lift_to_term(NULL);

    ASFE_unparse(NULL);
    ASC_unparse(NULL);

  }

  return 0; 
}

