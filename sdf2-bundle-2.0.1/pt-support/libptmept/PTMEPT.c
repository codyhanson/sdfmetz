#include <assert.h>

#include <aterm2.h>
#include <deprecated.h>
#include "PTMEPT.h"

/*{{{  typedefs */

typedef struct ATerm _PTPT_NatCon;
typedef struct ATerm _PTPT_IntCon;
typedef struct ATerm _PTPT_Tree;
typedef struct ATerm _PTPT_Args;
typedef struct ATerm _PTPT_TreeList;
typedef struct ATerm _PTPT_Production;
typedef struct ATerm _PTPT_Symbol;
typedef struct ATerm _PTPT_Symbols;
typedef struct ATerm _PTPT_SymbolList;
typedef struct ATerm _PTPT_CharRanges;
typedef struct ATerm _PTPT_CharRangeList;
typedef struct ATerm _PTPT_CharRange;
typedef struct ATerm _PTPT_OptExp;
typedef struct ATerm _PTPT_RealCon;
typedef struct ATerm _PTPT_ATermList;
typedef struct ATerm _PTPT_ATermElems;
typedef struct ATerm _PTPT_ACon;
typedef struct ATerm _PTPT_AFun;
typedef struct ATerm _PTPT_ATerm;
typedef struct ATerm _PTPT_ATermArgs;
typedef struct ATerm _PTPT_Ann;
typedef struct ATerm _PTPT_ATermAnnos;
typedef struct ATerm _PTPT_AlphaNumericalEscChar;
typedef struct ATerm _PTPT_DecimalEscChar;
typedef struct ATerm _PTPT_EscChar;
typedef struct ATerm _PTPT_LChar;
typedef struct ATerm _PTPT_QLiteral;
typedef struct ATerm _PTPT_UQLiteral;
typedef struct ATerm _PTPT_Literal;
typedef struct ATerm _PTPT_Attributes;
typedef struct ATerm _PTPT_Attrs;
typedef struct ATerm _PTPT_AttrList;
typedef struct ATerm _PTPT_Attr;
typedef struct ATerm _PTPT_Associativity;
typedef struct ATerm _PTPT_ParseTree;
typedef struct ATerm _PTPT_Start;
typedef struct ATerm _PTPT_OptLayout;

/*}}}  */

/*{{{  void PTPT_initPTMEPTApi(void) */

void PTPT_initPTMEPTApi(void)
{
  init_PTMEPT_dict();
}

/*}}}  */

/*{{{  term conversion functions */

/*{{{  PTPT_NatCon PTPT_NatConFromTerm(ATerm t) */

PTPT_NatCon PTPT_NatConFromTerm(ATerm t)
{
  return (PTPT_NatCon)t;
}

/*}}}  */
/*{{{  ATerm PTPT_NatConToTerm(PTPT_NatCon arg) */

ATerm PTPT_NatConToTerm(PTPT_NatCon arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_IntCon PTPT_IntConFromTerm(ATerm t) */

PTPT_IntCon PTPT_IntConFromTerm(ATerm t)
{
  return (PTPT_IntCon)t;
}

/*}}}  */
/*{{{  ATerm PTPT_IntConToTerm(PTPT_IntCon arg) */

ATerm PTPT_IntConToTerm(PTPT_IntCon arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_Tree PTPT_TreeFromTerm(ATerm t) */

PTPT_Tree PTPT_TreeFromTerm(ATerm t)
{
  return (PTPT_Tree)t;
}

/*}}}  */
/*{{{  ATerm PTPT_TreeToTerm(PTPT_Tree arg) */

ATerm PTPT_TreeToTerm(PTPT_Tree arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_Args PTPT_ArgsFromTerm(ATerm t) */

PTPT_Args PTPT_ArgsFromTerm(ATerm t)
{
  return (PTPT_Args)t;
}

/*}}}  */
/*{{{  ATerm PTPT_ArgsToTerm(PTPT_Args arg) */

ATerm PTPT_ArgsToTerm(PTPT_Args arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_TreeList PTPT_TreeListFromTerm(ATerm t) */

PTPT_TreeList PTPT_TreeListFromTerm(ATerm t)
{
  return (PTPT_TreeList)t;
}

/*}}}  */
/*{{{  ATerm PTPT_TreeListToTerm(PTPT_TreeList arg) */

ATerm PTPT_TreeListToTerm(PTPT_TreeList arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_Production PTPT_ProductionFromTerm(ATerm t) */

PTPT_Production PTPT_ProductionFromTerm(ATerm t)
{
  return (PTPT_Production)t;
}

/*}}}  */
/*{{{  ATerm PTPT_ProductionToTerm(PTPT_Production arg) */

ATerm PTPT_ProductionToTerm(PTPT_Production arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_SymbolFromTerm(ATerm t) */

PTPT_Symbol PTPT_SymbolFromTerm(ATerm t)
{
  return (PTPT_Symbol)t;
}

/*}}}  */
/*{{{  ATerm PTPT_SymbolToTerm(PTPT_Symbol arg) */

ATerm PTPT_SymbolToTerm(PTPT_Symbol arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_Symbols PTPT_SymbolsFromTerm(ATerm t) */

PTPT_Symbols PTPT_SymbolsFromTerm(ATerm t)
{
  return (PTPT_Symbols)t;
}

/*}}}  */
/*{{{  ATerm PTPT_SymbolsToTerm(PTPT_Symbols arg) */

ATerm PTPT_SymbolsToTerm(PTPT_Symbols arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_SymbolList PTPT_SymbolListFromTerm(ATerm t) */

PTPT_SymbolList PTPT_SymbolListFromTerm(ATerm t)
{
  return (PTPT_SymbolList)t;
}

/*}}}  */
/*{{{  ATerm PTPT_SymbolListToTerm(PTPT_SymbolList arg) */

ATerm PTPT_SymbolListToTerm(PTPT_SymbolList arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_CharRanges PTPT_CharRangesFromTerm(ATerm t) */

PTPT_CharRanges PTPT_CharRangesFromTerm(ATerm t)
{
  return (PTPT_CharRanges)t;
}

/*}}}  */
/*{{{  ATerm PTPT_CharRangesToTerm(PTPT_CharRanges arg) */

ATerm PTPT_CharRangesToTerm(PTPT_CharRanges arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_CharRangeList PTPT_CharRangeListFromTerm(ATerm t) */

PTPT_CharRangeList PTPT_CharRangeListFromTerm(ATerm t)
{
  return (PTPT_CharRangeList)t;
}

/*}}}  */
/*{{{  ATerm PTPT_CharRangeListToTerm(PTPT_CharRangeList arg) */

ATerm PTPT_CharRangeListToTerm(PTPT_CharRangeList arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_CharRange PTPT_CharRangeFromTerm(ATerm t) */

PTPT_CharRange PTPT_CharRangeFromTerm(ATerm t)
{
  return (PTPT_CharRange)t;
}

/*}}}  */
/*{{{  ATerm PTPT_CharRangeToTerm(PTPT_CharRange arg) */

ATerm PTPT_CharRangeToTerm(PTPT_CharRange arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_OptExp PTPT_OptExpFromTerm(ATerm t) */

PTPT_OptExp PTPT_OptExpFromTerm(ATerm t)
{
  return (PTPT_OptExp)t;
}

/*}}}  */
/*{{{  ATerm PTPT_OptExpToTerm(PTPT_OptExp arg) */

ATerm PTPT_OptExpToTerm(PTPT_OptExp arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_RealCon PTPT_RealConFromTerm(ATerm t) */

PTPT_RealCon PTPT_RealConFromTerm(ATerm t)
{
  return (PTPT_RealCon)t;
}

/*}}}  */
/*{{{  ATerm PTPT_RealConToTerm(PTPT_RealCon arg) */

ATerm PTPT_RealConToTerm(PTPT_RealCon arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_ATermList PTPT_ATermListFromTerm(ATerm t) */

PTPT_ATermList PTPT_ATermListFromTerm(ATerm t)
{
  return (PTPT_ATermList)t;
}

/*}}}  */
/*{{{  ATerm PTPT_ATermListToTerm(PTPT_ATermList arg) */

ATerm PTPT_ATermListToTerm(PTPT_ATermList arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_ATermElems PTPT_ATermElemsFromTerm(ATerm t) */

PTPT_ATermElems PTPT_ATermElemsFromTerm(ATerm t)
{
  return (PTPT_ATermElems)t;
}

/*}}}  */
/*{{{  ATerm PTPT_ATermElemsToTerm(PTPT_ATermElems arg) */

ATerm PTPT_ATermElemsToTerm(PTPT_ATermElems arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_ACon PTPT_AConFromTerm(ATerm t) */

PTPT_ACon PTPT_AConFromTerm(ATerm t)
{
  return (PTPT_ACon)t;
}

/*}}}  */
/*{{{  ATerm PTPT_AConToTerm(PTPT_ACon arg) */

ATerm PTPT_AConToTerm(PTPT_ACon arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_AFun PTPT_AFunFromTerm(ATerm t) */

PTPT_AFun PTPT_AFunFromTerm(ATerm t)
{
  return (PTPT_AFun)t;
}

/*}}}  */
/*{{{  ATerm PTPT_AFunToTerm(PTPT_AFun arg) */

ATerm PTPT_AFunToTerm(PTPT_AFun arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_ATerm PTPT_ATermFromTerm(ATerm t) */

PTPT_ATerm PTPT_ATermFromTerm(ATerm t)
{
  return (PTPT_ATerm)t;
}

/*}}}  */
/*{{{  ATerm PTPT_ATermToTerm(PTPT_ATerm arg) */

ATerm PTPT_ATermToTerm(PTPT_ATerm arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_ATermArgs PTPT_ATermArgsFromTerm(ATerm t) */

PTPT_ATermArgs PTPT_ATermArgsFromTerm(ATerm t)
{
  return (PTPT_ATermArgs)t;
}

/*}}}  */
/*{{{  ATerm PTPT_ATermArgsToTerm(PTPT_ATermArgs arg) */

ATerm PTPT_ATermArgsToTerm(PTPT_ATermArgs arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_Ann PTPT_AnnFromTerm(ATerm t) */

PTPT_Ann PTPT_AnnFromTerm(ATerm t)
{
  return (PTPT_Ann)t;
}

/*}}}  */
/*{{{  ATerm PTPT_AnnToTerm(PTPT_Ann arg) */

ATerm PTPT_AnnToTerm(PTPT_Ann arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_ATermAnnos PTPT_ATermAnnosFromTerm(ATerm t) */

PTPT_ATermAnnos PTPT_ATermAnnosFromTerm(ATerm t)
{
  return (PTPT_ATermAnnos)t;
}

/*}}}  */
/*{{{  ATerm PTPT_ATermAnnosToTerm(PTPT_ATermAnnos arg) */

ATerm PTPT_ATermAnnosToTerm(PTPT_ATermAnnos arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_AlphaNumericalEscChar PTPT_AlphaNumericalEscCharFromTerm(ATerm t) */

PTPT_AlphaNumericalEscChar PTPT_AlphaNumericalEscCharFromTerm(ATerm t)
{
  return (PTPT_AlphaNumericalEscChar)t;
}

/*}}}  */
/*{{{  ATerm PTPT_AlphaNumericalEscCharToTerm(PTPT_AlphaNumericalEscChar arg) */

ATerm PTPT_AlphaNumericalEscCharToTerm(PTPT_AlphaNumericalEscChar arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_DecimalEscChar PTPT_DecimalEscCharFromTerm(ATerm t) */

PTPT_DecimalEscChar PTPT_DecimalEscCharFromTerm(ATerm t)
{
  return (PTPT_DecimalEscChar)t;
}

/*}}}  */
/*{{{  ATerm PTPT_DecimalEscCharToTerm(PTPT_DecimalEscChar arg) */

ATerm PTPT_DecimalEscCharToTerm(PTPT_DecimalEscChar arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_EscChar PTPT_EscCharFromTerm(ATerm t) */

PTPT_EscChar PTPT_EscCharFromTerm(ATerm t)
{
  return (PTPT_EscChar)t;
}

/*}}}  */
/*{{{  ATerm PTPT_EscCharToTerm(PTPT_EscChar arg) */

ATerm PTPT_EscCharToTerm(PTPT_EscChar arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_LChar PTPT_LCharFromTerm(ATerm t) */

PTPT_LChar PTPT_LCharFromTerm(ATerm t)
{
  return (PTPT_LChar)t;
}

/*}}}  */
/*{{{  ATerm PTPT_LCharToTerm(PTPT_LChar arg) */

ATerm PTPT_LCharToTerm(PTPT_LChar arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_QLiteral PTPT_QLiteralFromTerm(ATerm t) */

PTPT_QLiteral PTPT_QLiteralFromTerm(ATerm t)
{
  return (PTPT_QLiteral)t;
}

/*}}}  */
/*{{{  ATerm PTPT_QLiteralToTerm(PTPT_QLiteral arg) */

ATerm PTPT_QLiteralToTerm(PTPT_QLiteral arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_UQLiteral PTPT_UQLiteralFromTerm(ATerm t) */

PTPT_UQLiteral PTPT_UQLiteralFromTerm(ATerm t)
{
  return (PTPT_UQLiteral)t;
}

/*}}}  */
/*{{{  ATerm PTPT_UQLiteralToTerm(PTPT_UQLiteral arg) */

ATerm PTPT_UQLiteralToTerm(PTPT_UQLiteral arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_Literal PTPT_LiteralFromTerm(ATerm t) */

PTPT_Literal PTPT_LiteralFromTerm(ATerm t)
{
  return (PTPT_Literal)t;
}

/*}}}  */
/*{{{  ATerm PTPT_LiteralToTerm(PTPT_Literal arg) */

ATerm PTPT_LiteralToTerm(PTPT_Literal arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_Attributes PTPT_AttributesFromTerm(ATerm t) */

PTPT_Attributes PTPT_AttributesFromTerm(ATerm t)
{
  return (PTPT_Attributes)t;
}

/*}}}  */
/*{{{  ATerm PTPT_AttributesToTerm(PTPT_Attributes arg) */

ATerm PTPT_AttributesToTerm(PTPT_Attributes arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_Attrs PTPT_AttrsFromTerm(ATerm t) */

PTPT_Attrs PTPT_AttrsFromTerm(ATerm t)
{
  return (PTPT_Attrs)t;
}

/*}}}  */
/*{{{  ATerm PTPT_AttrsToTerm(PTPT_Attrs arg) */

ATerm PTPT_AttrsToTerm(PTPT_Attrs arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_AttrList PTPT_AttrListFromTerm(ATerm t) */

PTPT_AttrList PTPT_AttrListFromTerm(ATerm t)
{
  return (PTPT_AttrList)t;
}

/*}}}  */
/*{{{  ATerm PTPT_AttrListToTerm(PTPT_AttrList arg) */

ATerm PTPT_AttrListToTerm(PTPT_AttrList arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_Attr PTPT_AttrFromTerm(ATerm t) */

PTPT_Attr PTPT_AttrFromTerm(ATerm t)
{
  return (PTPT_Attr)t;
}

/*}}}  */
/*{{{  ATerm PTPT_AttrToTerm(PTPT_Attr arg) */

ATerm PTPT_AttrToTerm(PTPT_Attr arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_Associativity PTPT_AssociativityFromTerm(ATerm t) */

PTPT_Associativity PTPT_AssociativityFromTerm(ATerm t)
{
  return (PTPT_Associativity)t;
}

/*}}}  */
/*{{{  ATerm PTPT_AssociativityToTerm(PTPT_Associativity arg) */

ATerm PTPT_AssociativityToTerm(PTPT_Associativity arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_ParseTree PTPT_ParseTreeFromTerm(ATerm t) */

PTPT_ParseTree PTPT_ParseTreeFromTerm(ATerm t)
{
  return (PTPT_ParseTree)t;
}

/*}}}  */
/*{{{  ATerm PTPT_ParseTreeToTerm(PTPT_ParseTree arg) */

ATerm PTPT_ParseTreeToTerm(PTPT_ParseTree arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_Start PTPT_StartFromTerm(ATerm t) */

PTPT_Start PTPT_StartFromTerm(ATerm t)
{
  return (PTPT_Start)t;
}

/*}}}  */
/*{{{  ATerm PTPT_StartToTerm(PTPT_Start arg) */

ATerm PTPT_StartToTerm(PTPT_Start arg)
{
  return (ATerm)arg;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_OptLayoutFromTerm(ATerm t) */

PTPT_OptLayout PTPT_OptLayoutFromTerm(ATerm t)
{
  return (PTPT_OptLayout)t;
}

/*}}}  */
/*{{{  ATerm PTPT_OptLayoutToTerm(PTPT_OptLayout arg) */

ATerm PTPT_OptLayoutToTerm(PTPT_OptLayout arg)
{
  return (ATerm)arg;
}

/*}}}  */

/*}}}  */
/*{{{  constructors */

/*{{{  PTPT_NatCon PTPT_makeNatConDigits(PTPT_CHARLIST chars) */

PTPT_NatCon PTPT_makeNatConDigits(PTPT_CHARLIST chars)
{
  return (PTPT_NatCon)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun2, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun4)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun4))), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATmakeList1((ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl1(PTPT_afun7, (ATerm)ATmakeAppl1(PTPT_afun8, (ATerm)ATmakeAppl1(PTPT_afun9, (ATerm)ATmakeList1((ATerm)ATmakeAppl2(PTPT_afun10, (ATerm)ATmakeInt(0), (ATerm)ATmakeInt(255)))))), (ATerm)chars)));
}

/*}}}  */
/*{{{  PTPT_IntCon PTPT_makeIntConNatural(PTPT_NatCon NatCon) */

PTPT_IntCon PTPT_makeIntConNatural(PTPT_NatCon NatCon)
{
  return (PTPT_IntCon)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun4)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun11))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun15)))))), (ATerm)ATmakeList1((ATerm)NatCon));
}

/*}}}  */
/*{{{  PTPT_IntCon PTPT_makeIntConPositive(PTPT_OptLayout wsAfterPos, PTPT_NatCon NatCon) */

PTPT_IntCon PTPT_makeIntConPositive(PTPT_OptLayout wsAfterPos, PTPT_NatCon NatCon)
{
  return (PTPT_IntCon)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun4)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun19))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun11))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun20)))))), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)NatCon), (ATerm)wsAfterPos), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun19))));
}

/*}}}  */
/*{{{  PTPT_IntCon PTPT_makeIntConNegative(PTPT_OptLayout wsAfterNeg, PTPT_NatCon NatCon) */

PTPT_IntCon PTPT_makeIntConNegative(PTPT_OptLayout wsAfterNeg, PTPT_NatCon NatCon)
{
  return (PTPT_IntCon)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun4)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun21))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun11))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun22)))))), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)NatCon), (ATerm)wsAfterNeg), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun21))));
}

/*}}}  */
/*{{{  PTPT_Tree PTPT_makeTreeAnnotated(PTPT_Tree Tree, PTPT_OptLayout wsAfterTree, PTPT_Ann Ann) */

PTPT_Tree PTPT_makeTreeAnnotated(PTPT_Tree Tree, PTPT_OptLayout wsAfterTree, PTPT_Ann Ann)
{
  return (PTPT_Tree)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun23)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun24)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun24))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun25)))))), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)Ann), (ATerm)wsAfterTree), (ATerm)Tree));
}

/*}}}  */
/*{{{  PTPT_Tree PTPT_makeTreeAppl(PTPT_OptLayout wsAfterAppl, PTPT_OptLayout wsAfterParenOpen, PTPT_Production prod, PTPT_OptLayout wsAfterProd, PTPT_OptLayout wsAfterComma, PTPT_Args args, PTPT_OptLayout wsAfterArgs) */

PTPT_Tree PTPT_makeTreeAppl(PTPT_OptLayout wsAfterAppl, PTPT_OptLayout wsAfterParenOpen, PTPT_Production prod, PTPT_OptLayout wsAfterProd, PTPT_OptLayout wsAfterComma, PTPT_Args args, PTPT_OptLayout wsAfterArgs)
{
  return (PTPT_Tree)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun27)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun29)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun31))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun24))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun31)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterArgs), (ATerm)args), (ATerm)wsAfterComma), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)wsAfterProd), (ATerm)prod), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterAppl), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun31))));
}

/*}}}  */
/*{{{  PTPT_Tree PTPT_makeTreeChar(PTPT_NatCon character) */

PTPT_Tree PTPT_makeTreeChar(PTPT_NatCon character)
{
  return (PTPT_Tree)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun4)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun24))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun32)))))), (ATerm)ATmakeList1((ATerm)character));
}

/*}}}  */
/*{{{  PTPT_Tree PTPT_makeTreeLit(PTPT_OptLayout wsAfterLit, PTPT_OptLayout wsAfterParenOpen, PTPT_QLiteral string, PTPT_OptLayout wsAfterString) */

PTPT_Tree PTPT_makeTreeLit(PTPT_OptLayout wsAfterLit, PTPT_OptLayout wsAfterParenOpen, PTPT_QLiteral string, PTPT_OptLayout wsAfterString)
{
  return (PTPT_Tree)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun33)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun34))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun24))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun34)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterString), (ATerm)string), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterLit), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun34))));
}

/*}}}  */
/*{{{  PTPT_Tree PTPT_makeTreeAmb(PTPT_OptLayout wsAfterAmb, PTPT_OptLayout wsAfterParenOpen, PTPT_Args args, PTPT_OptLayout wsAfterArgs) */

PTPT_Tree PTPT_makeTreeAmb(PTPT_OptLayout wsAfterAmb, PTPT_OptLayout wsAfterParenOpen, PTPT_Args args, PTPT_OptLayout wsAfterArgs)
{
  return (PTPT_Tree)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun27)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun35))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun24))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun35)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterArgs), (ATerm)args), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterAmb), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun35))));
}

/*}}}  */
/*{{{  PTPT_Args PTPT_makeArgsList(PTPT_OptLayout wsAfterBracketOpen, PTPT_TreeList list, PTPT_OptLayout wsAfterList) */

PTPT_Args PTPT_makeArgsList(PTPT_OptLayout wsAfterBracketOpen, PTPT_TreeList list, PTPT_OptLayout wsAfterList)
{
  return (PTPT_Args)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun36))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl2(PTPT_afun37, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun24)), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun38))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun27))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun39)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun36))), (ATerm)wsAfterList), (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl1(PTPT_afun7, (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl2(PTPT_afun37, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun24)), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))))), (ATerm)list)), (ATerm)wsAfterBracketOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun38))));
}

/*}}}  */
/*{{{  PTPT_TreeList PTPT_makeTreeListEmpty() */

PTPT_TreeList PTPT_makeTreeListEmpty()
{
  return (PTPT_TreeList)(ATerm)ATempty;
}

/*}}}  */
/*{{{  PTPT_TreeList PTPT_makeTreeListSingle(PTPT_Tree head) */

PTPT_TreeList PTPT_makeTreeListSingle(PTPT_Tree head)
{
  return (PTPT_TreeList)(ATerm)ATmakeList1((ATerm)head);
}

/*}}}  */
/*{{{  PTPT_TreeList PTPT_makeTreeListMany(PTPT_Tree head, PTPT_OptLayout wsAfterFirst, char * sep, PTPT_OptLayout wsAfterSep, PTPT_TreeList tail) */

PTPT_TreeList PTPT_makeTreeListMany(PTPT_Tree head, PTPT_OptLayout wsAfterFirst, char * sep, PTPT_OptLayout wsAfterSep, PTPT_TreeList tail)
{
  return (PTPT_TreeList)(ATerm)ATinsert(ATinsert(ATinsert(ATinsert((ATermList)tail, (ATerm)wsAfterSep), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(ATmakeAFun(sep, 0, ATtrue)))), (ATerm)wsAfterFirst), (ATerm)head);
}

/*}}}  */
/*{{{  PTPT_Production PTPT_makeProductionDefault(PTPT_OptLayout wsAfterProd, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbols lhs, PTPT_OptLayout wsAfterLhs, PTPT_OptLayout wsAfterComma1, PTPT_Symbol rhs, PTPT_OptLayout wsAfterRhs, PTPT_OptLayout wsAfterComma2, PTPT_Attributes attributes, PTPT_OptLayout wsAfterAttributes) */

PTPT_Production PTPT_makeProductionDefault(PTPT_OptLayout wsAfterProd, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbols lhs, PTPT_OptLayout wsAfterLhs, PTPT_OptLayout wsAfterComma1, PTPT_Symbol rhs, PTPT_OptLayout wsAfterRhs, PTPT_OptLayout wsAfterComma2, PTPT_Attributes attributes, PTPT_OptLayout wsAfterAttributes)
{
  return (PTPT_Production)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun40)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun42)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun43))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun29))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun44)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterAttributes), (ATerm)attributes), (ATerm)wsAfterComma2), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)wsAfterRhs), (ATerm)rhs), (ATerm)wsAfterComma1), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)wsAfterLhs), (ATerm)lhs), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterProd), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun43))));
}

/*}}}  */
/*{{{  PTPT_Production PTPT_makeProductionList(PTPT_OptLayout wsAfterList, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol rhs, PTPT_OptLayout wsAfterRhs) */

PTPT_Production PTPT_makeProductionList(PTPT_OptLayout wsAfterList, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol rhs, PTPT_OptLayout wsAfterRhs)
{
  return (PTPT_Production)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun39))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun29))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun39)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterRhs), (ATerm)rhs), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterList), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun39))));
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_makeSymbolEmpty() */

PTPT_Symbol PTPT_makeSymbolEmpty()
{
  return (PTPT_Symbol)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun45))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun45)))))), (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun45))));
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_makeSymbolLit(PTPT_OptLayout wsAfterLit, PTPT_OptLayout wsAfterParenOpen, PTPT_QLiteral string, PTPT_OptLayout wsAfterString) */

PTPT_Symbol PTPT_makeSymbolLit(PTPT_OptLayout wsAfterLit, PTPT_OptLayout wsAfterParenOpen, PTPT_QLiteral string, PTPT_OptLayout wsAfterString)
{
  return (PTPT_Symbol)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun33)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun34))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun34)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterString), (ATerm)string), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterLit), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun34))));
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_makeSymbolCf(PTPT_OptLayout wsAfterCf, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol symbol, PTPT_OptLayout wsAfterSymbol) */

PTPT_Symbol PTPT_makeSymbolCf(PTPT_OptLayout wsAfterCf, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol symbol, PTPT_OptLayout wsAfterSymbol)
{
  return (PTPT_Symbol)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun46))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun46)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterSymbol), (ATerm)symbol), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterCf), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun46))));
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_makeSymbolLex(PTPT_OptLayout wsAfterLex, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol symbol, PTPT_OptLayout wsAfterSymbol) */

PTPT_Symbol PTPT_makeSymbolLex(PTPT_OptLayout wsAfterLex, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol symbol, PTPT_OptLayout wsAfterSymbol)
{
  return (PTPT_Symbol)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun47))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun47)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterSymbol), (ATerm)symbol), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterLex), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun47))));
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_makeSymbolOpt(PTPT_OptLayout wsAfterOpt, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol symbol, PTPT_OptLayout wsAfterSymbol) */

PTPT_Symbol PTPT_makeSymbolOpt(PTPT_OptLayout wsAfterOpt, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol symbol, PTPT_OptLayout wsAfterSymbol)
{
  return (PTPT_Symbol)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun48))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun48)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterSymbol), (ATerm)symbol), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterOpt), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun48))));
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_makeSymbolAlt(PTPT_OptLayout wsAfterAlt, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol lhs, PTPT_OptLayout wsAfterLhs, PTPT_OptLayout wsAfterComma, PTPT_Symbol rhs, PTPT_OptLayout wsAfterRhs) */

PTPT_Symbol PTPT_makeSymbolAlt(PTPT_OptLayout wsAfterAlt, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol lhs, PTPT_OptLayout wsAfterLhs, PTPT_OptLayout wsAfterComma, PTPT_Symbol rhs, PTPT_OptLayout wsAfterRhs)
{
  return (PTPT_Symbol)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun49))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun49)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterRhs), (ATerm)rhs), (ATerm)wsAfterComma), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)wsAfterLhs), (ATerm)lhs), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterAlt), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun49))));
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_makeSymbolSeq(PTPT_OptLayout wsAfterSeq, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol lhs, PTPT_OptLayout wsAfterLhs, PTPT_OptLayout wsAfterComma, PTPT_Symbol rhs, PTPT_OptLayout wsAfterRhs) */

PTPT_Symbol PTPT_makeSymbolSeq(PTPT_OptLayout wsAfterSeq, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol lhs, PTPT_OptLayout wsAfterLhs, PTPT_OptLayout wsAfterComma, PTPT_Symbol rhs, PTPT_OptLayout wsAfterRhs)
{
  return (PTPT_Symbol)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun50))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun50)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterRhs), (ATerm)rhs), (ATerm)wsAfterComma), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)wsAfterLhs), (ATerm)lhs), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterSeq), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun50))));
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_makeSymbolTuple(PTPT_OptLayout wsAfterTuple, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol head, PTPT_OptLayout wsAfterHead, PTPT_OptLayout wsAfterComma, PTPT_Symbols rest, PTPT_OptLayout wsAfterRest) */

PTPT_Symbol PTPT_makeSymbolTuple(PTPT_OptLayout wsAfterTuple, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol head, PTPT_OptLayout wsAfterHead, PTPT_OptLayout wsAfterComma, PTPT_Symbols rest, PTPT_OptLayout wsAfterRest)
{
  return (PTPT_Symbol)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun42)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun51))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun51)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterRest), (ATerm)rest), (ATerm)wsAfterComma), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)wsAfterHead), (ATerm)head), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterTuple), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun51))));
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_makeSymbolSort(PTPT_OptLayout wsAfterSort, PTPT_OptLayout wsAfterParenOpen, PTPT_QLiteral string, PTPT_OptLayout wsAfterString) */

PTPT_Symbol PTPT_makeSymbolSort(PTPT_OptLayout wsAfterSort, PTPT_OptLayout wsAfterParenOpen, PTPT_QLiteral string, PTPT_OptLayout wsAfterString)
{
  return (PTPT_Symbol)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun33)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun52))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun52)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterString), (ATerm)string), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterSort), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun52))));
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_makeSymbolIter(PTPT_OptLayout wsAfterIter, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol symbol, PTPT_OptLayout wsAfterSymbol) */

PTPT_Symbol PTPT_makeSymbolIter(PTPT_OptLayout wsAfterIter, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol symbol, PTPT_OptLayout wsAfterSymbol)
{
  return (PTPT_Symbol)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun53))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun53)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterSymbol), (ATerm)symbol), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterIter), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun53))));
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_makeSymbolIterStar(PTPT_OptLayout wsAfterIterStar, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol symbol, PTPT_OptLayout wsAfterSymbol) */

PTPT_Symbol PTPT_makeSymbolIterStar(PTPT_OptLayout wsAfterIterStar, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol symbol, PTPT_OptLayout wsAfterSymbol)
{
  return (PTPT_Symbol)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun54))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun54)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterSymbol), (ATerm)symbol), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterIterStar), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun54))));
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_makeSymbolIterSep(PTPT_OptLayout wsAfterIterSep, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol symbol, PTPT_OptLayout wsAfterSymbol, PTPT_OptLayout wsAfterComma, PTPT_Symbol separator, PTPT_OptLayout wsAfterSeparator) */

PTPT_Symbol PTPT_makeSymbolIterSep(PTPT_OptLayout wsAfterIterSep, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol symbol, PTPT_OptLayout wsAfterSymbol, PTPT_OptLayout wsAfterComma, PTPT_Symbol separator, PTPT_OptLayout wsAfterSeparator)
{
  return (PTPT_Symbol)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun55))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun55)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterSeparator), (ATerm)separator), (ATerm)wsAfterComma), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)wsAfterSymbol), (ATerm)symbol), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterIterSep), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun55))));
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_makeSymbolIterStarSep(PTPT_OptLayout wsAfterIterStarSep, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol symbol, PTPT_OptLayout wsAfterSymbol, PTPT_OptLayout wsAfterComma, PTPT_Symbol separator, PTPT_OptLayout wsAfterSeparator) */

PTPT_Symbol PTPT_makeSymbolIterStarSep(PTPT_OptLayout wsAfterIterStarSep, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol symbol, PTPT_OptLayout wsAfterSymbol, PTPT_OptLayout wsAfterComma, PTPT_Symbol separator, PTPT_OptLayout wsAfterSeparator)
{
  return (PTPT_Symbol)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun56))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun56)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterSeparator), (ATerm)separator), (ATerm)wsAfterComma), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)wsAfterSymbol), (ATerm)symbol), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterIterStarSep), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun56))));
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_makeSymbolIterN(PTPT_OptLayout wsAfterIterN, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol symbol, PTPT_OptLayout wsAfterSymbol, PTPT_OptLayout wsAfterComma, PTPT_NatCon number, PTPT_OptLayout wsAfterNumber) */

PTPT_Symbol PTPT_makeSymbolIterN(PTPT_OptLayout wsAfterIterN, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol symbol, PTPT_OptLayout wsAfterSymbol, PTPT_OptLayout wsAfterComma, PTPT_NatCon number, PTPT_OptLayout wsAfterNumber)
{
  return (PTPT_Symbol)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun4)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun57))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun57)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterNumber), (ATerm)number), (ATerm)wsAfterComma), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)wsAfterSymbol), (ATerm)symbol), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterIterN), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun57))));
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_makeSymbolIterSepN(PTPT_OptLayout wsAfterIterSepN, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol symbol, PTPT_OptLayout wsAfterSymbol, PTPT_OptLayout wsAfterComma1, PTPT_Symbol separator, PTPT_OptLayout wsAfterSeparator, PTPT_OptLayout wsAfterComma2, PTPT_NatCon number, PTPT_OptLayout wsAfterNumber) */

PTPT_Symbol PTPT_makeSymbolIterSepN(PTPT_OptLayout wsAfterIterSepN, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol symbol, PTPT_OptLayout wsAfterSymbol, PTPT_OptLayout wsAfterComma1, PTPT_Symbol separator, PTPT_OptLayout wsAfterSeparator, PTPT_OptLayout wsAfterComma2, PTPT_NatCon number, PTPT_OptLayout wsAfterNumber)
{
  return (PTPT_Symbol)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun4)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun58))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun58)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterNumber), (ATerm)number), (ATerm)wsAfterComma2), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)wsAfterSeparator), (ATerm)separator), (ATerm)wsAfterComma1), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)wsAfterSymbol), (ATerm)symbol), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterIterSepN), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun58))));
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_makeSymbolFunc(PTPT_OptLayout wsAfterFunc, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbols symbols, PTPT_OptLayout wsAfterSymbols, PTPT_OptLayout wsAfterComma, PTPT_Symbol symbol, PTPT_OptLayout wsAfterSymbol) */

PTPT_Symbol PTPT_makeSymbolFunc(PTPT_OptLayout wsAfterFunc, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbols symbols, PTPT_OptLayout wsAfterSymbols, PTPT_OptLayout wsAfterComma, PTPT_Symbol symbol, PTPT_OptLayout wsAfterSymbol)
{
  return (PTPT_Symbol)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun42)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun59))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun59)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterSymbol), (ATerm)symbol), (ATerm)wsAfterComma), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)wsAfterSymbols), (ATerm)symbols), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterFunc), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun59))));
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_makeSymbolVarsym(PTPT_OptLayout wsAfterVarsym, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol symbol, PTPT_OptLayout wsAfterSymbol) */

PTPT_Symbol PTPT_makeSymbolVarsym(PTPT_OptLayout wsAfterVarsym, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol symbol, PTPT_OptLayout wsAfterSymbol)
{
  return (PTPT_Symbol)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun60))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun60)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterSymbol), (ATerm)symbol), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterVarsym), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun60))));
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_makeSymbolLayout() */

PTPT_Symbol PTPT_makeSymbolLayout()
{
  return (PTPT_Symbol)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun61))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun61)))))), (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun61))));
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_makeSymbolCharClass(PTPT_OptLayout wsAfterCharClass, PTPT_OptLayout wsAfterParenOpen, PTPT_CharRanges CharRanges, PTPT_OptLayout wsAfterCharRanges) */

PTPT_Symbol PTPT_makeSymbolCharClass(PTPT_OptLayout wsAfterCharClass, PTPT_OptLayout wsAfterParenOpen, PTPT_CharRanges CharRanges, PTPT_OptLayout wsAfterCharRanges)
{
  return (PTPT_Symbol)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun62)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun63))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun63)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterCharRanges), (ATerm)CharRanges), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterCharClass), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun63))));
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_makeSymbolStrategy(PTPT_OptLayout wsAfterStrategy, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol lhs, PTPT_OptLayout wsAfterLhs, PTPT_OptLayout wsAfterComma, PTPT_Symbol rhs, PTPT_OptLayout wsAfterRhs) */

PTPT_Symbol PTPT_makeSymbolStrategy(PTPT_OptLayout wsAfterStrategy, PTPT_OptLayout wsAfterParenOpen, PTPT_Symbol lhs, PTPT_OptLayout wsAfterLhs, PTPT_OptLayout wsAfterComma, PTPT_Symbol rhs, PTPT_OptLayout wsAfterRhs)
{
  return (PTPT_Symbol)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun64))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun64)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterRhs), (ATerm)rhs), (ATerm)wsAfterComma), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)wsAfterLhs), (ATerm)lhs), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterStrategy), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun64))));
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_makeSymbolParametrizedSort(PTPT_OptLayout wsAfterParametrizedSort, PTPT_OptLayout wsAfterParenOpen, PTPT_QLiteral sort, PTPT_OptLayout wsAfterSort, PTPT_OptLayout wsAfterComma, PTPT_Symbols parameters, PTPT_OptLayout wsAfterParameters) */

PTPT_Symbol PTPT_makeSymbolParametrizedSort(PTPT_OptLayout wsAfterParametrizedSort, PTPT_OptLayout wsAfterParenOpen, PTPT_QLiteral sort, PTPT_OptLayout wsAfterSort, PTPT_OptLayout wsAfterComma, PTPT_Symbols parameters, PTPT_OptLayout wsAfterParameters)
{
  return (PTPT_Symbol)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun42)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun33)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun65))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun65)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterParameters), (ATerm)parameters), (ATerm)wsAfterComma), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)wsAfterSort), (ATerm)sort), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterParametrizedSort), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun65))));
}

/*}}}  */
/*{{{  PTPT_Symbols PTPT_makeSymbolsList(PTPT_OptLayout wsAfterBracketOpen, PTPT_SymbolList list, PTPT_OptLayout wsAfterList) */

PTPT_Symbols PTPT_makeSymbolsList(PTPT_OptLayout wsAfterBracketOpen, PTPT_SymbolList list, PTPT_OptLayout wsAfterList)
{
  return (PTPT_Symbols)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun36))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl2(PTPT_afun37, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41)), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun38))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun42))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun39)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun36))), (ATerm)wsAfterList), (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl1(PTPT_afun7, (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl2(PTPT_afun37, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41)), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))))), (ATerm)list)), (ATerm)wsAfterBracketOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun38))));
}

/*}}}  */
/*{{{  PTPT_SymbolList PTPT_makeSymbolListEmpty() */

PTPT_SymbolList PTPT_makeSymbolListEmpty()
{
  return (PTPT_SymbolList)(ATerm)ATempty;
}

/*}}}  */
/*{{{  PTPT_SymbolList PTPT_makeSymbolListSingle(PTPT_Symbol head) */

PTPT_SymbolList PTPT_makeSymbolListSingle(PTPT_Symbol head)
{
  return (PTPT_SymbolList)(ATerm)ATmakeList1((ATerm)head);
}

/*}}}  */
/*{{{  PTPT_SymbolList PTPT_makeSymbolListMany(PTPT_Symbol head, PTPT_OptLayout wsAfterFirst, char * sep, PTPT_OptLayout wsAfterSep, PTPT_SymbolList tail) */

PTPT_SymbolList PTPT_makeSymbolListMany(PTPT_Symbol head, PTPT_OptLayout wsAfterFirst, char * sep, PTPT_OptLayout wsAfterSep, PTPT_SymbolList tail)
{
  return (PTPT_SymbolList)(ATerm)ATinsert(ATinsert(ATinsert(ATinsert((ATermList)tail, (ATerm)wsAfterSep), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(ATmakeAFun(sep, 0, ATtrue)))), (ATerm)wsAfterFirst), (ATerm)head);
}

/*}}}  */
/*{{{  PTPT_CharRanges PTPT_makeCharRangesList(PTPT_OptLayout wsAfterBracketOpen, PTPT_CharRangeList list, PTPT_OptLayout wsAfterList) */

PTPT_CharRanges PTPT_makeCharRangesList(PTPT_OptLayout wsAfterBracketOpen, PTPT_CharRangeList list, PTPT_OptLayout wsAfterList)
{
  return (PTPT_CharRanges)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun36))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl2(PTPT_afun37, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun66)), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun38))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun62))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun39)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun36))), (ATerm)wsAfterList), (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl1(PTPT_afun7, (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl2(PTPT_afun37, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun66)), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))))), (ATerm)list)), (ATerm)wsAfterBracketOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun38))));
}

/*}}}  */
/*{{{  PTPT_CharRangeList PTPT_makeCharRangeListEmpty() */

PTPT_CharRangeList PTPT_makeCharRangeListEmpty()
{
  return (PTPT_CharRangeList)(ATerm)ATempty;
}

/*}}}  */
/*{{{  PTPT_CharRangeList PTPT_makeCharRangeListSingle(PTPT_CharRange head) */

PTPT_CharRangeList PTPT_makeCharRangeListSingle(PTPT_CharRange head)
{
  return (PTPT_CharRangeList)(ATerm)ATmakeList1((ATerm)head);
}

/*}}}  */
/*{{{  PTPT_CharRangeList PTPT_makeCharRangeListMany(PTPT_CharRange head, PTPT_OptLayout wsAfterFirst, char * sep, PTPT_OptLayout wsAfterSep, PTPT_CharRangeList tail) */

PTPT_CharRangeList PTPT_makeCharRangeListMany(PTPT_CharRange head, PTPT_OptLayout wsAfterFirst, char * sep, PTPT_OptLayout wsAfterSep, PTPT_CharRangeList tail)
{
  return (PTPT_CharRangeList)(ATerm)ATinsert(ATinsert(ATinsert(ATinsert((ATermList)tail, (ATerm)wsAfterSep), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(ATmakeAFun(sep, 0, ATtrue)))), (ATerm)wsAfterFirst), (ATerm)head);
}

/*}}}  */
/*{{{  PTPT_CharRange PTPT_makeCharRangeCharacter(PTPT_NatCon integer) */

PTPT_CharRange PTPT_makeCharRangeCharacter(PTPT_NatCon integer)
{
  return (PTPT_CharRange)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun4)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun66))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun67)))))), (ATerm)ATmakeList1((ATerm)integer));
}

/*}}}  */
/*{{{  PTPT_CharRange PTPT_makeCharRangeRange(PTPT_OptLayout wsAfterRange, PTPT_OptLayout wsAfterParenOpen, PTPT_NatCon start, PTPT_OptLayout wsAfterStart, PTPT_OptLayout wsAfterComma, PTPT_NatCon end, PTPT_OptLayout wsAfterEnd) */

PTPT_CharRange PTPT_makeCharRangeRange(PTPT_OptLayout wsAfterRange, PTPT_OptLayout wsAfterParenOpen, PTPT_NatCon start, PTPT_OptLayout wsAfterStart, PTPT_OptLayout wsAfterComma, PTPT_NatCon end, PTPT_OptLayout wsAfterEnd)
{
  return (PTPT_CharRange)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun4)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun4)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun68))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun66))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun68)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterEnd), (ATerm)end), (ATerm)wsAfterComma), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)wsAfterStart), (ATerm)start), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterRange), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun68))));
}

/*}}}  */
/*{{{  PTPT_OptExp PTPT_makeOptExpPresent(PTPT_OptLayout wsAfterE, PTPT_IntCon IntCon) */

PTPT_OptExp PTPT_makeOptExpPresent(PTPT_OptLayout wsAfterE, PTPT_IntCon IntCon)
{
  return (PTPT_OptExp)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun11)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun69))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun70))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun71)))))), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)IntCon), (ATerm)wsAfterE), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun69))));
}

/*}}}  */
/*{{{  PTPT_OptExp PTPT_makeOptExpAbsent() */

PTPT_OptExp PTPT_makeOptExpAbsent()
{
  return (PTPT_OptExp)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATempty, (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun70))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun72)))))), (ATerm)ATempty);
}

/*}}}  */
/*{{{  PTPT_RealCon PTPT_makeRealConRealCon(PTPT_IntCon IntCon, PTPT_OptLayout wsAfterIntCon, PTPT_OptLayout wsAfterPeriod, PTPT_NatCon NatCon, PTPT_OptLayout wsAfterNatCon, PTPT_OptExp OptExp) */

PTPT_RealCon PTPT_makeRealConRealCon(PTPT_IntCon IntCon, PTPT_OptLayout wsAfterIntCon, PTPT_OptLayout wsAfterPeriod, PTPT_NatCon NatCon, PTPT_OptLayout wsAfterNatCon, PTPT_OptExp OptExp)
{
  return (PTPT_RealCon)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun70)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun4)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun73))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun11)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun74))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun75)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)OptExp), (ATerm)wsAfterNatCon), (ATerm)NatCon), (ATerm)wsAfterPeriod), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun73))), (ATerm)wsAfterIntCon), (ATerm)IntCon));
}

/*}}}  */
/*{{{  PTPT_ATermList PTPT_makeATermListNotEmpty(PTPT_OptLayout wsAfterBracketOpen, PTPT_ATermElems elems, PTPT_OptLayout wsAfterElems) */

PTPT_ATermList PTPT_makeATermListNotEmpty(PTPT_OptLayout wsAfterBracketOpen, PTPT_ATermElems elems, PTPT_OptLayout wsAfterElems)
{
  return (PTPT_ATermList)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun36))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl2(PTPT_afun37, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun76)), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun38))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun77))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun78)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun36))), (ATerm)wsAfterElems), (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl1(PTPT_afun7, (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl2(PTPT_afun37, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun76)), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))))), (ATerm)elems)), (ATerm)wsAfterBracketOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun38))));
}

/*}}}  */
/*{{{  PTPT_ATermElems PTPT_makeATermElemsEmpty() */

PTPT_ATermElems PTPT_makeATermElemsEmpty()
{
  return (PTPT_ATermElems)(ATerm)ATempty;
}

/*}}}  */
/*{{{  PTPT_ATermElems PTPT_makeATermElemsSingle(PTPT_ATerm head) */

PTPT_ATermElems PTPT_makeATermElemsSingle(PTPT_ATerm head)
{
  return (PTPT_ATermElems)(ATerm)ATmakeList1((ATerm)head);
}

/*}}}  */
/*{{{  PTPT_ATermElems PTPT_makeATermElemsMany(PTPT_ATerm head, PTPT_OptLayout wsAfterFirst, char * sep, PTPT_OptLayout wsAfterSep, PTPT_ATermElems tail) */

PTPT_ATermElems PTPT_makeATermElemsMany(PTPT_ATerm head, PTPT_OptLayout wsAfterFirst, char * sep, PTPT_OptLayout wsAfterSep, PTPT_ATermElems tail)
{
  return (PTPT_ATermElems)(ATerm)ATinsert(ATinsert(ATinsert(ATinsert((ATermList)tail, (ATerm)wsAfterSep), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(ATmakeAFun(sep, 0, ATtrue)))), (ATerm)wsAfterFirst), (ATerm)head);
}

/*}}}  */
/*{{{  PTPT_ACon PTPT_makeAConInt(PTPT_IntCon IntCon) */

PTPT_ACon PTPT_makeAConInt(PTPT_IntCon IntCon)
{
  return (PTPT_ACon)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun11)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun79))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun80)))))), (ATerm)ATmakeList1((ATerm)IntCon));
}

/*}}}  */
/*{{{  PTPT_ACon PTPT_makeAConReal(PTPT_RealCon RealCon) */

PTPT_ACon PTPT_makeAConReal(PTPT_RealCon RealCon)
{
  return (PTPT_ACon)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun74)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun79))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun81)))))), (ATerm)ATmakeList1((ATerm)RealCon));
}

/*}}}  */
/*{{{  PTPT_AFun PTPT_makeAFunDefault(PTPT_Literal Literal) */

PTPT_AFun PTPT_makeAFunDefault(PTPT_Literal Literal)
{
  return (PTPT_AFun)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun82)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun83))), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATmakeList1((ATerm)Literal));
}

/*}}}  */
/*{{{  PTPT_ATerm PTPT_makeATermConstant(PTPT_ACon ACon) */

PTPT_ATerm PTPT_makeATermConstant(PTPT_ACon ACon)
{
  return (PTPT_ATerm)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun79)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun76))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun84)))))), (ATerm)ATmakeList1((ATerm)ACon));
}

/*}}}  */
/*{{{  PTPT_ATerm PTPT_makeATermList(PTPT_ATermList list) */

PTPT_ATerm PTPT_makeATermList(PTPT_ATermList list)
{
  return (PTPT_ATerm)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun77)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun76))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun39)))))), (ATerm)ATmakeList1((ATerm)list));
}

/*}}}  */
/*{{{  PTPT_ATerm PTPT_makeATermFun(PTPT_AFun fun) */

PTPT_ATerm PTPT_makeATermFun(PTPT_AFun fun)
{
  return (PTPT_ATerm)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun83)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun76))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun85)))))), (ATerm)ATmakeList1((ATerm)fun));
}

/*}}}  */
/*{{{  PTPT_ATerm PTPT_makeATermAppl(PTPT_AFun fun, PTPT_OptLayout wsAfterFun, PTPT_OptLayout wsAfterParenOpen, PTPT_ATermArgs args, PTPT_OptLayout wsAfterArgs) */

PTPT_ATerm PTPT_makeATermAppl(PTPT_AFun fun, PTPT_OptLayout wsAfterFun, PTPT_OptLayout wsAfterParenOpen, PTPT_ATermArgs args, PTPT_OptLayout wsAfterArgs)
{
  return (PTPT_ATerm)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl2(PTPT_afun86, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun76)), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun83)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun76))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun31)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterArgs), (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl1(PTPT_afun7, (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl2(PTPT_afun86, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun76)), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))))), (ATerm)args)), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterFun), (ATerm)fun));
}

/*}}}  */
/*{{{  PTPT_ATerm PTPT_makeATermAnnotatedConstant(PTPT_ACon ACon, PTPT_OptLayout wsAfterACon, PTPT_Ann Ann) */

PTPT_ATerm PTPT_makeATermAnnotatedConstant(PTPT_ACon ACon, PTPT_OptLayout wsAfterACon, PTPT_Ann Ann)
{
  return (PTPT_ATerm)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun23)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun79)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun76))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun87)))))), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)Ann), (ATerm)wsAfterACon), (ATerm)ACon));
}

/*}}}  */
/*{{{  PTPT_ATerm PTPT_makeATermAnnotatedList(PTPT_ATermList list, PTPT_OptLayout wsAfterList, PTPT_Ann Ann) */

PTPT_ATerm PTPT_makeATermAnnotatedList(PTPT_ATermList list, PTPT_OptLayout wsAfterList, PTPT_Ann Ann)
{
  return (PTPT_ATerm)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun23)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun77)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun76))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun88)))))), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)Ann), (ATerm)wsAfterList), (ATerm)list));
}

/*}}}  */
/*{{{  PTPT_ATerm PTPT_makeATermAnnotatedFun(PTPT_AFun fun, PTPT_OptLayout wsAfterFun, PTPT_Ann Ann) */

PTPT_ATerm PTPT_makeATermAnnotatedFun(PTPT_AFun fun, PTPT_OptLayout wsAfterFun, PTPT_Ann Ann)
{
  return (PTPT_ATerm)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun23)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun83)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun76))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun89)))))), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)Ann), (ATerm)wsAfterFun), (ATerm)fun));
}

/*}}}  */
/*{{{  PTPT_ATerm PTPT_makeATermAnnotatedAppl(PTPT_AFun fun, PTPT_OptLayout wsAfterFun, PTPT_OptLayout wsAfterParenOpen, PTPT_ATermArgs args, PTPT_OptLayout wsAfterArgs, PTPT_OptLayout wsAfterParenClose, PTPT_Ann Ann) */

PTPT_ATerm PTPT_makeATermAnnotatedAppl(PTPT_AFun fun, PTPT_OptLayout wsAfterFun, PTPT_OptLayout wsAfterParenOpen, PTPT_ATermArgs args, PTPT_OptLayout wsAfterArgs, PTPT_OptLayout wsAfterParenClose, PTPT_Ann Ann)
{
  return (PTPT_ATerm)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun23)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl2(PTPT_afun86, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun76)), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun83)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun76))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun90)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)Ann), (ATerm)wsAfterParenClose), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterArgs), (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl1(PTPT_afun7, (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl2(PTPT_afun86, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun76)), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))))), (ATerm)args)), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterFun), (ATerm)fun));
}

/*}}}  */
/*{{{  PTPT_ATermArgs PTPT_makeATermArgsSingle(PTPT_ATerm head) */

PTPT_ATermArgs PTPT_makeATermArgsSingle(PTPT_ATerm head)
{
  return (PTPT_ATermArgs)(ATerm)ATmakeList1((ATerm)head);
}

/*}}}  */
/*{{{  PTPT_ATermArgs PTPT_makeATermArgsMany(PTPT_ATerm head, PTPT_OptLayout wsAfterFirst, char * sep, PTPT_OptLayout wsAfterSep, PTPT_ATermArgs tail) */

PTPT_ATermArgs PTPT_makeATermArgsMany(PTPT_ATerm head, PTPT_OptLayout wsAfterFirst, char * sep, PTPT_OptLayout wsAfterSep, PTPT_ATermArgs tail)
{
  return (PTPT_ATermArgs)(ATerm)ATinsert(ATinsert(ATinsert(ATinsert((ATermList)tail, (ATerm)wsAfterSep), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(ATmakeAFun(sep, 0, ATtrue)))), (ATerm)wsAfterFirst), (ATerm)head);
}

/*}}}  */
/*{{{  PTPT_Ann PTPT_makeAnnAnnotation(PTPT_OptLayout wsAfterBraceOpen, PTPT_ATermAnnos annos, PTPT_OptLayout wsAfterAnnos) */

PTPT_Ann PTPT_makeAnnAnnotation(PTPT_OptLayout wsAfterBraceOpen, PTPT_ATermAnnos annos, PTPT_OptLayout wsAfterAnnos)
{
  return (PTPT_Ann)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun91))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl2(PTPT_afun86, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun76)), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun92))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun23))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun93)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun91))), (ATerm)wsAfterAnnos), (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl1(PTPT_afun7, (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl2(PTPT_afun86, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun76)), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))))), (ATerm)annos)), (ATerm)wsAfterBraceOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun92))));
}

/*}}}  */
/*{{{  PTPT_ATermAnnos PTPT_makeATermAnnosSingle(PTPT_ATerm head) */

PTPT_ATermAnnos PTPT_makeATermAnnosSingle(PTPT_ATerm head)
{
  return (PTPT_ATermAnnos)(ATerm)ATmakeList1((ATerm)head);
}

/*}}}  */
/*{{{  PTPT_ATermAnnos PTPT_makeATermAnnosMany(PTPT_ATerm head, PTPT_OptLayout wsAfterFirst, char * sep, PTPT_OptLayout wsAfterSep, PTPT_ATermAnnos tail) */

PTPT_ATermAnnos PTPT_makeATermAnnosMany(PTPT_ATerm head, PTPT_OptLayout wsAfterFirst, char * sep, PTPT_OptLayout wsAfterSep, PTPT_ATermAnnos tail)
{
  return (PTPT_ATermAnnos)(ATerm)ATinsert(ATinsert(ATinsert(ATinsert((ATermList)tail, (ATerm)wsAfterSep), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(ATmakeAFun(sep, 0, ATtrue)))), (ATerm)wsAfterFirst), (ATerm)head);
}

/*}}}  */
/*{{{  PTPT_AlphaNumericalEscChar PTPT_makeAlphaNumericalEscCharDefault(PTPT_CHARLIST chars) */

PTPT_AlphaNumericalEscChar PTPT_makeAlphaNumericalEscCharDefault(PTPT_CHARLIST chars)
{
  return (PTPT_AlphaNumericalEscChar)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun2, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun94)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun94))), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATmakeList1((ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl1(PTPT_afun7, (ATerm)ATmakeAppl1(PTPT_afun8, (ATerm)ATmakeAppl1(PTPT_afun9, (ATerm)ATmakeList1((ATerm)ATmakeAppl2(PTPT_afun10, (ATerm)ATmakeInt(0), (ATerm)ATmakeInt(255)))))), (ATerm)chars)));
}

/*}}}  */
/*{{{  PTPT_DecimalEscChar PTPT_makeDecimalEscCharDec0Underscore199(PTPT_CHARLIST chars) */

PTPT_DecimalEscChar PTPT_makeDecimalEscCharDec0Underscore199(PTPT_CHARLIST chars)
{
  return (PTPT_DecimalEscChar)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun2, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun95)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun95))), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATmakeList1((ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl1(PTPT_afun7, (ATerm)ATmakeAppl1(PTPT_afun8, (ATerm)ATmakeAppl1(PTPT_afun9, (ATerm)ATmakeList1((ATerm)ATmakeAppl2(PTPT_afun10, (ATerm)ATmakeInt(0), (ATerm)ATmakeInt(255)))))), (ATerm)chars)));
}

/*}}}  */
/*{{{  PTPT_DecimalEscChar PTPT_makeDecimalEscCharDec200Underscore249(PTPT_CHARLIST chars) */

PTPT_DecimalEscChar PTPT_makeDecimalEscCharDec200Underscore249(PTPT_CHARLIST chars)
{
  return (PTPT_DecimalEscChar)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun2, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun95)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun95))), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATmakeList1((ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl1(PTPT_afun7, (ATerm)ATmakeAppl1(PTPT_afun8, (ATerm)ATmakeAppl1(PTPT_afun9, (ATerm)ATmakeList1((ATerm)ATmakeAppl2(PTPT_afun10, (ATerm)ATmakeInt(0), (ATerm)ATmakeInt(255)))))), (ATerm)chars)));
}

/*}}}  */
/*{{{  PTPT_DecimalEscChar PTPT_makeDecimalEscCharDec250Underscore255(PTPT_CHARLIST chars) */

PTPT_DecimalEscChar PTPT_makeDecimalEscCharDec250Underscore255(PTPT_CHARLIST chars)
{
  return (PTPT_DecimalEscChar)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun2, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun95)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun95))), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATmakeList1((ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl1(PTPT_afun7, (ATerm)ATmakeAppl1(PTPT_afun8, (ATerm)ATmakeAppl1(PTPT_afun9, (ATerm)ATmakeList1((ATerm)ATmakeAppl2(PTPT_afun10, (ATerm)ATmakeInt(0), (ATerm)ATmakeInt(255)))))), (ATerm)chars)));
}

/*}}}  */
/*{{{  PTPT_EscChar PTPT_makeEscCharAlphaNumeric(PTPT_CHARLIST chars) */

PTPT_EscChar PTPT_makeEscCharAlphaNumeric(PTPT_CHARLIST chars)
{
  return (PTPT_EscChar)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun2, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun96)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun96))), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATmakeList1((ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl1(PTPT_afun7, (ATerm)ATmakeAppl1(PTPT_afun8, (ATerm)ATmakeAppl1(PTPT_afun9, (ATerm)ATmakeList1((ATerm)ATmakeAppl2(PTPT_afun10, (ATerm)ATmakeInt(0), (ATerm)ATmakeInt(255)))))), (ATerm)chars)));
}

/*}}}  */
/*{{{  PTPT_EscChar PTPT_makeEscCharDecimal(PTPT_CHARLIST chars) */

PTPT_EscChar PTPT_makeEscCharDecimal(PTPT_CHARLIST chars)
{
  return (PTPT_EscChar)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun2, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun96)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun96))), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATmakeList1((ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl1(PTPT_afun7, (ATerm)ATmakeAppl1(PTPT_afun8, (ATerm)ATmakeAppl1(PTPT_afun9, (ATerm)ATmakeList1((ATerm)ATmakeAppl2(PTPT_afun10, (ATerm)ATmakeInt(0), (ATerm)ATmakeInt(255)))))), (ATerm)chars)));
}

/*}}}  */
/*{{{  PTPT_LChar PTPT_makeLCharNormal(PTPT_CHARLIST chars) */

PTPT_LChar PTPT_makeLCharNormal(PTPT_CHARLIST chars)
{
  return (PTPT_LChar)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun2, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun97)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun97))), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATmakeList1((ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl1(PTPT_afun7, (ATerm)ATmakeAppl1(PTPT_afun8, (ATerm)ATmakeAppl1(PTPT_afun9, (ATerm)ATmakeList1((ATerm)ATmakeAppl2(PTPT_afun10, (ATerm)ATmakeInt(0), (ATerm)ATmakeInt(255)))))), (ATerm)chars)));
}

/*}}}  */
/*{{{  PTPT_LChar PTPT_makeLCharEscaped(PTPT_CHARLIST chars) */

PTPT_LChar PTPT_makeLCharEscaped(PTPT_CHARLIST chars)
{
  return (PTPT_LChar)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun2, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun97)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun97))), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATmakeList1((ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl1(PTPT_afun7, (ATerm)ATmakeAppl1(PTPT_afun8, (ATerm)ATmakeAppl1(PTPT_afun9, (ATerm)ATmakeList1((ATerm)ATmakeAppl2(PTPT_afun10, (ATerm)ATmakeInt(0), (ATerm)ATmakeInt(255)))))), (ATerm)chars)));
}

/*}}}  */
/*{{{  PTPT_QLiteral PTPT_makeQLiteralQuoted(PTPT_CHARLIST chars) */

PTPT_QLiteral PTPT_makeQLiteralQuoted(PTPT_CHARLIST chars)
{
  return (PTPT_QLiteral)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun2, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun33)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun33))), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATmakeList1((ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl1(PTPT_afun7, (ATerm)ATmakeAppl1(PTPT_afun8, (ATerm)ATmakeAppl1(PTPT_afun9, (ATerm)ATmakeList1((ATerm)ATmakeAppl2(PTPT_afun10, (ATerm)ATmakeInt(0), (ATerm)ATmakeInt(255)))))), (ATerm)chars)));
}

/*}}}  */
/*{{{  PTPT_UQLiteral PTPT_makeUQLiteralOneChar(PTPT_CHARLIST chars) */

PTPT_UQLiteral PTPT_makeUQLiteralOneChar(PTPT_CHARLIST chars)
{
  return (PTPT_UQLiteral)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun2, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun98)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun98))), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATmakeList1((ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl1(PTPT_afun7, (ATerm)ATmakeAppl1(PTPT_afun8, (ATerm)ATmakeAppl1(PTPT_afun9, (ATerm)ATmakeList1((ATerm)ATmakeAppl2(PTPT_afun10, (ATerm)ATmakeInt(0), (ATerm)ATmakeInt(255)))))), (ATerm)chars)));
}

/*}}}  */
/*{{{  PTPT_UQLiteral PTPT_makeUQLiteralMoreChars(PTPT_CHARLIST chars) */

PTPT_UQLiteral PTPT_makeUQLiteralMoreChars(PTPT_CHARLIST chars)
{
  return (PTPT_UQLiteral)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun2, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun98)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun98))), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATmakeList1((ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl1(PTPT_afun7, (ATerm)ATmakeAppl1(PTPT_afun8, (ATerm)ATmakeAppl1(PTPT_afun9, (ATerm)ATmakeList1((ATerm)ATmakeAppl2(PTPT_afun10, (ATerm)ATmakeInt(0), (ATerm)ATmakeInt(255)))))), (ATerm)chars)));
}

/*}}}  */
/*{{{  PTPT_Literal PTPT_makeLiteralQlit(PTPT_QLiteral QLiteral) */

PTPT_Literal PTPT_makeLiteralQlit(PTPT_QLiteral QLiteral)
{
  return (PTPT_Literal)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun33)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun82))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun99)))))), (ATerm)ATmakeList1((ATerm)QLiteral));
}

/*}}}  */
/*{{{  PTPT_Literal PTPT_makeLiteralUqlit(PTPT_UQLiteral UQLiteral) */

PTPT_Literal PTPT_makeLiteralUqlit(PTPT_UQLiteral UQLiteral)
{
  return (PTPT_Literal)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun98)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun82))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun100)))))), (ATerm)ATmakeList1((ATerm)UQLiteral));
}

/*}}}  */
/*{{{  PTPT_Attributes PTPT_makeAttributesNoAttrs() */

PTPT_Attributes PTPT_makeAttributesNoAttrs()
{
  return (PTPT_Attributes)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun101))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun40))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun101)))))), (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun101))));
}

/*}}}  */
/*{{{  PTPT_Attributes PTPT_makeAttributesAttrs(PTPT_OptLayout wsAfterAttrs, PTPT_OptLayout wsAfterParenOpen, PTPT_Attrs attributes, PTPT_OptLayout wsAfterAttributes) */

PTPT_Attributes PTPT_makeAttributesAttrs(PTPT_OptLayout wsAfterAttrs, PTPT_OptLayout wsAfterParenOpen, PTPT_Attrs attributes, PTPT_OptLayout wsAfterAttributes)
{
  return (PTPT_Attributes)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun102)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun103))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun40))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun103)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterAttributes), (ATerm)attributes), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterAttrs), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun103))));
}

/*}}}  */
/*{{{  PTPT_Attrs PTPT_makeAttrsMany(PTPT_OptLayout wsAfterBracketOpen, PTPT_AttrList list, PTPT_OptLayout wsAfterList) */

PTPT_Attrs PTPT_makeAttrsMany(PTPT_OptLayout wsAfterBracketOpen, PTPT_AttrList list, PTPT_OptLayout wsAfterList)
{
  return (PTPT_Attrs)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun36))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl2(PTPT_afun86, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun104)), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun38))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun102))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun105)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun36))), (ATerm)wsAfterList), (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl1(PTPT_afun7, (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl2(PTPT_afun86, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun104)), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))))), (ATerm)list)), (ATerm)wsAfterBracketOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun38))));
}

/*}}}  */
/*{{{  PTPT_AttrList PTPT_makeAttrListSingle(PTPT_Attr head) */

PTPT_AttrList PTPT_makeAttrListSingle(PTPT_Attr head)
{
  return (PTPT_AttrList)(ATerm)ATmakeList1((ATerm)head);
}

/*}}}  */
/*{{{  PTPT_AttrList PTPT_makeAttrListMany(PTPT_Attr head, PTPT_OptLayout wsAfterFirst, char * sep, PTPT_OptLayout wsAfterSep, PTPT_AttrList tail) */

PTPT_AttrList PTPT_makeAttrListMany(PTPT_Attr head, PTPT_OptLayout wsAfterFirst, char * sep, PTPT_OptLayout wsAfterSep, PTPT_AttrList tail)
{
  return (PTPT_AttrList)(ATerm)ATinsert(ATinsert(ATinsert(ATinsert((ATermList)tail, (ATerm)wsAfterSep), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(ATmakeAFun(sep, 0, ATtrue)))), (ATerm)wsAfterFirst), (ATerm)head);
}

/*}}}  */
/*{{{  PTPT_Attr PTPT_makeAttrAssoc(PTPT_OptLayout wsAfterAssoc, PTPT_OptLayout wsAfterParenOpen, PTPT_Associativity associativity, PTPT_OptLayout wsAfterAssociativity) */

PTPT_Attr PTPT_makeAttrAssoc(PTPT_OptLayout wsAfterAssoc, PTPT_OptLayout wsAfterParenOpen, PTPT_Associativity associativity, PTPT_OptLayout wsAfterAssociativity)
{
  return (PTPT_Attr)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun106)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun107))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun104))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun107)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterAssociativity), (ATerm)associativity), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterAssoc), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun107))));
}

/*}}}  */
/*{{{  PTPT_Attr PTPT_makeAttrTerm(PTPT_OptLayout wsAfterTerm, PTPT_OptLayout wsAfterParenOpen, PTPT_ATerm aterm, PTPT_OptLayout wsAfterAterm) */

PTPT_Attr PTPT_makeAttrTerm(PTPT_OptLayout wsAfterTerm, PTPT_OptLayout wsAfterParenOpen, PTPT_ATerm aterm, PTPT_OptLayout wsAfterAterm)
{
  return (PTPT_Attr)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun76)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun108))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun104))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun108)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterAterm), (ATerm)aterm), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterTerm), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun108))));
}

/*}}}  */
/*{{{  PTPT_Attr PTPT_makeAttrId(PTPT_OptLayout wsAfterId, PTPT_OptLayout wsAfterParenOpen, PTPT_QLiteral moduleName, PTPT_OptLayout wsAfterModuleName) */

PTPT_Attr PTPT_makeAttrId(PTPT_OptLayout wsAfterId, PTPT_OptLayout wsAfterParenOpen, PTPT_QLiteral moduleName, PTPT_OptLayout wsAfterModuleName)
{
  return (PTPT_Attr)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun33)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun109))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun104))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun109)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterModuleName), (ATerm)moduleName), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterId), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun109))));
}

/*}}}  */
/*{{{  PTPT_Attr PTPT_makeAttrBracket() */

PTPT_Attr PTPT_makeAttrBracket()
{
  return (PTPT_Attr)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun110))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun104))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun110)))))), (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun110))));
}

/*}}}  */
/*{{{  PTPT_Attr PTPT_makeAttrReject() */

PTPT_Attr PTPT_makeAttrReject()
{
  return (PTPT_Attr)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun111))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun104))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun111)))))), (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun111))));
}

/*}}}  */
/*{{{  PTPT_Attr PTPT_makeAttrPrefer() */

PTPT_Attr PTPT_makeAttrPrefer()
{
  return (PTPT_Attr)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun112))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun104))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun112)))))), (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun112))));
}

/*}}}  */
/*{{{  PTPT_Attr PTPT_makeAttrAvoid() */

PTPT_Attr PTPT_makeAttrAvoid()
{
  return (PTPT_Attr)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun113))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun104))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun113)))))), (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun113))));
}

/*}}}  */
/*{{{  PTPT_Associativity PTPT_makeAssociativityLeft() */

PTPT_Associativity PTPT_makeAssociativityLeft()
{
  return (PTPT_Associativity)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun114))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun106))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun114)))))), (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun114))));
}

/*}}}  */
/*{{{  PTPT_Associativity PTPT_makeAssociativityRight() */

PTPT_Associativity PTPT_makeAssociativityRight()
{
  return (PTPT_Associativity)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun115))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun106))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun115)))))), (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun115))));
}

/*}}}  */
/*{{{  PTPT_Associativity PTPT_makeAssociativityAssoc() */

PTPT_Associativity PTPT_makeAssociativityAssoc()
{
  return (PTPT_Associativity)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun107))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun106))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun107)))))), (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun107))));
}

/*}}}  */
/*{{{  PTPT_Associativity PTPT_makeAssociativityNonAssoc() */

PTPT_Associativity PTPT_makeAssociativityNonAssoc()
{
  return (PTPT_Associativity)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun116))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun106))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun116)))))), (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun116))));
}

/*}}}  */
/*{{{  PTPT_ParseTree PTPT_makeParseTreeTop(PTPT_OptLayout wsAfterParsetree, PTPT_OptLayout wsAfterParenOpen, PTPT_Tree top, PTPT_OptLayout wsAfterTop, PTPT_OptLayout wsAfterComma, PTPT_NatCon ambCnt, PTPT_OptLayout wsAfterAmbCnt) */

PTPT_ParseTree PTPT_makeParseTreeTop(PTPT_OptLayout wsAfterParsetree, PTPT_OptLayout wsAfterParenOpen, PTPT_Tree top, PTPT_OptLayout wsAfterTop, PTPT_OptLayout wsAfterComma, PTPT_NatCon ambCnt, PTPT_OptLayout wsAfterAmbCnt)
{
  return (PTPT_ParseTree)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun4)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun24)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun117))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun118))), (ATerm)ATmakeAppl1(PTPT_afun12, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun13, (ATerm)ATmakeAppl1(PTPT_afun14, (ATerm)ATmakeAppl0(PTPT_afun119)))))), (ATerm)ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun26))), (ATerm)wsAfterAmbCnt), (ATerm)ambCnt), (ATerm)wsAfterComma), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun28))), (ATerm)wsAfterTop), (ATerm)top), (ATerm)wsAfterParenOpen), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun30))), (ATerm)wsAfterParsetree), (ATerm)ATmakeAppl1(PTPT_afun18, (ATerm)ATmakeAppl0(PTPT_afun117))));
}

/*}}}  */
/*{{{  PTPT_Start PTPT_makeStartParseTree(PTPT_OptLayout wsBefore, PTPT_ParseTree topParseTree, PTPT_OptLayout wsAfter, int ambCnt) */

PTPT_Start PTPT_makeStartParseTree(PTPT_OptLayout wsBefore, PTPT_ParseTree topParseTree, PTPT_OptLayout wsAfter, int ambCnt)
{
  return (PTPT_Start)(ATerm)ATmakeAppl2(PTPT_afun120, (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun118)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun121)), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)wsAfter), (ATerm)topParseTree), (ATerm)wsBefore)), (ATerm)ATmakeInt(ambCnt));
}

/*}}}  */
/*{{{  PTPT_Start PTPT_makeStartAssociativity(PTPT_OptLayout wsBefore, PTPT_Associativity topAssociativity, PTPT_OptLayout wsAfter, int ambCnt) */

PTPT_Start PTPT_makeStartAssociativity(PTPT_OptLayout wsBefore, PTPT_Associativity topAssociativity, PTPT_OptLayout wsAfter, int ambCnt)
{
  return (PTPT_Start)(ATerm)ATmakeAppl2(PTPT_afun120, (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun106)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun121)), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)wsAfter), (ATerm)topAssociativity), (ATerm)wsBefore)), (ATerm)ATmakeInt(ambCnt));
}

/*}}}  */
/*{{{  PTPT_Start PTPT_makeStartAttr(PTPT_OptLayout wsBefore, PTPT_Attr topAttr, PTPT_OptLayout wsAfter, int ambCnt) */

PTPT_Start PTPT_makeStartAttr(PTPT_OptLayout wsBefore, PTPT_Attr topAttr, PTPT_OptLayout wsAfter, int ambCnt)
{
  return (PTPT_Start)(ATerm)ATmakeAppl2(PTPT_afun120, (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun104)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun121)), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)wsAfter), (ATerm)topAttr), (ATerm)wsBefore)), (ATerm)ATmakeInt(ambCnt));
}

/*}}}  */
/*{{{  PTPT_Start PTPT_makeStartAttrs(PTPT_OptLayout wsBefore, PTPT_Attrs topAttrs, PTPT_OptLayout wsAfter, int ambCnt) */

PTPT_Start PTPT_makeStartAttrs(PTPT_OptLayout wsBefore, PTPT_Attrs topAttrs, PTPT_OptLayout wsAfter, int ambCnt)
{
  return (PTPT_Start)(ATerm)ATmakeAppl2(PTPT_afun120, (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun102)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun121)), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)wsAfter), (ATerm)topAttrs), (ATerm)wsBefore)), (ATerm)ATmakeInt(ambCnt));
}

/*}}}  */
/*{{{  PTPT_Start PTPT_makeStartAttributes(PTPT_OptLayout wsBefore, PTPT_Attributes topAttributes, PTPT_OptLayout wsAfter, int ambCnt) */

PTPT_Start PTPT_makeStartAttributes(PTPT_OptLayout wsBefore, PTPT_Attributes topAttributes, PTPT_OptLayout wsAfter, int ambCnt)
{
  return (PTPT_Start)(ATerm)ATmakeAppl2(PTPT_afun120, (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun40)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun121)), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)wsAfter), (ATerm)topAttributes), (ATerm)wsBefore)), (ATerm)ATmakeInt(ambCnt));
}

/*}}}  */
/*{{{  PTPT_Start PTPT_makeStartQLiteral(PTPT_OptLayout wsBefore, PTPT_QLiteral topQLiteral, PTPT_OptLayout wsAfter, int ambCnt) */

PTPT_Start PTPT_makeStartQLiteral(PTPT_OptLayout wsBefore, PTPT_QLiteral topQLiteral, PTPT_OptLayout wsAfter, int ambCnt)
{
  return (PTPT_Start)(ATerm)ATmakeAppl2(PTPT_afun120, (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun33)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun121)), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)wsAfter), (ATerm)topQLiteral), (ATerm)wsBefore)), (ATerm)ATmakeInt(ambCnt));
}

/*}}}  */
/*{{{  PTPT_Start PTPT_makeStartUQLiteral(PTPT_OptLayout wsBefore, PTPT_UQLiteral topUQLiteral, PTPT_OptLayout wsAfter, int ambCnt) */

PTPT_Start PTPT_makeStartUQLiteral(PTPT_OptLayout wsBefore, PTPT_UQLiteral topUQLiteral, PTPT_OptLayout wsAfter, int ambCnt)
{
  return (PTPT_Start)(ATerm)ATmakeAppl2(PTPT_afun120, (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun98)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun121)), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)wsAfter), (ATerm)topUQLiteral), (ATerm)wsBefore)), (ATerm)ATmakeInt(ambCnt));
}

/*}}}  */
/*{{{  PTPT_Start PTPT_makeStartLiteral(PTPT_OptLayout wsBefore, PTPT_Literal topLiteral, PTPT_OptLayout wsAfter, int ambCnt) */

PTPT_Start PTPT_makeStartLiteral(PTPT_OptLayout wsBefore, PTPT_Literal topLiteral, PTPT_OptLayout wsAfter, int ambCnt)
{
  return (PTPT_Start)(ATerm)ATmakeAppl2(PTPT_afun120, (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun82)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun121)), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)wsAfter), (ATerm)topLiteral), (ATerm)wsBefore)), (ATerm)ATmakeInt(ambCnt));
}

/*}}}  */
/*{{{  PTPT_Start PTPT_makeStartAnn(PTPT_OptLayout wsBefore, PTPT_Ann topAnn, PTPT_OptLayout wsAfter, int ambCnt) */

PTPT_Start PTPT_makeStartAnn(PTPT_OptLayout wsBefore, PTPT_Ann topAnn, PTPT_OptLayout wsAfter, int ambCnt)
{
  return (PTPT_Start)(ATerm)ATmakeAppl2(PTPT_afun120, (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun23)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun121)), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)wsAfter), (ATerm)topAnn), (ATerm)wsBefore)), (ATerm)ATmakeInt(ambCnt));
}

/*}}}  */
/*{{{  PTPT_Start PTPT_makeStartATerm(PTPT_OptLayout wsBefore, PTPT_ATerm topATerm, PTPT_OptLayout wsAfter, int ambCnt) */

PTPT_Start PTPT_makeStartATerm(PTPT_OptLayout wsBefore, PTPT_ATerm topATerm, PTPT_OptLayout wsAfter, int ambCnt)
{
  return (PTPT_Start)(ATerm)ATmakeAppl2(PTPT_afun120, (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun76)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun121)), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)wsAfter), (ATerm)topATerm), (ATerm)wsBefore)), (ATerm)ATmakeInt(ambCnt));
}

/*}}}  */
/*{{{  PTPT_Start PTPT_makeStartAFun(PTPT_OptLayout wsBefore, PTPT_AFun topAFun, PTPT_OptLayout wsAfter, int ambCnt) */

PTPT_Start PTPT_makeStartAFun(PTPT_OptLayout wsBefore, PTPT_AFun topAFun, PTPT_OptLayout wsAfter, int ambCnt)
{
  return (PTPT_Start)(ATerm)ATmakeAppl2(PTPT_afun120, (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun83)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun121)), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)wsAfter), (ATerm)topAFun), (ATerm)wsBefore)), (ATerm)ATmakeInt(ambCnt));
}

/*}}}  */
/*{{{  PTPT_Start PTPT_makeStartACon(PTPT_OptLayout wsBefore, PTPT_ACon topACon, PTPT_OptLayout wsAfter, int ambCnt) */

PTPT_Start PTPT_makeStartACon(PTPT_OptLayout wsBefore, PTPT_ACon topACon, PTPT_OptLayout wsAfter, int ambCnt)
{
  return (PTPT_Start)(ATerm)ATmakeAppl2(PTPT_afun120, (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun79)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun121)), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)wsAfter), (ATerm)topACon), (ATerm)wsBefore)), (ATerm)ATmakeInt(ambCnt));
}

/*}}}  */
/*{{{  PTPT_Start PTPT_makeStartATermList(PTPT_OptLayout wsBefore, PTPT_ATermList topATermList, PTPT_OptLayout wsAfter, int ambCnt) */

PTPT_Start PTPT_makeStartATermList(PTPT_OptLayout wsBefore, PTPT_ATermList topATermList, PTPT_OptLayout wsAfter, int ambCnt)
{
  return (PTPT_Start)(ATerm)ATmakeAppl2(PTPT_afun120, (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun77)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun121)), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)wsAfter), (ATerm)topATermList), (ATerm)wsBefore)), (ATerm)ATmakeInt(ambCnt));
}

/*}}}  */
/*{{{  PTPT_Start PTPT_makeStartRealCon(PTPT_OptLayout wsBefore, PTPT_RealCon topRealCon, PTPT_OptLayout wsAfter, int ambCnt) */

PTPT_Start PTPT_makeStartRealCon(PTPT_OptLayout wsBefore, PTPT_RealCon topRealCon, PTPT_OptLayout wsAfter, int ambCnt)
{
  return (PTPT_Start)(ATerm)ATmakeAppl2(PTPT_afun120, (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun74)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun121)), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)wsAfter), (ATerm)topRealCon), (ATerm)wsBefore)), (ATerm)ATmakeInt(ambCnt));
}

/*}}}  */
/*{{{  PTPT_Start PTPT_makeStartOptExp(PTPT_OptLayout wsBefore, PTPT_OptExp topOptExp, PTPT_OptLayout wsAfter, int ambCnt) */

PTPT_Start PTPT_makeStartOptExp(PTPT_OptLayout wsBefore, PTPT_OptExp topOptExp, PTPT_OptLayout wsAfter, int ambCnt)
{
  return (PTPT_Start)(ATerm)ATmakeAppl2(PTPT_afun120, (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun70)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun121)), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)wsAfter), (ATerm)topOptExp), (ATerm)wsBefore)), (ATerm)ATmakeInt(ambCnt));
}

/*}}}  */
/*{{{  PTPT_Start PTPT_makeStartCharRanges(PTPT_OptLayout wsBefore, PTPT_CharRanges topCharRanges, PTPT_OptLayout wsAfter, int ambCnt) */

PTPT_Start PTPT_makeStartCharRanges(PTPT_OptLayout wsBefore, PTPT_CharRanges topCharRanges, PTPT_OptLayout wsAfter, int ambCnt)
{
  return (PTPT_Start)(ATerm)ATmakeAppl2(PTPT_afun120, (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun62)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun121)), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)wsAfter), (ATerm)topCharRanges), (ATerm)wsBefore)), (ATerm)ATmakeInt(ambCnt));
}

/*}}}  */
/*{{{  PTPT_Start PTPT_makeStartCharRange(PTPT_OptLayout wsBefore, PTPT_CharRange topCharRange, PTPT_OptLayout wsAfter, int ambCnt) */

PTPT_Start PTPT_makeStartCharRange(PTPT_OptLayout wsBefore, PTPT_CharRange topCharRange, PTPT_OptLayout wsAfter, int ambCnt)
{
  return (PTPT_Start)(ATerm)ATmakeAppl2(PTPT_afun120, (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun66)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun121)), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)wsAfter), (ATerm)topCharRange), (ATerm)wsBefore)), (ATerm)ATmakeInt(ambCnt));
}

/*}}}  */
/*{{{  PTPT_Start PTPT_makeStartSymbols(PTPT_OptLayout wsBefore, PTPT_Symbols topSymbols, PTPT_OptLayout wsAfter, int ambCnt) */

PTPT_Start PTPT_makeStartSymbols(PTPT_OptLayout wsBefore, PTPT_Symbols topSymbols, PTPT_OptLayout wsAfter, int ambCnt)
{
  return (PTPT_Start)(ATerm)ATmakeAppl2(PTPT_afun120, (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun42)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun121)), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)wsAfter), (ATerm)topSymbols), (ATerm)wsBefore)), (ATerm)ATmakeInt(ambCnt));
}

/*}}}  */
/*{{{  PTPT_Start PTPT_makeStartSymbol(PTPT_OptLayout wsBefore, PTPT_Symbol topSymbol, PTPT_OptLayout wsAfter, int ambCnt) */

PTPT_Start PTPT_makeStartSymbol(PTPT_OptLayout wsBefore, PTPT_Symbol topSymbol, PTPT_OptLayout wsAfter, int ambCnt)
{
  return (PTPT_Start)(ATerm)ATmakeAppl2(PTPT_afun120, (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun41)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun121)), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)wsAfter), (ATerm)topSymbol), (ATerm)wsBefore)), (ATerm)ATmakeInt(ambCnt));
}

/*}}}  */
/*{{{  PTPT_Start PTPT_makeStartProduction(PTPT_OptLayout wsBefore, PTPT_Production topProduction, PTPT_OptLayout wsAfter, int ambCnt) */

PTPT_Start PTPT_makeStartProduction(PTPT_OptLayout wsBefore, PTPT_Production topProduction, PTPT_OptLayout wsAfter, int ambCnt)
{
  return (PTPT_Start)(ATerm)ATmakeAppl2(PTPT_afun120, (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun29)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun121)), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)wsAfter), (ATerm)topProduction), (ATerm)wsBefore)), (ATerm)ATmakeInt(ambCnt));
}

/*}}}  */
/*{{{  PTPT_Start PTPT_makeStartArgs(PTPT_OptLayout wsBefore, PTPT_Args topArgs, PTPT_OptLayout wsAfter, int ambCnt) */

PTPT_Start PTPT_makeStartArgs(PTPT_OptLayout wsBefore, PTPT_Args topArgs, PTPT_OptLayout wsAfter, int ambCnt)
{
  return (PTPT_Start)(ATerm)ATmakeAppl2(PTPT_afun120, (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun27)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun121)), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)wsAfter), (ATerm)topArgs), (ATerm)wsBefore)), (ATerm)ATmakeInt(ambCnt));
}

/*}}}  */
/*{{{  PTPT_Start PTPT_makeStartTree(PTPT_OptLayout wsBefore, PTPT_Tree topTree, PTPT_OptLayout wsAfter, int ambCnt) */

PTPT_Start PTPT_makeStartTree(PTPT_OptLayout wsBefore, PTPT_Tree topTree, PTPT_OptLayout wsAfter, int ambCnt)
{
  return (PTPT_Start)(ATerm)ATmakeAppl2(PTPT_afun120, (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun24)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun121)), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)wsAfter), (ATerm)topTree), (ATerm)wsBefore)), (ATerm)ATmakeInt(ambCnt));
}

/*}}}  */
/*{{{  PTPT_Start PTPT_makeStartIntCon(PTPT_OptLayout wsBefore, PTPT_IntCon topIntCon, PTPT_OptLayout wsAfter, int ambCnt) */

PTPT_Start PTPT_makeStartIntCon(PTPT_OptLayout wsBefore, PTPT_IntCon topIntCon, PTPT_OptLayout wsAfter, int ambCnt)
{
  return (PTPT_Start)(ATerm)ATmakeAppl2(PTPT_afun120, (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun11)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun121)), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)wsAfter), (ATerm)topIntCon), (ATerm)wsBefore)), (ATerm)ATmakeInt(ambCnt));
}

/*}}}  */
/*{{{  PTPT_Start PTPT_makeStartNatCon(PTPT_OptLayout wsBefore, PTPT_NatCon topNatCon, PTPT_OptLayout wsAfter, int ambCnt) */

PTPT_Start PTPT_makeStartNatCon(PTPT_OptLayout wsBefore, PTPT_NatCon topNatCon, PTPT_OptLayout wsAfter, int ambCnt)
{
  return (PTPT_Start)(ATerm)ATmakeAppl2(PTPT_afun120, (ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun4)))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17)))), (ATerm)ATmakeAppl1(PTPT_afun3, (ATerm)ATmakeAppl0(PTPT_afun121)), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATinsert(ATinsert(ATmakeList1((ATerm)wsAfter), (ATerm)topNatCon), (ATerm)wsBefore)), (ATerm)ATmakeInt(ambCnt));
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_makeOptLayoutAbsent() */

PTPT_OptLayout PTPT_makeOptLayoutAbsent()
{
  return (PTPT_OptLayout)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATempty, (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17))), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)ATempty);
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_makeOptLayoutPresent(PTPT_CHARLIST chars) */

PTPT_OptLayout PTPT_makeOptLayoutPresent(PTPT_CHARLIST chars)
{
  return (PTPT_OptLayout)(ATerm)ATmakeAppl2(PTPT_afun0, (ATerm)ATmakeAppl3(PTPT_afun1, (ATerm)ATmakeList1((ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl0(PTPT_afun17))), (ATerm)ATmakeAppl1(PTPT_afun5, (ATerm)ATmakeAppl1(PTPT_afun16, (ATerm)ATmakeAppl0(PTPT_afun17))), (ATerm)ATmakeAppl0(PTPT_afun6)), (ATerm)chars);
}

/*}}}  */

/*}}}  */
/*{{{  equality functions */

ATbool PTPT_isEqualNatCon(PTPT_NatCon arg0, PTPT_NatCon arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualIntCon(PTPT_IntCon arg0, PTPT_IntCon arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualTree(PTPT_Tree arg0, PTPT_Tree arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualArgs(PTPT_Args arg0, PTPT_Args arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualTreeList(PTPT_TreeList arg0, PTPT_TreeList arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualProduction(PTPT_Production arg0, PTPT_Production arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualSymbol(PTPT_Symbol arg0, PTPT_Symbol arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualSymbols(PTPT_Symbols arg0, PTPT_Symbols arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualSymbolList(PTPT_SymbolList arg0, PTPT_SymbolList arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualCharRanges(PTPT_CharRanges arg0, PTPT_CharRanges arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualCharRangeList(PTPT_CharRangeList arg0, PTPT_CharRangeList arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualCharRange(PTPT_CharRange arg0, PTPT_CharRange arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualOptExp(PTPT_OptExp arg0, PTPT_OptExp arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualRealCon(PTPT_RealCon arg0, PTPT_RealCon arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualATermList(PTPT_ATermList arg0, PTPT_ATermList arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualATermElems(PTPT_ATermElems arg0, PTPT_ATermElems arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualACon(PTPT_ACon arg0, PTPT_ACon arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualAFun(PTPT_AFun arg0, PTPT_AFun arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualATerm(PTPT_ATerm arg0, PTPT_ATerm arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualATermArgs(PTPT_ATermArgs arg0, PTPT_ATermArgs arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualAnn(PTPT_Ann arg0, PTPT_Ann arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualATermAnnos(PTPT_ATermAnnos arg0, PTPT_ATermAnnos arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualAlphaNumericalEscChar(PTPT_AlphaNumericalEscChar arg0, PTPT_AlphaNumericalEscChar arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualDecimalEscChar(PTPT_DecimalEscChar arg0, PTPT_DecimalEscChar arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualEscChar(PTPT_EscChar arg0, PTPT_EscChar arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualLChar(PTPT_LChar arg0, PTPT_LChar arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualQLiteral(PTPT_QLiteral arg0, PTPT_QLiteral arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualUQLiteral(PTPT_UQLiteral arg0, PTPT_UQLiteral arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualLiteral(PTPT_Literal arg0, PTPT_Literal arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualAttributes(PTPT_Attributes arg0, PTPT_Attributes arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualAttrs(PTPT_Attrs arg0, PTPT_Attrs arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualAttrList(PTPT_AttrList arg0, PTPT_AttrList arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualAttr(PTPT_Attr arg0, PTPT_Attr arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualAssociativity(PTPT_Associativity arg0, PTPT_Associativity arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualParseTree(PTPT_ParseTree arg0, PTPT_ParseTree arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualStart(PTPT_Start arg0, PTPT_Start arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

ATbool PTPT_isEqualOptLayout(PTPT_OptLayout arg0, PTPT_OptLayout arg1)
{
  return ATisEqual((ATerm)arg0, (ATerm)arg1);
}

/*}}}  */
/*{{{  PTPT_NatCon accessors */

/*{{{  ATbool PTPT_isValidNatCon(PTPT_NatCon arg) */

ATbool PTPT_isValidNatCon(PTPT_NatCon arg)
{
  if (PTPT_isNatConDigits(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isNatConDigits(PTPT_NatCon arg) */

inline ATbool PTPT_isNatConDigits(PTPT_NatCon arg)
{
#ifndef DISABLE_DYNAMIC_CHECKING
  assert(arg != NULL);
  assert(ATmatchTerm((ATerm)arg, PTPT_patternNatConDigits, NULL));
#endif
  return ATtrue;
}

/*}}}  */
/*{{{  ATbool PTPT_hasNatConChars(PTPT_NatCon arg) */

ATbool PTPT_hasNatConChars(PTPT_NatCon arg)
{
  if (PTPT_isNatConDigits(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_CHARLIST PTPT_getNatConChars(PTPT_NatCon arg) */

PTPT_CHARLIST PTPT_getNatConChars(PTPT_NatCon arg)
{
  
    return (PTPT_CHARLIST)ATgetArgument((ATermAppl)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1)), 1);
}

/*}}}  */
/*{{{  PTPT_NatCon PTPT_setNatConChars(PTPT_NatCon arg, PTPT_CHARLIST chars) */

PTPT_NatCon PTPT_setNatConChars(PTPT_NatCon arg, PTPT_CHARLIST chars)
{
  if (PTPT_isNatConDigits(arg)) {
    return (PTPT_NatCon)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)ATsetArgument((ATermAppl)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1)), (ATerm)chars, 1), 0), 1);
  }

  ATabort("NatCon has no Chars: %t\n", arg);
  return (PTPT_NatCon)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_IntCon accessors */

/*{{{  ATbool PTPT_isValidIntCon(PTPT_IntCon arg) */

ATbool PTPT_isValidIntCon(PTPT_IntCon arg)
{
  if (PTPT_isIntConNatural(arg)) {
    return ATtrue;
  }
  else if (PTPT_isIntConPositive(arg)) {
    return ATtrue;
  }
  else if (PTPT_isIntConNegative(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isIntConNatural(PTPT_IntCon arg) */

inline ATbool PTPT_isIntConNatural(PTPT_IntCon arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternIntConNatural, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isIntConPositive(PTPT_IntCon arg) */

inline ATbool PTPT_isIntConPositive(PTPT_IntCon arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternIntConPositive, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isIntConNegative(PTPT_IntCon arg) */

inline ATbool PTPT_isIntConNegative(PTPT_IntCon arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternIntConNegative, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  ATbool PTPT_hasIntConNatCon(PTPT_IntCon arg) */

ATbool PTPT_hasIntConNatCon(PTPT_IntCon arg)
{
  if (PTPT_isIntConNatural(arg)) {
    return ATtrue;
  }
  else if (PTPT_isIntConPositive(arg)) {
    return ATtrue;
  }
  else if (PTPT_isIntConNegative(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_NatCon PTPT_getIntConNatCon(PTPT_IntCon arg) */

PTPT_NatCon PTPT_getIntConNatCon(PTPT_IntCon arg)
{
  if (PTPT_isIntConNatural(arg)) {
    return (PTPT_NatCon)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1));
  }
  else if (PTPT_isIntConPositive(arg)) {
    return (PTPT_NatCon)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 2);
  }
  else 
    return (PTPT_NatCon)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 2);
}

/*}}}  */
/*{{{  PTPT_IntCon PTPT_setIntConNatCon(PTPT_IntCon arg, PTPT_NatCon NatCon) */

PTPT_IntCon PTPT_setIntConNatCon(PTPT_IntCon arg, PTPT_NatCon NatCon)
{
  if (PTPT_isIntConNatural(arg)) {
    return (PTPT_IntCon)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)NatCon, 0), 1);
  }
  else if (PTPT_isIntConPositive(arg)) {
    return (PTPT_IntCon)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)NatCon, 2), 1);
  }
  else if (PTPT_isIntConNegative(arg)) {
    return (PTPT_IntCon)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)NatCon, 2), 1);
  }

  ATabort("IntCon has no NatCon: %t\n", arg);
  return (PTPT_IntCon)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasIntConWsAfterPos(PTPT_IntCon arg) */

ATbool PTPT_hasIntConWsAfterPos(PTPT_IntCon arg)
{
  if (PTPT_isIntConPositive(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getIntConWsAfterPos(PTPT_IntCon arg) */

PTPT_OptLayout PTPT_getIntConWsAfterPos(PTPT_IntCon arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_IntCon PTPT_setIntConWsAfterPos(PTPT_IntCon arg, PTPT_OptLayout wsAfterPos) */

PTPT_IntCon PTPT_setIntConWsAfterPos(PTPT_IntCon arg, PTPT_OptLayout wsAfterPos)
{
  if (PTPT_isIntConPositive(arg)) {
    return (PTPT_IntCon)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterPos, 1), 1);
  }

  ATabort("IntCon has no WsAfterPos: %t\n", arg);
  return (PTPT_IntCon)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasIntConWsAfterNeg(PTPT_IntCon arg) */

ATbool PTPT_hasIntConWsAfterNeg(PTPT_IntCon arg)
{
  if (PTPT_isIntConNegative(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getIntConWsAfterNeg(PTPT_IntCon arg) */

PTPT_OptLayout PTPT_getIntConWsAfterNeg(PTPT_IntCon arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_IntCon PTPT_setIntConWsAfterNeg(PTPT_IntCon arg, PTPT_OptLayout wsAfterNeg) */

PTPT_IntCon PTPT_setIntConWsAfterNeg(PTPT_IntCon arg, PTPT_OptLayout wsAfterNeg)
{
  if (PTPT_isIntConNegative(arg)) {
    return (PTPT_IntCon)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterNeg, 1), 1);
  }

  ATabort("IntCon has no WsAfterNeg: %t\n", arg);
  return (PTPT_IntCon)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_Tree accessors */

/*{{{  ATbool PTPT_isValidTree(PTPT_Tree arg) */

ATbool PTPT_isValidTree(PTPT_Tree arg)
{
  if (PTPT_isTreeAnnotated(arg)) {
    return ATtrue;
  }
  else if (PTPT_isTreeAppl(arg)) {
    return ATtrue;
  }
  else if (PTPT_isTreeChar(arg)) {
    return ATtrue;
  }
  else if (PTPT_isTreeLit(arg)) {
    return ATtrue;
  }
  else if (PTPT_isTreeAmb(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isTreeAnnotated(PTPT_Tree arg) */

inline ATbool PTPT_isTreeAnnotated(PTPT_Tree arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternTreeAnnotated, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isTreeAppl(PTPT_Tree arg) */

inline ATbool PTPT_isTreeAppl(PTPT_Tree arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternTreeAppl, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isTreeChar(PTPT_Tree arg) */

inline ATbool PTPT_isTreeChar(PTPT_Tree arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternTreeChar, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isTreeLit(PTPT_Tree arg) */

inline ATbool PTPT_isTreeLit(PTPT_Tree arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternTreeLit, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isTreeAmb(PTPT_Tree arg) */

inline ATbool PTPT_isTreeAmb(PTPT_Tree arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternTreeAmb, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  ATbool PTPT_hasTreeTree(PTPT_Tree arg) */

ATbool PTPT_hasTreeTree(PTPT_Tree arg)
{
  if (PTPT_isTreeAnnotated(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Tree PTPT_getTreeTree(PTPT_Tree arg) */

PTPT_Tree PTPT_getTreeTree(PTPT_Tree arg)
{
  
    return (PTPT_Tree)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1));
}

/*}}}  */
/*{{{  PTPT_Tree PTPT_setTreeTree(PTPT_Tree arg, PTPT_Tree Tree) */

PTPT_Tree PTPT_setTreeTree(PTPT_Tree arg, PTPT_Tree Tree)
{
  if (PTPT_isTreeAnnotated(arg)) {
    return (PTPT_Tree)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)Tree, 0), 1);
  }

  ATabort("Tree has no Tree: %t\n", arg);
  return (PTPT_Tree)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasTreeWsAfterTree(PTPT_Tree arg) */

ATbool PTPT_hasTreeWsAfterTree(PTPT_Tree arg)
{
  if (PTPT_isTreeAnnotated(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getTreeWsAfterTree(PTPT_Tree arg) */

PTPT_OptLayout PTPT_getTreeWsAfterTree(PTPT_Tree arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Tree PTPT_setTreeWsAfterTree(PTPT_Tree arg, PTPT_OptLayout wsAfterTree) */

PTPT_Tree PTPT_setTreeWsAfterTree(PTPT_Tree arg, PTPT_OptLayout wsAfterTree)
{
  if (PTPT_isTreeAnnotated(arg)) {
    return (PTPT_Tree)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterTree, 1), 1);
  }

  ATabort("Tree has no WsAfterTree: %t\n", arg);
  return (PTPT_Tree)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasTreeAnn(PTPT_Tree arg) */

ATbool PTPT_hasTreeAnn(PTPT_Tree arg)
{
  if (PTPT_isTreeAnnotated(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Ann PTPT_getTreeAnn(PTPT_Tree arg) */

PTPT_Ann PTPT_getTreeAnn(PTPT_Tree arg)
{
  
    return (PTPT_Ann)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 2);
}

/*}}}  */
/*{{{  PTPT_Tree PTPT_setTreeAnn(PTPT_Tree arg, PTPT_Ann Ann) */

PTPT_Tree PTPT_setTreeAnn(PTPT_Tree arg, PTPT_Ann Ann)
{
  if (PTPT_isTreeAnnotated(arg)) {
    return (PTPT_Tree)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)Ann, 2), 1);
  }

  ATabort("Tree has no Ann: %t\n", arg);
  return (PTPT_Tree)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasTreeWsAfterAppl(PTPT_Tree arg) */

ATbool PTPT_hasTreeWsAfterAppl(PTPT_Tree arg)
{
  if (PTPT_isTreeAppl(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getTreeWsAfterAppl(PTPT_Tree arg) */

PTPT_OptLayout PTPT_getTreeWsAfterAppl(PTPT_Tree arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Tree PTPT_setTreeWsAfterAppl(PTPT_Tree arg, PTPT_OptLayout wsAfterAppl) */

PTPT_Tree PTPT_setTreeWsAfterAppl(PTPT_Tree arg, PTPT_OptLayout wsAfterAppl)
{
  if (PTPT_isTreeAppl(arg)) {
    return (PTPT_Tree)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterAppl, 1), 1);
  }

  ATabort("Tree has no WsAfterAppl: %t\n", arg);
  return (PTPT_Tree)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasTreeWsAfterParenOpen(PTPT_Tree arg) */

ATbool PTPT_hasTreeWsAfterParenOpen(PTPT_Tree arg)
{
  if (PTPT_isTreeAppl(arg)) {
    return ATtrue;
  }
  else if (PTPT_isTreeLit(arg)) {
    return ATtrue;
  }
  else if (PTPT_isTreeAmb(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getTreeWsAfterParenOpen(PTPT_Tree arg) */

PTPT_OptLayout PTPT_getTreeWsAfterParenOpen(PTPT_Tree arg)
{
  if (PTPT_isTreeAppl(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
  }
  else if (PTPT_isTreeLit(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
  }
  else 
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
}

/*}}}  */
/*{{{  PTPT_Tree PTPT_setTreeWsAfterParenOpen(PTPT_Tree arg, PTPT_OptLayout wsAfterParenOpen) */

PTPT_Tree PTPT_setTreeWsAfterParenOpen(PTPT_Tree arg, PTPT_OptLayout wsAfterParenOpen)
{
  if (PTPT_isTreeAppl(arg)) {
    return (PTPT_Tree)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }
  else if (PTPT_isTreeLit(arg)) {
    return (PTPT_Tree)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }
  else if (PTPT_isTreeAmb(arg)) {
    return (PTPT_Tree)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }

  ATabort("Tree has no WsAfterParenOpen: %t\n", arg);
  return (PTPT_Tree)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasTreeProd(PTPT_Tree arg) */

ATbool PTPT_hasTreeProd(PTPT_Tree arg)
{
  if (PTPT_isTreeAppl(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Production PTPT_getTreeProd(PTPT_Tree arg) */

PTPT_Production PTPT_getTreeProd(PTPT_Tree arg)
{
  
    return (PTPT_Production)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
}

/*}}}  */
/*{{{  PTPT_Tree PTPT_setTreeProd(PTPT_Tree arg, PTPT_Production prod) */

PTPT_Tree PTPT_setTreeProd(PTPT_Tree arg, PTPT_Production prod)
{
  if (PTPT_isTreeAppl(arg)) {
    return (PTPT_Tree)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)prod, 4), 1);
  }

  ATabort("Tree has no Prod: %t\n", arg);
  return (PTPT_Tree)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasTreeWsAfterProd(PTPT_Tree arg) */

ATbool PTPT_hasTreeWsAfterProd(PTPT_Tree arg)
{
  if (PTPT_isTreeAppl(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getTreeWsAfterProd(PTPT_Tree arg) */

PTPT_OptLayout PTPT_getTreeWsAfterProd(PTPT_Tree arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
}

/*}}}  */
/*{{{  PTPT_Tree PTPT_setTreeWsAfterProd(PTPT_Tree arg, PTPT_OptLayout wsAfterProd) */

PTPT_Tree PTPT_setTreeWsAfterProd(PTPT_Tree arg, PTPT_OptLayout wsAfterProd)
{
  if (PTPT_isTreeAppl(arg)) {
    return (PTPT_Tree)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterProd, 5), 1);
  }

  ATabort("Tree has no WsAfterProd: %t\n", arg);
  return (PTPT_Tree)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasTreeWsAfterComma(PTPT_Tree arg) */

ATbool PTPT_hasTreeWsAfterComma(PTPT_Tree arg)
{
  if (PTPT_isTreeAppl(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getTreeWsAfterComma(PTPT_Tree arg) */

PTPT_OptLayout PTPT_getTreeWsAfterComma(PTPT_Tree arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 7);
}

/*}}}  */
/*{{{  PTPT_Tree PTPT_setTreeWsAfterComma(PTPT_Tree arg, PTPT_OptLayout wsAfterComma) */

PTPT_Tree PTPT_setTreeWsAfterComma(PTPT_Tree arg, PTPT_OptLayout wsAfterComma)
{
  if (PTPT_isTreeAppl(arg)) {
    return (PTPT_Tree)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterComma, 7), 1);
  }

  ATabort("Tree has no WsAfterComma: %t\n", arg);
  return (PTPT_Tree)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasTreeArgs(PTPT_Tree arg) */

ATbool PTPT_hasTreeArgs(PTPT_Tree arg)
{
  if (PTPT_isTreeAppl(arg)) {
    return ATtrue;
  }
  else if (PTPT_isTreeAmb(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Args PTPT_getTreeArgs(PTPT_Tree arg) */

PTPT_Args PTPT_getTreeArgs(PTPT_Tree arg)
{
  if (PTPT_isTreeAppl(arg)) {
    return (PTPT_Args)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 8);
  }
  else 
    return (PTPT_Args)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
}

/*}}}  */
/*{{{  PTPT_Tree PTPT_setTreeArgs(PTPT_Tree arg, PTPT_Args args) */

PTPT_Tree PTPT_setTreeArgs(PTPT_Tree arg, PTPT_Args args)
{
  if (PTPT_isTreeAppl(arg)) {
    return (PTPT_Tree)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)args, 8), 1);
  }
  else if (PTPT_isTreeAmb(arg)) {
    return (PTPT_Tree)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)args, 4), 1);
  }

  ATabort("Tree has no Args: %t\n", arg);
  return (PTPT_Tree)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasTreeWsAfterArgs(PTPT_Tree arg) */

ATbool PTPT_hasTreeWsAfterArgs(PTPT_Tree arg)
{
  if (PTPT_isTreeAppl(arg)) {
    return ATtrue;
  }
  else if (PTPT_isTreeAmb(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getTreeWsAfterArgs(PTPT_Tree arg) */

PTPT_OptLayout PTPT_getTreeWsAfterArgs(PTPT_Tree arg)
{
  if (PTPT_isTreeAppl(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 9);
  }
  else 
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
}

/*}}}  */
/*{{{  PTPT_Tree PTPT_setTreeWsAfterArgs(PTPT_Tree arg, PTPT_OptLayout wsAfterArgs) */

PTPT_Tree PTPT_setTreeWsAfterArgs(PTPT_Tree arg, PTPT_OptLayout wsAfterArgs)
{
  if (PTPT_isTreeAppl(arg)) {
    return (PTPT_Tree)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterArgs, 9), 1);
  }
  else if (PTPT_isTreeAmb(arg)) {
    return (PTPT_Tree)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterArgs, 5), 1);
  }

  ATabort("Tree has no WsAfterArgs: %t\n", arg);
  return (PTPT_Tree)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasTreeCharacter(PTPT_Tree arg) */

ATbool PTPT_hasTreeCharacter(PTPT_Tree arg)
{
  if (PTPT_isTreeChar(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_NatCon PTPT_getTreeCharacter(PTPT_Tree arg) */

PTPT_NatCon PTPT_getTreeCharacter(PTPT_Tree arg)
{
  
    return (PTPT_NatCon)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1));
}

/*}}}  */
/*{{{  PTPT_Tree PTPT_setTreeCharacter(PTPT_Tree arg, PTPT_NatCon character) */

PTPT_Tree PTPT_setTreeCharacter(PTPT_Tree arg, PTPT_NatCon character)
{
  if (PTPT_isTreeChar(arg)) {
    return (PTPT_Tree)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)character, 0), 1);
  }

  ATabort("Tree has no Character: %t\n", arg);
  return (PTPT_Tree)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasTreeWsAfterLit(PTPT_Tree arg) */

ATbool PTPT_hasTreeWsAfterLit(PTPT_Tree arg)
{
  if (PTPT_isTreeLit(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getTreeWsAfterLit(PTPT_Tree arg) */

PTPT_OptLayout PTPT_getTreeWsAfterLit(PTPT_Tree arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Tree PTPT_setTreeWsAfterLit(PTPT_Tree arg, PTPT_OptLayout wsAfterLit) */

PTPT_Tree PTPT_setTreeWsAfterLit(PTPT_Tree arg, PTPT_OptLayout wsAfterLit)
{
  if (PTPT_isTreeLit(arg)) {
    return (PTPT_Tree)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterLit, 1), 1);
  }

  ATabort("Tree has no WsAfterLit: %t\n", arg);
  return (PTPT_Tree)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasTreeString(PTPT_Tree arg) */

ATbool PTPT_hasTreeString(PTPT_Tree arg)
{
  if (PTPT_isTreeLit(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_QLiteral PTPT_getTreeString(PTPT_Tree arg) */

PTPT_QLiteral PTPT_getTreeString(PTPT_Tree arg)
{
  
    return (PTPT_QLiteral)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
}

/*}}}  */
/*{{{  PTPT_Tree PTPT_setTreeString(PTPT_Tree arg, PTPT_QLiteral string) */

PTPT_Tree PTPT_setTreeString(PTPT_Tree arg, PTPT_QLiteral string)
{
  if (PTPT_isTreeLit(arg)) {
    return (PTPT_Tree)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)string, 4), 1);
  }

  ATabort("Tree has no String: %t\n", arg);
  return (PTPT_Tree)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasTreeWsAfterString(PTPT_Tree arg) */

ATbool PTPT_hasTreeWsAfterString(PTPT_Tree arg)
{
  if (PTPT_isTreeLit(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getTreeWsAfterString(PTPT_Tree arg) */

PTPT_OptLayout PTPT_getTreeWsAfterString(PTPT_Tree arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
}

/*}}}  */
/*{{{  PTPT_Tree PTPT_setTreeWsAfterString(PTPT_Tree arg, PTPT_OptLayout wsAfterString) */

PTPT_Tree PTPT_setTreeWsAfterString(PTPT_Tree arg, PTPT_OptLayout wsAfterString)
{
  if (PTPT_isTreeLit(arg)) {
    return (PTPT_Tree)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterString, 5), 1);
  }

  ATabort("Tree has no WsAfterString: %t\n", arg);
  return (PTPT_Tree)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasTreeWsAfterAmb(PTPT_Tree arg) */

ATbool PTPT_hasTreeWsAfterAmb(PTPT_Tree arg)
{
  if (PTPT_isTreeAmb(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getTreeWsAfterAmb(PTPT_Tree arg) */

PTPT_OptLayout PTPT_getTreeWsAfterAmb(PTPT_Tree arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Tree PTPT_setTreeWsAfterAmb(PTPT_Tree arg, PTPT_OptLayout wsAfterAmb) */

PTPT_Tree PTPT_setTreeWsAfterAmb(PTPT_Tree arg, PTPT_OptLayout wsAfterAmb)
{
  if (PTPT_isTreeAmb(arg)) {
    return (PTPT_Tree)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterAmb, 1), 1);
  }

  ATabort("Tree has no WsAfterAmb: %t\n", arg);
  return (PTPT_Tree)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_Args accessors */

/*{{{  ATbool PTPT_isValidArgs(PTPT_Args arg) */

ATbool PTPT_isValidArgs(PTPT_Args arg)
{
  if (PTPT_isArgsList(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isArgsList(PTPT_Args arg) */

inline ATbool PTPT_isArgsList(PTPT_Args arg)
{
#ifndef DISABLE_DYNAMIC_CHECKING
  assert(arg != NULL);
  assert(ATmatchTerm((ATerm)arg, PTPT_patternArgsList, NULL, NULL, NULL));
#endif
  return ATtrue;
}

/*}}}  */
/*{{{  ATbool PTPT_hasArgsWsAfterBracketOpen(PTPT_Args arg) */

ATbool PTPT_hasArgsWsAfterBracketOpen(PTPT_Args arg)
{
  if (PTPT_isArgsList(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getArgsWsAfterBracketOpen(PTPT_Args arg) */

PTPT_OptLayout PTPT_getArgsWsAfterBracketOpen(PTPT_Args arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Args PTPT_setArgsWsAfterBracketOpen(PTPT_Args arg, PTPT_OptLayout wsAfterBracketOpen) */

PTPT_Args PTPT_setArgsWsAfterBracketOpen(PTPT_Args arg, PTPT_OptLayout wsAfterBracketOpen)
{
  if (PTPT_isArgsList(arg)) {
    return (PTPT_Args)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterBracketOpen, 1), 1);
  }

  ATabort("Args has no WsAfterBracketOpen: %t\n", arg);
  return (PTPT_Args)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasArgsList(PTPT_Args arg) */

ATbool PTPT_hasArgsList(PTPT_Args arg)
{
  if (PTPT_isArgsList(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_TreeList PTPT_getArgsList(PTPT_Args arg) */

PTPT_TreeList PTPT_getArgsList(PTPT_Args arg)
{
  
    return (PTPT_TreeList)ATgetArgument((ATermAppl)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 2), 1);
}

/*}}}  */
/*{{{  PTPT_Args PTPT_setArgsList(PTPT_Args arg, PTPT_TreeList list) */

PTPT_Args PTPT_setArgsList(PTPT_Args arg, PTPT_TreeList list)
{
  if (PTPT_isArgsList(arg)) {
    return (PTPT_Args)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)ATsetArgument((ATermAppl)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 2), (ATerm)list, 1), 2), 1);
  }

  ATabort("Args has no List: %t\n", arg);
  return (PTPT_Args)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasArgsWsAfterList(PTPT_Args arg) */

ATbool PTPT_hasArgsWsAfterList(PTPT_Args arg)
{
  if (PTPT_isArgsList(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getArgsWsAfterList(PTPT_Args arg) */

PTPT_OptLayout PTPT_getArgsWsAfterList(PTPT_Args arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
}

/*}}}  */
/*{{{  PTPT_Args PTPT_setArgsWsAfterList(PTPT_Args arg, PTPT_OptLayout wsAfterList) */

PTPT_Args PTPT_setArgsWsAfterList(PTPT_Args arg, PTPT_OptLayout wsAfterList)
{
  if (PTPT_isArgsList(arg)) {
    return (PTPT_Args)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterList, 3), 1);
  }

  ATabort("Args has no WsAfterList: %t\n", arg);
  return (PTPT_Args)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_TreeList accessors */

/*{{{  ATbool PTPT_isValidTreeList(PTPT_TreeList arg) */

ATbool PTPT_isValidTreeList(PTPT_TreeList arg)
{
  if (PTPT_isTreeListEmpty(arg)) {
    return ATtrue;
  }
  else if (PTPT_isTreeListSingle(arg)) {
    return ATtrue;
  }
  else if (PTPT_isTreeListMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isTreeListEmpty(PTPT_TreeList arg) */

inline ATbool PTPT_isTreeListEmpty(PTPT_TreeList arg)
{
  if (!ATisEmpty((ATermList)arg)) {
    return ATfalse;
  }
#ifndef DISABLE_DYNAMIC_CHECKING
  assert(arg != NULL);
  assert(ATmatchTerm((ATerm)arg, PTPT_patternTreeListEmpty));
#endif
  return ATtrue;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isTreeListSingle(PTPT_TreeList arg) */

inline ATbool PTPT_isTreeListSingle(PTPT_TreeList arg)
{
  if (ATisEmpty((ATermList)arg)) {
    return ATfalse;
  }
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternTreeListSingle, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isTreeListMany(PTPT_TreeList arg) */

inline ATbool PTPT_isTreeListMany(PTPT_TreeList arg)
{
  if (ATisEmpty((ATermList)arg)) {
    return ATfalse;
  }
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternTreeListMany, NULL, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  ATbool PTPT_hasTreeListHead(PTPT_TreeList arg) */

ATbool PTPT_hasTreeListHead(PTPT_TreeList arg)
{
  if (PTPT_isTreeListSingle(arg)) {
    return ATtrue;
  }
  else if (PTPT_isTreeListMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Tree PTPT_getTreeListHead(PTPT_TreeList arg) */

PTPT_Tree PTPT_getTreeListHead(PTPT_TreeList arg)
{
  if (PTPT_isTreeListSingle(arg)) {
    return (PTPT_Tree)ATgetFirst((ATermList)arg);
  }
  else 
    return (PTPT_Tree)ATgetFirst((ATermList)arg);
}

/*}}}  */
/*{{{  PTPT_TreeList PTPT_setTreeListHead(PTPT_TreeList arg, PTPT_Tree head) */

PTPT_TreeList PTPT_setTreeListHead(PTPT_TreeList arg, PTPT_Tree head)
{
  if (PTPT_isTreeListSingle(arg)) {
    return (PTPT_TreeList)ATreplace((ATermList)arg, (ATerm)head, 0);
  }
  else if (PTPT_isTreeListMany(arg)) {
    return (PTPT_TreeList)ATreplace((ATermList)arg, (ATerm)head, 0);
  }

  ATabort("TreeList has no Head: %t\n", arg);
  return (PTPT_TreeList)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasTreeListWsAfterFirst(PTPT_TreeList arg) */

ATbool PTPT_hasTreeListWsAfterFirst(PTPT_TreeList arg)
{
  if (PTPT_isTreeListMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getTreeListWsAfterFirst(PTPT_TreeList arg) */

PTPT_OptLayout PTPT_getTreeListWsAfterFirst(PTPT_TreeList arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)arg, 1);
}

/*}}}  */
/*{{{  PTPT_TreeList PTPT_setTreeListWsAfterFirst(PTPT_TreeList arg, PTPT_OptLayout wsAfterFirst) */

PTPT_TreeList PTPT_setTreeListWsAfterFirst(PTPT_TreeList arg, PTPT_OptLayout wsAfterFirst)
{
  if (PTPT_isTreeListMany(arg)) {
    return (PTPT_TreeList)ATreplace((ATermList)arg, (ATerm)wsAfterFirst, 1);
  }

  ATabort("TreeList has no WsAfterFirst: %t\n", arg);
  return (PTPT_TreeList)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasTreeListSep(PTPT_TreeList arg) */

ATbool PTPT_hasTreeListSep(PTPT_TreeList arg)
{
  if (PTPT_isTreeListMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  char * PTPT_getTreeListSep(PTPT_TreeList arg) */

char * PTPT_getTreeListSep(PTPT_TreeList arg)
{
  
    return (char *)ATgetName(ATgetAFun((ATermAppl)ATgetArgument((ATermAppl)ATelementAt((ATermList)arg, 2), 0)));
}

/*}}}  */
/*{{{  PTPT_TreeList PTPT_setTreeListSep(PTPT_TreeList arg, char * sep) */

PTPT_TreeList PTPT_setTreeListSep(PTPT_TreeList arg, char * sep)
{
  if (PTPT_isTreeListMany(arg)) {
    return (PTPT_TreeList)ATreplace((ATermList)arg, (ATerm)ATsetArgument((ATermAppl)ATelementAt((ATermList)arg, 2), (ATerm)ATmakeAppl0(ATmakeAFun(sep, 0, ATtrue)), 0), 2);
  }

  ATabort("TreeList has no Sep: %t\n", arg);
  return (PTPT_TreeList)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasTreeListWsAfterSep(PTPT_TreeList arg) */

ATbool PTPT_hasTreeListWsAfterSep(PTPT_TreeList arg)
{
  if (PTPT_isTreeListMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getTreeListWsAfterSep(PTPT_TreeList arg) */

PTPT_OptLayout PTPT_getTreeListWsAfterSep(PTPT_TreeList arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)arg, 3);
}

/*}}}  */
/*{{{  PTPT_TreeList PTPT_setTreeListWsAfterSep(PTPT_TreeList arg, PTPT_OptLayout wsAfterSep) */

PTPT_TreeList PTPT_setTreeListWsAfterSep(PTPT_TreeList arg, PTPT_OptLayout wsAfterSep)
{
  if (PTPT_isTreeListMany(arg)) {
    return (PTPT_TreeList)ATreplace((ATermList)arg, (ATerm)wsAfterSep, 3);
  }

  ATabort("TreeList has no WsAfterSep: %t\n", arg);
  return (PTPT_TreeList)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasTreeListTail(PTPT_TreeList arg) */

ATbool PTPT_hasTreeListTail(PTPT_TreeList arg)
{
  if (PTPT_isTreeListMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_TreeList PTPT_getTreeListTail(PTPT_TreeList arg) */

PTPT_TreeList PTPT_getTreeListTail(PTPT_TreeList arg)
{
  
    return (PTPT_TreeList)ATgetTail((ATermList)arg, 4);
}

/*}}}  */
/*{{{  PTPT_TreeList PTPT_setTreeListTail(PTPT_TreeList arg, PTPT_TreeList tail) */

PTPT_TreeList PTPT_setTreeListTail(PTPT_TreeList arg, PTPT_TreeList tail)
{
  if (PTPT_isTreeListMany(arg)) {
    return (PTPT_TreeList)ATreplaceTail((ATermList)arg, (ATermList)tail, 4);
  }

  ATabort("TreeList has no Tail: %t\n", arg);
  return (PTPT_TreeList)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_Production accessors */

/*{{{  ATbool PTPT_isValidProduction(PTPT_Production arg) */

ATbool PTPT_isValidProduction(PTPT_Production arg)
{
  if (PTPT_isProductionDefault(arg)) {
    return ATtrue;
  }
  else if (PTPT_isProductionList(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isProductionDefault(PTPT_Production arg) */

inline ATbool PTPT_isProductionDefault(PTPT_Production arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternProductionDefault, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isProductionList(PTPT_Production arg) */

inline ATbool PTPT_isProductionList(PTPT_Production arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternProductionList, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  ATbool PTPT_hasProductionWsAfterProd(PTPT_Production arg) */

ATbool PTPT_hasProductionWsAfterProd(PTPT_Production arg)
{
  if (PTPT_isProductionDefault(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getProductionWsAfterProd(PTPT_Production arg) */

PTPT_OptLayout PTPT_getProductionWsAfterProd(PTPT_Production arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Production PTPT_setProductionWsAfterProd(PTPT_Production arg, PTPT_OptLayout wsAfterProd) */

PTPT_Production PTPT_setProductionWsAfterProd(PTPT_Production arg, PTPT_OptLayout wsAfterProd)
{
  if (PTPT_isProductionDefault(arg)) {
    return (PTPT_Production)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterProd, 1), 1);
  }

  ATabort("Production has no WsAfterProd: %t\n", arg);
  return (PTPT_Production)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasProductionWsAfterParenOpen(PTPT_Production arg) */

ATbool PTPT_hasProductionWsAfterParenOpen(PTPT_Production arg)
{
  if (PTPT_isProductionDefault(arg)) {
    return ATtrue;
  }
  else if (PTPT_isProductionList(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getProductionWsAfterParenOpen(PTPT_Production arg) */

PTPT_OptLayout PTPT_getProductionWsAfterParenOpen(PTPT_Production arg)
{
  if (PTPT_isProductionDefault(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
  }
  else 
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
}

/*}}}  */
/*{{{  PTPT_Production PTPT_setProductionWsAfterParenOpen(PTPT_Production arg, PTPT_OptLayout wsAfterParenOpen) */

PTPT_Production PTPT_setProductionWsAfterParenOpen(PTPT_Production arg, PTPT_OptLayout wsAfterParenOpen)
{
  if (PTPT_isProductionDefault(arg)) {
    return (PTPT_Production)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }
  else if (PTPT_isProductionList(arg)) {
    return (PTPT_Production)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }

  ATabort("Production has no WsAfterParenOpen: %t\n", arg);
  return (PTPT_Production)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasProductionLhs(PTPT_Production arg) */

ATbool PTPT_hasProductionLhs(PTPT_Production arg)
{
  if (PTPT_isProductionDefault(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Symbols PTPT_getProductionLhs(PTPT_Production arg) */

PTPT_Symbols PTPT_getProductionLhs(PTPT_Production arg)
{
  
    return (PTPT_Symbols)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
}

/*}}}  */
/*{{{  PTPT_Production PTPT_setProductionLhs(PTPT_Production arg, PTPT_Symbols lhs) */

PTPT_Production PTPT_setProductionLhs(PTPT_Production arg, PTPT_Symbols lhs)
{
  if (PTPT_isProductionDefault(arg)) {
    return (PTPT_Production)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)lhs, 4), 1);
  }

  ATabort("Production has no Lhs: %t\n", arg);
  return (PTPT_Production)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasProductionWsAfterLhs(PTPT_Production arg) */

ATbool PTPT_hasProductionWsAfterLhs(PTPT_Production arg)
{
  if (PTPT_isProductionDefault(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getProductionWsAfterLhs(PTPT_Production arg) */

PTPT_OptLayout PTPT_getProductionWsAfterLhs(PTPT_Production arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
}

/*}}}  */
/*{{{  PTPT_Production PTPT_setProductionWsAfterLhs(PTPT_Production arg, PTPT_OptLayout wsAfterLhs) */

PTPT_Production PTPT_setProductionWsAfterLhs(PTPT_Production arg, PTPT_OptLayout wsAfterLhs)
{
  if (PTPT_isProductionDefault(arg)) {
    return (PTPT_Production)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterLhs, 5), 1);
  }

  ATabort("Production has no WsAfterLhs: %t\n", arg);
  return (PTPT_Production)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasProductionWsAfterComma1(PTPT_Production arg) */

ATbool PTPT_hasProductionWsAfterComma1(PTPT_Production arg)
{
  if (PTPT_isProductionDefault(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getProductionWsAfterComma1(PTPT_Production arg) */

PTPT_OptLayout PTPT_getProductionWsAfterComma1(PTPT_Production arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 7);
}

/*}}}  */
/*{{{  PTPT_Production PTPT_setProductionWsAfterComma1(PTPT_Production arg, PTPT_OptLayout wsAfterComma1) */

PTPT_Production PTPT_setProductionWsAfterComma1(PTPT_Production arg, PTPT_OptLayout wsAfterComma1)
{
  if (PTPT_isProductionDefault(arg)) {
    return (PTPT_Production)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterComma1, 7), 1);
  }

  ATabort("Production has no WsAfterComma1: %t\n", arg);
  return (PTPT_Production)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasProductionRhs(PTPT_Production arg) */

ATbool PTPT_hasProductionRhs(PTPT_Production arg)
{
  if (PTPT_isProductionDefault(arg)) {
    return ATtrue;
  }
  else if (PTPT_isProductionList(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_getProductionRhs(PTPT_Production arg) */

PTPT_Symbol PTPT_getProductionRhs(PTPT_Production arg)
{
  if (PTPT_isProductionDefault(arg)) {
    return (PTPT_Symbol)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 8);
  }
  else 
    return (PTPT_Symbol)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
}

/*}}}  */
/*{{{  PTPT_Production PTPT_setProductionRhs(PTPT_Production arg, PTPT_Symbol rhs) */

PTPT_Production PTPT_setProductionRhs(PTPT_Production arg, PTPT_Symbol rhs)
{
  if (PTPT_isProductionDefault(arg)) {
    return (PTPT_Production)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)rhs, 8), 1);
  }
  else if (PTPT_isProductionList(arg)) {
    return (PTPT_Production)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)rhs, 4), 1);
  }

  ATabort("Production has no Rhs: %t\n", arg);
  return (PTPT_Production)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasProductionWsAfterRhs(PTPT_Production arg) */

ATbool PTPT_hasProductionWsAfterRhs(PTPT_Production arg)
{
  if (PTPT_isProductionDefault(arg)) {
    return ATtrue;
  }
  else if (PTPT_isProductionList(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getProductionWsAfterRhs(PTPT_Production arg) */

PTPT_OptLayout PTPT_getProductionWsAfterRhs(PTPT_Production arg)
{
  if (PTPT_isProductionDefault(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 9);
  }
  else 
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
}

/*}}}  */
/*{{{  PTPT_Production PTPT_setProductionWsAfterRhs(PTPT_Production arg, PTPT_OptLayout wsAfterRhs) */

PTPT_Production PTPT_setProductionWsAfterRhs(PTPT_Production arg, PTPT_OptLayout wsAfterRhs)
{
  if (PTPT_isProductionDefault(arg)) {
    return (PTPT_Production)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterRhs, 9), 1);
  }
  else if (PTPT_isProductionList(arg)) {
    return (PTPT_Production)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterRhs, 5), 1);
  }

  ATabort("Production has no WsAfterRhs: %t\n", arg);
  return (PTPT_Production)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasProductionWsAfterComma2(PTPT_Production arg) */

ATbool PTPT_hasProductionWsAfterComma2(PTPT_Production arg)
{
  if (PTPT_isProductionDefault(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getProductionWsAfterComma2(PTPT_Production arg) */

PTPT_OptLayout PTPT_getProductionWsAfterComma2(PTPT_Production arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 11);
}

/*}}}  */
/*{{{  PTPT_Production PTPT_setProductionWsAfterComma2(PTPT_Production arg, PTPT_OptLayout wsAfterComma2) */

PTPT_Production PTPT_setProductionWsAfterComma2(PTPT_Production arg, PTPT_OptLayout wsAfterComma2)
{
  if (PTPT_isProductionDefault(arg)) {
    return (PTPT_Production)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterComma2, 11), 1);
  }

  ATabort("Production has no WsAfterComma2: %t\n", arg);
  return (PTPT_Production)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasProductionAttributes(PTPT_Production arg) */

ATbool PTPT_hasProductionAttributes(PTPT_Production arg)
{
  if (PTPT_isProductionDefault(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Attributes PTPT_getProductionAttributes(PTPT_Production arg) */

PTPT_Attributes PTPT_getProductionAttributes(PTPT_Production arg)
{
  
    return (PTPT_Attributes)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 12);
}

/*}}}  */
/*{{{  PTPT_Production PTPT_setProductionAttributes(PTPT_Production arg, PTPT_Attributes attributes) */

PTPT_Production PTPT_setProductionAttributes(PTPT_Production arg, PTPT_Attributes attributes)
{
  if (PTPT_isProductionDefault(arg)) {
    return (PTPT_Production)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)attributes, 12), 1);
  }

  ATabort("Production has no Attributes: %t\n", arg);
  return (PTPT_Production)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasProductionWsAfterAttributes(PTPT_Production arg) */

ATbool PTPT_hasProductionWsAfterAttributes(PTPT_Production arg)
{
  if (PTPT_isProductionDefault(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getProductionWsAfterAttributes(PTPT_Production arg) */

PTPT_OptLayout PTPT_getProductionWsAfterAttributes(PTPT_Production arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 13);
}

/*}}}  */
/*{{{  PTPT_Production PTPT_setProductionWsAfterAttributes(PTPT_Production arg, PTPT_OptLayout wsAfterAttributes) */

PTPT_Production PTPT_setProductionWsAfterAttributes(PTPT_Production arg, PTPT_OptLayout wsAfterAttributes)
{
  if (PTPT_isProductionDefault(arg)) {
    return (PTPT_Production)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterAttributes, 13), 1);
  }

  ATabort("Production has no WsAfterAttributes: %t\n", arg);
  return (PTPT_Production)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasProductionWsAfterList(PTPT_Production arg) */

ATbool PTPT_hasProductionWsAfterList(PTPT_Production arg)
{
  if (PTPT_isProductionList(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getProductionWsAfterList(PTPT_Production arg) */

PTPT_OptLayout PTPT_getProductionWsAfterList(PTPT_Production arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Production PTPT_setProductionWsAfterList(PTPT_Production arg, PTPT_OptLayout wsAfterList) */

PTPT_Production PTPT_setProductionWsAfterList(PTPT_Production arg, PTPT_OptLayout wsAfterList)
{
  if (PTPT_isProductionList(arg)) {
    return (PTPT_Production)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterList, 1), 1);
  }

  ATabort("Production has no WsAfterList: %t\n", arg);
  return (PTPT_Production)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_Symbol accessors */

/*{{{  ATbool PTPT_isValidSymbol(PTPT_Symbol arg) */

ATbool PTPT_isValidSymbol(PTPT_Symbol arg)
{
  if (PTPT_isSymbolEmpty(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolLit(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolCf(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolLex(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolOpt(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolAlt(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolSeq(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolTuple(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolSort(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIter(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterStar(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterSep(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterStarSep(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterN(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterSepN(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolFunc(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolVarsym(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolLayout(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolCharClass(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolStrategy(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolParametrizedSort(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isSymbolEmpty(PTPT_Symbol arg) */

inline ATbool PTPT_isSymbolEmpty(PTPT_Symbol arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternSymbolEmpty);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isSymbolLit(PTPT_Symbol arg) */

inline ATbool PTPT_isSymbolLit(PTPT_Symbol arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternSymbolLit, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isSymbolCf(PTPT_Symbol arg) */

inline ATbool PTPT_isSymbolCf(PTPT_Symbol arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternSymbolCf, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isSymbolLex(PTPT_Symbol arg) */

inline ATbool PTPT_isSymbolLex(PTPT_Symbol arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternSymbolLex, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isSymbolOpt(PTPT_Symbol arg) */

inline ATbool PTPT_isSymbolOpt(PTPT_Symbol arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternSymbolOpt, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isSymbolAlt(PTPT_Symbol arg) */

inline ATbool PTPT_isSymbolAlt(PTPT_Symbol arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternSymbolAlt, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isSymbolSeq(PTPT_Symbol arg) */

inline ATbool PTPT_isSymbolSeq(PTPT_Symbol arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternSymbolSeq, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isSymbolTuple(PTPT_Symbol arg) */

inline ATbool PTPT_isSymbolTuple(PTPT_Symbol arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternSymbolTuple, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isSymbolSort(PTPT_Symbol arg) */

inline ATbool PTPT_isSymbolSort(PTPT_Symbol arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternSymbolSort, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isSymbolIter(PTPT_Symbol arg) */

inline ATbool PTPT_isSymbolIter(PTPT_Symbol arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternSymbolIter, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isSymbolIterStar(PTPT_Symbol arg) */

inline ATbool PTPT_isSymbolIterStar(PTPT_Symbol arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternSymbolIterStar, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isSymbolIterSep(PTPT_Symbol arg) */

inline ATbool PTPT_isSymbolIterSep(PTPT_Symbol arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternSymbolIterSep, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isSymbolIterStarSep(PTPT_Symbol arg) */

inline ATbool PTPT_isSymbolIterStarSep(PTPT_Symbol arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternSymbolIterStarSep, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isSymbolIterN(PTPT_Symbol arg) */

inline ATbool PTPT_isSymbolIterN(PTPT_Symbol arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternSymbolIterN, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isSymbolIterSepN(PTPT_Symbol arg) */

inline ATbool PTPT_isSymbolIterSepN(PTPT_Symbol arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternSymbolIterSepN, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isSymbolFunc(PTPT_Symbol arg) */

inline ATbool PTPT_isSymbolFunc(PTPT_Symbol arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternSymbolFunc, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isSymbolVarsym(PTPT_Symbol arg) */

inline ATbool PTPT_isSymbolVarsym(PTPT_Symbol arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternSymbolVarsym, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isSymbolLayout(PTPT_Symbol arg) */

inline ATbool PTPT_isSymbolLayout(PTPT_Symbol arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternSymbolLayout);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isSymbolCharClass(PTPT_Symbol arg) */

inline ATbool PTPT_isSymbolCharClass(PTPT_Symbol arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternSymbolCharClass, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isSymbolStrategy(PTPT_Symbol arg) */

inline ATbool PTPT_isSymbolStrategy(PTPT_Symbol arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternSymbolStrategy, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isSymbolParametrizedSort(PTPT_Symbol arg) */

inline ATbool PTPT_isSymbolParametrizedSort(PTPT_Symbol arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternSymbolParametrizedSort, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterLit(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterLit(PTPT_Symbol arg)
{
  if (PTPT_isSymbolLit(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterLit(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterLit(PTPT_Symbol arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterLit(PTPT_Symbol arg, PTPT_OptLayout wsAfterLit) */

PTPT_Symbol PTPT_setSymbolWsAfterLit(PTPT_Symbol arg, PTPT_OptLayout wsAfterLit)
{
  if (PTPT_isSymbolLit(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterLit, 1), 1);
  }

  ATabort("Symbol has no WsAfterLit: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterParenOpen(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterParenOpen(PTPT_Symbol arg)
{
  if (PTPT_isSymbolLit(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolCf(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolLex(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolOpt(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolAlt(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolSeq(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolTuple(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolSort(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIter(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterStar(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterSep(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterStarSep(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterN(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterSepN(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolFunc(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolVarsym(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolCharClass(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolStrategy(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolParametrizedSort(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterParenOpen(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterParenOpen(PTPT_Symbol arg)
{
  if (PTPT_isSymbolLit(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
  }
  else if (PTPT_isSymbolCf(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
  }
  else if (PTPT_isSymbolLex(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
  }
  else if (PTPT_isSymbolOpt(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
  }
  else if (PTPT_isSymbolAlt(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
  }
  else if (PTPT_isSymbolSeq(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
  }
  else if (PTPT_isSymbolTuple(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
  }
  else if (PTPT_isSymbolSort(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
  }
  else if (PTPT_isSymbolIter(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
  }
  else if (PTPT_isSymbolIterStar(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
  }
  else if (PTPT_isSymbolIterSep(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
  }
  else if (PTPT_isSymbolIterStarSep(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
  }
  else if (PTPT_isSymbolIterN(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
  }
  else if (PTPT_isSymbolIterSepN(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
  }
  else if (PTPT_isSymbolFunc(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
  }
  else if (PTPT_isSymbolVarsym(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
  }
  else if (PTPT_isSymbolCharClass(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
  }
  else if (PTPT_isSymbolStrategy(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
  }
  else 
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterParenOpen(PTPT_Symbol arg, PTPT_OptLayout wsAfterParenOpen) */

PTPT_Symbol PTPT_setSymbolWsAfterParenOpen(PTPT_Symbol arg, PTPT_OptLayout wsAfterParenOpen)
{
  if (PTPT_isSymbolLit(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }
  else if (PTPT_isSymbolCf(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }
  else if (PTPT_isSymbolLex(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }
  else if (PTPT_isSymbolOpt(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }
  else if (PTPT_isSymbolAlt(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }
  else if (PTPT_isSymbolSeq(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }
  else if (PTPT_isSymbolTuple(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }
  else if (PTPT_isSymbolSort(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }
  else if (PTPT_isSymbolIter(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }
  else if (PTPT_isSymbolIterStar(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }
  else if (PTPT_isSymbolIterSep(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }
  else if (PTPT_isSymbolIterStarSep(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }
  else if (PTPT_isSymbolIterN(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }
  else if (PTPT_isSymbolIterSepN(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }
  else if (PTPT_isSymbolFunc(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }
  else if (PTPT_isSymbolVarsym(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }
  else if (PTPT_isSymbolCharClass(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }
  else if (PTPT_isSymbolStrategy(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }
  else if (PTPT_isSymbolParametrizedSort(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }

  ATabort("Symbol has no WsAfterParenOpen: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolString(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolString(PTPT_Symbol arg)
{
  if (PTPT_isSymbolLit(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolSort(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_QLiteral PTPT_getSymbolString(PTPT_Symbol arg) */

PTPT_QLiteral PTPT_getSymbolString(PTPT_Symbol arg)
{
  if (PTPT_isSymbolLit(arg)) {
    return (PTPT_QLiteral)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
  }
  else 
    return (PTPT_QLiteral)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolString(PTPT_Symbol arg, PTPT_QLiteral string) */

PTPT_Symbol PTPT_setSymbolString(PTPT_Symbol arg, PTPT_QLiteral string)
{
  if (PTPT_isSymbolLit(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)string, 4), 1);
  }
  else if (PTPT_isSymbolSort(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)string, 4), 1);
  }

  ATabort("Symbol has no String: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterString(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterString(PTPT_Symbol arg)
{
  if (PTPT_isSymbolLit(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolSort(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterString(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterString(PTPT_Symbol arg)
{
  if (PTPT_isSymbolLit(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
  }
  else 
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterString(PTPT_Symbol arg, PTPT_OptLayout wsAfterString) */

PTPT_Symbol PTPT_setSymbolWsAfterString(PTPT_Symbol arg, PTPT_OptLayout wsAfterString)
{
  if (PTPT_isSymbolLit(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterString, 5), 1);
  }
  else if (PTPT_isSymbolSort(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterString, 5), 1);
  }

  ATabort("Symbol has no WsAfterString: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterCf(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterCf(PTPT_Symbol arg)
{
  if (PTPT_isSymbolCf(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterCf(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterCf(PTPT_Symbol arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterCf(PTPT_Symbol arg, PTPT_OptLayout wsAfterCf) */

PTPT_Symbol PTPT_setSymbolWsAfterCf(PTPT_Symbol arg, PTPT_OptLayout wsAfterCf)
{
  if (PTPT_isSymbolCf(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterCf, 1), 1);
  }

  ATabort("Symbol has no WsAfterCf: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolSymbol(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolSymbol(PTPT_Symbol arg)
{
  if (PTPT_isSymbolCf(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolLex(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolOpt(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIter(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterStar(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterSep(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterStarSep(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterN(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterSepN(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolFunc(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolVarsym(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_getSymbolSymbol(PTPT_Symbol arg) */

PTPT_Symbol PTPT_getSymbolSymbol(PTPT_Symbol arg)
{
  if (PTPT_isSymbolCf(arg)) {
    return (PTPT_Symbol)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
  }
  else if (PTPT_isSymbolLex(arg)) {
    return (PTPT_Symbol)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
  }
  else if (PTPT_isSymbolOpt(arg)) {
    return (PTPT_Symbol)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
  }
  else if (PTPT_isSymbolIter(arg)) {
    return (PTPT_Symbol)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
  }
  else if (PTPT_isSymbolIterStar(arg)) {
    return (PTPT_Symbol)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
  }
  else if (PTPT_isSymbolIterSep(arg)) {
    return (PTPT_Symbol)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
  }
  else if (PTPT_isSymbolIterStarSep(arg)) {
    return (PTPT_Symbol)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
  }
  else if (PTPT_isSymbolIterN(arg)) {
    return (PTPT_Symbol)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
  }
  else if (PTPT_isSymbolIterSepN(arg)) {
    return (PTPT_Symbol)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
  }
  else if (PTPT_isSymbolFunc(arg)) {
    return (PTPT_Symbol)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 8);
  }
  else 
    return (PTPT_Symbol)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolSymbol(PTPT_Symbol arg, PTPT_Symbol symbol) */

PTPT_Symbol PTPT_setSymbolSymbol(PTPT_Symbol arg, PTPT_Symbol symbol)
{
  if (PTPT_isSymbolCf(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)symbol, 4), 1);
  }
  else if (PTPT_isSymbolLex(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)symbol, 4), 1);
  }
  else if (PTPT_isSymbolOpt(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)symbol, 4), 1);
  }
  else if (PTPT_isSymbolIter(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)symbol, 4), 1);
  }
  else if (PTPT_isSymbolIterStar(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)symbol, 4), 1);
  }
  else if (PTPT_isSymbolIterSep(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)symbol, 4), 1);
  }
  else if (PTPT_isSymbolIterStarSep(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)symbol, 4), 1);
  }
  else if (PTPT_isSymbolIterN(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)symbol, 4), 1);
  }
  else if (PTPT_isSymbolIterSepN(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)symbol, 4), 1);
  }
  else if (PTPT_isSymbolFunc(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)symbol, 8), 1);
  }
  else if (PTPT_isSymbolVarsym(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)symbol, 4), 1);
  }

  ATabort("Symbol has no Symbol: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterSymbol(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterSymbol(PTPT_Symbol arg)
{
  if (PTPT_isSymbolCf(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolLex(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolOpt(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIter(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterStar(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterSep(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterStarSep(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterN(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterSepN(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolFunc(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolVarsym(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterSymbol(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterSymbol(PTPT_Symbol arg)
{
  if (PTPT_isSymbolCf(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
  }
  else if (PTPT_isSymbolLex(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
  }
  else if (PTPT_isSymbolOpt(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
  }
  else if (PTPT_isSymbolIter(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
  }
  else if (PTPT_isSymbolIterStar(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
  }
  else if (PTPT_isSymbolIterSep(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
  }
  else if (PTPT_isSymbolIterStarSep(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
  }
  else if (PTPT_isSymbolIterN(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
  }
  else if (PTPT_isSymbolIterSepN(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
  }
  else if (PTPT_isSymbolFunc(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 9);
  }
  else 
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterSymbol(PTPT_Symbol arg, PTPT_OptLayout wsAfterSymbol) */

PTPT_Symbol PTPT_setSymbolWsAfterSymbol(PTPT_Symbol arg, PTPT_OptLayout wsAfterSymbol)
{
  if (PTPT_isSymbolCf(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterSymbol, 5), 1);
  }
  else if (PTPT_isSymbolLex(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterSymbol, 5), 1);
  }
  else if (PTPT_isSymbolOpt(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterSymbol, 5), 1);
  }
  else if (PTPT_isSymbolIter(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterSymbol, 5), 1);
  }
  else if (PTPT_isSymbolIterStar(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterSymbol, 5), 1);
  }
  else if (PTPT_isSymbolIterSep(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterSymbol, 5), 1);
  }
  else if (PTPT_isSymbolIterStarSep(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterSymbol, 5), 1);
  }
  else if (PTPT_isSymbolIterN(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterSymbol, 5), 1);
  }
  else if (PTPT_isSymbolIterSepN(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterSymbol, 5), 1);
  }
  else if (PTPT_isSymbolFunc(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterSymbol, 9), 1);
  }
  else if (PTPT_isSymbolVarsym(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterSymbol, 5), 1);
  }

  ATabort("Symbol has no WsAfterSymbol: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterLex(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterLex(PTPT_Symbol arg)
{
  if (PTPT_isSymbolLex(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterLex(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterLex(PTPT_Symbol arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterLex(PTPT_Symbol arg, PTPT_OptLayout wsAfterLex) */

PTPT_Symbol PTPT_setSymbolWsAfterLex(PTPT_Symbol arg, PTPT_OptLayout wsAfterLex)
{
  if (PTPT_isSymbolLex(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterLex, 1), 1);
  }

  ATabort("Symbol has no WsAfterLex: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterOpt(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterOpt(PTPT_Symbol arg)
{
  if (PTPT_isSymbolOpt(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterOpt(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterOpt(PTPT_Symbol arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterOpt(PTPT_Symbol arg, PTPT_OptLayout wsAfterOpt) */

PTPT_Symbol PTPT_setSymbolWsAfterOpt(PTPT_Symbol arg, PTPT_OptLayout wsAfterOpt)
{
  if (PTPT_isSymbolOpt(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterOpt, 1), 1);
  }

  ATabort("Symbol has no WsAfterOpt: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterAlt(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterAlt(PTPT_Symbol arg)
{
  if (PTPT_isSymbolAlt(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterAlt(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterAlt(PTPT_Symbol arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterAlt(PTPT_Symbol arg, PTPT_OptLayout wsAfterAlt) */

PTPT_Symbol PTPT_setSymbolWsAfterAlt(PTPT_Symbol arg, PTPT_OptLayout wsAfterAlt)
{
  if (PTPT_isSymbolAlt(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterAlt, 1), 1);
  }

  ATabort("Symbol has no WsAfterAlt: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolLhs(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolLhs(PTPT_Symbol arg)
{
  if (PTPT_isSymbolAlt(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolSeq(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolStrategy(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_getSymbolLhs(PTPT_Symbol arg) */

PTPT_Symbol PTPT_getSymbolLhs(PTPT_Symbol arg)
{
  if (PTPT_isSymbolAlt(arg)) {
    return (PTPT_Symbol)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
  }
  else if (PTPT_isSymbolSeq(arg)) {
    return (PTPT_Symbol)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
  }
  else 
    return (PTPT_Symbol)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolLhs(PTPT_Symbol arg, PTPT_Symbol lhs) */

PTPT_Symbol PTPT_setSymbolLhs(PTPT_Symbol arg, PTPT_Symbol lhs)
{
  if (PTPT_isSymbolAlt(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)lhs, 4), 1);
  }
  else if (PTPT_isSymbolSeq(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)lhs, 4), 1);
  }
  else if (PTPT_isSymbolStrategy(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)lhs, 4), 1);
  }

  ATabort("Symbol has no Lhs: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterLhs(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterLhs(PTPT_Symbol arg)
{
  if (PTPT_isSymbolAlt(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolSeq(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolStrategy(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterLhs(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterLhs(PTPT_Symbol arg)
{
  if (PTPT_isSymbolAlt(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
  }
  else if (PTPT_isSymbolSeq(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
  }
  else 
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterLhs(PTPT_Symbol arg, PTPT_OptLayout wsAfterLhs) */

PTPT_Symbol PTPT_setSymbolWsAfterLhs(PTPT_Symbol arg, PTPT_OptLayout wsAfterLhs)
{
  if (PTPT_isSymbolAlt(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterLhs, 5), 1);
  }
  else if (PTPT_isSymbolSeq(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterLhs, 5), 1);
  }
  else if (PTPT_isSymbolStrategy(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterLhs, 5), 1);
  }

  ATabort("Symbol has no WsAfterLhs: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterComma(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterComma(PTPT_Symbol arg)
{
  if (PTPT_isSymbolAlt(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolSeq(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolTuple(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterSep(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterStarSep(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterN(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolFunc(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolStrategy(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolParametrizedSort(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterComma(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterComma(PTPT_Symbol arg)
{
  if (PTPT_isSymbolAlt(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 7);
  }
  else if (PTPT_isSymbolSeq(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 7);
  }
  else if (PTPT_isSymbolTuple(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 7);
  }
  else if (PTPT_isSymbolIterSep(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 7);
  }
  else if (PTPT_isSymbolIterStarSep(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 7);
  }
  else if (PTPT_isSymbolIterN(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 7);
  }
  else if (PTPT_isSymbolFunc(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 7);
  }
  else if (PTPT_isSymbolStrategy(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 7);
  }
  else 
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 7);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterComma(PTPT_Symbol arg, PTPT_OptLayout wsAfterComma) */

PTPT_Symbol PTPT_setSymbolWsAfterComma(PTPT_Symbol arg, PTPT_OptLayout wsAfterComma)
{
  if (PTPT_isSymbolAlt(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterComma, 7), 1);
  }
  else if (PTPT_isSymbolSeq(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterComma, 7), 1);
  }
  else if (PTPT_isSymbolTuple(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterComma, 7), 1);
  }
  else if (PTPT_isSymbolIterSep(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterComma, 7), 1);
  }
  else if (PTPT_isSymbolIterStarSep(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterComma, 7), 1);
  }
  else if (PTPT_isSymbolIterN(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterComma, 7), 1);
  }
  else if (PTPT_isSymbolFunc(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterComma, 7), 1);
  }
  else if (PTPT_isSymbolStrategy(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterComma, 7), 1);
  }
  else if (PTPT_isSymbolParametrizedSort(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterComma, 7), 1);
  }

  ATabort("Symbol has no WsAfterComma: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolRhs(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolRhs(PTPT_Symbol arg)
{
  if (PTPT_isSymbolAlt(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolSeq(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolStrategy(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_getSymbolRhs(PTPT_Symbol arg) */

PTPT_Symbol PTPT_getSymbolRhs(PTPT_Symbol arg)
{
  if (PTPT_isSymbolAlt(arg)) {
    return (PTPT_Symbol)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 8);
  }
  else if (PTPT_isSymbolSeq(arg)) {
    return (PTPT_Symbol)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 8);
  }
  else 
    return (PTPT_Symbol)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 8);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolRhs(PTPT_Symbol arg, PTPT_Symbol rhs) */

PTPT_Symbol PTPT_setSymbolRhs(PTPT_Symbol arg, PTPT_Symbol rhs)
{
  if (PTPT_isSymbolAlt(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)rhs, 8), 1);
  }
  else if (PTPT_isSymbolSeq(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)rhs, 8), 1);
  }
  else if (PTPT_isSymbolStrategy(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)rhs, 8), 1);
  }

  ATabort("Symbol has no Rhs: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterRhs(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterRhs(PTPT_Symbol arg)
{
  if (PTPT_isSymbolAlt(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolSeq(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolStrategy(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterRhs(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterRhs(PTPT_Symbol arg)
{
  if (PTPT_isSymbolAlt(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 9);
  }
  else if (PTPT_isSymbolSeq(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 9);
  }
  else 
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 9);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterRhs(PTPT_Symbol arg, PTPT_OptLayout wsAfterRhs) */

PTPT_Symbol PTPT_setSymbolWsAfterRhs(PTPT_Symbol arg, PTPT_OptLayout wsAfterRhs)
{
  if (PTPT_isSymbolAlt(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterRhs, 9), 1);
  }
  else if (PTPT_isSymbolSeq(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterRhs, 9), 1);
  }
  else if (PTPT_isSymbolStrategy(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterRhs, 9), 1);
  }

  ATabort("Symbol has no WsAfterRhs: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterSeq(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterSeq(PTPT_Symbol arg)
{
  if (PTPT_isSymbolSeq(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterSeq(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterSeq(PTPT_Symbol arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterSeq(PTPT_Symbol arg, PTPT_OptLayout wsAfterSeq) */

PTPT_Symbol PTPT_setSymbolWsAfterSeq(PTPT_Symbol arg, PTPT_OptLayout wsAfterSeq)
{
  if (PTPT_isSymbolSeq(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterSeq, 1), 1);
  }

  ATabort("Symbol has no WsAfterSeq: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterTuple(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterTuple(PTPT_Symbol arg)
{
  if (PTPT_isSymbolTuple(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterTuple(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterTuple(PTPT_Symbol arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterTuple(PTPT_Symbol arg, PTPT_OptLayout wsAfterTuple) */

PTPT_Symbol PTPT_setSymbolWsAfterTuple(PTPT_Symbol arg, PTPT_OptLayout wsAfterTuple)
{
  if (PTPT_isSymbolTuple(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterTuple, 1), 1);
  }

  ATabort("Symbol has no WsAfterTuple: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolHead(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolHead(PTPT_Symbol arg)
{
  if (PTPT_isSymbolTuple(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_getSymbolHead(PTPT_Symbol arg) */

PTPT_Symbol PTPT_getSymbolHead(PTPT_Symbol arg)
{
  
    return (PTPT_Symbol)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolHead(PTPT_Symbol arg, PTPT_Symbol head) */

PTPT_Symbol PTPT_setSymbolHead(PTPT_Symbol arg, PTPT_Symbol head)
{
  if (PTPT_isSymbolTuple(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)head, 4), 1);
  }

  ATabort("Symbol has no Head: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterHead(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterHead(PTPT_Symbol arg)
{
  if (PTPT_isSymbolTuple(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterHead(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterHead(PTPT_Symbol arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterHead(PTPT_Symbol arg, PTPT_OptLayout wsAfterHead) */

PTPT_Symbol PTPT_setSymbolWsAfterHead(PTPT_Symbol arg, PTPT_OptLayout wsAfterHead)
{
  if (PTPT_isSymbolTuple(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterHead, 5), 1);
  }

  ATabort("Symbol has no WsAfterHead: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolRest(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolRest(PTPT_Symbol arg)
{
  if (PTPT_isSymbolTuple(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Symbols PTPT_getSymbolRest(PTPT_Symbol arg) */

PTPT_Symbols PTPT_getSymbolRest(PTPT_Symbol arg)
{
  
    return (PTPT_Symbols)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 8);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolRest(PTPT_Symbol arg, PTPT_Symbols rest) */

PTPT_Symbol PTPT_setSymbolRest(PTPT_Symbol arg, PTPT_Symbols rest)
{
  if (PTPT_isSymbolTuple(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)rest, 8), 1);
  }

  ATabort("Symbol has no Rest: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterRest(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterRest(PTPT_Symbol arg)
{
  if (PTPT_isSymbolTuple(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterRest(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterRest(PTPT_Symbol arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 9);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterRest(PTPT_Symbol arg, PTPT_OptLayout wsAfterRest) */

PTPT_Symbol PTPT_setSymbolWsAfterRest(PTPT_Symbol arg, PTPT_OptLayout wsAfterRest)
{
  if (PTPT_isSymbolTuple(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterRest, 9), 1);
  }

  ATabort("Symbol has no WsAfterRest: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterSort(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterSort(PTPT_Symbol arg)
{
  if (PTPT_isSymbolSort(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolParametrizedSort(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterSort(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterSort(PTPT_Symbol arg)
{
  if (PTPT_isSymbolSort(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
  }
  else 
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterSort(PTPT_Symbol arg, PTPT_OptLayout wsAfterSort) */

PTPT_Symbol PTPT_setSymbolWsAfterSort(PTPT_Symbol arg, PTPT_OptLayout wsAfterSort)
{
  if (PTPT_isSymbolSort(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterSort, 1), 1);
  }
  else if (PTPT_isSymbolParametrizedSort(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterSort, 5), 1);
  }

  ATabort("Symbol has no WsAfterSort: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterIter(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterIter(PTPT_Symbol arg)
{
  if (PTPT_isSymbolIter(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterIter(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterIter(PTPT_Symbol arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterIter(PTPT_Symbol arg, PTPT_OptLayout wsAfterIter) */

PTPT_Symbol PTPT_setSymbolWsAfterIter(PTPT_Symbol arg, PTPT_OptLayout wsAfterIter)
{
  if (PTPT_isSymbolIter(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterIter, 1), 1);
  }

  ATabort("Symbol has no WsAfterIter: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterIterStar(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterIterStar(PTPT_Symbol arg)
{
  if (PTPT_isSymbolIterStar(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterIterStar(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterIterStar(PTPT_Symbol arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterIterStar(PTPT_Symbol arg, PTPT_OptLayout wsAfterIterStar) */

PTPT_Symbol PTPT_setSymbolWsAfterIterStar(PTPT_Symbol arg, PTPT_OptLayout wsAfterIterStar)
{
  if (PTPT_isSymbolIterStar(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterIterStar, 1), 1);
  }

  ATabort("Symbol has no WsAfterIterStar: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterIterSep(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterIterSep(PTPT_Symbol arg)
{
  if (PTPT_isSymbolIterSep(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterIterSep(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterIterSep(PTPT_Symbol arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterIterSep(PTPT_Symbol arg, PTPT_OptLayout wsAfterIterSep) */

PTPT_Symbol PTPT_setSymbolWsAfterIterSep(PTPT_Symbol arg, PTPT_OptLayout wsAfterIterSep)
{
  if (PTPT_isSymbolIterSep(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterIterSep, 1), 1);
  }

  ATabort("Symbol has no WsAfterIterSep: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolSeparator(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolSeparator(PTPT_Symbol arg)
{
  if (PTPT_isSymbolIterSep(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterStarSep(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterSepN(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_getSymbolSeparator(PTPT_Symbol arg) */

PTPT_Symbol PTPT_getSymbolSeparator(PTPT_Symbol arg)
{
  if (PTPT_isSymbolIterSep(arg)) {
    return (PTPT_Symbol)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 8);
  }
  else if (PTPT_isSymbolIterStarSep(arg)) {
    return (PTPT_Symbol)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 8);
  }
  else 
    return (PTPT_Symbol)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 8);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolSeparator(PTPT_Symbol arg, PTPT_Symbol separator) */

PTPT_Symbol PTPT_setSymbolSeparator(PTPT_Symbol arg, PTPT_Symbol separator)
{
  if (PTPT_isSymbolIterSep(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)separator, 8), 1);
  }
  else if (PTPT_isSymbolIterStarSep(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)separator, 8), 1);
  }
  else if (PTPT_isSymbolIterSepN(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)separator, 8), 1);
  }

  ATabort("Symbol has no Separator: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterSeparator(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterSeparator(PTPT_Symbol arg)
{
  if (PTPT_isSymbolIterSep(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterStarSep(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterSepN(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterSeparator(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterSeparator(PTPT_Symbol arg)
{
  if (PTPT_isSymbolIterSep(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 9);
  }
  else if (PTPT_isSymbolIterStarSep(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 9);
  }
  else 
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 9);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterSeparator(PTPT_Symbol arg, PTPT_OptLayout wsAfterSeparator) */

PTPT_Symbol PTPT_setSymbolWsAfterSeparator(PTPT_Symbol arg, PTPT_OptLayout wsAfterSeparator)
{
  if (PTPT_isSymbolIterSep(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterSeparator, 9), 1);
  }
  else if (PTPT_isSymbolIterStarSep(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterSeparator, 9), 1);
  }
  else if (PTPT_isSymbolIterSepN(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterSeparator, 9), 1);
  }

  ATabort("Symbol has no WsAfterSeparator: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterIterStarSep(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterIterStarSep(PTPT_Symbol arg)
{
  if (PTPT_isSymbolIterStarSep(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterIterStarSep(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterIterStarSep(PTPT_Symbol arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterIterStarSep(PTPT_Symbol arg, PTPT_OptLayout wsAfterIterStarSep) */

PTPT_Symbol PTPT_setSymbolWsAfterIterStarSep(PTPT_Symbol arg, PTPT_OptLayout wsAfterIterStarSep)
{
  if (PTPT_isSymbolIterStarSep(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterIterStarSep, 1), 1);
  }

  ATabort("Symbol has no WsAfterIterStarSep: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterIterN(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterIterN(PTPT_Symbol arg)
{
  if (PTPT_isSymbolIterN(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterIterN(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterIterN(PTPT_Symbol arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterIterN(PTPT_Symbol arg, PTPT_OptLayout wsAfterIterN) */

PTPT_Symbol PTPT_setSymbolWsAfterIterN(PTPT_Symbol arg, PTPT_OptLayout wsAfterIterN)
{
  if (PTPT_isSymbolIterN(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterIterN, 1), 1);
  }

  ATabort("Symbol has no WsAfterIterN: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolNumber(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolNumber(PTPT_Symbol arg)
{
  if (PTPT_isSymbolIterN(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterSepN(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_NatCon PTPT_getSymbolNumber(PTPT_Symbol arg) */

PTPT_NatCon PTPT_getSymbolNumber(PTPT_Symbol arg)
{
  if (PTPT_isSymbolIterN(arg)) {
    return (PTPT_NatCon)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 8);
  }
  else 
    return (PTPT_NatCon)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 12);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolNumber(PTPT_Symbol arg, PTPT_NatCon number) */

PTPT_Symbol PTPT_setSymbolNumber(PTPT_Symbol arg, PTPT_NatCon number)
{
  if (PTPT_isSymbolIterN(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)number, 8), 1);
  }
  else if (PTPT_isSymbolIterSepN(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)number, 12), 1);
  }

  ATabort("Symbol has no Number: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterNumber(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterNumber(PTPT_Symbol arg)
{
  if (PTPT_isSymbolIterN(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolIterSepN(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterNumber(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterNumber(PTPT_Symbol arg)
{
  if (PTPT_isSymbolIterN(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 9);
  }
  else 
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 13);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterNumber(PTPT_Symbol arg, PTPT_OptLayout wsAfterNumber) */

PTPT_Symbol PTPT_setSymbolWsAfterNumber(PTPT_Symbol arg, PTPT_OptLayout wsAfterNumber)
{
  if (PTPT_isSymbolIterN(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterNumber, 9), 1);
  }
  else if (PTPT_isSymbolIterSepN(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterNumber, 13), 1);
  }

  ATabort("Symbol has no WsAfterNumber: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterIterSepN(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterIterSepN(PTPT_Symbol arg)
{
  if (PTPT_isSymbolIterSepN(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterIterSepN(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterIterSepN(PTPT_Symbol arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterIterSepN(PTPT_Symbol arg, PTPT_OptLayout wsAfterIterSepN) */

PTPT_Symbol PTPT_setSymbolWsAfterIterSepN(PTPT_Symbol arg, PTPT_OptLayout wsAfterIterSepN)
{
  if (PTPT_isSymbolIterSepN(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterIterSepN, 1), 1);
  }

  ATabort("Symbol has no WsAfterIterSepN: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterComma1(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterComma1(PTPT_Symbol arg)
{
  if (PTPT_isSymbolIterSepN(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterComma1(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterComma1(PTPT_Symbol arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 7);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterComma1(PTPT_Symbol arg, PTPT_OptLayout wsAfterComma1) */

PTPT_Symbol PTPT_setSymbolWsAfterComma1(PTPT_Symbol arg, PTPT_OptLayout wsAfterComma1)
{
  if (PTPT_isSymbolIterSepN(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterComma1, 7), 1);
  }

  ATabort("Symbol has no WsAfterComma1: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterComma2(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterComma2(PTPT_Symbol arg)
{
  if (PTPT_isSymbolIterSepN(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterComma2(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterComma2(PTPT_Symbol arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 11);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterComma2(PTPT_Symbol arg, PTPT_OptLayout wsAfterComma2) */

PTPT_Symbol PTPT_setSymbolWsAfterComma2(PTPT_Symbol arg, PTPT_OptLayout wsAfterComma2)
{
  if (PTPT_isSymbolIterSepN(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterComma2, 11), 1);
  }

  ATabort("Symbol has no WsAfterComma2: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterFunc(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterFunc(PTPT_Symbol arg)
{
  if (PTPT_isSymbolFunc(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterFunc(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterFunc(PTPT_Symbol arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterFunc(PTPT_Symbol arg, PTPT_OptLayout wsAfterFunc) */

PTPT_Symbol PTPT_setSymbolWsAfterFunc(PTPT_Symbol arg, PTPT_OptLayout wsAfterFunc)
{
  if (PTPT_isSymbolFunc(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterFunc, 1), 1);
  }

  ATabort("Symbol has no WsAfterFunc: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolSymbols(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolSymbols(PTPT_Symbol arg)
{
  if (PTPT_isSymbolFunc(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Symbols PTPT_getSymbolSymbols(PTPT_Symbol arg) */

PTPT_Symbols PTPT_getSymbolSymbols(PTPT_Symbol arg)
{
  
    return (PTPT_Symbols)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolSymbols(PTPT_Symbol arg, PTPT_Symbols symbols) */

PTPT_Symbol PTPT_setSymbolSymbols(PTPT_Symbol arg, PTPT_Symbols symbols)
{
  if (PTPT_isSymbolFunc(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)symbols, 4), 1);
  }

  ATabort("Symbol has no Symbols: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterSymbols(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterSymbols(PTPT_Symbol arg)
{
  if (PTPT_isSymbolFunc(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterSymbols(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterSymbols(PTPT_Symbol arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterSymbols(PTPT_Symbol arg, PTPT_OptLayout wsAfterSymbols) */

PTPT_Symbol PTPT_setSymbolWsAfterSymbols(PTPT_Symbol arg, PTPT_OptLayout wsAfterSymbols)
{
  if (PTPT_isSymbolFunc(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterSymbols, 5), 1);
  }

  ATabort("Symbol has no WsAfterSymbols: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterVarsym(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterVarsym(PTPT_Symbol arg)
{
  if (PTPT_isSymbolVarsym(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterVarsym(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterVarsym(PTPT_Symbol arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterVarsym(PTPT_Symbol arg, PTPT_OptLayout wsAfterVarsym) */

PTPT_Symbol PTPT_setSymbolWsAfterVarsym(PTPT_Symbol arg, PTPT_OptLayout wsAfterVarsym)
{
  if (PTPT_isSymbolVarsym(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterVarsym, 1), 1);
  }

  ATabort("Symbol has no WsAfterVarsym: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterCharClass(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterCharClass(PTPT_Symbol arg)
{
  if (PTPT_isSymbolCharClass(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterCharClass(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterCharClass(PTPT_Symbol arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterCharClass(PTPT_Symbol arg, PTPT_OptLayout wsAfterCharClass) */

PTPT_Symbol PTPT_setSymbolWsAfterCharClass(PTPT_Symbol arg, PTPT_OptLayout wsAfterCharClass)
{
  if (PTPT_isSymbolCharClass(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterCharClass, 1), 1);
  }

  ATabort("Symbol has no WsAfterCharClass: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolCharRanges(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolCharRanges(PTPT_Symbol arg)
{
  if (PTPT_isSymbolCharClass(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_CharRanges PTPT_getSymbolCharRanges(PTPT_Symbol arg) */

PTPT_CharRanges PTPT_getSymbolCharRanges(PTPT_Symbol arg)
{
  
    return (PTPT_CharRanges)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolCharRanges(PTPT_Symbol arg, PTPT_CharRanges CharRanges) */

PTPT_Symbol PTPT_setSymbolCharRanges(PTPT_Symbol arg, PTPT_CharRanges CharRanges)
{
  if (PTPT_isSymbolCharClass(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)CharRanges, 4), 1);
  }

  ATabort("Symbol has no CharRanges: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterCharRanges(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterCharRanges(PTPT_Symbol arg)
{
  if (PTPT_isSymbolCharClass(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterCharRanges(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterCharRanges(PTPT_Symbol arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterCharRanges(PTPT_Symbol arg, PTPT_OptLayout wsAfterCharRanges) */

PTPT_Symbol PTPT_setSymbolWsAfterCharRanges(PTPT_Symbol arg, PTPT_OptLayout wsAfterCharRanges)
{
  if (PTPT_isSymbolCharClass(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterCharRanges, 5), 1);
  }

  ATabort("Symbol has no WsAfterCharRanges: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterStrategy(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterStrategy(PTPT_Symbol arg)
{
  if (PTPT_isSymbolStrategy(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterStrategy(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterStrategy(PTPT_Symbol arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterStrategy(PTPT_Symbol arg, PTPT_OptLayout wsAfterStrategy) */

PTPT_Symbol PTPT_setSymbolWsAfterStrategy(PTPT_Symbol arg, PTPT_OptLayout wsAfterStrategy)
{
  if (PTPT_isSymbolStrategy(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterStrategy, 1), 1);
  }

  ATabort("Symbol has no WsAfterStrategy: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterParametrizedSort(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterParametrizedSort(PTPT_Symbol arg)
{
  if (PTPT_isSymbolParametrizedSort(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterParametrizedSort(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterParametrizedSort(PTPT_Symbol arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterParametrizedSort(PTPT_Symbol arg, PTPT_OptLayout wsAfterParametrizedSort) */

PTPT_Symbol PTPT_setSymbolWsAfterParametrizedSort(PTPT_Symbol arg, PTPT_OptLayout wsAfterParametrizedSort)
{
  if (PTPT_isSymbolParametrizedSort(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParametrizedSort, 1), 1);
  }

  ATabort("Symbol has no WsAfterParametrizedSort: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolSort(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolSort(PTPT_Symbol arg)
{
  if (PTPT_isSymbolParametrizedSort(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_QLiteral PTPT_getSymbolSort(PTPT_Symbol arg) */

PTPT_QLiteral PTPT_getSymbolSort(PTPT_Symbol arg)
{
  
    return (PTPT_QLiteral)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolSort(PTPT_Symbol arg, PTPT_QLiteral sort) */

PTPT_Symbol PTPT_setSymbolSort(PTPT_Symbol arg, PTPT_QLiteral sort)
{
  if (PTPT_isSymbolParametrizedSort(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)sort, 4), 1);
  }

  ATabort("Symbol has no Sort: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolParameters(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolParameters(PTPT_Symbol arg)
{
  if (PTPT_isSymbolParametrizedSort(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Symbols PTPT_getSymbolParameters(PTPT_Symbol arg) */

PTPT_Symbols PTPT_getSymbolParameters(PTPT_Symbol arg)
{
  
    return (PTPT_Symbols)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 8);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolParameters(PTPT_Symbol arg, PTPT_Symbols parameters) */

PTPT_Symbol PTPT_setSymbolParameters(PTPT_Symbol arg, PTPT_Symbols parameters)
{
  if (PTPT_isSymbolParametrizedSort(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)parameters, 8), 1);
  }

  ATabort("Symbol has no Parameters: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolWsAfterParameters(PTPT_Symbol arg) */

ATbool PTPT_hasSymbolWsAfterParameters(PTPT_Symbol arg)
{
  if (PTPT_isSymbolParametrizedSort(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolWsAfterParameters(PTPT_Symbol arg) */

PTPT_OptLayout PTPT_getSymbolWsAfterParameters(PTPT_Symbol arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 9);
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_setSymbolWsAfterParameters(PTPT_Symbol arg, PTPT_OptLayout wsAfterParameters) */

PTPT_Symbol PTPT_setSymbolWsAfterParameters(PTPT_Symbol arg, PTPT_OptLayout wsAfterParameters)
{
  if (PTPT_isSymbolParametrizedSort(arg)) {
    return (PTPT_Symbol)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParameters, 9), 1);
  }

  ATabort("Symbol has no WsAfterParameters: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_Symbols accessors */

/*{{{  ATbool PTPT_isValidSymbols(PTPT_Symbols arg) */

ATbool PTPT_isValidSymbols(PTPT_Symbols arg)
{
  if (PTPT_isSymbolsList(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isSymbolsList(PTPT_Symbols arg) */

inline ATbool PTPT_isSymbolsList(PTPT_Symbols arg)
{
#ifndef DISABLE_DYNAMIC_CHECKING
  assert(arg != NULL);
  assert(ATmatchTerm((ATerm)arg, PTPT_patternSymbolsList, NULL, NULL, NULL));
#endif
  return ATtrue;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolsWsAfterBracketOpen(PTPT_Symbols arg) */

ATbool PTPT_hasSymbolsWsAfterBracketOpen(PTPT_Symbols arg)
{
  if (PTPT_isSymbolsList(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolsWsAfterBracketOpen(PTPT_Symbols arg) */

PTPT_OptLayout PTPT_getSymbolsWsAfterBracketOpen(PTPT_Symbols arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Symbols PTPT_setSymbolsWsAfterBracketOpen(PTPT_Symbols arg, PTPT_OptLayout wsAfterBracketOpen) */

PTPT_Symbols PTPT_setSymbolsWsAfterBracketOpen(PTPT_Symbols arg, PTPT_OptLayout wsAfterBracketOpen)
{
  if (PTPT_isSymbolsList(arg)) {
    return (PTPT_Symbols)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterBracketOpen, 1), 1);
  }

  ATabort("Symbols has no WsAfterBracketOpen: %t\n", arg);
  return (PTPT_Symbols)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolsList(PTPT_Symbols arg) */

ATbool PTPT_hasSymbolsList(PTPT_Symbols arg)
{
  if (PTPT_isSymbolsList(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_SymbolList PTPT_getSymbolsList(PTPT_Symbols arg) */

PTPT_SymbolList PTPT_getSymbolsList(PTPT_Symbols arg)
{
  
    return (PTPT_SymbolList)ATgetArgument((ATermAppl)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 2), 1);
}

/*}}}  */
/*{{{  PTPT_Symbols PTPT_setSymbolsList(PTPT_Symbols arg, PTPT_SymbolList list) */

PTPT_Symbols PTPT_setSymbolsList(PTPT_Symbols arg, PTPT_SymbolList list)
{
  if (PTPT_isSymbolsList(arg)) {
    return (PTPT_Symbols)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)ATsetArgument((ATermAppl)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 2), (ATerm)list, 1), 2), 1);
  }

  ATabort("Symbols has no List: %t\n", arg);
  return (PTPT_Symbols)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolsWsAfterList(PTPT_Symbols arg) */

ATbool PTPT_hasSymbolsWsAfterList(PTPT_Symbols arg)
{
  if (PTPT_isSymbolsList(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolsWsAfterList(PTPT_Symbols arg) */

PTPT_OptLayout PTPT_getSymbolsWsAfterList(PTPT_Symbols arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
}

/*}}}  */
/*{{{  PTPT_Symbols PTPT_setSymbolsWsAfterList(PTPT_Symbols arg, PTPT_OptLayout wsAfterList) */

PTPT_Symbols PTPT_setSymbolsWsAfterList(PTPT_Symbols arg, PTPT_OptLayout wsAfterList)
{
  if (PTPT_isSymbolsList(arg)) {
    return (PTPT_Symbols)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterList, 3), 1);
  }

  ATabort("Symbols has no WsAfterList: %t\n", arg);
  return (PTPT_Symbols)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_SymbolList accessors */

/*{{{  ATbool PTPT_isValidSymbolList(PTPT_SymbolList arg) */

ATbool PTPT_isValidSymbolList(PTPT_SymbolList arg)
{
  if (PTPT_isSymbolListEmpty(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolListSingle(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolListMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isSymbolListEmpty(PTPT_SymbolList arg) */

inline ATbool PTPT_isSymbolListEmpty(PTPT_SymbolList arg)
{
  if (!ATisEmpty((ATermList)arg)) {
    return ATfalse;
  }
#ifndef DISABLE_DYNAMIC_CHECKING
  assert(arg != NULL);
  assert(ATmatchTerm((ATerm)arg, PTPT_patternSymbolListEmpty));
#endif
  return ATtrue;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isSymbolListSingle(PTPT_SymbolList arg) */

inline ATbool PTPT_isSymbolListSingle(PTPT_SymbolList arg)
{
  if (ATisEmpty((ATermList)arg)) {
    return ATfalse;
  }
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternSymbolListSingle, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isSymbolListMany(PTPT_SymbolList arg) */

inline ATbool PTPT_isSymbolListMany(PTPT_SymbolList arg)
{
  if (ATisEmpty((ATermList)arg)) {
    return ATfalse;
  }
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternSymbolListMany, NULL, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolListHead(PTPT_SymbolList arg) */

ATbool PTPT_hasSymbolListHead(PTPT_SymbolList arg)
{
  if (PTPT_isSymbolListSingle(arg)) {
    return ATtrue;
  }
  else if (PTPT_isSymbolListMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_getSymbolListHead(PTPT_SymbolList arg) */

PTPT_Symbol PTPT_getSymbolListHead(PTPT_SymbolList arg)
{
  if (PTPT_isSymbolListSingle(arg)) {
    return (PTPT_Symbol)ATgetFirst((ATermList)arg);
  }
  else 
    return (PTPT_Symbol)ATgetFirst((ATermList)arg);
}

/*}}}  */
/*{{{  PTPT_SymbolList PTPT_setSymbolListHead(PTPT_SymbolList arg, PTPT_Symbol head) */

PTPT_SymbolList PTPT_setSymbolListHead(PTPT_SymbolList arg, PTPT_Symbol head)
{
  if (PTPT_isSymbolListSingle(arg)) {
    return (PTPT_SymbolList)ATreplace((ATermList)arg, (ATerm)head, 0);
  }
  else if (PTPT_isSymbolListMany(arg)) {
    return (PTPT_SymbolList)ATreplace((ATermList)arg, (ATerm)head, 0);
  }

  ATabort("SymbolList has no Head: %t\n", arg);
  return (PTPT_SymbolList)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolListWsAfterFirst(PTPT_SymbolList arg) */

ATbool PTPT_hasSymbolListWsAfterFirst(PTPT_SymbolList arg)
{
  if (PTPT_isSymbolListMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolListWsAfterFirst(PTPT_SymbolList arg) */

PTPT_OptLayout PTPT_getSymbolListWsAfterFirst(PTPT_SymbolList arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)arg, 1);
}

/*}}}  */
/*{{{  PTPT_SymbolList PTPT_setSymbolListWsAfterFirst(PTPT_SymbolList arg, PTPT_OptLayout wsAfterFirst) */

PTPT_SymbolList PTPT_setSymbolListWsAfterFirst(PTPT_SymbolList arg, PTPT_OptLayout wsAfterFirst)
{
  if (PTPT_isSymbolListMany(arg)) {
    return (PTPT_SymbolList)ATreplace((ATermList)arg, (ATerm)wsAfterFirst, 1);
  }

  ATabort("SymbolList has no WsAfterFirst: %t\n", arg);
  return (PTPT_SymbolList)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolListSep(PTPT_SymbolList arg) */

ATbool PTPT_hasSymbolListSep(PTPT_SymbolList arg)
{
  if (PTPT_isSymbolListMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  char * PTPT_getSymbolListSep(PTPT_SymbolList arg) */

char * PTPT_getSymbolListSep(PTPT_SymbolList arg)
{
  
    return (char *)ATgetName(ATgetAFun((ATermAppl)ATgetArgument((ATermAppl)ATelementAt((ATermList)arg, 2), 0)));
}

/*}}}  */
/*{{{  PTPT_SymbolList PTPT_setSymbolListSep(PTPT_SymbolList arg, char * sep) */

PTPT_SymbolList PTPT_setSymbolListSep(PTPT_SymbolList arg, char * sep)
{
  if (PTPT_isSymbolListMany(arg)) {
    return (PTPT_SymbolList)ATreplace((ATermList)arg, (ATerm)ATsetArgument((ATermAppl)ATelementAt((ATermList)arg, 2), (ATerm)ATmakeAppl0(ATmakeAFun(sep, 0, ATtrue)), 0), 2);
  }

  ATabort("SymbolList has no Sep: %t\n", arg);
  return (PTPT_SymbolList)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolListWsAfterSep(PTPT_SymbolList arg) */

ATbool PTPT_hasSymbolListWsAfterSep(PTPT_SymbolList arg)
{
  if (PTPT_isSymbolListMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getSymbolListWsAfterSep(PTPT_SymbolList arg) */

PTPT_OptLayout PTPT_getSymbolListWsAfterSep(PTPT_SymbolList arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)arg, 3);
}

/*}}}  */
/*{{{  PTPT_SymbolList PTPT_setSymbolListWsAfterSep(PTPT_SymbolList arg, PTPT_OptLayout wsAfterSep) */

PTPT_SymbolList PTPT_setSymbolListWsAfterSep(PTPT_SymbolList arg, PTPT_OptLayout wsAfterSep)
{
  if (PTPT_isSymbolListMany(arg)) {
    return (PTPT_SymbolList)ATreplace((ATermList)arg, (ATerm)wsAfterSep, 3);
  }

  ATabort("SymbolList has no WsAfterSep: %t\n", arg);
  return (PTPT_SymbolList)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasSymbolListTail(PTPT_SymbolList arg) */

ATbool PTPT_hasSymbolListTail(PTPT_SymbolList arg)
{
  if (PTPT_isSymbolListMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_SymbolList PTPT_getSymbolListTail(PTPT_SymbolList arg) */

PTPT_SymbolList PTPT_getSymbolListTail(PTPT_SymbolList arg)
{
  
    return (PTPT_SymbolList)ATgetTail((ATermList)arg, 4);
}

/*}}}  */
/*{{{  PTPT_SymbolList PTPT_setSymbolListTail(PTPT_SymbolList arg, PTPT_SymbolList tail) */

PTPT_SymbolList PTPT_setSymbolListTail(PTPT_SymbolList arg, PTPT_SymbolList tail)
{
  if (PTPT_isSymbolListMany(arg)) {
    return (PTPT_SymbolList)ATreplaceTail((ATermList)arg, (ATermList)tail, 4);
  }

  ATabort("SymbolList has no Tail: %t\n", arg);
  return (PTPT_SymbolList)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_CharRanges accessors */

/*{{{  ATbool PTPT_isValidCharRanges(PTPT_CharRanges arg) */

ATbool PTPT_isValidCharRanges(PTPT_CharRanges arg)
{
  if (PTPT_isCharRangesList(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isCharRangesList(PTPT_CharRanges arg) */

inline ATbool PTPT_isCharRangesList(PTPT_CharRanges arg)
{
#ifndef DISABLE_DYNAMIC_CHECKING
  assert(arg != NULL);
  assert(ATmatchTerm((ATerm)arg, PTPT_patternCharRangesList, NULL, NULL, NULL));
#endif
  return ATtrue;
}

/*}}}  */
/*{{{  ATbool PTPT_hasCharRangesWsAfterBracketOpen(PTPT_CharRanges arg) */

ATbool PTPT_hasCharRangesWsAfterBracketOpen(PTPT_CharRanges arg)
{
  if (PTPT_isCharRangesList(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getCharRangesWsAfterBracketOpen(PTPT_CharRanges arg) */

PTPT_OptLayout PTPT_getCharRangesWsAfterBracketOpen(PTPT_CharRanges arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_CharRanges PTPT_setCharRangesWsAfterBracketOpen(PTPT_CharRanges arg, PTPT_OptLayout wsAfterBracketOpen) */

PTPT_CharRanges PTPT_setCharRangesWsAfterBracketOpen(PTPT_CharRanges arg, PTPT_OptLayout wsAfterBracketOpen)
{
  if (PTPT_isCharRangesList(arg)) {
    return (PTPT_CharRanges)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterBracketOpen, 1), 1);
  }

  ATabort("CharRanges has no WsAfterBracketOpen: %t\n", arg);
  return (PTPT_CharRanges)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasCharRangesList(PTPT_CharRanges arg) */

ATbool PTPT_hasCharRangesList(PTPT_CharRanges arg)
{
  if (PTPT_isCharRangesList(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_CharRangeList PTPT_getCharRangesList(PTPT_CharRanges arg) */

PTPT_CharRangeList PTPT_getCharRangesList(PTPT_CharRanges arg)
{
  
    return (PTPT_CharRangeList)ATgetArgument((ATermAppl)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 2), 1);
}

/*}}}  */
/*{{{  PTPT_CharRanges PTPT_setCharRangesList(PTPT_CharRanges arg, PTPT_CharRangeList list) */

PTPT_CharRanges PTPT_setCharRangesList(PTPT_CharRanges arg, PTPT_CharRangeList list)
{
  if (PTPT_isCharRangesList(arg)) {
    return (PTPT_CharRanges)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)ATsetArgument((ATermAppl)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 2), (ATerm)list, 1), 2), 1);
  }

  ATabort("CharRanges has no List: %t\n", arg);
  return (PTPT_CharRanges)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasCharRangesWsAfterList(PTPT_CharRanges arg) */

ATbool PTPT_hasCharRangesWsAfterList(PTPT_CharRanges arg)
{
  if (PTPT_isCharRangesList(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getCharRangesWsAfterList(PTPT_CharRanges arg) */

PTPT_OptLayout PTPT_getCharRangesWsAfterList(PTPT_CharRanges arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
}

/*}}}  */
/*{{{  PTPT_CharRanges PTPT_setCharRangesWsAfterList(PTPT_CharRanges arg, PTPT_OptLayout wsAfterList) */

PTPT_CharRanges PTPT_setCharRangesWsAfterList(PTPT_CharRanges arg, PTPT_OptLayout wsAfterList)
{
  if (PTPT_isCharRangesList(arg)) {
    return (PTPT_CharRanges)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterList, 3), 1);
  }

  ATabort("CharRanges has no WsAfterList: %t\n", arg);
  return (PTPT_CharRanges)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_CharRangeList accessors */

/*{{{  ATbool PTPT_isValidCharRangeList(PTPT_CharRangeList arg) */

ATbool PTPT_isValidCharRangeList(PTPT_CharRangeList arg)
{
  if (PTPT_isCharRangeListEmpty(arg)) {
    return ATtrue;
  }
  else if (PTPT_isCharRangeListSingle(arg)) {
    return ATtrue;
  }
  else if (PTPT_isCharRangeListMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isCharRangeListEmpty(PTPT_CharRangeList arg) */

inline ATbool PTPT_isCharRangeListEmpty(PTPT_CharRangeList arg)
{
  if (!ATisEmpty((ATermList)arg)) {
    return ATfalse;
  }
#ifndef DISABLE_DYNAMIC_CHECKING
  assert(arg != NULL);
  assert(ATmatchTerm((ATerm)arg, PTPT_patternCharRangeListEmpty));
#endif
  return ATtrue;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isCharRangeListSingle(PTPT_CharRangeList arg) */

inline ATbool PTPT_isCharRangeListSingle(PTPT_CharRangeList arg)
{
  if (ATisEmpty((ATermList)arg)) {
    return ATfalse;
  }
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternCharRangeListSingle, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isCharRangeListMany(PTPT_CharRangeList arg) */

inline ATbool PTPT_isCharRangeListMany(PTPT_CharRangeList arg)
{
  if (ATisEmpty((ATermList)arg)) {
    return ATfalse;
  }
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternCharRangeListMany, NULL, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  ATbool PTPT_hasCharRangeListHead(PTPT_CharRangeList arg) */

ATbool PTPT_hasCharRangeListHead(PTPT_CharRangeList arg)
{
  if (PTPT_isCharRangeListSingle(arg)) {
    return ATtrue;
  }
  else if (PTPT_isCharRangeListMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_CharRange PTPT_getCharRangeListHead(PTPT_CharRangeList arg) */

PTPT_CharRange PTPT_getCharRangeListHead(PTPT_CharRangeList arg)
{
  if (PTPT_isCharRangeListSingle(arg)) {
    return (PTPT_CharRange)ATgetFirst((ATermList)arg);
  }
  else 
    return (PTPT_CharRange)ATgetFirst((ATermList)arg);
}

/*}}}  */
/*{{{  PTPT_CharRangeList PTPT_setCharRangeListHead(PTPT_CharRangeList arg, PTPT_CharRange head) */

PTPT_CharRangeList PTPT_setCharRangeListHead(PTPT_CharRangeList arg, PTPT_CharRange head)
{
  if (PTPT_isCharRangeListSingle(arg)) {
    return (PTPT_CharRangeList)ATreplace((ATermList)arg, (ATerm)head, 0);
  }
  else if (PTPT_isCharRangeListMany(arg)) {
    return (PTPT_CharRangeList)ATreplace((ATermList)arg, (ATerm)head, 0);
  }

  ATabort("CharRangeList has no Head: %t\n", arg);
  return (PTPT_CharRangeList)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasCharRangeListWsAfterFirst(PTPT_CharRangeList arg) */

ATbool PTPT_hasCharRangeListWsAfterFirst(PTPT_CharRangeList arg)
{
  if (PTPT_isCharRangeListMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getCharRangeListWsAfterFirst(PTPT_CharRangeList arg) */

PTPT_OptLayout PTPT_getCharRangeListWsAfterFirst(PTPT_CharRangeList arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)arg, 1);
}

/*}}}  */
/*{{{  PTPT_CharRangeList PTPT_setCharRangeListWsAfterFirst(PTPT_CharRangeList arg, PTPT_OptLayout wsAfterFirst) */

PTPT_CharRangeList PTPT_setCharRangeListWsAfterFirst(PTPT_CharRangeList arg, PTPT_OptLayout wsAfterFirst)
{
  if (PTPT_isCharRangeListMany(arg)) {
    return (PTPT_CharRangeList)ATreplace((ATermList)arg, (ATerm)wsAfterFirst, 1);
  }

  ATabort("CharRangeList has no WsAfterFirst: %t\n", arg);
  return (PTPT_CharRangeList)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasCharRangeListSep(PTPT_CharRangeList arg) */

ATbool PTPT_hasCharRangeListSep(PTPT_CharRangeList arg)
{
  if (PTPT_isCharRangeListMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  char * PTPT_getCharRangeListSep(PTPT_CharRangeList arg) */

char * PTPT_getCharRangeListSep(PTPT_CharRangeList arg)
{
  
    return (char *)ATgetName(ATgetAFun((ATermAppl)ATgetArgument((ATermAppl)ATelementAt((ATermList)arg, 2), 0)));
}

/*}}}  */
/*{{{  PTPT_CharRangeList PTPT_setCharRangeListSep(PTPT_CharRangeList arg, char * sep) */

PTPT_CharRangeList PTPT_setCharRangeListSep(PTPT_CharRangeList arg, char * sep)
{
  if (PTPT_isCharRangeListMany(arg)) {
    return (PTPT_CharRangeList)ATreplace((ATermList)arg, (ATerm)ATsetArgument((ATermAppl)ATelementAt((ATermList)arg, 2), (ATerm)ATmakeAppl0(ATmakeAFun(sep, 0, ATtrue)), 0), 2);
  }

  ATabort("CharRangeList has no Sep: %t\n", arg);
  return (PTPT_CharRangeList)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasCharRangeListWsAfterSep(PTPT_CharRangeList arg) */

ATbool PTPT_hasCharRangeListWsAfterSep(PTPT_CharRangeList arg)
{
  if (PTPT_isCharRangeListMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getCharRangeListWsAfterSep(PTPT_CharRangeList arg) */

PTPT_OptLayout PTPT_getCharRangeListWsAfterSep(PTPT_CharRangeList arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)arg, 3);
}

/*}}}  */
/*{{{  PTPT_CharRangeList PTPT_setCharRangeListWsAfterSep(PTPT_CharRangeList arg, PTPT_OptLayout wsAfterSep) */

PTPT_CharRangeList PTPT_setCharRangeListWsAfterSep(PTPT_CharRangeList arg, PTPT_OptLayout wsAfterSep)
{
  if (PTPT_isCharRangeListMany(arg)) {
    return (PTPT_CharRangeList)ATreplace((ATermList)arg, (ATerm)wsAfterSep, 3);
  }

  ATabort("CharRangeList has no WsAfterSep: %t\n", arg);
  return (PTPT_CharRangeList)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasCharRangeListTail(PTPT_CharRangeList arg) */

ATbool PTPT_hasCharRangeListTail(PTPT_CharRangeList arg)
{
  if (PTPT_isCharRangeListMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_CharRangeList PTPT_getCharRangeListTail(PTPT_CharRangeList arg) */

PTPT_CharRangeList PTPT_getCharRangeListTail(PTPT_CharRangeList arg)
{
  
    return (PTPT_CharRangeList)ATgetTail((ATermList)arg, 4);
}

/*}}}  */
/*{{{  PTPT_CharRangeList PTPT_setCharRangeListTail(PTPT_CharRangeList arg, PTPT_CharRangeList tail) */

PTPT_CharRangeList PTPT_setCharRangeListTail(PTPT_CharRangeList arg, PTPT_CharRangeList tail)
{
  if (PTPT_isCharRangeListMany(arg)) {
    return (PTPT_CharRangeList)ATreplaceTail((ATermList)arg, (ATermList)tail, 4);
  }

  ATabort("CharRangeList has no Tail: %t\n", arg);
  return (PTPT_CharRangeList)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_CharRange accessors */

/*{{{  ATbool PTPT_isValidCharRange(PTPT_CharRange arg) */

ATbool PTPT_isValidCharRange(PTPT_CharRange arg)
{
  if (PTPT_isCharRangeCharacter(arg)) {
    return ATtrue;
  }
  else if (PTPT_isCharRangeRange(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isCharRangeCharacter(PTPT_CharRange arg) */

inline ATbool PTPT_isCharRangeCharacter(PTPT_CharRange arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternCharRangeCharacter, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isCharRangeRange(PTPT_CharRange arg) */

inline ATbool PTPT_isCharRangeRange(PTPT_CharRange arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternCharRangeRange, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  ATbool PTPT_hasCharRangeInteger(PTPT_CharRange arg) */

ATbool PTPT_hasCharRangeInteger(PTPT_CharRange arg)
{
  if (PTPT_isCharRangeCharacter(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_NatCon PTPT_getCharRangeInteger(PTPT_CharRange arg) */

PTPT_NatCon PTPT_getCharRangeInteger(PTPT_CharRange arg)
{
  
    return (PTPT_NatCon)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1));
}

/*}}}  */
/*{{{  PTPT_CharRange PTPT_setCharRangeInteger(PTPT_CharRange arg, PTPT_NatCon integer) */

PTPT_CharRange PTPT_setCharRangeInteger(PTPT_CharRange arg, PTPT_NatCon integer)
{
  if (PTPT_isCharRangeCharacter(arg)) {
    return (PTPT_CharRange)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)integer, 0), 1);
  }

  ATabort("CharRange has no Integer: %t\n", arg);
  return (PTPT_CharRange)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasCharRangeWsAfterRange(PTPT_CharRange arg) */

ATbool PTPT_hasCharRangeWsAfterRange(PTPT_CharRange arg)
{
  if (PTPT_isCharRangeRange(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getCharRangeWsAfterRange(PTPT_CharRange arg) */

PTPT_OptLayout PTPT_getCharRangeWsAfterRange(PTPT_CharRange arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_CharRange PTPT_setCharRangeWsAfterRange(PTPT_CharRange arg, PTPT_OptLayout wsAfterRange) */

PTPT_CharRange PTPT_setCharRangeWsAfterRange(PTPT_CharRange arg, PTPT_OptLayout wsAfterRange)
{
  if (PTPT_isCharRangeRange(arg)) {
    return (PTPT_CharRange)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterRange, 1), 1);
  }

  ATabort("CharRange has no WsAfterRange: %t\n", arg);
  return (PTPT_CharRange)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasCharRangeWsAfterParenOpen(PTPT_CharRange arg) */

ATbool PTPT_hasCharRangeWsAfterParenOpen(PTPT_CharRange arg)
{
  if (PTPT_isCharRangeRange(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getCharRangeWsAfterParenOpen(PTPT_CharRange arg) */

PTPT_OptLayout PTPT_getCharRangeWsAfterParenOpen(PTPT_CharRange arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
}

/*}}}  */
/*{{{  PTPT_CharRange PTPT_setCharRangeWsAfterParenOpen(PTPT_CharRange arg, PTPT_OptLayout wsAfterParenOpen) */

PTPT_CharRange PTPT_setCharRangeWsAfterParenOpen(PTPT_CharRange arg, PTPT_OptLayout wsAfterParenOpen)
{
  if (PTPT_isCharRangeRange(arg)) {
    return (PTPT_CharRange)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }

  ATabort("CharRange has no WsAfterParenOpen: %t\n", arg);
  return (PTPT_CharRange)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasCharRangeStart(PTPT_CharRange arg) */

ATbool PTPT_hasCharRangeStart(PTPT_CharRange arg)
{
  if (PTPT_isCharRangeRange(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_NatCon PTPT_getCharRangeStart(PTPT_CharRange arg) */

PTPT_NatCon PTPT_getCharRangeStart(PTPT_CharRange arg)
{
  
    return (PTPT_NatCon)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
}

/*}}}  */
/*{{{  PTPT_CharRange PTPT_setCharRangeStart(PTPT_CharRange arg, PTPT_NatCon start) */

PTPT_CharRange PTPT_setCharRangeStart(PTPT_CharRange arg, PTPT_NatCon start)
{
  if (PTPT_isCharRangeRange(arg)) {
    return (PTPT_CharRange)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)start, 4), 1);
  }

  ATabort("CharRange has no Start: %t\n", arg);
  return (PTPT_CharRange)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasCharRangeWsAfterStart(PTPT_CharRange arg) */

ATbool PTPT_hasCharRangeWsAfterStart(PTPT_CharRange arg)
{
  if (PTPT_isCharRangeRange(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getCharRangeWsAfterStart(PTPT_CharRange arg) */

PTPT_OptLayout PTPT_getCharRangeWsAfterStart(PTPT_CharRange arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
}

/*}}}  */
/*{{{  PTPT_CharRange PTPT_setCharRangeWsAfterStart(PTPT_CharRange arg, PTPT_OptLayout wsAfterStart) */

PTPT_CharRange PTPT_setCharRangeWsAfterStart(PTPT_CharRange arg, PTPT_OptLayout wsAfterStart)
{
  if (PTPT_isCharRangeRange(arg)) {
    return (PTPT_CharRange)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterStart, 5), 1);
  }

  ATabort("CharRange has no WsAfterStart: %t\n", arg);
  return (PTPT_CharRange)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasCharRangeWsAfterComma(PTPT_CharRange arg) */

ATbool PTPT_hasCharRangeWsAfterComma(PTPT_CharRange arg)
{
  if (PTPT_isCharRangeRange(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getCharRangeWsAfterComma(PTPT_CharRange arg) */

PTPT_OptLayout PTPT_getCharRangeWsAfterComma(PTPT_CharRange arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 7);
}

/*}}}  */
/*{{{  PTPT_CharRange PTPT_setCharRangeWsAfterComma(PTPT_CharRange arg, PTPT_OptLayout wsAfterComma) */

PTPT_CharRange PTPT_setCharRangeWsAfterComma(PTPT_CharRange arg, PTPT_OptLayout wsAfterComma)
{
  if (PTPT_isCharRangeRange(arg)) {
    return (PTPT_CharRange)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterComma, 7), 1);
  }

  ATabort("CharRange has no WsAfterComma: %t\n", arg);
  return (PTPT_CharRange)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasCharRangeEnd(PTPT_CharRange arg) */

ATbool PTPT_hasCharRangeEnd(PTPT_CharRange arg)
{
  if (PTPT_isCharRangeRange(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_NatCon PTPT_getCharRangeEnd(PTPT_CharRange arg) */

PTPT_NatCon PTPT_getCharRangeEnd(PTPT_CharRange arg)
{
  
    return (PTPT_NatCon)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 8);
}

/*}}}  */
/*{{{  PTPT_CharRange PTPT_setCharRangeEnd(PTPT_CharRange arg, PTPT_NatCon end) */

PTPT_CharRange PTPT_setCharRangeEnd(PTPT_CharRange arg, PTPT_NatCon end)
{
  if (PTPT_isCharRangeRange(arg)) {
    return (PTPT_CharRange)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)end, 8), 1);
  }

  ATabort("CharRange has no End: %t\n", arg);
  return (PTPT_CharRange)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasCharRangeWsAfterEnd(PTPT_CharRange arg) */

ATbool PTPT_hasCharRangeWsAfterEnd(PTPT_CharRange arg)
{
  if (PTPT_isCharRangeRange(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getCharRangeWsAfterEnd(PTPT_CharRange arg) */

PTPT_OptLayout PTPT_getCharRangeWsAfterEnd(PTPT_CharRange arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 9);
}

/*}}}  */
/*{{{  PTPT_CharRange PTPT_setCharRangeWsAfterEnd(PTPT_CharRange arg, PTPT_OptLayout wsAfterEnd) */

PTPT_CharRange PTPT_setCharRangeWsAfterEnd(PTPT_CharRange arg, PTPT_OptLayout wsAfterEnd)
{
  if (PTPT_isCharRangeRange(arg)) {
    return (PTPT_CharRange)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterEnd, 9), 1);
  }

  ATabort("CharRange has no WsAfterEnd: %t\n", arg);
  return (PTPT_CharRange)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_OptExp accessors */

/*{{{  ATbool PTPT_isValidOptExp(PTPT_OptExp arg) */

ATbool PTPT_isValidOptExp(PTPT_OptExp arg)
{
  if (PTPT_isOptExpPresent(arg)) {
    return ATtrue;
  }
  else if (PTPT_isOptExpAbsent(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isOptExpPresent(PTPT_OptExp arg) */

inline ATbool PTPT_isOptExpPresent(PTPT_OptExp arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternOptExpPresent, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isOptExpAbsent(PTPT_OptExp arg) */

inline ATbool PTPT_isOptExpAbsent(PTPT_OptExp arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternOptExpAbsent);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  ATbool PTPT_hasOptExpWsAfterE(PTPT_OptExp arg) */

ATbool PTPT_hasOptExpWsAfterE(PTPT_OptExp arg)
{
  if (PTPT_isOptExpPresent(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getOptExpWsAfterE(PTPT_OptExp arg) */

PTPT_OptLayout PTPT_getOptExpWsAfterE(PTPT_OptExp arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_OptExp PTPT_setOptExpWsAfterE(PTPT_OptExp arg, PTPT_OptLayout wsAfterE) */

PTPT_OptExp PTPT_setOptExpWsAfterE(PTPT_OptExp arg, PTPT_OptLayout wsAfterE)
{
  if (PTPT_isOptExpPresent(arg)) {
    return (PTPT_OptExp)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterE, 1), 1);
  }

  ATabort("OptExp has no WsAfterE: %t\n", arg);
  return (PTPT_OptExp)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasOptExpIntCon(PTPT_OptExp arg) */

ATbool PTPT_hasOptExpIntCon(PTPT_OptExp arg)
{
  if (PTPT_isOptExpPresent(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_IntCon PTPT_getOptExpIntCon(PTPT_OptExp arg) */

PTPT_IntCon PTPT_getOptExpIntCon(PTPT_OptExp arg)
{
  
    return (PTPT_IntCon)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 2);
}

/*}}}  */
/*{{{  PTPT_OptExp PTPT_setOptExpIntCon(PTPT_OptExp arg, PTPT_IntCon IntCon) */

PTPT_OptExp PTPT_setOptExpIntCon(PTPT_OptExp arg, PTPT_IntCon IntCon)
{
  if (PTPT_isOptExpPresent(arg)) {
    return (PTPT_OptExp)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)IntCon, 2), 1);
  }

  ATabort("OptExp has no IntCon: %t\n", arg);
  return (PTPT_OptExp)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_RealCon accessors */

/*{{{  ATbool PTPT_isValidRealCon(PTPT_RealCon arg) */

ATbool PTPT_isValidRealCon(PTPT_RealCon arg)
{
  if (PTPT_isRealConRealCon(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isRealConRealCon(PTPT_RealCon arg) */

inline ATbool PTPT_isRealConRealCon(PTPT_RealCon arg)
{
#ifndef DISABLE_DYNAMIC_CHECKING
  assert(arg != NULL);
  assert(ATmatchTerm((ATerm)arg, PTPT_patternRealConRealCon, NULL, NULL, NULL, NULL, NULL, NULL));
#endif
  return ATtrue;
}

/*}}}  */
/*{{{  ATbool PTPT_hasRealConIntCon(PTPT_RealCon arg) */

ATbool PTPT_hasRealConIntCon(PTPT_RealCon arg)
{
  if (PTPT_isRealConRealCon(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_IntCon PTPT_getRealConIntCon(PTPT_RealCon arg) */

PTPT_IntCon PTPT_getRealConIntCon(PTPT_RealCon arg)
{
  
    return (PTPT_IntCon)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1));
}

/*}}}  */
/*{{{  PTPT_RealCon PTPT_setRealConIntCon(PTPT_RealCon arg, PTPT_IntCon IntCon) */

PTPT_RealCon PTPT_setRealConIntCon(PTPT_RealCon arg, PTPT_IntCon IntCon)
{
  if (PTPT_isRealConRealCon(arg)) {
    return (PTPT_RealCon)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)IntCon, 0), 1);
  }

  ATabort("RealCon has no IntCon: %t\n", arg);
  return (PTPT_RealCon)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasRealConWsAfterIntCon(PTPT_RealCon arg) */

ATbool PTPT_hasRealConWsAfterIntCon(PTPT_RealCon arg)
{
  if (PTPT_isRealConRealCon(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getRealConWsAfterIntCon(PTPT_RealCon arg) */

PTPT_OptLayout PTPT_getRealConWsAfterIntCon(PTPT_RealCon arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_RealCon PTPT_setRealConWsAfterIntCon(PTPT_RealCon arg, PTPT_OptLayout wsAfterIntCon) */

PTPT_RealCon PTPT_setRealConWsAfterIntCon(PTPT_RealCon arg, PTPT_OptLayout wsAfterIntCon)
{
  if (PTPT_isRealConRealCon(arg)) {
    return (PTPT_RealCon)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterIntCon, 1), 1);
  }

  ATabort("RealCon has no WsAfterIntCon: %t\n", arg);
  return (PTPT_RealCon)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasRealConWsAfterPeriod(PTPT_RealCon arg) */

ATbool PTPT_hasRealConWsAfterPeriod(PTPT_RealCon arg)
{
  if (PTPT_isRealConRealCon(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getRealConWsAfterPeriod(PTPT_RealCon arg) */

PTPT_OptLayout PTPT_getRealConWsAfterPeriod(PTPT_RealCon arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
}

/*}}}  */
/*{{{  PTPT_RealCon PTPT_setRealConWsAfterPeriod(PTPT_RealCon arg, PTPT_OptLayout wsAfterPeriod) */

PTPT_RealCon PTPT_setRealConWsAfterPeriod(PTPT_RealCon arg, PTPT_OptLayout wsAfterPeriod)
{
  if (PTPT_isRealConRealCon(arg)) {
    return (PTPT_RealCon)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterPeriod, 3), 1);
  }

  ATabort("RealCon has no WsAfterPeriod: %t\n", arg);
  return (PTPT_RealCon)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasRealConNatCon(PTPT_RealCon arg) */

ATbool PTPT_hasRealConNatCon(PTPT_RealCon arg)
{
  if (PTPT_isRealConRealCon(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_NatCon PTPT_getRealConNatCon(PTPT_RealCon arg) */

PTPT_NatCon PTPT_getRealConNatCon(PTPT_RealCon arg)
{
  
    return (PTPT_NatCon)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
}

/*}}}  */
/*{{{  PTPT_RealCon PTPT_setRealConNatCon(PTPT_RealCon arg, PTPT_NatCon NatCon) */

PTPT_RealCon PTPT_setRealConNatCon(PTPT_RealCon arg, PTPT_NatCon NatCon)
{
  if (PTPT_isRealConRealCon(arg)) {
    return (PTPT_RealCon)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)NatCon, 4), 1);
  }

  ATabort("RealCon has no NatCon: %t\n", arg);
  return (PTPT_RealCon)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasRealConWsAfterNatCon(PTPT_RealCon arg) */

ATbool PTPT_hasRealConWsAfterNatCon(PTPT_RealCon arg)
{
  if (PTPT_isRealConRealCon(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getRealConWsAfterNatCon(PTPT_RealCon arg) */

PTPT_OptLayout PTPT_getRealConWsAfterNatCon(PTPT_RealCon arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
}

/*}}}  */
/*{{{  PTPT_RealCon PTPT_setRealConWsAfterNatCon(PTPT_RealCon arg, PTPT_OptLayout wsAfterNatCon) */

PTPT_RealCon PTPT_setRealConWsAfterNatCon(PTPT_RealCon arg, PTPT_OptLayout wsAfterNatCon)
{
  if (PTPT_isRealConRealCon(arg)) {
    return (PTPT_RealCon)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterNatCon, 5), 1);
  }

  ATabort("RealCon has no WsAfterNatCon: %t\n", arg);
  return (PTPT_RealCon)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasRealConOptExp(PTPT_RealCon arg) */

ATbool PTPT_hasRealConOptExp(PTPT_RealCon arg)
{
  if (PTPT_isRealConRealCon(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptExp PTPT_getRealConOptExp(PTPT_RealCon arg) */

PTPT_OptExp PTPT_getRealConOptExp(PTPT_RealCon arg)
{
  
    return (PTPT_OptExp)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 6);
}

/*}}}  */
/*{{{  PTPT_RealCon PTPT_setRealConOptExp(PTPT_RealCon arg, PTPT_OptExp OptExp) */

PTPT_RealCon PTPT_setRealConOptExp(PTPT_RealCon arg, PTPT_OptExp OptExp)
{
  if (PTPT_isRealConRealCon(arg)) {
    return (PTPT_RealCon)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)OptExp, 6), 1);
  }

  ATabort("RealCon has no OptExp: %t\n", arg);
  return (PTPT_RealCon)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_ATermList accessors */

/*{{{  ATbool PTPT_isValidATermList(PTPT_ATermList arg) */

ATbool PTPT_isValidATermList(PTPT_ATermList arg)
{
  if (PTPT_isATermListNotEmpty(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isATermListNotEmpty(PTPT_ATermList arg) */

inline ATbool PTPT_isATermListNotEmpty(PTPT_ATermList arg)
{
#ifndef DISABLE_DYNAMIC_CHECKING
  assert(arg != NULL);
  assert(ATmatchTerm((ATerm)arg, PTPT_patternATermListNotEmpty, NULL, NULL, NULL));
#endif
  return ATtrue;
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermListWsAfterBracketOpen(PTPT_ATermList arg) */

ATbool PTPT_hasATermListWsAfterBracketOpen(PTPT_ATermList arg)
{
  if (PTPT_isATermListNotEmpty(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getATermListWsAfterBracketOpen(PTPT_ATermList arg) */

PTPT_OptLayout PTPT_getATermListWsAfterBracketOpen(PTPT_ATermList arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_ATermList PTPT_setATermListWsAfterBracketOpen(PTPT_ATermList arg, PTPT_OptLayout wsAfterBracketOpen) */

PTPT_ATermList PTPT_setATermListWsAfterBracketOpen(PTPT_ATermList arg, PTPT_OptLayout wsAfterBracketOpen)
{
  if (PTPT_isATermListNotEmpty(arg)) {
    return (PTPT_ATermList)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterBracketOpen, 1), 1);
  }

  ATabort("ATermList has no WsAfterBracketOpen: %t\n", arg);
  return (PTPT_ATermList)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermListElems(PTPT_ATermList arg) */

ATbool PTPT_hasATermListElems(PTPT_ATermList arg)
{
  if (PTPT_isATermListNotEmpty(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_ATermElems PTPT_getATermListElems(PTPT_ATermList arg) */

PTPT_ATermElems PTPT_getATermListElems(PTPT_ATermList arg)
{
  
    return (PTPT_ATermElems)ATgetArgument((ATermAppl)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 2), 1);
}

/*}}}  */
/*{{{  PTPT_ATermList PTPT_setATermListElems(PTPT_ATermList arg, PTPT_ATermElems elems) */

PTPT_ATermList PTPT_setATermListElems(PTPT_ATermList arg, PTPT_ATermElems elems)
{
  if (PTPT_isATermListNotEmpty(arg)) {
    return (PTPT_ATermList)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)ATsetArgument((ATermAppl)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 2), (ATerm)elems, 1), 2), 1);
  }

  ATabort("ATermList has no Elems: %t\n", arg);
  return (PTPT_ATermList)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermListWsAfterElems(PTPT_ATermList arg) */

ATbool PTPT_hasATermListWsAfterElems(PTPT_ATermList arg)
{
  if (PTPT_isATermListNotEmpty(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getATermListWsAfterElems(PTPT_ATermList arg) */

PTPT_OptLayout PTPT_getATermListWsAfterElems(PTPT_ATermList arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
}

/*}}}  */
/*{{{  PTPT_ATermList PTPT_setATermListWsAfterElems(PTPT_ATermList arg, PTPT_OptLayout wsAfterElems) */

PTPT_ATermList PTPT_setATermListWsAfterElems(PTPT_ATermList arg, PTPT_OptLayout wsAfterElems)
{
  if (PTPT_isATermListNotEmpty(arg)) {
    return (PTPT_ATermList)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterElems, 3), 1);
  }

  ATabort("ATermList has no WsAfterElems: %t\n", arg);
  return (PTPT_ATermList)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_ATermElems accessors */

/*{{{  ATbool PTPT_isValidATermElems(PTPT_ATermElems arg) */

ATbool PTPT_isValidATermElems(PTPT_ATermElems arg)
{
  if (PTPT_isATermElemsEmpty(arg)) {
    return ATtrue;
  }
  else if (PTPT_isATermElemsSingle(arg)) {
    return ATtrue;
  }
  else if (PTPT_isATermElemsMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isATermElemsEmpty(PTPT_ATermElems arg) */

inline ATbool PTPT_isATermElemsEmpty(PTPT_ATermElems arg)
{
  if (!ATisEmpty((ATermList)arg)) {
    return ATfalse;
  }
#ifndef DISABLE_DYNAMIC_CHECKING
  assert(arg != NULL);
  assert(ATmatchTerm((ATerm)arg, PTPT_patternATermElemsEmpty));
#endif
  return ATtrue;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isATermElemsSingle(PTPT_ATermElems arg) */

inline ATbool PTPT_isATermElemsSingle(PTPT_ATermElems arg)
{
  if (ATisEmpty((ATermList)arg)) {
    return ATfalse;
  }
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternATermElemsSingle, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isATermElemsMany(PTPT_ATermElems arg) */

inline ATbool PTPT_isATermElemsMany(PTPT_ATermElems arg)
{
  if (ATisEmpty((ATermList)arg)) {
    return ATfalse;
  }
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternATermElemsMany, NULL, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermElemsHead(PTPT_ATermElems arg) */

ATbool PTPT_hasATermElemsHead(PTPT_ATermElems arg)
{
  if (PTPT_isATermElemsSingle(arg)) {
    return ATtrue;
  }
  else if (PTPT_isATermElemsMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_ATerm PTPT_getATermElemsHead(PTPT_ATermElems arg) */

PTPT_ATerm PTPT_getATermElemsHead(PTPT_ATermElems arg)
{
  if (PTPT_isATermElemsSingle(arg)) {
    return (PTPT_ATerm)ATgetFirst((ATermList)arg);
  }
  else 
    return (PTPT_ATerm)ATgetFirst((ATermList)arg);
}

/*}}}  */
/*{{{  PTPT_ATermElems PTPT_setATermElemsHead(PTPT_ATermElems arg, PTPT_ATerm head) */

PTPT_ATermElems PTPT_setATermElemsHead(PTPT_ATermElems arg, PTPT_ATerm head)
{
  if (PTPT_isATermElemsSingle(arg)) {
    return (PTPT_ATermElems)ATreplace((ATermList)arg, (ATerm)head, 0);
  }
  else if (PTPT_isATermElemsMany(arg)) {
    return (PTPT_ATermElems)ATreplace((ATermList)arg, (ATerm)head, 0);
  }

  ATabort("ATermElems has no Head: %t\n", arg);
  return (PTPT_ATermElems)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermElemsWsAfterFirst(PTPT_ATermElems arg) */

ATbool PTPT_hasATermElemsWsAfterFirst(PTPT_ATermElems arg)
{
  if (PTPT_isATermElemsMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getATermElemsWsAfterFirst(PTPT_ATermElems arg) */

PTPT_OptLayout PTPT_getATermElemsWsAfterFirst(PTPT_ATermElems arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)arg, 1);
}

/*}}}  */
/*{{{  PTPT_ATermElems PTPT_setATermElemsWsAfterFirst(PTPT_ATermElems arg, PTPT_OptLayout wsAfterFirst) */

PTPT_ATermElems PTPT_setATermElemsWsAfterFirst(PTPT_ATermElems arg, PTPT_OptLayout wsAfterFirst)
{
  if (PTPT_isATermElemsMany(arg)) {
    return (PTPT_ATermElems)ATreplace((ATermList)arg, (ATerm)wsAfterFirst, 1);
  }

  ATabort("ATermElems has no WsAfterFirst: %t\n", arg);
  return (PTPT_ATermElems)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermElemsSep(PTPT_ATermElems arg) */

ATbool PTPT_hasATermElemsSep(PTPT_ATermElems arg)
{
  if (PTPT_isATermElemsMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  char * PTPT_getATermElemsSep(PTPT_ATermElems arg) */

char * PTPT_getATermElemsSep(PTPT_ATermElems arg)
{
  
    return (char *)ATgetName(ATgetAFun((ATermAppl)ATgetArgument((ATermAppl)ATelementAt((ATermList)arg, 2), 0)));
}

/*}}}  */
/*{{{  PTPT_ATermElems PTPT_setATermElemsSep(PTPT_ATermElems arg, char * sep) */

PTPT_ATermElems PTPT_setATermElemsSep(PTPT_ATermElems arg, char * sep)
{
  if (PTPT_isATermElemsMany(arg)) {
    return (PTPT_ATermElems)ATreplace((ATermList)arg, (ATerm)ATsetArgument((ATermAppl)ATelementAt((ATermList)arg, 2), (ATerm)ATmakeAppl0(ATmakeAFun(sep, 0, ATtrue)), 0), 2);
  }

  ATabort("ATermElems has no Sep: %t\n", arg);
  return (PTPT_ATermElems)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermElemsWsAfterSep(PTPT_ATermElems arg) */

ATbool PTPT_hasATermElemsWsAfterSep(PTPT_ATermElems arg)
{
  if (PTPT_isATermElemsMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getATermElemsWsAfterSep(PTPT_ATermElems arg) */

PTPT_OptLayout PTPT_getATermElemsWsAfterSep(PTPT_ATermElems arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)arg, 3);
}

/*}}}  */
/*{{{  PTPT_ATermElems PTPT_setATermElemsWsAfterSep(PTPT_ATermElems arg, PTPT_OptLayout wsAfterSep) */

PTPT_ATermElems PTPT_setATermElemsWsAfterSep(PTPT_ATermElems arg, PTPT_OptLayout wsAfterSep)
{
  if (PTPT_isATermElemsMany(arg)) {
    return (PTPT_ATermElems)ATreplace((ATermList)arg, (ATerm)wsAfterSep, 3);
  }

  ATabort("ATermElems has no WsAfterSep: %t\n", arg);
  return (PTPT_ATermElems)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermElemsTail(PTPT_ATermElems arg) */

ATbool PTPT_hasATermElemsTail(PTPT_ATermElems arg)
{
  if (PTPT_isATermElemsMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_ATermElems PTPT_getATermElemsTail(PTPT_ATermElems arg) */

PTPT_ATermElems PTPT_getATermElemsTail(PTPT_ATermElems arg)
{
  
    return (PTPT_ATermElems)ATgetTail((ATermList)arg, 4);
}

/*}}}  */
/*{{{  PTPT_ATermElems PTPT_setATermElemsTail(PTPT_ATermElems arg, PTPT_ATermElems tail) */

PTPT_ATermElems PTPT_setATermElemsTail(PTPT_ATermElems arg, PTPT_ATermElems tail)
{
  if (PTPT_isATermElemsMany(arg)) {
    return (PTPT_ATermElems)ATreplaceTail((ATermList)arg, (ATermList)tail, 4);
  }

  ATabort("ATermElems has no Tail: %t\n", arg);
  return (PTPT_ATermElems)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_ACon accessors */

/*{{{  ATbool PTPT_isValidACon(PTPT_ACon arg) */

ATbool PTPT_isValidACon(PTPT_ACon arg)
{
  if (PTPT_isAConInt(arg)) {
    return ATtrue;
  }
  else if (PTPT_isAConReal(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isAConInt(PTPT_ACon arg) */

inline ATbool PTPT_isAConInt(PTPT_ACon arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternAConInt, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isAConReal(PTPT_ACon arg) */

inline ATbool PTPT_isAConReal(PTPT_ACon arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternAConReal, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  ATbool PTPT_hasAConIntCon(PTPT_ACon arg) */

ATbool PTPT_hasAConIntCon(PTPT_ACon arg)
{
  if (PTPT_isAConInt(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_IntCon PTPT_getAConIntCon(PTPT_ACon arg) */

PTPT_IntCon PTPT_getAConIntCon(PTPT_ACon arg)
{
  
    return (PTPT_IntCon)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1));
}

/*}}}  */
/*{{{  PTPT_ACon PTPT_setAConIntCon(PTPT_ACon arg, PTPT_IntCon IntCon) */

PTPT_ACon PTPT_setAConIntCon(PTPT_ACon arg, PTPT_IntCon IntCon)
{
  if (PTPT_isAConInt(arg)) {
    return (PTPT_ACon)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)IntCon, 0), 1);
  }

  ATabort("ACon has no IntCon: %t\n", arg);
  return (PTPT_ACon)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasAConRealCon(PTPT_ACon arg) */

ATbool PTPT_hasAConRealCon(PTPT_ACon arg)
{
  if (PTPT_isAConReal(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_RealCon PTPT_getAConRealCon(PTPT_ACon arg) */

PTPT_RealCon PTPT_getAConRealCon(PTPT_ACon arg)
{
  
    return (PTPT_RealCon)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1));
}

/*}}}  */
/*{{{  PTPT_ACon PTPT_setAConRealCon(PTPT_ACon arg, PTPT_RealCon RealCon) */

PTPT_ACon PTPT_setAConRealCon(PTPT_ACon arg, PTPT_RealCon RealCon)
{
  if (PTPT_isAConReal(arg)) {
    return (PTPT_ACon)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)RealCon, 0), 1);
  }

  ATabort("ACon has no RealCon: %t\n", arg);
  return (PTPT_ACon)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_AFun accessors */

/*{{{  ATbool PTPT_isValidAFun(PTPT_AFun arg) */

ATbool PTPT_isValidAFun(PTPT_AFun arg)
{
  if (PTPT_isAFunDefault(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isAFunDefault(PTPT_AFun arg) */

inline ATbool PTPT_isAFunDefault(PTPT_AFun arg)
{
#ifndef DISABLE_DYNAMIC_CHECKING
  assert(arg != NULL);
  assert(ATmatchTerm((ATerm)arg, PTPT_patternAFunDefault, NULL));
#endif
  return ATtrue;
}

/*}}}  */
/*{{{  ATbool PTPT_hasAFunLiteral(PTPT_AFun arg) */

ATbool PTPT_hasAFunLiteral(PTPT_AFun arg)
{
  if (PTPT_isAFunDefault(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Literal PTPT_getAFunLiteral(PTPT_AFun arg) */

PTPT_Literal PTPT_getAFunLiteral(PTPT_AFun arg)
{
  
    return (PTPT_Literal)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1));
}

/*}}}  */
/*{{{  PTPT_AFun PTPT_setAFunLiteral(PTPT_AFun arg, PTPT_Literal Literal) */

PTPT_AFun PTPT_setAFunLiteral(PTPT_AFun arg, PTPT_Literal Literal)
{
  if (PTPT_isAFunDefault(arg)) {
    return (PTPT_AFun)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)Literal, 0), 1);
  }

  ATabort("AFun has no Literal: %t\n", arg);
  return (PTPT_AFun)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_ATerm accessors */

/*{{{  ATbool PTPT_isValidATerm(PTPT_ATerm arg) */

ATbool PTPT_isValidATerm(PTPT_ATerm arg)
{
  if (PTPT_isATermConstant(arg)) {
    return ATtrue;
  }
  else if (PTPT_isATermList(arg)) {
    return ATtrue;
  }
  else if (PTPT_isATermFun(arg)) {
    return ATtrue;
  }
  else if (PTPT_isATermAppl(arg)) {
    return ATtrue;
  }
  else if (PTPT_isATermAnnotatedConstant(arg)) {
    return ATtrue;
  }
  else if (PTPT_isATermAnnotatedList(arg)) {
    return ATtrue;
  }
  else if (PTPT_isATermAnnotatedFun(arg)) {
    return ATtrue;
  }
  else if (PTPT_isATermAnnotatedAppl(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isATermConstant(PTPT_ATerm arg) */

inline ATbool PTPT_isATermConstant(PTPT_ATerm arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternATermConstant, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isATermList(PTPT_ATerm arg) */

inline ATbool PTPT_isATermList(PTPT_ATerm arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternATermList, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isATermFun(PTPT_ATerm arg) */

inline ATbool PTPT_isATermFun(PTPT_ATerm arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternATermFun, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isATermAppl(PTPT_ATerm arg) */

inline ATbool PTPT_isATermAppl(PTPT_ATerm arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternATermAppl, NULL, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isATermAnnotatedConstant(PTPT_ATerm arg) */

inline ATbool PTPT_isATermAnnotatedConstant(PTPT_ATerm arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternATermAnnotatedConstant, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isATermAnnotatedList(PTPT_ATerm arg) */

inline ATbool PTPT_isATermAnnotatedList(PTPT_ATerm arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternATermAnnotatedList, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isATermAnnotatedFun(PTPT_ATerm arg) */

inline ATbool PTPT_isATermAnnotatedFun(PTPT_ATerm arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternATermAnnotatedFun, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isATermAnnotatedAppl(PTPT_ATerm arg) */

inline ATbool PTPT_isATermAnnotatedAppl(PTPT_ATerm arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternATermAnnotatedAppl, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermACon(PTPT_ATerm arg) */

ATbool PTPT_hasATermACon(PTPT_ATerm arg)
{
  if (PTPT_isATermConstant(arg)) {
    return ATtrue;
  }
  else if (PTPT_isATermAnnotatedConstant(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_ACon PTPT_getATermACon(PTPT_ATerm arg) */

PTPT_ACon PTPT_getATermACon(PTPT_ATerm arg)
{
  if (PTPT_isATermConstant(arg)) {
    return (PTPT_ACon)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1));
  }
  else 
    return (PTPT_ACon)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1));
}

/*}}}  */
/*{{{  PTPT_ATerm PTPT_setATermACon(PTPT_ATerm arg, PTPT_ACon ACon) */

PTPT_ATerm PTPT_setATermACon(PTPT_ATerm arg, PTPT_ACon ACon)
{
  if (PTPT_isATermConstant(arg)) {
    return (PTPT_ATerm)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)ACon, 0), 1);
  }
  else if (PTPT_isATermAnnotatedConstant(arg)) {
    return (PTPT_ATerm)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)ACon, 0), 1);
  }

  ATabort("ATerm has no ACon: %t\n", arg);
  return (PTPT_ATerm)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermList(PTPT_ATerm arg) */

ATbool PTPT_hasATermList(PTPT_ATerm arg)
{
  if (PTPT_isATermList(arg)) {
    return ATtrue;
  }
  else if (PTPT_isATermAnnotatedList(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_ATermList PTPT_getATermList(PTPT_ATerm arg) */

PTPT_ATermList PTPT_getATermList(PTPT_ATerm arg)
{
  if (PTPT_isATermList(arg)) {
    return (PTPT_ATermList)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1));
  }
  else 
    return (PTPT_ATermList)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1));
}

/*}}}  */
/*{{{  PTPT_ATerm PTPT_setATermList(PTPT_ATerm arg, PTPT_ATermList list) */

PTPT_ATerm PTPT_setATermList(PTPT_ATerm arg, PTPT_ATermList list)
{
  if (PTPT_isATermList(arg)) {
    return (PTPT_ATerm)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)list, 0), 1);
  }
  else if (PTPT_isATermAnnotatedList(arg)) {
    return (PTPT_ATerm)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)list, 0), 1);
  }

  ATabort("ATerm has no List: %t\n", arg);
  return (PTPT_ATerm)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermFun(PTPT_ATerm arg) */

ATbool PTPT_hasATermFun(PTPT_ATerm arg)
{
  if (PTPT_isATermFun(arg)) {
    return ATtrue;
  }
  else if (PTPT_isATermAppl(arg)) {
    return ATtrue;
  }
  else if (PTPT_isATermAnnotatedFun(arg)) {
    return ATtrue;
  }
  else if (PTPT_isATermAnnotatedAppl(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_AFun PTPT_getATermFun(PTPT_ATerm arg) */

PTPT_AFun PTPT_getATermFun(PTPT_ATerm arg)
{
  if (PTPT_isATermFun(arg)) {
    return (PTPT_AFun)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1));
  }
  else if (PTPT_isATermAppl(arg)) {
    return (PTPT_AFun)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1));
  }
  else if (PTPT_isATermAnnotatedFun(arg)) {
    return (PTPT_AFun)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1));
  }
  else 
    return (PTPT_AFun)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1));
}

/*}}}  */
/*{{{  PTPT_ATerm PTPT_setATermFun(PTPT_ATerm arg, PTPT_AFun fun) */

PTPT_ATerm PTPT_setATermFun(PTPT_ATerm arg, PTPT_AFun fun)
{
  if (PTPT_isATermFun(arg)) {
    return (PTPT_ATerm)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)fun, 0), 1);
  }
  else if (PTPT_isATermAppl(arg)) {
    return (PTPT_ATerm)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)fun, 0), 1);
  }
  else if (PTPT_isATermAnnotatedFun(arg)) {
    return (PTPT_ATerm)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)fun, 0), 1);
  }
  else if (PTPT_isATermAnnotatedAppl(arg)) {
    return (PTPT_ATerm)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)fun, 0), 1);
  }

  ATabort("ATerm has no Fun: %t\n", arg);
  return (PTPT_ATerm)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermWsAfterFun(PTPT_ATerm arg) */

ATbool PTPT_hasATermWsAfterFun(PTPT_ATerm arg)
{
  if (PTPT_isATermAppl(arg)) {
    return ATtrue;
  }
  else if (PTPT_isATermAnnotatedFun(arg)) {
    return ATtrue;
  }
  else if (PTPT_isATermAnnotatedAppl(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getATermWsAfterFun(PTPT_ATerm arg) */

PTPT_OptLayout PTPT_getATermWsAfterFun(PTPT_ATerm arg)
{
  if (PTPT_isATermAppl(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
  }
  else if (PTPT_isATermAnnotatedFun(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
  }
  else 
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_ATerm PTPT_setATermWsAfterFun(PTPT_ATerm arg, PTPT_OptLayout wsAfterFun) */

PTPT_ATerm PTPT_setATermWsAfterFun(PTPT_ATerm arg, PTPT_OptLayout wsAfterFun)
{
  if (PTPT_isATermAppl(arg)) {
    return (PTPT_ATerm)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterFun, 1), 1);
  }
  else if (PTPT_isATermAnnotatedFun(arg)) {
    return (PTPT_ATerm)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterFun, 1), 1);
  }
  else if (PTPT_isATermAnnotatedAppl(arg)) {
    return (PTPT_ATerm)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterFun, 1), 1);
  }

  ATabort("ATerm has no WsAfterFun: %t\n", arg);
  return (PTPT_ATerm)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermWsAfterParenOpen(PTPT_ATerm arg) */

ATbool PTPT_hasATermWsAfterParenOpen(PTPT_ATerm arg)
{
  if (PTPT_isATermAppl(arg)) {
    return ATtrue;
  }
  else if (PTPT_isATermAnnotatedAppl(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getATermWsAfterParenOpen(PTPT_ATerm arg) */

PTPT_OptLayout PTPT_getATermWsAfterParenOpen(PTPT_ATerm arg)
{
  if (PTPT_isATermAppl(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
  }
  else 
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
}

/*}}}  */
/*{{{  PTPT_ATerm PTPT_setATermWsAfterParenOpen(PTPT_ATerm arg, PTPT_OptLayout wsAfterParenOpen) */

PTPT_ATerm PTPT_setATermWsAfterParenOpen(PTPT_ATerm arg, PTPT_OptLayout wsAfterParenOpen)
{
  if (PTPT_isATermAppl(arg)) {
    return (PTPT_ATerm)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }
  else if (PTPT_isATermAnnotatedAppl(arg)) {
    return (PTPT_ATerm)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }

  ATabort("ATerm has no WsAfterParenOpen: %t\n", arg);
  return (PTPT_ATerm)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermArgs(PTPT_ATerm arg) */

ATbool PTPT_hasATermArgs(PTPT_ATerm arg)
{
  if (PTPT_isATermAppl(arg)) {
    return ATtrue;
  }
  else if (PTPT_isATermAnnotatedAppl(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_ATermArgs PTPT_getATermArgs(PTPT_ATerm arg) */

PTPT_ATermArgs PTPT_getATermArgs(PTPT_ATerm arg)
{
  if (PTPT_isATermAppl(arg)) {
    return (PTPT_ATermArgs)ATgetArgument((ATermAppl)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4), 1);
  }
  else 
    return (PTPT_ATermArgs)ATgetArgument((ATermAppl)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4), 1);
}

/*}}}  */
/*{{{  PTPT_ATerm PTPT_setATermArgs(PTPT_ATerm arg, PTPT_ATermArgs args) */

PTPT_ATerm PTPT_setATermArgs(PTPT_ATerm arg, PTPT_ATermArgs args)
{
  if (PTPT_isATermAppl(arg)) {
    return (PTPT_ATerm)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)ATsetArgument((ATermAppl)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4), (ATerm)args, 1), 4), 1);
  }
  else if (PTPT_isATermAnnotatedAppl(arg)) {
    return (PTPT_ATerm)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)ATsetArgument((ATermAppl)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4), (ATerm)args, 1), 4), 1);
  }

  ATabort("ATerm has no Args: %t\n", arg);
  return (PTPT_ATerm)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermWsAfterArgs(PTPT_ATerm arg) */

ATbool PTPT_hasATermWsAfterArgs(PTPT_ATerm arg)
{
  if (PTPT_isATermAppl(arg)) {
    return ATtrue;
  }
  else if (PTPT_isATermAnnotatedAppl(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getATermWsAfterArgs(PTPT_ATerm arg) */

PTPT_OptLayout PTPT_getATermWsAfterArgs(PTPT_ATerm arg)
{
  if (PTPT_isATermAppl(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
  }
  else 
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
}

/*}}}  */
/*{{{  PTPT_ATerm PTPT_setATermWsAfterArgs(PTPT_ATerm arg, PTPT_OptLayout wsAfterArgs) */

PTPT_ATerm PTPT_setATermWsAfterArgs(PTPT_ATerm arg, PTPT_OptLayout wsAfterArgs)
{
  if (PTPT_isATermAppl(arg)) {
    return (PTPT_ATerm)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterArgs, 5), 1);
  }
  else if (PTPT_isATermAnnotatedAppl(arg)) {
    return (PTPT_ATerm)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterArgs, 5), 1);
  }

  ATabort("ATerm has no WsAfterArgs: %t\n", arg);
  return (PTPT_ATerm)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermWsAfterACon(PTPT_ATerm arg) */

ATbool PTPT_hasATermWsAfterACon(PTPT_ATerm arg)
{
  if (PTPT_isATermAnnotatedConstant(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getATermWsAfterACon(PTPT_ATerm arg) */

PTPT_OptLayout PTPT_getATermWsAfterACon(PTPT_ATerm arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_ATerm PTPT_setATermWsAfterACon(PTPT_ATerm arg, PTPT_OptLayout wsAfterACon) */

PTPT_ATerm PTPT_setATermWsAfterACon(PTPT_ATerm arg, PTPT_OptLayout wsAfterACon)
{
  if (PTPT_isATermAnnotatedConstant(arg)) {
    return (PTPT_ATerm)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterACon, 1), 1);
  }

  ATabort("ATerm has no WsAfterACon: %t\n", arg);
  return (PTPT_ATerm)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermAnn(PTPT_ATerm arg) */

ATbool PTPT_hasATermAnn(PTPT_ATerm arg)
{
  if (PTPT_isATermAnnotatedConstant(arg)) {
    return ATtrue;
  }
  else if (PTPT_isATermAnnotatedList(arg)) {
    return ATtrue;
  }
  else if (PTPT_isATermAnnotatedFun(arg)) {
    return ATtrue;
  }
  else if (PTPT_isATermAnnotatedAppl(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Ann PTPT_getATermAnn(PTPT_ATerm arg) */

PTPT_Ann PTPT_getATermAnn(PTPT_ATerm arg)
{
  if (PTPT_isATermAnnotatedConstant(arg)) {
    return (PTPT_Ann)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 2);
  }
  else if (PTPT_isATermAnnotatedList(arg)) {
    return (PTPT_Ann)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 2);
  }
  else if (PTPT_isATermAnnotatedFun(arg)) {
    return (PTPT_Ann)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 2);
  }
  else 
    return (PTPT_Ann)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 8);
}

/*}}}  */
/*{{{  PTPT_ATerm PTPT_setATermAnn(PTPT_ATerm arg, PTPT_Ann Ann) */

PTPT_ATerm PTPT_setATermAnn(PTPT_ATerm arg, PTPT_Ann Ann)
{
  if (PTPT_isATermAnnotatedConstant(arg)) {
    return (PTPT_ATerm)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)Ann, 2), 1);
  }
  else if (PTPT_isATermAnnotatedList(arg)) {
    return (PTPT_ATerm)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)Ann, 2), 1);
  }
  else if (PTPT_isATermAnnotatedFun(arg)) {
    return (PTPT_ATerm)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)Ann, 2), 1);
  }
  else if (PTPT_isATermAnnotatedAppl(arg)) {
    return (PTPT_ATerm)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)Ann, 8), 1);
  }

  ATabort("ATerm has no Ann: %t\n", arg);
  return (PTPT_ATerm)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermWsAfterList(PTPT_ATerm arg) */

ATbool PTPT_hasATermWsAfterList(PTPT_ATerm arg)
{
  if (PTPT_isATermAnnotatedList(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getATermWsAfterList(PTPT_ATerm arg) */

PTPT_OptLayout PTPT_getATermWsAfterList(PTPT_ATerm arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_ATerm PTPT_setATermWsAfterList(PTPT_ATerm arg, PTPT_OptLayout wsAfterList) */

PTPT_ATerm PTPT_setATermWsAfterList(PTPT_ATerm arg, PTPT_OptLayout wsAfterList)
{
  if (PTPT_isATermAnnotatedList(arg)) {
    return (PTPT_ATerm)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterList, 1), 1);
  }

  ATabort("ATerm has no WsAfterList: %t\n", arg);
  return (PTPT_ATerm)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermWsAfterParenClose(PTPT_ATerm arg) */

ATbool PTPT_hasATermWsAfterParenClose(PTPT_ATerm arg)
{
  if (PTPT_isATermAnnotatedAppl(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getATermWsAfterParenClose(PTPT_ATerm arg) */

PTPT_OptLayout PTPT_getATermWsAfterParenClose(PTPT_ATerm arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 7);
}

/*}}}  */
/*{{{  PTPT_ATerm PTPT_setATermWsAfterParenClose(PTPT_ATerm arg, PTPT_OptLayout wsAfterParenClose) */

PTPT_ATerm PTPT_setATermWsAfterParenClose(PTPT_ATerm arg, PTPT_OptLayout wsAfterParenClose)
{
  if (PTPT_isATermAnnotatedAppl(arg)) {
    return (PTPT_ATerm)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenClose, 7), 1);
  }

  ATabort("ATerm has no WsAfterParenClose: %t\n", arg);
  return (PTPT_ATerm)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_ATermArgs accessors */

/*{{{  ATbool PTPT_isValidATermArgs(PTPT_ATermArgs arg) */

ATbool PTPT_isValidATermArgs(PTPT_ATermArgs arg)
{
  if (PTPT_isATermArgsSingle(arg)) {
    return ATtrue;
  }
  else if (PTPT_isATermArgsMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isATermArgsSingle(PTPT_ATermArgs arg) */

inline ATbool PTPT_isATermArgsSingle(PTPT_ATermArgs arg)
{
  if (ATisEmpty((ATermList)arg)) {
    return ATfalse;
  }
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternATermArgsSingle, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isATermArgsMany(PTPT_ATermArgs arg) */

inline ATbool PTPT_isATermArgsMany(PTPT_ATermArgs arg)
{
  if (ATisEmpty((ATermList)arg)) {
    return ATfalse;
  }
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternATermArgsMany, NULL, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermArgsHead(PTPT_ATermArgs arg) */

ATbool PTPT_hasATermArgsHead(PTPT_ATermArgs arg)
{
  if (PTPT_isATermArgsSingle(arg)) {
    return ATtrue;
  }
  else if (PTPT_isATermArgsMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_ATerm PTPT_getATermArgsHead(PTPT_ATermArgs arg) */

PTPT_ATerm PTPT_getATermArgsHead(PTPT_ATermArgs arg)
{
  if (PTPT_isATermArgsSingle(arg)) {
    return (PTPT_ATerm)ATgetFirst((ATermList)arg);
  }
  else 
    return (PTPT_ATerm)ATgetFirst((ATermList)arg);
}

/*}}}  */
/*{{{  PTPT_ATermArgs PTPT_setATermArgsHead(PTPT_ATermArgs arg, PTPT_ATerm head) */

PTPT_ATermArgs PTPT_setATermArgsHead(PTPT_ATermArgs arg, PTPT_ATerm head)
{
  if (PTPT_isATermArgsSingle(arg)) {
    return (PTPT_ATermArgs)ATreplace((ATermList)arg, (ATerm)head, 0);
  }
  else if (PTPT_isATermArgsMany(arg)) {
    return (PTPT_ATermArgs)ATreplace((ATermList)arg, (ATerm)head, 0);
  }

  ATabort("ATermArgs has no Head: %t\n", arg);
  return (PTPT_ATermArgs)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermArgsWsAfterFirst(PTPT_ATermArgs arg) */

ATbool PTPT_hasATermArgsWsAfterFirst(PTPT_ATermArgs arg)
{
  if (PTPT_isATermArgsMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getATermArgsWsAfterFirst(PTPT_ATermArgs arg) */

PTPT_OptLayout PTPT_getATermArgsWsAfterFirst(PTPT_ATermArgs arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)arg, 1);
}

/*}}}  */
/*{{{  PTPT_ATermArgs PTPT_setATermArgsWsAfterFirst(PTPT_ATermArgs arg, PTPT_OptLayout wsAfterFirst) */

PTPT_ATermArgs PTPT_setATermArgsWsAfterFirst(PTPT_ATermArgs arg, PTPT_OptLayout wsAfterFirst)
{
  if (PTPT_isATermArgsMany(arg)) {
    return (PTPT_ATermArgs)ATreplace((ATermList)arg, (ATerm)wsAfterFirst, 1);
  }

  ATabort("ATermArgs has no WsAfterFirst: %t\n", arg);
  return (PTPT_ATermArgs)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermArgsSep(PTPT_ATermArgs arg) */

ATbool PTPT_hasATermArgsSep(PTPT_ATermArgs arg)
{
  if (PTPT_isATermArgsMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  char * PTPT_getATermArgsSep(PTPT_ATermArgs arg) */

char * PTPT_getATermArgsSep(PTPT_ATermArgs arg)
{
  
    return (char *)ATgetName(ATgetAFun((ATermAppl)ATgetArgument((ATermAppl)ATelementAt((ATermList)arg, 2), 0)));
}

/*}}}  */
/*{{{  PTPT_ATermArgs PTPT_setATermArgsSep(PTPT_ATermArgs arg, char * sep) */

PTPT_ATermArgs PTPT_setATermArgsSep(PTPT_ATermArgs arg, char * sep)
{
  if (PTPT_isATermArgsMany(arg)) {
    return (PTPT_ATermArgs)ATreplace((ATermList)arg, (ATerm)ATsetArgument((ATermAppl)ATelementAt((ATermList)arg, 2), (ATerm)ATmakeAppl0(ATmakeAFun(sep, 0, ATtrue)), 0), 2);
  }

  ATabort("ATermArgs has no Sep: %t\n", arg);
  return (PTPT_ATermArgs)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermArgsWsAfterSep(PTPT_ATermArgs arg) */

ATbool PTPT_hasATermArgsWsAfterSep(PTPT_ATermArgs arg)
{
  if (PTPT_isATermArgsMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getATermArgsWsAfterSep(PTPT_ATermArgs arg) */

PTPT_OptLayout PTPT_getATermArgsWsAfterSep(PTPT_ATermArgs arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)arg, 3);
}

/*}}}  */
/*{{{  PTPT_ATermArgs PTPT_setATermArgsWsAfterSep(PTPT_ATermArgs arg, PTPT_OptLayout wsAfterSep) */

PTPT_ATermArgs PTPT_setATermArgsWsAfterSep(PTPT_ATermArgs arg, PTPT_OptLayout wsAfterSep)
{
  if (PTPT_isATermArgsMany(arg)) {
    return (PTPT_ATermArgs)ATreplace((ATermList)arg, (ATerm)wsAfterSep, 3);
  }

  ATabort("ATermArgs has no WsAfterSep: %t\n", arg);
  return (PTPT_ATermArgs)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermArgsTail(PTPT_ATermArgs arg) */

ATbool PTPT_hasATermArgsTail(PTPT_ATermArgs arg)
{
  if (PTPT_isATermArgsMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_ATermArgs PTPT_getATermArgsTail(PTPT_ATermArgs arg) */

PTPT_ATermArgs PTPT_getATermArgsTail(PTPT_ATermArgs arg)
{
  
    return (PTPT_ATermArgs)ATgetTail((ATermList)arg, 4);
}

/*}}}  */
/*{{{  PTPT_ATermArgs PTPT_setATermArgsTail(PTPT_ATermArgs arg, PTPT_ATermArgs tail) */

PTPT_ATermArgs PTPT_setATermArgsTail(PTPT_ATermArgs arg, PTPT_ATermArgs tail)
{
  if (PTPT_isATermArgsMany(arg)) {
    return (PTPT_ATermArgs)ATreplaceTail((ATermList)arg, (ATermList)tail, 4);
  }

  ATabort("ATermArgs has no Tail: %t\n", arg);
  return (PTPT_ATermArgs)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_Ann accessors */

/*{{{  ATbool PTPT_isValidAnn(PTPT_Ann arg) */

ATbool PTPT_isValidAnn(PTPT_Ann arg)
{
  if (PTPT_isAnnAnnotation(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isAnnAnnotation(PTPT_Ann arg) */

inline ATbool PTPT_isAnnAnnotation(PTPT_Ann arg)
{
#ifndef DISABLE_DYNAMIC_CHECKING
  assert(arg != NULL);
  assert(ATmatchTerm((ATerm)arg, PTPT_patternAnnAnnotation, NULL, NULL, NULL));
#endif
  return ATtrue;
}

/*}}}  */
/*{{{  ATbool PTPT_hasAnnWsAfterBraceOpen(PTPT_Ann arg) */

ATbool PTPT_hasAnnWsAfterBraceOpen(PTPT_Ann arg)
{
  if (PTPT_isAnnAnnotation(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getAnnWsAfterBraceOpen(PTPT_Ann arg) */

PTPT_OptLayout PTPT_getAnnWsAfterBraceOpen(PTPT_Ann arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Ann PTPT_setAnnWsAfterBraceOpen(PTPT_Ann arg, PTPT_OptLayout wsAfterBraceOpen) */

PTPT_Ann PTPT_setAnnWsAfterBraceOpen(PTPT_Ann arg, PTPT_OptLayout wsAfterBraceOpen)
{
  if (PTPT_isAnnAnnotation(arg)) {
    return (PTPT_Ann)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterBraceOpen, 1), 1);
  }

  ATabort("Ann has no WsAfterBraceOpen: %t\n", arg);
  return (PTPT_Ann)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasAnnAnnos(PTPT_Ann arg) */

ATbool PTPT_hasAnnAnnos(PTPT_Ann arg)
{
  if (PTPT_isAnnAnnotation(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_ATermAnnos PTPT_getAnnAnnos(PTPT_Ann arg) */

PTPT_ATermAnnos PTPT_getAnnAnnos(PTPT_Ann arg)
{
  
    return (PTPT_ATermAnnos)ATgetArgument((ATermAppl)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 2), 1);
}

/*}}}  */
/*{{{  PTPT_Ann PTPT_setAnnAnnos(PTPT_Ann arg, PTPT_ATermAnnos annos) */

PTPT_Ann PTPT_setAnnAnnos(PTPT_Ann arg, PTPT_ATermAnnos annos)
{
  if (PTPT_isAnnAnnotation(arg)) {
    return (PTPT_Ann)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)ATsetArgument((ATermAppl)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 2), (ATerm)annos, 1), 2), 1);
  }

  ATabort("Ann has no Annos: %t\n", arg);
  return (PTPT_Ann)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasAnnWsAfterAnnos(PTPT_Ann arg) */

ATbool PTPT_hasAnnWsAfterAnnos(PTPT_Ann arg)
{
  if (PTPT_isAnnAnnotation(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getAnnWsAfterAnnos(PTPT_Ann arg) */

PTPT_OptLayout PTPT_getAnnWsAfterAnnos(PTPT_Ann arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
}

/*}}}  */
/*{{{  PTPT_Ann PTPT_setAnnWsAfterAnnos(PTPT_Ann arg, PTPT_OptLayout wsAfterAnnos) */

PTPT_Ann PTPT_setAnnWsAfterAnnos(PTPT_Ann arg, PTPT_OptLayout wsAfterAnnos)
{
  if (PTPT_isAnnAnnotation(arg)) {
    return (PTPT_Ann)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterAnnos, 3), 1);
  }

  ATabort("Ann has no WsAfterAnnos: %t\n", arg);
  return (PTPT_Ann)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_ATermAnnos accessors */

/*{{{  ATbool PTPT_isValidATermAnnos(PTPT_ATermAnnos arg) */

ATbool PTPT_isValidATermAnnos(PTPT_ATermAnnos arg)
{
  if (PTPT_isATermAnnosSingle(arg)) {
    return ATtrue;
  }
  else if (PTPT_isATermAnnosMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isATermAnnosSingle(PTPT_ATermAnnos arg) */

inline ATbool PTPT_isATermAnnosSingle(PTPT_ATermAnnos arg)
{
  if (ATisEmpty((ATermList)arg)) {
    return ATfalse;
  }
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternATermAnnosSingle, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isATermAnnosMany(PTPT_ATermAnnos arg) */

inline ATbool PTPT_isATermAnnosMany(PTPT_ATermAnnos arg)
{
  if (ATisEmpty((ATermList)arg)) {
    return ATfalse;
  }
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternATermAnnosMany, NULL, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermAnnosHead(PTPT_ATermAnnos arg) */

ATbool PTPT_hasATermAnnosHead(PTPT_ATermAnnos arg)
{
  if (PTPT_isATermAnnosSingle(arg)) {
    return ATtrue;
  }
  else if (PTPT_isATermAnnosMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_ATerm PTPT_getATermAnnosHead(PTPT_ATermAnnos arg) */

PTPT_ATerm PTPT_getATermAnnosHead(PTPT_ATermAnnos arg)
{
  if (PTPT_isATermAnnosSingle(arg)) {
    return (PTPT_ATerm)ATgetFirst((ATermList)arg);
  }
  else 
    return (PTPT_ATerm)ATgetFirst((ATermList)arg);
}

/*}}}  */
/*{{{  PTPT_ATermAnnos PTPT_setATermAnnosHead(PTPT_ATermAnnos arg, PTPT_ATerm head) */

PTPT_ATermAnnos PTPT_setATermAnnosHead(PTPT_ATermAnnos arg, PTPT_ATerm head)
{
  if (PTPT_isATermAnnosSingle(arg)) {
    return (PTPT_ATermAnnos)ATreplace((ATermList)arg, (ATerm)head, 0);
  }
  else if (PTPT_isATermAnnosMany(arg)) {
    return (PTPT_ATermAnnos)ATreplace((ATermList)arg, (ATerm)head, 0);
  }

  ATabort("ATermAnnos has no Head: %t\n", arg);
  return (PTPT_ATermAnnos)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermAnnosWsAfterFirst(PTPT_ATermAnnos arg) */

ATbool PTPT_hasATermAnnosWsAfterFirst(PTPT_ATermAnnos arg)
{
  if (PTPT_isATermAnnosMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getATermAnnosWsAfterFirst(PTPT_ATermAnnos arg) */

PTPT_OptLayout PTPT_getATermAnnosWsAfterFirst(PTPT_ATermAnnos arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)arg, 1);
}

/*}}}  */
/*{{{  PTPT_ATermAnnos PTPT_setATermAnnosWsAfterFirst(PTPT_ATermAnnos arg, PTPT_OptLayout wsAfterFirst) */

PTPT_ATermAnnos PTPT_setATermAnnosWsAfterFirst(PTPT_ATermAnnos arg, PTPT_OptLayout wsAfterFirst)
{
  if (PTPT_isATermAnnosMany(arg)) {
    return (PTPT_ATermAnnos)ATreplace((ATermList)arg, (ATerm)wsAfterFirst, 1);
  }

  ATabort("ATermAnnos has no WsAfterFirst: %t\n", arg);
  return (PTPT_ATermAnnos)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermAnnosSep(PTPT_ATermAnnos arg) */

ATbool PTPT_hasATermAnnosSep(PTPT_ATermAnnos arg)
{
  if (PTPT_isATermAnnosMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  char * PTPT_getATermAnnosSep(PTPT_ATermAnnos arg) */

char * PTPT_getATermAnnosSep(PTPT_ATermAnnos arg)
{
  
    return (char *)ATgetName(ATgetAFun((ATermAppl)ATgetArgument((ATermAppl)ATelementAt((ATermList)arg, 2), 0)));
}

/*}}}  */
/*{{{  PTPT_ATermAnnos PTPT_setATermAnnosSep(PTPT_ATermAnnos arg, char * sep) */

PTPT_ATermAnnos PTPT_setATermAnnosSep(PTPT_ATermAnnos arg, char * sep)
{
  if (PTPT_isATermAnnosMany(arg)) {
    return (PTPT_ATermAnnos)ATreplace((ATermList)arg, (ATerm)ATsetArgument((ATermAppl)ATelementAt((ATermList)arg, 2), (ATerm)ATmakeAppl0(ATmakeAFun(sep, 0, ATtrue)), 0), 2);
  }

  ATabort("ATermAnnos has no Sep: %t\n", arg);
  return (PTPT_ATermAnnos)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermAnnosWsAfterSep(PTPT_ATermAnnos arg) */

ATbool PTPT_hasATermAnnosWsAfterSep(PTPT_ATermAnnos arg)
{
  if (PTPT_isATermAnnosMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getATermAnnosWsAfterSep(PTPT_ATermAnnos arg) */

PTPT_OptLayout PTPT_getATermAnnosWsAfterSep(PTPT_ATermAnnos arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)arg, 3);
}

/*}}}  */
/*{{{  PTPT_ATermAnnos PTPT_setATermAnnosWsAfterSep(PTPT_ATermAnnos arg, PTPT_OptLayout wsAfterSep) */

PTPT_ATermAnnos PTPT_setATermAnnosWsAfterSep(PTPT_ATermAnnos arg, PTPT_OptLayout wsAfterSep)
{
  if (PTPT_isATermAnnosMany(arg)) {
    return (PTPT_ATermAnnos)ATreplace((ATermList)arg, (ATerm)wsAfterSep, 3);
  }

  ATabort("ATermAnnos has no WsAfterSep: %t\n", arg);
  return (PTPT_ATermAnnos)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasATermAnnosTail(PTPT_ATermAnnos arg) */

ATbool PTPT_hasATermAnnosTail(PTPT_ATermAnnos arg)
{
  if (PTPT_isATermAnnosMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_ATermAnnos PTPT_getATermAnnosTail(PTPT_ATermAnnos arg) */

PTPT_ATermAnnos PTPT_getATermAnnosTail(PTPT_ATermAnnos arg)
{
  
    return (PTPT_ATermAnnos)ATgetTail((ATermList)arg, 4);
}

/*}}}  */
/*{{{  PTPT_ATermAnnos PTPT_setATermAnnosTail(PTPT_ATermAnnos arg, PTPT_ATermAnnos tail) */

PTPT_ATermAnnos PTPT_setATermAnnosTail(PTPT_ATermAnnos arg, PTPT_ATermAnnos tail)
{
  if (PTPT_isATermAnnosMany(arg)) {
    return (PTPT_ATermAnnos)ATreplaceTail((ATermList)arg, (ATermList)tail, 4);
  }

  ATabort("ATermAnnos has no Tail: %t\n", arg);
  return (PTPT_ATermAnnos)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_AlphaNumericalEscChar accessors */

/*{{{  ATbool PTPT_isValidAlphaNumericalEscChar(PTPT_AlphaNumericalEscChar arg) */

ATbool PTPT_isValidAlphaNumericalEscChar(PTPT_AlphaNumericalEscChar arg)
{
  if (PTPT_isAlphaNumericalEscCharDefault(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isAlphaNumericalEscCharDefault(PTPT_AlphaNumericalEscChar arg) */

inline ATbool PTPT_isAlphaNumericalEscCharDefault(PTPT_AlphaNumericalEscChar arg)
{
#ifndef DISABLE_DYNAMIC_CHECKING
  assert(arg != NULL);
  assert(ATmatchTerm((ATerm)arg, PTPT_patternAlphaNumericalEscCharDefault, NULL));
#endif
  return ATtrue;
}

/*}}}  */
/*{{{  ATbool PTPT_hasAlphaNumericalEscCharChars(PTPT_AlphaNumericalEscChar arg) */

ATbool PTPT_hasAlphaNumericalEscCharChars(PTPT_AlphaNumericalEscChar arg)
{
  if (PTPT_isAlphaNumericalEscCharDefault(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_CHARLIST PTPT_getAlphaNumericalEscCharChars(PTPT_AlphaNumericalEscChar arg) */

PTPT_CHARLIST PTPT_getAlphaNumericalEscCharChars(PTPT_AlphaNumericalEscChar arg)
{
  
    return (PTPT_CHARLIST)ATgetArgument((ATermAppl)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1)), 1);
}

/*}}}  */
/*{{{  PTPT_AlphaNumericalEscChar PTPT_setAlphaNumericalEscCharChars(PTPT_AlphaNumericalEscChar arg, PTPT_CHARLIST chars) */

PTPT_AlphaNumericalEscChar PTPT_setAlphaNumericalEscCharChars(PTPT_AlphaNumericalEscChar arg, PTPT_CHARLIST chars)
{
  if (PTPT_isAlphaNumericalEscCharDefault(arg)) {
    return (PTPT_AlphaNumericalEscChar)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)ATsetArgument((ATermAppl)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1)), (ATerm)chars, 1), 0), 1);
  }

  ATabort("AlphaNumericalEscChar has no Chars: %t\n", arg);
  return (PTPT_AlphaNumericalEscChar)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_DecimalEscChar accessors */

/*{{{  ATbool PTPT_isValidDecimalEscChar(PTPT_DecimalEscChar arg) */

ATbool PTPT_isValidDecimalEscChar(PTPT_DecimalEscChar arg)
{
  if (PTPT_isDecimalEscCharDec0Underscore199(arg)) {
    return ATtrue;
  }
  else if (PTPT_isDecimalEscCharDec200Underscore249(arg)) {
    return ATtrue;
  }
  else if (PTPT_isDecimalEscCharDec250Underscore255(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isDecimalEscCharDec0Underscore199(PTPT_DecimalEscChar arg) */

inline ATbool PTPT_isDecimalEscCharDec0Underscore199(PTPT_DecimalEscChar arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternDecimalEscCharDec0Underscore199, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isDecimalEscCharDec200Underscore249(PTPT_DecimalEscChar arg) */

inline ATbool PTPT_isDecimalEscCharDec200Underscore249(PTPT_DecimalEscChar arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternDecimalEscCharDec200Underscore249, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isDecimalEscCharDec250Underscore255(PTPT_DecimalEscChar arg) */

inline ATbool PTPT_isDecimalEscCharDec250Underscore255(PTPT_DecimalEscChar arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternDecimalEscCharDec250Underscore255, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  ATbool PTPT_hasDecimalEscCharChars(PTPT_DecimalEscChar arg) */

ATbool PTPT_hasDecimalEscCharChars(PTPT_DecimalEscChar arg)
{
  if (PTPT_isDecimalEscCharDec0Underscore199(arg)) {
    return ATtrue;
  }
  else if (PTPT_isDecimalEscCharDec200Underscore249(arg)) {
    return ATtrue;
  }
  else if (PTPT_isDecimalEscCharDec250Underscore255(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_CHARLIST PTPT_getDecimalEscCharChars(PTPT_DecimalEscChar arg) */

PTPT_CHARLIST PTPT_getDecimalEscCharChars(PTPT_DecimalEscChar arg)
{
  if (PTPT_isDecimalEscCharDec0Underscore199(arg)) {
    return (PTPT_CHARLIST)ATgetArgument((ATermAppl)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1)), 1);
  }
  else if (PTPT_isDecimalEscCharDec200Underscore249(arg)) {
    return (PTPT_CHARLIST)ATgetArgument((ATermAppl)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1)), 1);
  }
  else 
    return (PTPT_CHARLIST)ATgetArgument((ATermAppl)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1)), 1);
}

/*}}}  */
/*{{{  PTPT_DecimalEscChar PTPT_setDecimalEscCharChars(PTPT_DecimalEscChar arg, PTPT_CHARLIST chars) */

PTPT_DecimalEscChar PTPT_setDecimalEscCharChars(PTPT_DecimalEscChar arg, PTPT_CHARLIST chars)
{
  if (PTPT_isDecimalEscCharDec0Underscore199(arg)) {
    return (PTPT_DecimalEscChar)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)ATsetArgument((ATermAppl)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1)), (ATerm)chars, 1), 0), 1);
  }
  else if (PTPT_isDecimalEscCharDec200Underscore249(arg)) {
    return (PTPT_DecimalEscChar)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)ATsetArgument((ATermAppl)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1)), (ATerm)chars, 1), 0), 1);
  }
  else if (PTPT_isDecimalEscCharDec250Underscore255(arg)) {
    return (PTPT_DecimalEscChar)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)ATsetArgument((ATermAppl)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1)), (ATerm)chars, 1), 0), 1);
  }

  ATabort("DecimalEscChar has no Chars: %t\n", arg);
  return (PTPT_DecimalEscChar)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_EscChar accessors */

/*{{{  ATbool PTPT_isValidEscChar(PTPT_EscChar arg) */

ATbool PTPT_isValidEscChar(PTPT_EscChar arg)
{
  if (PTPT_isEscCharAlphaNumeric(arg)) {
    return ATtrue;
  }
  else if (PTPT_isEscCharDecimal(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isEscCharAlphaNumeric(PTPT_EscChar arg) */

inline ATbool PTPT_isEscCharAlphaNumeric(PTPT_EscChar arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternEscCharAlphaNumeric, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isEscCharDecimal(PTPT_EscChar arg) */

inline ATbool PTPT_isEscCharDecimal(PTPT_EscChar arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternEscCharDecimal, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  ATbool PTPT_hasEscCharChars(PTPT_EscChar arg) */

ATbool PTPT_hasEscCharChars(PTPT_EscChar arg)
{
  if (PTPT_isEscCharAlphaNumeric(arg)) {
    return ATtrue;
  }
  else if (PTPT_isEscCharDecimal(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_CHARLIST PTPT_getEscCharChars(PTPT_EscChar arg) */

PTPT_CHARLIST PTPT_getEscCharChars(PTPT_EscChar arg)
{
  if (PTPT_isEscCharAlphaNumeric(arg)) {
    return (PTPT_CHARLIST)ATgetArgument((ATermAppl)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1)), 1);
  }
  else 
    return (PTPT_CHARLIST)ATgetArgument((ATermAppl)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1)), 1);
}

/*}}}  */
/*{{{  PTPT_EscChar PTPT_setEscCharChars(PTPT_EscChar arg, PTPT_CHARLIST chars) */

PTPT_EscChar PTPT_setEscCharChars(PTPT_EscChar arg, PTPT_CHARLIST chars)
{
  if (PTPT_isEscCharAlphaNumeric(arg)) {
    return (PTPT_EscChar)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)ATsetArgument((ATermAppl)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1)), (ATerm)chars, 1), 0), 1);
  }
  else if (PTPT_isEscCharDecimal(arg)) {
    return (PTPT_EscChar)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)ATsetArgument((ATermAppl)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1)), (ATerm)chars, 1), 0), 1);
  }

  ATabort("EscChar has no Chars: %t\n", arg);
  return (PTPT_EscChar)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_LChar accessors */

/*{{{  ATbool PTPT_isValidLChar(PTPT_LChar arg) */

ATbool PTPT_isValidLChar(PTPT_LChar arg)
{
  if (PTPT_isLCharNormal(arg)) {
    return ATtrue;
  }
  else if (PTPT_isLCharEscaped(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isLCharNormal(PTPT_LChar arg) */

inline ATbool PTPT_isLCharNormal(PTPT_LChar arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternLCharNormal, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isLCharEscaped(PTPT_LChar arg) */

inline ATbool PTPT_isLCharEscaped(PTPT_LChar arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternLCharEscaped, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  ATbool PTPT_hasLCharChars(PTPT_LChar arg) */

ATbool PTPT_hasLCharChars(PTPT_LChar arg)
{
  if (PTPT_isLCharNormal(arg)) {
    return ATtrue;
  }
  else if (PTPT_isLCharEscaped(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_CHARLIST PTPT_getLCharChars(PTPT_LChar arg) */

PTPT_CHARLIST PTPT_getLCharChars(PTPT_LChar arg)
{
  if (PTPT_isLCharNormal(arg)) {
    return (PTPT_CHARLIST)ATgetArgument((ATermAppl)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1)), 1);
  }
  else 
    return (PTPT_CHARLIST)ATgetArgument((ATermAppl)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1)), 1);
}

/*}}}  */
/*{{{  PTPT_LChar PTPT_setLCharChars(PTPT_LChar arg, PTPT_CHARLIST chars) */

PTPT_LChar PTPT_setLCharChars(PTPT_LChar arg, PTPT_CHARLIST chars)
{
  if (PTPT_isLCharNormal(arg)) {
    return (PTPT_LChar)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)ATsetArgument((ATermAppl)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1)), (ATerm)chars, 1), 0), 1);
  }
  else if (PTPT_isLCharEscaped(arg)) {
    return (PTPT_LChar)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)ATsetArgument((ATermAppl)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1)), (ATerm)chars, 1), 0), 1);
  }

  ATabort("LChar has no Chars: %t\n", arg);
  return (PTPT_LChar)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_QLiteral accessors */

/*{{{  ATbool PTPT_isValidQLiteral(PTPT_QLiteral arg) */

ATbool PTPT_isValidQLiteral(PTPT_QLiteral arg)
{
  if (PTPT_isQLiteralQuoted(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isQLiteralQuoted(PTPT_QLiteral arg) */

inline ATbool PTPT_isQLiteralQuoted(PTPT_QLiteral arg)
{
#ifndef DISABLE_DYNAMIC_CHECKING
  assert(arg != NULL);
  assert(ATmatchTerm((ATerm)arg, PTPT_patternQLiteralQuoted, NULL));
#endif
  return ATtrue;
}

/*}}}  */
/*{{{  ATbool PTPT_hasQLiteralChars(PTPT_QLiteral arg) */

ATbool PTPT_hasQLiteralChars(PTPT_QLiteral arg)
{
  if (PTPT_isQLiteralQuoted(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_CHARLIST PTPT_getQLiteralChars(PTPT_QLiteral arg) */

PTPT_CHARLIST PTPT_getQLiteralChars(PTPT_QLiteral arg)
{
  
    return (PTPT_CHARLIST)ATgetArgument((ATermAppl)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1)), 1);
}

/*}}}  */
/*{{{  PTPT_QLiteral PTPT_setQLiteralChars(PTPT_QLiteral arg, PTPT_CHARLIST chars) */

PTPT_QLiteral PTPT_setQLiteralChars(PTPT_QLiteral arg, PTPT_CHARLIST chars)
{
  if (PTPT_isQLiteralQuoted(arg)) {
    return (PTPT_QLiteral)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)ATsetArgument((ATermAppl)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1)), (ATerm)chars, 1), 0), 1);
  }

  ATabort("QLiteral has no Chars: %t\n", arg);
  return (PTPT_QLiteral)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_UQLiteral accessors */

/*{{{  ATbool PTPT_isValidUQLiteral(PTPT_UQLiteral arg) */

ATbool PTPT_isValidUQLiteral(PTPT_UQLiteral arg)
{
  if (PTPT_isUQLiteralOneChar(arg)) {
    return ATtrue;
  }
  else if (PTPT_isUQLiteralMoreChars(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isUQLiteralOneChar(PTPT_UQLiteral arg) */

inline ATbool PTPT_isUQLiteralOneChar(PTPT_UQLiteral arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternUQLiteralOneChar, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isUQLiteralMoreChars(PTPT_UQLiteral arg) */

inline ATbool PTPT_isUQLiteralMoreChars(PTPT_UQLiteral arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternUQLiteralMoreChars, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  ATbool PTPT_hasUQLiteralChars(PTPT_UQLiteral arg) */

ATbool PTPT_hasUQLiteralChars(PTPT_UQLiteral arg)
{
  if (PTPT_isUQLiteralOneChar(arg)) {
    return ATtrue;
  }
  else if (PTPT_isUQLiteralMoreChars(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_CHARLIST PTPT_getUQLiteralChars(PTPT_UQLiteral arg) */

PTPT_CHARLIST PTPT_getUQLiteralChars(PTPT_UQLiteral arg)
{
  if (PTPT_isUQLiteralOneChar(arg)) {
    return (PTPT_CHARLIST)ATgetArgument((ATermAppl)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1)), 1);
  }
  else 
    return (PTPT_CHARLIST)ATgetArgument((ATermAppl)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1)), 1);
}

/*}}}  */
/*{{{  PTPT_UQLiteral PTPT_setUQLiteralChars(PTPT_UQLiteral arg, PTPT_CHARLIST chars) */

PTPT_UQLiteral PTPT_setUQLiteralChars(PTPT_UQLiteral arg, PTPT_CHARLIST chars)
{
  if (PTPT_isUQLiteralOneChar(arg)) {
    return (PTPT_UQLiteral)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)ATsetArgument((ATermAppl)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1)), (ATerm)chars, 1), 0), 1);
  }
  else if (PTPT_isUQLiteralMoreChars(arg)) {
    return (PTPT_UQLiteral)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)ATsetArgument((ATermAppl)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1)), (ATerm)chars, 1), 0), 1);
  }

  ATabort("UQLiteral has no Chars: %t\n", arg);
  return (PTPT_UQLiteral)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_Literal accessors */

/*{{{  ATbool PTPT_isValidLiteral(PTPT_Literal arg) */

ATbool PTPT_isValidLiteral(PTPT_Literal arg)
{
  if (PTPT_isLiteralQlit(arg)) {
    return ATtrue;
  }
  else if (PTPT_isLiteralUqlit(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isLiteralQlit(PTPT_Literal arg) */

inline ATbool PTPT_isLiteralQlit(PTPT_Literal arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternLiteralQlit, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isLiteralUqlit(PTPT_Literal arg) */

inline ATbool PTPT_isLiteralUqlit(PTPT_Literal arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternLiteralUqlit, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  ATbool PTPT_hasLiteralQLiteral(PTPT_Literal arg) */

ATbool PTPT_hasLiteralQLiteral(PTPT_Literal arg)
{
  if (PTPT_isLiteralQlit(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_QLiteral PTPT_getLiteralQLiteral(PTPT_Literal arg) */

PTPT_QLiteral PTPT_getLiteralQLiteral(PTPT_Literal arg)
{
  
    return (PTPT_QLiteral)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1));
}

/*}}}  */
/*{{{  PTPT_Literal PTPT_setLiteralQLiteral(PTPT_Literal arg, PTPT_QLiteral QLiteral) */

PTPT_Literal PTPT_setLiteralQLiteral(PTPT_Literal arg, PTPT_QLiteral QLiteral)
{
  if (PTPT_isLiteralQlit(arg)) {
    return (PTPT_Literal)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)QLiteral, 0), 1);
  }

  ATabort("Literal has no QLiteral: %t\n", arg);
  return (PTPT_Literal)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasLiteralUQLiteral(PTPT_Literal arg) */

ATbool PTPT_hasLiteralUQLiteral(PTPT_Literal arg)
{
  if (PTPT_isLiteralUqlit(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_UQLiteral PTPT_getLiteralUQLiteral(PTPT_Literal arg) */

PTPT_UQLiteral PTPT_getLiteralUQLiteral(PTPT_Literal arg)
{
  
    return (PTPT_UQLiteral)ATgetFirst((ATermList)ATgetArgument((ATermAppl)arg, 1));
}

/*}}}  */
/*{{{  PTPT_Literal PTPT_setLiteralUQLiteral(PTPT_Literal arg, PTPT_UQLiteral UQLiteral) */

PTPT_Literal PTPT_setLiteralUQLiteral(PTPT_Literal arg, PTPT_UQLiteral UQLiteral)
{
  if (PTPT_isLiteralUqlit(arg)) {
    return (PTPT_Literal)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)UQLiteral, 0), 1);
  }

  ATabort("Literal has no UQLiteral: %t\n", arg);
  return (PTPT_Literal)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_Attributes accessors */

/*{{{  ATbool PTPT_isValidAttributes(PTPT_Attributes arg) */

ATbool PTPT_isValidAttributes(PTPT_Attributes arg)
{
  if (PTPT_isAttributesNoAttrs(arg)) {
    return ATtrue;
  }
  else if (PTPT_isAttributesAttrs(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isAttributesNoAttrs(PTPT_Attributes arg) */

inline ATbool PTPT_isAttributesNoAttrs(PTPT_Attributes arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternAttributesNoAttrs);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isAttributesAttrs(PTPT_Attributes arg) */

inline ATbool PTPT_isAttributesAttrs(PTPT_Attributes arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternAttributesAttrs, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  ATbool PTPT_hasAttributesWsAfterAttrs(PTPT_Attributes arg) */

ATbool PTPT_hasAttributesWsAfterAttrs(PTPT_Attributes arg)
{
  if (PTPT_isAttributesAttrs(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getAttributesWsAfterAttrs(PTPT_Attributes arg) */

PTPT_OptLayout PTPT_getAttributesWsAfterAttrs(PTPT_Attributes arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Attributes PTPT_setAttributesWsAfterAttrs(PTPT_Attributes arg, PTPT_OptLayout wsAfterAttrs) */

PTPT_Attributes PTPT_setAttributesWsAfterAttrs(PTPT_Attributes arg, PTPT_OptLayout wsAfterAttrs)
{
  if (PTPT_isAttributesAttrs(arg)) {
    return (PTPT_Attributes)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterAttrs, 1), 1);
  }

  ATabort("Attributes has no WsAfterAttrs: %t\n", arg);
  return (PTPT_Attributes)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasAttributesWsAfterParenOpen(PTPT_Attributes arg) */

ATbool PTPT_hasAttributesWsAfterParenOpen(PTPT_Attributes arg)
{
  if (PTPT_isAttributesAttrs(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getAttributesWsAfterParenOpen(PTPT_Attributes arg) */

PTPT_OptLayout PTPT_getAttributesWsAfterParenOpen(PTPT_Attributes arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
}

/*}}}  */
/*{{{  PTPT_Attributes PTPT_setAttributesWsAfterParenOpen(PTPT_Attributes arg, PTPT_OptLayout wsAfterParenOpen) */

PTPT_Attributes PTPT_setAttributesWsAfterParenOpen(PTPT_Attributes arg, PTPT_OptLayout wsAfterParenOpen)
{
  if (PTPT_isAttributesAttrs(arg)) {
    return (PTPT_Attributes)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }

  ATabort("Attributes has no WsAfterParenOpen: %t\n", arg);
  return (PTPT_Attributes)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasAttributesAttributes(PTPT_Attributes arg) */

ATbool PTPT_hasAttributesAttributes(PTPT_Attributes arg)
{
  if (PTPT_isAttributesAttrs(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Attrs PTPT_getAttributesAttributes(PTPT_Attributes arg) */

PTPT_Attrs PTPT_getAttributesAttributes(PTPT_Attributes arg)
{
  
    return (PTPT_Attrs)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
}

/*}}}  */
/*{{{  PTPT_Attributes PTPT_setAttributesAttributes(PTPT_Attributes arg, PTPT_Attrs attributes) */

PTPT_Attributes PTPT_setAttributesAttributes(PTPT_Attributes arg, PTPT_Attrs attributes)
{
  if (PTPT_isAttributesAttrs(arg)) {
    return (PTPT_Attributes)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)attributes, 4), 1);
  }

  ATabort("Attributes has no Attributes: %t\n", arg);
  return (PTPT_Attributes)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasAttributesWsAfterAttributes(PTPT_Attributes arg) */

ATbool PTPT_hasAttributesWsAfterAttributes(PTPT_Attributes arg)
{
  if (PTPT_isAttributesAttrs(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getAttributesWsAfterAttributes(PTPT_Attributes arg) */

PTPT_OptLayout PTPT_getAttributesWsAfterAttributes(PTPT_Attributes arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
}

/*}}}  */
/*{{{  PTPT_Attributes PTPT_setAttributesWsAfterAttributes(PTPT_Attributes arg, PTPT_OptLayout wsAfterAttributes) */

PTPT_Attributes PTPT_setAttributesWsAfterAttributes(PTPT_Attributes arg, PTPT_OptLayout wsAfterAttributes)
{
  if (PTPT_isAttributesAttrs(arg)) {
    return (PTPT_Attributes)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterAttributes, 5), 1);
  }

  ATabort("Attributes has no WsAfterAttributes: %t\n", arg);
  return (PTPT_Attributes)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_Attrs accessors */

/*{{{  ATbool PTPT_isValidAttrs(PTPT_Attrs arg) */

ATbool PTPT_isValidAttrs(PTPT_Attrs arg)
{
  if (PTPT_isAttrsMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isAttrsMany(PTPT_Attrs arg) */

inline ATbool PTPT_isAttrsMany(PTPT_Attrs arg)
{
#ifndef DISABLE_DYNAMIC_CHECKING
  assert(arg != NULL);
  assert(ATmatchTerm((ATerm)arg, PTPT_patternAttrsMany, NULL, NULL, NULL));
#endif
  return ATtrue;
}

/*}}}  */
/*{{{  ATbool PTPT_hasAttrsWsAfterBracketOpen(PTPT_Attrs arg) */

ATbool PTPT_hasAttrsWsAfterBracketOpen(PTPT_Attrs arg)
{
  if (PTPT_isAttrsMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getAttrsWsAfterBracketOpen(PTPT_Attrs arg) */

PTPT_OptLayout PTPT_getAttrsWsAfterBracketOpen(PTPT_Attrs arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Attrs PTPT_setAttrsWsAfterBracketOpen(PTPT_Attrs arg, PTPT_OptLayout wsAfterBracketOpen) */

PTPT_Attrs PTPT_setAttrsWsAfterBracketOpen(PTPT_Attrs arg, PTPT_OptLayout wsAfterBracketOpen)
{
  if (PTPT_isAttrsMany(arg)) {
    return (PTPT_Attrs)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterBracketOpen, 1), 1);
  }

  ATabort("Attrs has no WsAfterBracketOpen: %t\n", arg);
  return (PTPT_Attrs)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasAttrsList(PTPT_Attrs arg) */

ATbool PTPT_hasAttrsList(PTPT_Attrs arg)
{
  if (PTPT_isAttrsMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_AttrList PTPT_getAttrsList(PTPT_Attrs arg) */

PTPT_AttrList PTPT_getAttrsList(PTPT_Attrs arg)
{
  
    return (PTPT_AttrList)ATgetArgument((ATermAppl)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 2), 1);
}

/*}}}  */
/*{{{  PTPT_Attrs PTPT_setAttrsList(PTPT_Attrs arg, PTPT_AttrList list) */

PTPT_Attrs PTPT_setAttrsList(PTPT_Attrs arg, PTPT_AttrList list)
{
  if (PTPT_isAttrsMany(arg)) {
    return (PTPT_Attrs)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)ATsetArgument((ATermAppl)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 2), (ATerm)list, 1), 2), 1);
  }

  ATabort("Attrs has no List: %t\n", arg);
  return (PTPT_Attrs)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasAttrsWsAfterList(PTPT_Attrs arg) */

ATbool PTPT_hasAttrsWsAfterList(PTPT_Attrs arg)
{
  if (PTPT_isAttrsMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getAttrsWsAfterList(PTPT_Attrs arg) */

PTPT_OptLayout PTPT_getAttrsWsAfterList(PTPT_Attrs arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
}

/*}}}  */
/*{{{  PTPT_Attrs PTPT_setAttrsWsAfterList(PTPT_Attrs arg, PTPT_OptLayout wsAfterList) */

PTPT_Attrs PTPT_setAttrsWsAfterList(PTPT_Attrs arg, PTPT_OptLayout wsAfterList)
{
  if (PTPT_isAttrsMany(arg)) {
    return (PTPT_Attrs)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterList, 3), 1);
  }

  ATabort("Attrs has no WsAfterList: %t\n", arg);
  return (PTPT_Attrs)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_AttrList accessors */

/*{{{  ATbool PTPT_isValidAttrList(PTPT_AttrList arg) */

ATbool PTPT_isValidAttrList(PTPT_AttrList arg)
{
  if (PTPT_isAttrListSingle(arg)) {
    return ATtrue;
  }
  else if (PTPT_isAttrListMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isAttrListSingle(PTPT_AttrList arg) */

inline ATbool PTPT_isAttrListSingle(PTPT_AttrList arg)
{
  if (ATisEmpty((ATermList)arg)) {
    return ATfalse;
  }
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternAttrListSingle, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isAttrListMany(PTPT_AttrList arg) */

inline ATbool PTPT_isAttrListMany(PTPT_AttrList arg)
{
  if (ATisEmpty((ATermList)arg)) {
    return ATfalse;
  }
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternAttrListMany, NULL, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  ATbool PTPT_hasAttrListHead(PTPT_AttrList arg) */

ATbool PTPT_hasAttrListHead(PTPT_AttrList arg)
{
  if (PTPT_isAttrListSingle(arg)) {
    return ATtrue;
  }
  else if (PTPT_isAttrListMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Attr PTPT_getAttrListHead(PTPT_AttrList arg) */

PTPT_Attr PTPT_getAttrListHead(PTPT_AttrList arg)
{
  if (PTPT_isAttrListSingle(arg)) {
    return (PTPT_Attr)ATgetFirst((ATermList)arg);
  }
  else 
    return (PTPT_Attr)ATgetFirst((ATermList)arg);
}

/*}}}  */
/*{{{  PTPT_AttrList PTPT_setAttrListHead(PTPT_AttrList arg, PTPT_Attr head) */

PTPT_AttrList PTPT_setAttrListHead(PTPT_AttrList arg, PTPT_Attr head)
{
  if (PTPT_isAttrListSingle(arg)) {
    return (PTPT_AttrList)ATreplace((ATermList)arg, (ATerm)head, 0);
  }
  else if (PTPT_isAttrListMany(arg)) {
    return (PTPT_AttrList)ATreplace((ATermList)arg, (ATerm)head, 0);
  }

  ATabort("AttrList has no Head: %t\n", arg);
  return (PTPT_AttrList)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasAttrListWsAfterFirst(PTPT_AttrList arg) */

ATbool PTPT_hasAttrListWsAfterFirst(PTPT_AttrList arg)
{
  if (PTPT_isAttrListMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getAttrListWsAfterFirst(PTPT_AttrList arg) */

PTPT_OptLayout PTPT_getAttrListWsAfterFirst(PTPT_AttrList arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)arg, 1);
}

/*}}}  */
/*{{{  PTPT_AttrList PTPT_setAttrListWsAfterFirst(PTPT_AttrList arg, PTPT_OptLayout wsAfterFirst) */

PTPT_AttrList PTPT_setAttrListWsAfterFirst(PTPT_AttrList arg, PTPT_OptLayout wsAfterFirst)
{
  if (PTPT_isAttrListMany(arg)) {
    return (PTPT_AttrList)ATreplace((ATermList)arg, (ATerm)wsAfterFirst, 1);
  }

  ATabort("AttrList has no WsAfterFirst: %t\n", arg);
  return (PTPT_AttrList)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasAttrListSep(PTPT_AttrList arg) */

ATbool PTPT_hasAttrListSep(PTPT_AttrList arg)
{
  if (PTPT_isAttrListMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  char * PTPT_getAttrListSep(PTPT_AttrList arg) */

char * PTPT_getAttrListSep(PTPT_AttrList arg)
{
  
    return (char *)ATgetName(ATgetAFun((ATermAppl)ATgetArgument((ATermAppl)ATelementAt((ATermList)arg, 2), 0)));
}

/*}}}  */
/*{{{  PTPT_AttrList PTPT_setAttrListSep(PTPT_AttrList arg, char * sep) */

PTPT_AttrList PTPT_setAttrListSep(PTPT_AttrList arg, char * sep)
{
  if (PTPT_isAttrListMany(arg)) {
    return (PTPT_AttrList)ATreplace((ATermList)arg, (ATerm)ATsetArgument((ATermAppl)ATelementAt((ATermList)arg, 2), (ATerm)ATmakeAppl0(ATmakeAFun(sep, 0, ATtrue)), 0), 2);
  }

  ATabort("AttrList has no Sep: %t\n", arg);
  return (PTPT_AttrList)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasAttrListWsAfterSep(PTPT_AttrList arg) */

ATbool PTPT_hasAttrListWsAfterSep(PTPT_AttrList arg)
{
  if (PTPT_isAttrListMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getAttrListWsAfterSep(PTPT_AttrList arg) */

PTPT_OptLayout PTPT_getAttrListWsAfterSep(PTPT_AttrList arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)arg, 3);
}

/*}}}  */
/*{{{  PTPT_AttrList PTPT_setAttrListWsAfterSep(PTPT_AttrList arg, PTPT_OptLayout wsAfterSep) */

PTPT_AttrList PTPT_setAttrListWsAfterSep(PTPT_AttrList arg, PTPT_OptLayout wsAfterSep)
{
  if (PTPT_isAttrListMany(arg)) {
    return (PTPT_AttrList)ATreplace((ATermList)arg, (ATerm)wsAfterSep, 3);
  }

  ATabort("AttrList has no WsAfterSep: %t\n", arg);
  return (PTPT_AttrList)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasAttrListTail(PTPT_AttrList arg) */

ATbool PTPT_hasAttrListTail(PTPT_AttrList arg)
{
  if (PTPT_isAttrListMany(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_AttrList PTPT_getAttrListTail(PTPT_AttrList arg) */

PTPT_AttrList PTPT_getAttrListTail(PTPT_AttrList arg)
{
  
    return (PTPT_AttrList)ATgetTail((ATermList)arg, 4);
}

/*}}}  */
/*{{{  PTPT_AttrList PTPT_setAttrListTail(PTPT_AttrList arg, PTPT_AttrList tail) */

PTPT_AttrList PTPT_setAttrListTail(PTPT_AttrList arg, PTPT_AttrList tail)
{
  if (PTPT_isAttrListMany(arg)) {
    return (PTPT_AttrList)ATreplaceTail((ATermList)arg, (ATermList)tail, 4);
  }

  ATabort("AttrList has no Tail: %t\n", arg);
  return (PTPT_AttrList)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_Attr accessors */

/*{{{  ATbool PTPT_isValidAttr(PTPT_Attr arg) */

ATbool PTPT_isValidAttr(PTPT_Attr arg)
{
  if (PTPT_isAttrAssoc(arg)) {
    return ATtrue;
  }
  else if (PTPT_isAttrTerm(arg)) {
    return ATtrue;
  }
  else if (PTPT_isAttrId(arg)) {
    return ATtrue;
  }
  else if (PTPT_isAttrBracket(arg)) {
    return ATtrue;
  }
  else if (PTPT_isAttrReject(arg)) {
    return ATtrue;
  }
  else if (PTPT_isAttrPrefer(arg)) {
    return ATtrue;
  }
  else if (PTPT_isAttrAvoid(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isAttrAssoc(PTPT_Attr arg) */

inline ATbool PTPT_isAttrAssoc(PTPT_Attr arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternAttrAssoc, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isAttrTerm(PTPT_Attr arg) */

inline ATbool PTPT_isAttrTerm(PTPT_Attr arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternAttrTerm, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isAttrId(PTPT_Attr arg) */

inline ATbool PTPT_isAttrId(PTPT_Attr arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternAttrId, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isAttrBracket(PTPT_Attr arg) */

inline ATbool PTPT_isAttrBracket(PTPT_Attr arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternAttrBracket);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isAttrReject(PTPT_Attr arg) */

inline ATbool PTPT_isAttrReject(PTPT_Attr arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternAttrReject);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isAttrPrefer(PTPT_Attr arg) */

inline ATbool PTPT_isAttrPrefer(PTPT_Attr arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternAttrPrefer);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isAttrAvoid(PTPT_Attr arg) */

inline ATbool PTPT_isAttrAvoid(PTPT_Attr arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternAttrAvoid);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  ATbool PTPT_hasAttrWsAfterAssoc(PTPT_Attr arg) */

ATbool PTPT_hasAttrWsAfterAssoc(PTPT_Attr arg)
{
  if (PTPT_isAttrAssoc(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getAttrWsAfterAssoc(PTPT_Attr arg) */

PTPT_OptLayout PTPT_getAttrWsAfterAssoc(PTPT_Attr arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Attr PTPT_setAttrWsAfterAssoc(PTPT_Attr arg, PTPT_OptLayout wsAfterAssoc) */

PTPT_Attr PTPT_setAttrWsAfterAssoc(PTPT_Attr arg, PTPT_OptLayout wsAfterAssoc)
{
  if (PTPT_isAttrAssoc(arg)) {
    return (PTPT_Attr)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterAssoc, 1), 1);
  }

  ATabort("Attr has no WsAfterAssoc: %t\n", arg);
  return (PTPT_Attr)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasAttrWsAfterParenOpen(PTPT_Attr arg) */

ATbool PTPT_hasAttrWsAfterParenOpen(PTPT_Attr arg)
{
  if (PTPT_isAttrAssoc(arg)) {
    return ATtrue;
  }
  else if (PTPT_isAttrTerm(arg)) {
    return ATtrue;
  }
  else if (PTPT_isAttrId(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getAttrWsAfterParenOpen(PTPT_Attr arg) */

PTPT_OptLayout PTPT_getAttrWsAfterParenOpen(PTPT_Attr arg)
{
  if (PTPT_isAttrAssoc(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
  }
  else if (PTPT_isAttrTerm(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
  }
  else 
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
}

/*}}}  */
/*{{{  PTPT_Attr PTPT_setAttrWsAfterParenOpen(PTPT_Attr arg, PTPT_OptLayout wsAfterParenOpen) */

PTPT_Attr PTPT_setAttrWsAfterParenOpen(PTPT_Attr arg, PTPT_OptLayout wsAfterParenOpen)
{
  if (PTPT_isAttrAssoc(arg)) {
    return (PTPT_Attr)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }
  else if (PTPT_isAttrTerm(arg)) {
    return (PTPT_Attr)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }
  else if (PTPT_isAttrId(arg)) {
    return (PTPT_Attr)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }

  ATabort("Attr has no WsAfterParenOpen: %t\n", arg);
  return (PTPT_Attr)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasAttrAssociativity(PTPT_Attr arg) */

ATbool PTPT_hasAttrAssociativity(PTPT_Attr arg)
{
  if (PTPT_isAttrAssoc(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Associativity PTPT_getAttrAssociativity(PTPT_Attr arg) */

PTPT_Associativity PTPT_getAttrAssociativity(PTPT_Attr arg)
{
  
    return (PTPT_Associativity)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
}

/*}}}  */
/*{{{  PTPT_Attr PTPT_setAttrAssociativity(PTPT_Attr arg, PTPT_Associativity associativity) */

PTPT_Attr PTPT_setAttrAssociativity(PTPT_Attr arg, PTPT_Associativity associativity)
{
  if (PTPT_isAttrAssoc(arg)) {
    return (PTPT_Attr)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)associativity, 4), 1);
  }

  ATabort("Attr has no Associativity: %t\n", arg);
  return (PTPT_Attr)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasAttrWsAfterAssociativity(PTPT_Attr arg) */

ATbool PTPT_hasAttrWsAfterAssociativity(PTPT_Attr arg)
{
  if (PTPT_isAttrAssoc(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getAttrWsAfterAssociativity(PTPT_Attr arg) */

PTPT_OptLayout PTPT_getAttrWsAfterAssociativity(PTPT_Attr arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
}

/*}}}  */
/*{{{  PTPT_Attr PTPT_setAttrWsAfterAssociativity(PTPT_Attr arg, PTPT_OptLayout wsAfterAssociativity) */

PTPT_Attr PTPT_setAttrWsAfterAssociativity(PTPT_Attr arg, PTPT_OptLayout wsAfterAssociativity)
{
  if (PTPT_isAttrAssoc(arg)) {
    return (PTPT_Attr)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterAssociativity, 5), 1);
  }

  ATabort("Attr has no WsAfterAssociativity: %t\n", arg);
  return (PTPT_Attr)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasAttrWsAfterTerm(PTPT_Attr arg) */

ATbool PTPT_hasAttrWsAfterTerm(PTPT_Attr arg)
{
  if (PTPT_isAttrTerm(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getAttrWsAfterTerm(PTPT_Attr arg) */

PTPT_OptLayout PTPT_getAttrWsAfterTerm(PTPT_Attr arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Attr PTPT_setAttrWsAfterTerm(PTPT_Attr arg, PTPT_OptLayout wsAfterTerm) */

PTPT_Attr PTPT_setAttrWsAfterTerm(PTPT_Attr arg, PTPT_OptLayout wsAfterTerm)
{
  if (PTPT_isAttrTerm(arg)) {
    return (PTPT_Attr)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterTerm, 1), 1);
  }

  ATabort("Attr has no WsAfterTerm: %t\n", arg);
  return (PTPT_Attr)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasAttrAterm(PTPT_Attr arg) */

ATbool PTPT_hasAttrAterm(PTPT_Attr arg)
{
  if (PTPT_isAttrTerm(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_ATerm PTPT_getAttrAterm(PTPT_Attr arg) */

PTPT_ATerm PTPT_getAttrAterm(PTPT_Attr arg)
{
  
    return (PTPT_ATerm)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
}

/*}}}  */
/*{{{  PTPT_Attr PTPT_setAttrAterm(PTPT_Attr arg, PTPT_ATerm aterm) */

PTPT_Attr PTPT_setAttrAterm(PTPT_Attr arg, PTPT_ATerm aterm)
{
  if (PTPT_isAttrTerm(arg)) {
    return (PTPT_Attr)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)aterm, 4), 1);
  }

  ATabort("Attr has no Aterm: %t\n", arg);
  return (PTPT_Attr)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasAttrWsAfterAterm(PTPT_Attr arg) */

ATbool PTPT_hasAttrWsAfterAterm(PTPT_Attr arg)
{
  if (PTPT_isAttrTerm(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getAttrWsAfterAterm(PTPT_Attr arg) */

PTPT_OptLayout PTPT_getAttrWsAfterAterm(PTPT_Attr arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
}

/*}}}  */
/*{{{  PTPT_Attr PTPT_setAttrWsAfterAterm(PTPT_Attr arg, PTPT_OptLayout wsAfterAterm) */

PTPT_Attr PTPT_setAttrWsAfterAterm(PTPT_Attr arg, PTPT_OptLayout wsAfterAterm)
{
  if (PTPT_isAttrTerm(arg)) {
    return (PTPT_Attr)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterAterm, 5), 1);
  }

  ATabort("Attr has no WsAfterAterm: %t\n", arg);
  return (PTPT_Attr)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasAttrWsAfterId(PTPT_Attr arg) */

ATbool PTPT_hasAttrWsAfterId(PTPT_Attr arg)
{
  if (PTPT_isAttrId(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getAttrWsAfterId(PTPT_Attr arg) */

PTPT_OptLayout PTPT_getAttrWsAfterId(PTPT_Attr arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_Attr PTPT_setAttrWsAfterId(PTPT_Attr arg, PTPT_OptLayout wsAfterId) */

PTPT_Attr PTPT_setAttrWsAfterId(PTPT_Attr arg, PTPT_OptLayout wsAfterId)
{
  if (PTPT_isAttrId(arg)) {
    return (PTPT_Attr)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterId, 1), 1);
  }

  ATabort("Attr has no WsAfterId: %t\n", arg);
  return (PTPT_Attr)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasAttrModuleName(PTPT_Attr arg) */

ATbool PTPT_hasAttrModuleName(PTPT_Attr arg)
{
  if (PTPT_isAttrId(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_QLiteral PTPT_getAttrModuleName(PTPT_Attr arg) */

PTPT_QLiteral PTPT_getAttrModuleName(PTPT_Attr arg)
{
  
    return (PTPT_QLiteral)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
}

/*}}}  */
/*{{{  PTPT_Attr PTPT_setAttrModuleName(PTPT_Attr arg, PTPT_QLiteral moduleName) */

PTPT_Attr PTPT_setAttrModuleName(PTPT_Attr arg, PTPT_QLiteral moduleName)
{
  if (PTPT_isAttrId(arg)) {
    return (PTPT_Attr)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)moduleName, 4), 1);
  }

  ATabort("Attr has no ModuleName: %t\n", arg);
  return (PTPT_Attr)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasAttrWsAfterModuleName(PTPT_Attr arg) */

ATbool PTPT_hasAttrWsAfterModuleName(PTPT_Attr arg)
{
  if (PTPT_isAttrId(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getAttrWsAfterModuleName(PTPT_Attr arg) */

PTPT_OptLayout PTPT_getAttrWsAfterModuleName(PTPT_Attr arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
}

/*}}}  */
/*{{{  PTPT_Attr PTPT_setAttrWsAfterModuleName(PTPT_Attr arg, PTPT_OptLayout wsAfterModuleName) */

PTPT_Attr PTPT_setAttrWsAfterModuleName(PTPT_Attr arg, PTPT_OptLayout wsAfterModuleName)
{
  if (PTPT_isAttrId(arg)) {
    return (PTPT_Attr)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterModuleName, 5), 1);
  }

  ATabort("Attr has no WsAfterModuleName: %t\n", arg);
  return (PTPT_Attr)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_Associativity accessors */

/*{{{  ATbool PTPT_isValidAssociativity(PTPT_Associativity arg) */

ATbool PTPT_isValidAssociativity(PTPT_Associativity arg)
{
  if (PTPT_isAssociativityLeft(arg)) {
    return ATtrue;
  }
  else if (PTPT_isAssociativityRight(arg)) {
    return ATtrue;
  }
  else if (PTPT_isAssociativityAssoc(arg)) {
    return ATtrue;
  }
  else if (PTPT_isAssociativityNonAssoc(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isAssociativityLeft(PTPT_Associativity arg) */

inline ATbool PTPT_isAssociativityLeft(PTPT_Associativity arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternAssociativityLeft);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isAssociativityRight(PTPT_Associativity arg) */

inline ATbool PTPT_isAssociativityRight(PTPT_Associativity arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternAssociativityRight);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isAssociativityAssoc(PTPT_Associativity arg) */

inline ATbool PTPT_isAssociativityAssoc(PTPT_Associativity arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternAssociativityAssoc);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isAssociativityNonAssoc(PTPT_Associativity arg) */

inline ATbool PTPT_isAssociativityNonAssoc(PTPT_Associativity arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternAssociativityNonAssoc);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_ParseTree accessors */

/*{{{  ATbool PTPT_isValidParseTree(PTPT_ParseTree arg) */

ATbool PTPT_isValidParseTree(PTPT_ParseTree arg)
{
  if (PTPT_isParseTreeTop(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isParseTreeTop(PTPT_ParseTree arg) */

inline ATbool PTPT_isParseTreeTop(PTPT_ParseTree arg)
{
#ifndef DISABLE_DYNAMIC_CHECKING
  assert(arg != NULL);
  assert(ATmatchTerm((ATerm)arg, PTPT_patternParseTreeTop, NULL, NULL, NULL, NULL, NULL, NULL, NULL));
#endif
  return ATtrue;
}

/*}}}  */
/*{{{  ATbool PTPT_hasParseTreeWsAfterParsetree(PTPT_ParseTree arg) */

ATbool PTPT_hasParseTreeWsAfterParsetree(PTPT_ParseTree arg)
{
  if (PTPT_isParseTreeTop(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getParseTreeWsAfterParsetree(PTPT_ParseTree arg) */

PTPT_OptLayout PTPT_getParseTreeWsAfterParsetree(PTPT_ParseTree arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 1);
}

/*}}}  */
/*{{{  PTPT_ParseTree PTPT_setParseTreeWsAfterParsetree(PTPT_ParseTree arg, PTPT_OptLayout wsAfterParsetree) */

PTPT_ParseTree PTPT_setParseTreeWsAfterParsetree(PTPT_ParseTree arg, PTPT_OptLayout wsAfterParsetree)
{
  if (PTPT_isParseTreeTop(arg)) {
    return (PTPT_ParseTree)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParsetree, 1), 1);
  }

  ATabort("ParseTree has no WsAfterParsetree: %t\n", arg);
  return (PTPT_ParseTree)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasParseTreeWsAfterParenOpen(PTPT_ParseTree arg) */

ATbool PTPT_hasParseTreeWsAfterParenOpen(PTPT_ParseTree arg)
{
  if (PTPT_isParseTreeTop(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getParseTreeWsAfterParenOpen(PTPT_ParseTree arg) */

PTPT_OptLayout PTPT_getParseTreeWsAfterParenOpen(PTPT_ParseTree arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 3);
}

/*}}}  */
/*{{{  PTPT_ParseTree PTPT_setParseTreeWsAfterParenOpen(PTPT_ParseTree arg, PTPT_OptLayout wsAfterParenOpen) */

PTPT_ParseTree PTPT_setParseTreeWsAfterParenOpen(PTPT_ParseTree arg, PTPT_OptLayout wsAfterParenOpen)
{
  if (PTPT_isParseTreeTop(arg)) {
    return (PTPT_ParseTree)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterParenOpen, 3), 1);
  }

  ATabort("ParseTree has no WsAfterParenOpen: %t\n", arg);
  return (PTPT_ParseTree)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasParseTreeTop(PTPT_ParseTree arg) */

ATbool PTPT_hasParseTreeTop(PTPT_ParseTree arg)
{
  if (PTPT_isParseTreeTop(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Tree PTPT_getParseTreeTop(PTPT_ParseTree arg) */

PTPT_Tree PTPT_getParseTreeTop(PTPT_ParseTree arg)
{
  
    return (PTPT_Tree)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 4);
}

/*}}}  */
/*{{{  PTPT_ParseTree PTPT_setParseTreeTop(PTPT_ParseTree arg, PTPT_Tree top) */

PTPT_ParseTree PTPT_setParseTreeTop(PTPT_ParseTree arg, PTPT_Tree top)
{
  if (PTPT_isParseTreeTop(arg)) {
    return (PTPT_ParseTree)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)top, 4), 1);
  }

  ATabort("ParseTree has no Top: %t\n", arg);
  return (PTPT_ParseTree)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasParseTreeWsAfterTop(PTPT_ParseTree arg) */

ATbool PTPT_hasParseTreeWsAfterTop(PTPT_ParseTree arg)
{
  if (PTPT_isParseTreeTop(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getParseTreeWsAfterTop(PTPT_ParseTree arg) */

PTPT_OptLayout PTPT_getParseTreeWsAfterTop(PTPT_ParseTree arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 5);
}

/*}}}  */
/*{{{  PTPT_ParseTree PTPT_setParseTreeWsAfterTop(PTPT_ParseTree arg, PTPT_OptLayout wsAfterTop) */

PTPT_ParseTree PTPT_setParseTreeWsAfterTop(PTPT_ParseTree arg, PTPT_OptLayout wsAfterTop)
{
  if (PTPT_isParseTreeTop(arg)) {
    return (PTPT_ParseTree)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterTop, 5), 1);
  }

  ATabort("ParseTree has no WsAfterTop: %t\n", arg);
  return (PTPT_ParseTree)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasParseTreeWsAfterComma(PTPT_ParseTree arg) */

ATbool PTPT_hasParseTreeWsAfterComma(PTPT_ParseTree arg)
{
  if (PTPT_isParseTreeTop(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getParseTreeWsAfterComma(PTPT_ParseTree arg) */

PTPT_OptLayout PTPT_getParseTreeWsAfterComma(PTPT_ParseTree arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 7);
}

/*}}}  */
/*{{{  PTPT_ParseTree PTPT_setParseTreeWsAfterComma(PTPT_ParseTree arg, PTPT_OptLayout wsAfterComma) */

PTPT_ParseTree PTPT_setParseTreeWsAfterComma(PTPT_ParseTree arg, PTPT_OptLayout wsAfterComma)
{
  if (PTPT_isParseTreeTop(arg)) {
    return (PTPT_ParseTree)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterComma, 7), 1);
  }

  ATabort("ParseTree has no WsAfterComma: %t\n", arg);
  return (PTPT_ParseTree)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasParseTreeAmbCnt(PTPT_ParseTree arg) */

ATbool PTPT_hasParseTreeAmbCnt(PTPT_ParseTree arg)
{
  if (PTPT_isParseTreeTop(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_NatCon PTPT_getParseTreeAmbCnt(PTPT_ParseTree arg) */

PTPT_NatCon PTPT_getParseTreeAmbCnt(PTPT_ParseTree arg)
{
  
    return (PTPT_NatCon)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 8);
}

/*}}}  */
/*{{{  PTPT_ParseTree PTPT_setParseTreeAmbCnt(PTPT_ParseTree arg, PTPT_NatCon ambCnt) */

PTPT_ParseTree PTPT_setParseTreeAmbCnt(PTPT_ParseTree arg, PTPT_NatCon ambCnt)
{
  if (PTPT_isParseTreeTop(arg)) {
    return (PTPT_ParseTree)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)ambCnt, 8), 1);
  }

  ATabort("ParseTree has no AmbCnt: %t\n", arg);
  return (PTPT_ParseTree)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasParseTreeWsAfterAmbCnt(PTPT_ParseTree arg) */

ATbool PTPT_hasParseTreeWsAfterAmbCnt(PTPT_ParseTree arg)
{
  if (PTPT_isParseTreeTop(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getParseTreeWsAfterAmbCnt(PTPT_ParseTree arg) */

PTPT_OptLayout PTPT_getParseTreeWsAfterAmbCnt(PTPT_ParseTree arg)
{
  
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)arg, 1), 9);
}

/*}}}  */
/*{{{  PTPT_ParseTree PTPT_setParseTreeWsAfterAmbCnt(PTPT_ParseTree arg, PTPT_OptLayout wsAfterAmbCnt) */

PTPT_ParseTree PTPT_setParseTreeWsAfterAmbCnt(PTPT_ParseTree arg, PTPT_OptLayout wsAfterAmbCnt)
{
  if (PTPT_isParseTreeTop(arg)) {
    return (PTPT_ParseTree)ATsetArgument((ATermAppl)arg, (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)arg, 1), (ATerm)wsAfterAmbCnt, 9), 1);
  }

  ATabort("ParseTree has no WsAfterAmbCnt: %t\n", arg);
  return (PTPT_ParseTree)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_Start accessors */

/*{{{  ATbool PTPT_isValidStart(PTPT_Start arg) */

ATbool PTPT_isValidStart(PTPT_Start arg)
{
  if (PTPT_isStartParseTree(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartAssociativity(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartAttr(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartAttrs(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartAttributes(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartQLiteral(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartUQLiteral(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartLiteral(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartAnn(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartATerm(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartAFun(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartACon(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartATermList(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartRealCon(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartOptExp(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartCharRanges(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartCharRange(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartSymbols(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartSymbol(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartProduction(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartArgs(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartTree(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartIntCon(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartNatCon(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isStartParseTree(PTPT_Start arg) */

inline ATbool PTPT_isStartParseTree(PTPT_Start arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternStartParseTree, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isStartAssociativity(PTPT_Start arg) */

inline ATbool PTPT_isStartAssociativity(PTPT_Start arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternStartAssociativity, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isStartAttr(PTPT_Start arg) */

inline ATbool PTPT_isStartAttr(PTPT_Start arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternStartAttr, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isStartAttrs(PTPT_Start arg) */

inline ATbool PTPT_isStartAttrs(PTPT_Start arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternStartAttrs, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isStartAttributes(PTPT_Start arg) */

inline ATbool PTPT_isStartAttributes(PTPT_Start arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternStartAttributes, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isStartQLiteral(PTPT_Start arg) */

inline ATbool PTPT_isStartQLiteral(PTPT_Start arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternStartQLiteral, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isStartUQLiteral(PTPT_Start arg) */

inline ATbool PTPT_isStartUQLiteral(PTPT_Start arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternStartUQLiteral, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isStartLiteral(PTPT_Start arg) */

inline ATbool PTPT_isStartLiteral(PTPT_Start arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternStartLiteral, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isStartAnn(PTPT_Start arg) */

inline ATbool PTPT_isStartAnn(PTPT_Start arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternStartAnn, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isStartATerm(PTPT_Start arg) */

inline ATbool PTPT_isStartATerm(PTPT_Start arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternStartATerm, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isStartAFun(PTPT_Start arg) */

inline ATbool PTPT_isStartAFun(PTPT_Start arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternStartAFun, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isStartACon(PTPT_Start arg) */

inline ATbool PTPT_isStartACon(PTPT_Start arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternStartACon, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isStartATermList(PTPT_Start arg) */

inline ATbool PTPT_isStartATermList(PTPT_Start arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternStartATermList, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isStartRealCon(PTPT_Start arg) */

inline ATbool PTPT_isStartRealCon(PTPT_Start arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternStartRealCon, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isStartOptExp(PTPT_Start arg) */

inline ATbool PTPT_isStartOptExp(PTPT_Start arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternStartOptExp, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isStartCharRanges(PTPT_Start arg) */

inline ATbool PTPT_isStartCharRanges(PTPT_Start arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternStartCharRanges, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isStartCharRange(PTPT_Start arg) */

inline ATbool PTPT_isStartCharRange(PTPT_Start arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternStartCharRange, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isStartSymbols(PTPT_Start arg) */

inline ATbool PTPT_isStartSymbols(PTPT_Start arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternStartSymbols, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isStartSymbol(PTPT_Start arg) */

inline ATbool PTPT_isStartSymbol(PTPT_Start arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternStartSymbol, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isStartProduction(PTPT_Start arg) */

inline ATbool PTPT_isStartProduction(PTPT_Start arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternStartProduction, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isStartArgs(PTPT_Start arg) */

inline ATbool PTPT_isStartArgs(PTPT_Start arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternStartArgs, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isStartTree(PTPT_Start arg) */

inline ATbool PTPT_isStartTree(PTPT_Start arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternStartTree, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isStartIntCon(PTPT_Start arg) */

inline ATbool PTPT_isStartIntCon(PTPT_Start arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternStartIntCon, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isStartNatCon(PTPT_Start arg) */

inline ATbool PTPT_isStartNatCon(PTPT_Start arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternStartNatCon, NULL, NULL, NULL, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  ATbool PTPT_hasStartWsBefore(PTPT_Start arg) */

ATbool PTPT_hasStartWsBefore(PTPT_Start arg)
{
  if (PTPT_isStartParseTree(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartAssociativity(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartAttr(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartAttrs(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartAttributes(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartQLiteral(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartUQLiteral(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartLiteral(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartAnn(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartATerm(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartAFun(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartACon(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartATermList(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartRealCon(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartOptExp(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartCharRanges(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartCharRange(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartSymbols(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartSymbol(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartProduction(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartArgs(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartTree(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartIntCon(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartNatCon(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getStartWsBefore(PTPT_Start arg) */

PTPT_OptLayout PTPT_getStartWsBefore(PTPT_Start arg)
{
  if (PTPT_isStartParseTree(arg)) {
    return (PTPT_OptLayout)ATgetFirst((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1));
  }
  else if (PTPT_isStartAssociativity(arg)) {
    return (PTPT_OptLayout)ATgetFirst((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1));
  }
  else if (PTPT_isStartAttr(arg)) {
    return (PTPT_OptLayout)ATgetFirst((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1));
  }
  else if (PTPT_isStartAttrs(arg)) {
    return (PTPT_OptLayout)ATgetFirst((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1));
  }
  else if (PTPT_isStartAttributes(arg)) {
    return (PTPT_OptLayout)ATgetFirst((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1));
  }
  else if (PTPT_isStartQLiteral(arg)) {
    return (PTPT_OptLayout)ATgetFirst((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1));
  }
  else if (PTPT_isStartUQLiteral(arg)) {
    return (PTPT_OptLayout)ATgetFirst((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1));
  }
  else if (PTPT_isStartLiteral(arg)) {
    return (PTPT_OptLayout)ATgetFirst((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1));
  }
  else if (PTPT_isStartAnn(arg)) {
    return (PTPT_OptLayout)ATgetFirst((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1));
  }
  else if (PTPT_isStartATerm(arg)) {
    return (PTPT_OptLayout)ATgetFirst((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1));
  }
  else if (PTPT_isStartAFun(arg)) {
    return (PTPT_OptLayout)ATgetFirst((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1));
  }
  else if (PTPT_isStartACon(arg)) {
    return (PTPT_OptLayout)ATgetFirst((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1));
  }
  else if (PTPT_isStartATermList(arg)) {
    return (PTPT_OptLayout)ATgetFirst((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1));
  }
  else if (PTPT_isStartRealCon(arg)) {
    return (PTPT_OptLayout)ATgetFirst((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1));
  }
  else if (PTPT_isStartOptExp(arg)) {
    return (PTPT_OptLayout)ATgetFirst((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1));
  }
  else if (PTPT_isStartCharRanges(arg)) {
    return (PTPT_OptLayout)ATgetFirst((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1));
  }
  else if (PTPT_isStartCharRange(arg)) {
    return (PTPT_OptLayout)ATgetFirst((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1));
  }
  else if (PTPT_isStartSymbols(arg)) {
    return (PTPT_OptLayout)ATgetFirst((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1));
  }
  else if (PTPT_isStartSymbol(arg)) {
    return (PTPT_OptLayout)ATgetFirst((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1));
  }
  else if (PTPT_isStartProduction(arg)) {
    return (PTPT_OptLayout)ATgetFirst((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1));
  }
  else if (PTPT_isStartArgs(arg)) {
    return (PTPT_OptLayout)ATgetFirst((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1));
  }
  else if (PTPT_isStartTree(arg)) {
    return (PTPT_OptLayout)ATgetFirst((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1));
  }
  else if (PTPT_isStartIntCon(arg)) {
    return (PTPT_OptLayout)ATgetFirst((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1));
  }
  else 
    return (PTPT_OptLayout)ATgetFirst((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1));
}

/*}}}  */
/*{{{  PTPT_Start PTPT_setStartWsBefore(PTPT_Start arg, PTPT_OptLayout wsBefore) */

PTPT_Start PTPT_setStartWsBefore(PTPT_Start arg, PTPT_OptLayout wsBefore)
{
  if (PTPT_isStartParseTree(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsBefore, 0), 1), 0);
  }
  else if (PTPT_isStartAssociativity(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsBefore, 0), 1), 0);
  }
  else if (PTPT_isStartAttr(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsBefore, 0), 1), 0);
  }
  else if (PTPT_isStartAttrs(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsBefore, 0), 1), 0);
  }
  else if (PTPT_isStartAttributes(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsBefore, 0), 1), 0);
  }
  else if (PTPT_isStartQLiteral(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsBefore, 0), 1), 0);
  }
  else if (PTPT_isStartUQLiteral(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsBefore, 0), 1), 0);
  }
  else if (PTPT_isStartLiteral(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsBefore, 0), 1), 0);
  }
  else if (PTPT_isStartAnn(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsBefore, 0), 1), 0);
  }
  else if (PTPT_isStartATerm(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsBefore, 0), 1), 0);
  }
  else if (PTPT_isStartAFun(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsBefore, 0), 1), 0);
  }
  else if (PTPT_isStartACon(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsBefore, 0), 1), 0);
  }
  else if (PTPT_isStartATermList(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsBefore, 0), 1), 0);
  }
  else if (PTPT_isStartRealCon(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsBefore, 0), 1), 0);
  }
  else if (PTPT_isStartOptExp(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsBefore, 0), 1), 0);
  }
  else if (PTPT_isStartCharRanges(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsBefore, 0), 1), 0);
  }
  else if (PTPT_isStartCharRange(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsBefore, 0), 1), 0);
  }
  else if (PTPT_isStartSymbols(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsBefore, 0), 1), 0);
  }
  else if (PTPT_isStartSymbol(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsBefore, 0), 1), 0);
  }
  else if (PTPT_isStartProduction(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsBefore, 0), 1), 0);
  }
  else if (PTPT_isStartArgs(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsBefore, 0), 1), 0);
  }
  else if (PTPT_isStartTree(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsBefore, 0), 1), 0);
  }
  else if (PTPT_isStartIntCon(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsBefore, 0), 1), 0);
  }
  else if (PTPT_isStartNatCon(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsBefore, 0), 1), 0);
  }

  ATabort("Start has no WsBefore: %t\n", arg);
  return (PTPT_Start)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasStartTopParseTree(PTPT_Start arg) */

ATbool PTPT_hasStartTopParseTree(PTPT_Start arg)
{
  if (PTPT_isStartParseTree(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_ParseTree PTPT_getStartTopParseTree(PTPT_Start arg) */

PTPT_ParseTree PTPT_getStartTopParseTree(PTPT_Start arg)
{
  
    return (PTPT_ParseTree)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 1);
}

/*}}}  */
/*{{{  PTPT_Start PTPT_setStartTopParseTree(PTPT_Start arg, PTPT_ParseTree topParseTree) */

PTPT_Start PTPT_setStartTopParseTree(PTPT_Start arg, PTPT_ParseTree topParseTree)
{
  if (PTPT_isStartParseTree(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)topParseTree, 1), 1), 0);
  }

  ATabort("Start has no TopParseTree: %t\n", arg);
  return (PTPT_Start)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasStartWsAfter(PTPT_Start arg) */

ATbool PTPT_hasStartWsAfter(PTPT_Start arg)
{
  if (PTPT_isStartParseTree(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartAssociativity(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartAttr(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartAttrs(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartAttributes(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartQLiteral(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartUQLiteral(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartLiteral(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartAnn(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartATerm(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartAFun(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartACon(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartATermList(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartRealCon(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartOptExp(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartCharRanges(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartCharRange(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartSymbols(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartSymbol(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartProduction(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartArgs(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartTree(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartIntCon(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartNatCon(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_getStartWsAfter(PTPT_Start arg) */

PTPT_OptLayout PTPT_getStartWsAfter(PTPT_Start arg)
{
  if (PTPT_isStartParseTree(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 2);
  }
  else if (PTPT_isStartAssociativity(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 2);
  }
  else if (PTPT_isStartAttr(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 2);
  }
  else if (PTPT_isStartAttrs(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 2);
  }
  else if (PTPT_isStartAttributes(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 2);
  }
  else if (PTPT_isStartQLiteral(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 2);
  }
  else if (PTPT_isStartUQLiteral(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 2);
  }
  else if (PTPT_isStartLiteral(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 2);
  }
  else if (PTPT_isStartAnn(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 2);
  }
  else if (PTPT_isStartATerm(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 2);
  }
  else if (PTPT_isStartAFun(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 2);
  }
  else if (PTPT_isStartACon(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 2);
  }
  else if (PTPT_isStartATermList(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 2);
  }
  else if (PTPT_isStartRealCon(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 2);
  }
  else if (PTPT_isStartOptExp(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 2);
  }
  else if (PTPT_isStartCharRanges(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 2);
  }
  else if (PTPT_isStartCharRange(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 2);
  }
  else if (PTPT_isStartSymbols(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 2);
  }
  else if (PTPT_isStartSymbol(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 2);
  }
  else if (PTPT_isStartProduction(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 2);
  }
  else if (PTPT_isStartArgs(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 2);
  }
  else if (PTPT_isStartTree(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 2);
  }
  else if (PTPT_isStartIntCon(arg)) {
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 2);
  }
  else 
    return (PTPT_OptLayout)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 2);
}

/*}}}  */
/*{{{  PTPT_Start PTPT_setStartWsAfter(PTPT_Start arg, PTPT_OptLayout wsAfter) */

PTPT_Start PTPT_setStartWsAfter(PTPT_Start arg, PTPT_OptLayout wsAfter)
{
  if (PTPT_isStartParseTree(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsAfter, 2), 1), 0);
  }
  else if (PTPT_isStartAssociativity(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsAfter, 2), 1), 0);
  }
  else if (PTPT_isStartAttr(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsAfter, 2), 1), 0);
  }
  else if (PTPT_isStartAttrs(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsAfter, 2), 1), 0);
  }
  else if (PTPT_isStartAttributes(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsAfter, 2), 1), 0);
  }
  else if (PTPT_isStartQLiteral(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsAfter, 2), 1), 0);
  }
  else if (PTPT_isStartUQLiteral(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsAfter, 2), 1), 0);
  }
  else if (PTPT_isStartLiteral(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsAfter, 2), 1), 0);
  }
  else if (PTPT_isStartAnn(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsAfter, 2), 1), 0);
  }
  else if (PTPT_isStartATerm(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsAfter, 2), 1), 0);
  }
  else if (PTPT_isStartAFun(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsAfter, 2), 1), 0);
  }
  else if (PTPT_isStartACon(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsAfter, 2), 1), 0);
  }
  else if (PTPT_isStartATermList(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsAfter, 2), 1), 0);
  }
  else if (PTPT_isStartRealCon(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsAfter, 2), 1), 0);
  }
  else if (PTPT_isStartOptExp(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsAfter, 2), 1), 0);
  }
  else if (PTPT_isStartCharRanges(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsAfter, 2), 1), 0);
  }
  else if (PTPT_isStartCharRange(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsAfter, 2), 1), 0);
  }
  else if (PTPT_isStartSymbols(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsAfter, 2), 1), 0);
  }
  else if (PTPT_isStartSymbol(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsAfter, 2), 1), 0);
  }
  else if (PTPT_isStartProduction(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsAfter, 2), 1), 0);
  }
  else if (PTPT_isStartArgs(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsAfter, 2), 1), 0);
  }
  else if (PTPT_isStartTree(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsAfter, 2), 1), 0);
  }
  else if (PTPT_isStartIntCon(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsAfter, 2), 1), 0);
  }
  else if (PTPT_isStartNatCon(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)wsAfter, 2), 1), 0);
  }

  ATabort("Start has no WsAfter: %t\n", arg);
  return (PTPT_Start)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasStartAmbCnt(PTPT_Start arg) */

ATbool PTPT_hasStartAmbCnt(PTPT_Start arg)
{
  if (PTPT_isStartParseTree(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartAssociativity(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartAttr(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartAttrs(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartAttributes(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartQLiteral(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartUQLiteral(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartLiteral(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartAnn(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartATerm(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartAFun(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartACon(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartATermList(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartRealCon(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartOptExp(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartCharRanges(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartCharRange(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartSymbols(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartSymbol(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartProduction(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartArgs(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartTree(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartIntCon(arg)) {
    return ATtrue;
  }
  else if (PTPT_isStartNatCon(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  int PTPT_getStartAmbCnt(PTPT_Start arg) */

int PTPT_getStartAmbCnt(PTPT_Start arg)
{
  if (PTPT_isStartParseTree(arg)) {
    return (int)ATgetInt((ATermInt)ATgetArgument((ATermAppl)arg, 1));
  }
  else if (PTPT_isStartAssociativity(arg)) {
    return (int)ATgetInt((ATermInt)ATgetArgument((ATermAppl)arg, 1));
  }
  else if (PTPT_isStartAttr(arg)) {
    return (int)ATgetInt((ATermInt)ATgetArgument((ATermAppl)arg, 1));
  }
  else if (PTPT_isStartAttrs(arg)) {
    return (int)ATgetInt((ATermInt)ATgetArgument((ATermAppl)arg, 1));
  }
  else if (PTPT_isStartAttributes(arg)) {
    return (int)ATgetInt((ATermInt)ATgetArgument((ATermAppl)arg, 1));
  }
  else if (PTPT_isStartQLiteral(arg)) {
    return (int)ATgetInt((ATermInt)ATgetArgument((ATermAppl)arg, 1));
  }
  else if (PTPT_isStartUQLiteral(arg)) {
    return (int)ATgetInt((ATermInt)ATgetArgument((ATermAppl)arg, 1));
  }
  else if (PTPT_isStartLiteral(arg)) {
    return (int)ATgetInt((ATermInt)ATgetArgument((ATermAppl)arg, 1));
  }
  else if (PTPT_isStartAnn(arg)) {
    return (int)ATgetInt((ATermInt)ATgetArgument((ATermAppl)arg, 1));
  }
  else if (PTPT_isStartATerm(arg)) {
    return (int)ATgetInt((ATermInt)ATgetArgument((ATermAppl)arg, 1));
  }
  else if (PTPT_isStartAFun(arg)) {
    return (int)ATgetInt((ATermInt)ATgetArgument((ATermAppl)arg, 1));
  }
  else if (PTPT_isStartACon(arg)) {
    return (int)ATgetInt((ATermInt)ATgetArgument((ATermAppl)arg, 1));
  }
  else if (PTPT_isStartATermList(arg)) {
    return (int)ATgetInt((ATermInt)ATgetArgument((ATermAppl)arg, 1));
  }
  else if (PTPT_isStartRealCon(arg)) {
    return (int)ATgetInt((ATermInt)ATgetArgument((ATermAppl)arg, 1));
  }
  else if (PTPT_isStartOptExp(arg)) {
    return (int)ATgetInt((ATermInt)ATgetArgument((ATermAppl)arg, 1));
  }
  else if (PTPT_isStartCharRanges(arg)) {
    return (int)ATgetInt((ATermInt)ATgetArgument((ATermAppl)arg, 1));
  }
  else if (PTPT_isStartCharRange(arg)) {
    return (int)ATgetInt((ATermInt)ATgetArgument((ATermAppl)arg, 1));
  }
  else if (PTPT_isStartSymbols(arg)) {
    return (int)ATgetInt((ATermInt)ATgetArgument((ATermAppl)arg, 1));
  }
  else if (PTPT_isStartSymbol(arg)) {
    return (int)ATgetInt((ATermInt)ATgetArgument((ATermAppl)arg, 1));
  }
  else if (PTPT_isStartProduction(arg)) {
    return (int)ATgetInt((ATermInt)ATgetArgument((ATermAppl)arg, 1));
  }
  else if (PTPT_isStartArgs(arg)) {
    return (int)ATgetInt((ATermInt)ATgetArgument((ATermAppl)arg, 1));
  }
  else if (PTPT_isStartTree(arg)) {
    return (int)ATgetInt((ATermInt)ATgetArgument((ATermAppl)arg, 1));
  }
  else if (PTPT_isStartIntCon(arg)) {
    return (int)ATgetInt((ATermInt)ATgetArgument((ATermAppl)arg, 1));
  }
  else 
    return (int)ATgetInt((ATermInt)ATgetArgument((ATermAppl)arg, 1));
}

/*}}}  */
/*{{{  PTPT_Start PTPT_setStartAmbCnt(PTPT_Start arg, int ambCnt) */

PTPT_Start PTPT_setStartAmbCnt(PTPT_Start arg, int ambCnt)
{
  if (PTPT_isStartParseTree(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATmakeInt(ambCnt), 1);
  }
  else if (PTPT_isStartAssociativity(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATmakeInt(ambCnt), 1);
  }
  else if (PTPT_isStartAttr(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATmakeInt(ambCnt), 1);
  }
  else if (PTPT_isStartAttrs(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATmakeInt(ambCnt), 1);
  }
  else if (PTPT_isStartAttributes(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATmakeInt(ambCnt), 1);
  }
  else if (PTPT_isStartQLiteral(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATmakeInt(ambCnt), 1);
  }
  else if (PTPT_isStartUQLiteral(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATmakeInt(ambCnt), 1);
  }
  else if (PTPT_isStartLiteral(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATmakeInt(ambCnt), 1);
  }
  else if (PTPT_isStartAnn(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATmakeInt(ambCnt), 1);
  }
  else if (PTPT_isStartATerm(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATmakeInt(ambCnt), 1);
  }
  else if (PTPT_isStartAFun(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATmakeInt(ambCnt), 1);
  }
  else if (PTPT_isStartACon(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATmakeInt(ambCnt), 1);
  }
  else if (PTPT_isStartATermList(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATmakeInt(ambCnt), 1);
  }
  else if (PTPT_isStartRealCon(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATmakeInt(ambCnt), 1);
  }
  else if (PTPT_isStartOptExp(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATmakeInt(ambCnt), 1);
  }
  else if (PTPT_isStartCharRanges(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATmakeInt(ambCnt), 1);
  }
  else if (PTPT_isStartCharRange(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATmakeInt(ambCnt), 1);
  }
  else if (PTPT_isStartSymbols(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATmakeInt(ambCnt), 1);
  }
  else if (PTPT_isStartSymbol(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATmakeInt(ambCnt), 1);
  }
  else if (PTPT_isStartProduction(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATmakeInt(ambCnt), 1);
  }
  else if (PTPT_isStartArgs(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATmakeInt(ambCnt), 1);
  }
  else if (PTPT_isStartTree(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATmakeInt(ambCnt), 1);
  }
  else if (PTPT_isStartIntCon(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATmakeInt(ambCnt), 1);
  }
  else if (PTPT_isStartNatCon(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATmakeInt(ambCnt), 1);
  }

  ATabort("Start has no AmbCnt: %t\n", arg);
  return (PTPT_Start)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasStartTopAssociativity(PTPT_Start arg) */

ATbool PTPT_hasStartTopAssociativity(PTPT_Start arg)
{
  if (PTPT_isStartAssociativity(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Associativity PTPT_getStartTopAssociativity(PTPT_Start arg) */

PTPT_Associativity PTPT_getStartTopAssociativity(PTPT_Start arg)
{
  
    return (PTPT_Associativity)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 1);
}

/*}}}  */
/*{{{  PTPT_Start PTPT_setStartTopAssociativity(PTPT_Start arg, PTPT_Associativity topAssociativity) */

PTPT_Start PTPT_setStartTopAssociativity(PTPT_Start arg, PTPT_Associativity topAssociativity)
{
  if (PTPT_isStartAssociativity(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)topAssociativity, 1), 1), 0);
  }

  ATabort("Start has no TopAssociativity: %t\n", arg);
  return (PTPT_Start)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasStartTopAttr(PTPT_Start arg) */

ATbool PTPT_hasStartTopAttr(PTPT_Start arg)
{
  if (PTPT_isStartAttr(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Attr PTPT_getStartTopAttr(PTPT_Start arg) */

PTPT_Attr PTPT_getStartTopAttr(PTPT_Start arg)
{
  
    return (PTPT_Attr)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 1);
}

/*}}}  */
/*{{{  PTPT_Start PTPT_setStartTopAttr(PTPT_Start arg, PTPT_Attr topAttr) */

PTPT_Start PTPT_setStartTopAttr(PTPT_Start arg, PTPT_Attr topAttr)
{
  if (PTPT_isStartAttr(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)topAttr, 1), 1), 0);
  }

  ATabort("Start has no TopAttr: %t\n", arg);
  return (PTPT_Start)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasStartTopAttrs(PTPT_Start arg) */

ATbool PTPT_hasStartTopAttrs(PTPT_Start arg)
{
  if (PTPT_isStartAttrs(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Attrs PTPT_getStartTopAttrs(PTPT_Start arg) */

PTPT_Attrs PTPT_getStartTopAttrs(PTPT_Start arg)
{
  
    return (PTPT_Attrs)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 1);
}

/*}}}  */
/*{{{  PTPT_Start PTPT_setStartTopAttrs(PTPT_Start arg, PTPT_Attrs topAttrs) */

PTPT_Start PTPT_setStartTopAttrs(PTPT_Start arg, PTPT_Attrs topAttrs)
{
  if (PTPT_isStartAttrs(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)topAttrs, 1), 1), 0);
  }

  ATabort("Start has no TopAttrs: %t\n", arg);
  return (PTPT_Start)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasStartTopAttributes(PTPT_Start arg) */

ATbool PTPT_hasStartTopAttributes(PTPT_Start arg)
{
  if (PTPT_isStartAttributes(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Attributes PTPT_getStartTopAttributes(PTPT_Start arg) */

PTPT_Attributes PTPT_getStartTopAttributes(PTPT_Start arg)
{
  
    return (PTPT_Attributes)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 1);
}

/*}}}  */
/*{{{  PTPT_Start PTPT_setStartTopAttributes(PTPT_Start arg, PTPT_Attributes topAttributes) */

PTPT_Start PTPT_setStartTopAttributes(PTPT_Start arg, PTPT_Attributes topAttributes)
{
  if (PTPT_isStartAttributes(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)topAttributes, 1), 1), 0);
  }

  ATabort("Start has no TopAttributes: %t\n", arg);
  return (PTPT_Start)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasStartTopQLiteral(PTPT_Start arg) */

ATbool PTPT_hasStartTopQLiteral(PTPT_Start arg)
{
  if (PTPT_isStartQLiteral(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_QLiteral PTPT_getStartTopQLiteral(PTPT_Start arg) */

PTPT_QLiteral PTPT_getStartTopQLiteral(PTPT_Start arg)
{
  
    return (PTPT_QLiteral)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 1);
}

/*}}}  */
/*{{{  PTPT_Start PTPT_setStartTopQLiteral(PTPT_Start arg, PTPT_QLiteral topQLiteral) */

PTPT_Start PTPT_setStartTopQLiteral(PTPT_Start arg, PTPT_QLiteral topQLiteral)
{
  if (PTPT_isStartQLiteral(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)topQLiteral, 1), 1), 0);
  }

  ATabort("Start has no TopQLiteral: %t\n", arg);
  return (PTPT_Start)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasStartTopUQLiteral(PTPT_Start arg) */

ATbool PTPT_hasStartTopUQLiteral(PTPT_Start arg)
{
  if (PTPT_isStartUQLiteral(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_UQLiteral PTPT_getStartTopUQLiteral(PTPT_Start arg) */

PTPT_UQLiteral PTPT_getStartTopUQLiteral(PTPT_Start arg)
{
  
    return (PTPT_UQLiteral)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 1);
}

/*}}}  */
/*{{{  PTPT_Start PTPT_setStartTopUQLiteral(PTPT_Start arg, PTPT_UQLiteral topUQLiteral) */

PTPT_Start PTPT_setStartTopUQLiteral(PTPT_Start arg, PTPT_UQLiteral topUQLiteral)
{
  if (PTPT_isStartUQLiteral(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)topUQLiteral, 1), 1), 0);
  }

  ATabort("Start has no TopUQLiteral: %t\n", arg);
  return (PTPT_Start)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasStartTopLiteral(PTPT_Start arg) */

ATbool PTPT_hasStartTopLiteral(PTPT_Start arg)
{
  if (PTPT_isStartLiteral(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Literal PTPT_getStartTopLiteral(PTPT_Start arg) */

PTPT_Literal PTPT_getStartTopLiteral(PTPT_Start arg)
{
  
    return (PTPT_Literal)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 1);
}

/*}}}  */
/*{{{  PTPT_Start PTPT_setStartTopLiteral(PTPT_Start arg, PTPT_Literal topLiteral) */

PTPT_Start PTPT_setStartTopLiteral(PTPT_Start arg, PTPT_Literal topLiteral)
{
  if (PTPT_isStartLiteral(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)topLiteral, 1), 1), 0);
  }

  ATabort("Start has no TopLiteral: %t\n", arg);
  return (PTPT_Start)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasStartTopAnn(PTPT_Start arg) */

ATbool PTPT_hasStartTopAnn(PTPT_Start arg)
{
  if (PTPT_isStartAnn(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Ann PTPT_getStartTopAnn(PTPT_Start arg) */

PTPT_Ann PTPT_getStartTopAnn(PTPT_Start arg)
{
  
    return (PTPT_Ann)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 1);
}

/*}}}  */
/*{{{  PTPT_Start PTPT_setStartTopAnn(PTPT_Start arg, PTPT_Ann topAnn) */

PTPT_Start PTPT_setStartTopAnn(PTPT_Start arg, PTPT_Ann topAnn)
{
  if (PTPT_isStartAnn(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)topAnn, 1), 1), 0);
  }

  ATabort("Start has no TopAnn: %t\n", arg);
  return (PTPT_Start)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasStartTopATerm(PTPT_Start arg) */

ATbool PTPT_hasStartTopATerm(PTPT_Start arg)
{
  if (PTPT_isStartATerm(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_ATerm PTPT_getStartTopATerm(PTPT_Start arg) */

PTPT_ATerm PTPT_getStartTopATerm(PTPT_Start arg)
{
  
    return (PTPT_ATerm)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 1);
}

/*}}}  */
/*{{{  PTPT_Start PTPT_setStartTopATerm(PTPT_Start arg, PTPT_ATerm topATerm) */

PTPT_Start PTPT_setStartTopATerm(PTPT_Start arg, PTPT_ATerm topATerm)
{
  if (PTPT_isStartATerm(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)topATerm, 1), 1), 0);
  }

  ATabort("Start has no TopATerm: %t\n", arg);
  return (PTPT_Start)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasStartTopAFun(PTPT_Start arg) */

ATbool PTPT_hasStartTopAFun(PTPT_Start arg)
{
  if (PTPT_isStartAFun(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_AFun PTPT_getStartTopAFun(PTPT_Start arg) */

PTPT_AFun PTPT_getStartTopAFun(PTPT_Start arg)
{
  
    return (PTPT_AFun)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 1);
}

/*}}}  */
/*{{{  PTPT_Start PTPT_setStartTopAFun(PTPT_Start arg, PTPT_AFun topAFun) */

PTPT_Start PTPT_setStartTopAFun(PTPT_Start arg, PTPT_AFun topAFun)
{
  if (PTPT_isStartAFun(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)topAFun, 1), 1), 0);
  }

  ATabort("Start has no TopAFun: %t\n", arg);
  return (PTPT_Start)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasStartTopACon(PTPT_Start arg) */

ATbool PTPT_hasStartTopACon(PTPT_Start arg)
{
  if (PTPT_isStartACon(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_ACon PTPT_getStartTopACon(PTPT_Start arg) */

PTPT_ACon PTPT_getStartTopACon(PTPT_Start arg)
{
  
    return (PTPT_ACon)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 1);
}

/*}}}  */
/*{{{  PTPT_Start PTPT_setStartTopACon(PTPT_Start arg, PTPT_ACon topACon) */

PTPT_Start PTPT_setStartTopACon(PTPT_Start arg, PTPT_ACon topACon)
{
  if (PTPT_isStartACon(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)topACon, 1), 1), 0);
  }

  ATabort("Start has no TopACon: %t\n", arg);
  return (PTPT_Start)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasStartTopATermList(PTPT_Start arg) */

ATbool PTPT_hasStartTopATermList(PTPT_Start arg)
{
  if (PTPT_isStartATermList(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_ATermList PTPT_getStartTopATermList(PTPT_Start arg) */

PTPT_ATermList PTPT_getStartTopATermList(PTPT_Start arg)
{
  
    return (PTPT_ATermList)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 1);
}

/*}}}  */
/*{{{  PTPT_Start PTPT_setStartTopATermList(PTPT_Start arg, PTPT_ATermList topATermList) */

PTPT_Start PTPT_setStartTopATermList(PTPT_Start arg, PTPT_ATermList topATermList)
{
  if (PTPT_isStartATermList(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)topATermList, 1), 1), 0);
  }

  ATabort("Start has no TopATermList: %t\n", arg);
  return (PTPT_Start)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasStartTopRealCon(PTPT_Start arg) */

ATbool PTPT_hasStartTopRealCon(PTPT_Start arg)
{
  if (PTPT_isStartRealCon(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_RealCon PTPT_getStartTopRealCon(PTPT_Start arg) */

PTPT_RealCon PTPT_getStartTopRealCon(PTPT_Start arg)
{
  
    return (PTPT_RealCon)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 1);
}

/*}}}  */
/*{{{  PTPT_Start PTPT_setStartTopRealCon(PTPT_Start arg, PTPT_RealCon topRealCon) */

PTPT_Start PTPT_setStartTopRealCon(PTPT_Start arg, PTPT_RealCon topRealCon)
{
  if (PTPT_isStartRealCon(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)topRealCon, 1), 1), 0);
  }

  ATabort("Start has no TopRealCon: %t\n", arg);
  return (PTPT_Start)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasStartTopOptExp(PTPT_Start arg) */

ATbool PTPT_hasStartTopOptExp(PTPT_Start arg)
{
  if (PTPT_isStartOptExp(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_OptExp PTPT_getStartTopOptExp(PTPT_Start arg) */

PTPT_OptExp PTPT_getStartTopOptExp(PTPT_Start arg)
{
  
    return (PTPT_OptExp)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 1);
}

/*}}}  */
/*{{{  PTPT_Start PTPT_setStartTopOptExp(PTPT_Start arg, PTPT_OptExp topOptExp) */

PTPT_Start PTPT_setStartTopOptExp(PTPT_Start arg, PTPT_OptExp topOptExp)
{
  if (PTPT_isStartOptExp(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)topOptExp, 1), 1), 0);
  }

  ATabort("Start has no TopOptExp: %t\n", arg);
  return (PTPT_Start)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasStartTopCharRanges(PTPT_Start arg) */

ATbool PTPT_hasStartTopCharRanges(PTPT_Start arg)
{
  if (PTPT_isStartCharRanges(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_CharRanges PTPT_getStartTopCharRanges(PTPT_Start arg) */

PTPT_CharRanges PTPT_getStartTopCharRanges(PTPT_Start arg)
{
  
    return (PTPT_CharRanges)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 1);
}

/*}}}  */
/*{{{  PTPT_Start PTPT_setStartTopCharRanges(PTPT_Start arg, PTPT_CharRanges topCharRanges) */

PTPT_Start PTPT_setStartTopCharRanges(PTPT_Start arg, PTPT_CharRanges topCharRanges)
{
  if (PTPT_isStartCharRanges(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)topCharRanges, 1), 1), 0);
  }

  ATabort("Start has no TopCharRanges: %t\n", arg);
  return (PTPT_Start)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasStartTopCharRange(PTPT_Start arg) */

ATbool PTPT_hasStartTopCharRange(PTPT_Start arg)
{
  if (PTPT_isStartCharRange(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_CharRange PTPT_getStartTopCharRange(PTPT_Start arg) */

PTPT_CharRange PTPT_getStartTopCharRange(PTPT_Start arg)
{
  
    return (PTPT_CharRange)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 1);
}

/*}}}  */
/*{{{  PTPT_Start PTPT_setStartTopCharRange(PTPT_Start arg, PTPT_CharRange topCharRange) */

PTPT_Start PTPT_setStartTopCharRange(PTPT_Start arg, PTPT_CharRange topCharRange)
{
  if (PTPT_isStartCharRange(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)topCharRange, 1), 1), 0);
  }

  ATabort("Start has no TopCharRange: %t\n", arg);
  return (PTPT_Start)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasStartTopSymbols(PTPT_Start arg) */

ATbool PTPT_hasStartTopSymbols(PTPT_Start arg)
{
  if (PTPT_isStartSymbols(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Symbols PTPT_getStartTopSymbols(PTPT_Start arg) */

PTPT_Symbols PTPT_getStartTopSymbols(PTPT_Start arg)
{
  
    return (PTPT_Symbols)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 1);
}

/*}}}  */
/*{{{  PTPT_Start PTPT_setStartTopSymbols(PTPT_Start arg, PTPT_Symbols topSymbols) */

PTPT_Start PTPT_setStartTopSymbols(PTPT_Start arg, PTPT_Symbols topSymbols)
{
  if (PTPT_isStartSymbols(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)topSymbols, 1), 1), 0);
  }

  ATabort("Start has no TopSymbols: %t\n", arg);
  return (PTPT_Start)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasStartTopSymbol(PTPT_Start arg) */

ATbool PTPT_hasStartTopSymbol(PTPT_Start arg)
{
  if (PTPT_isStartSymbol(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_getStartTopSymbol(PTPT_Start arg) */

PTPT_Symbol PTPT_getStartTopSymbol(PTPT_Start arg)
{
  
    return (PTPT_Symbol)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 1);
}

/*}}}  */
/*{{{  PTPT_Start PTPT_setStartTopSymbol(PTPT_Start arg, PTPT_Symbol topSymbol) */

PTPT_Start PTPT_setStartTopSymbol(PTPT_Start arg, PTPT_Symbol topSymbol)
{
  if (PTPT_isStartSymbol(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)topSymbol, 1), 1), 0);
  }

  ATabort("Start has no TopSymbol: %t\n", arg);
  return (PTPT_Start)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasStartTopProduction(PTPT_Start arg) */

ATbool PTPT_hasStartTopProduction(PTPT_Start arg)
{
  if (PTPT_isStartProduction(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Production PTPT_getStartTopProduction(PTPT_Start arg) */

PTPT_Production PTPT_getStartTopProduction(PTPT_Start arg)
{
  
    return (PTPT_Production)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 1);
}

/*}}}  */
/*{{{  PTPT_Start PTPT_setStartTopProduction(PTPT_Start arg, PTPT_Production topProduction) */

PTPT_Start PTPT_setStartTopProduction(PTPT_Start arg, PTPT_Production topProduction)
{
  if (PTPT_isStartProduction(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)topProduction, 1), 1), 0);
  }

  ATabort("Start has no TopProduction: %t\n", arg);
  return (PTPT_Start)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasStartTopArgs(PTPT_Start arg) */

ATbool PTPT_hasStartTopArgs(PTPT_Start arg)
{
  if (PTPT_isStartArgs(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Args PTPT_getStartTopArgs(PTPT_Start arg) */

PTPT_Args PTPT_getStartTopArgs(PTPT_Start arg)
{
  
    return (PTPT_Args)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 1);
}

/*}}}  */
/*{{{  PTPT_Start PTPT_setStartTopArgs(PTPT_Start arg, PTPT_Args topArgs) */

PTPT_Start PTPT_setStartTopArgs(PTPT_Start arg, PTPT_Args topArgs)
{
  if (PTPT_isStartArgs(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)topArgs, 1), 1), 0);
  }

  ATabort("Start has no TopArgs: %t\n", arg);
  return (PTPT_Start)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasStartTopTree(PTPT_Start arg) */

ATbool PTPT_hasStartTopTree(PTPT_Start arg)
{
  if (PTPT_isStartTree(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_Tree PTPT_getStartTopTree(PTPT_Start arg) */

PTPT_Tree PTPT_getStartTopTree(PTPT_Start arg)
{
  
    return (PTPT_Tree)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 1);
}

/*}}}  */
/*{{{  PTPT_Start PTPT_setStartTopTree(PTPT_Start arg, PTPT_Tree topTree) */

PTPT_Start PTPT_setStartTopTree(PTPT_Start arg, PTPT_Tree topTree)
{
  if (PTPT_isStartTree(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)topTree, 1), 1), 0);
  }

  ATabort("Start has no TopTree: %t\n", arg);
  return (PTPT_Start)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasStartTopIntCon(PTPT_Start arg) */

ATbool PTPT_hasStartTopIntCon(PTPT_Start arg)
{
  if (PTPT_isStartIntCon(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_IntCon PTPT_getStartTopIntCon(PTPT_Start arg) */

PTPT_IntCon PTPT_getStartTopIntCon(PTPT_Start arg)
{
  
    return (PTPT_IntCon)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 1);
}

/*}}}  */
/*{{{  PTPT_Start PTPT_setStartTopIntCon(PTPT_Start arg, PTPT_IntCon topIntCon) */

PTPT_Start PTPT_setStartTopIntCon(PTPT_Start arg, PTPT_IntCon topIntCon)
{
  if (PTPT_isStartIntCon(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)topIntCon, 1), 1), 0);
  }

  ATabort("Start has no TopIntCon: %t\n", arg);
  return (PTPT_Start)NULL;
}

/*}}}  */
/*{{{  ATbool PTPT_hasStartTopNatCon(PTPT_Start arg) */

ATbool PTPT_hasStartTopNatCon(PTPT_Start arg)
{
  if (PTPT_isStartNatCon(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_NatCon PTPT_getStartTopNatCon(PTPT_Start arg) */

PTPT_NatCon PTPT_getStartTopNatCon(PTPT_Start arg)
{
  
    return (PTPT_NatCon)ATelementAt((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), 1);
}

/*}}}  */
/*{{{  PTPT_Start PTPT_setStartTopNatCon(PTPT_Start arg, PTPT_NatCon topNatCon) */

PTPT_Start PTPT_setStartTopNatCon(PTPT_Start arg, PTPT_NatCon topNatCon)
{
  if (PTPT_isStartNatCon(arg)) {
    return (PTPT_Start)ATsetArgument((ATermAppl)arg, (ATerm)ATsetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), (ATerm)ATreplace((ATermList)ATgetArgument((ATermAppl)ATgetArgument((ATermAppl)arg, 0), 1), (ATerm)topNatCon, 1), 1), 0);
  }

  ATabort("Start has no TopNatCon: %t\n", arg);
  return (PTPT_Start)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  PTPT_OptLayout accessors */

/*{{{  ATbool PTPT_isValidOptLayout(PTPT_OptLayout arg) */

ATbool PTPT_isValidOptLayout(PTPT_OptLayout arg)
{
  if (PTPT_isOptLayoutAbsent(arg)) {
    return ATtrue;
  }
  else if (PTPT_isOptLayoutPresent(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  inline ATbool PTPT_isOptLayoutAbsent(PTPT_OptLayout arg) */

inline ATbool PTPT_isOptLayoutAbsent(PTPT_OptLayout arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternOptLayoutAbsent);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  inline ATbool PTPT_isOptLayoutPresent(PTPT_OptLayout arg) */

inline ATbool PTPT_isOptLayoutPresent(PTPT_OptLayout arg)
{
  {
    static ATerm last_arg = NULL;
    static int last_gc = -1;
    static ATbool last_result;

    assert(arg != NULL);

    if (last_gc != ATgetGCCount() || (ATerm)arg != last_arg) {
      last_arg = (ATerm)arg;
      last_result = ATmatchTerm((ATerm)arg, PTPT_patternOptLayoutPresent, NULL);
      last_gc = ATgetGCCount();
    }

    return last_result;
  }
}

/*}}}  */
/*{{{  ATbool PTPT_hasOptLayoutChars(PTPT_OptLayout arg) */

ATbool PTPT_hasOptLayoutChars(PTPT_OptLayout arg)
{
  if (PTPT_isOptLayoutPresent(arg)) {
    return ATtrue;
  }
  return ATfalse;
}

/*}}}  */
/*{{{  PTPT_CHARLIST PTPT_getOptLayoutChars(PTPT_OptLayout arg) */

PTPT_CHARLIST PTPT_getOptLayoutChars(PTPT_OptLayout arg)
{
  
    return (PTPT_CHARLIST)ATgetArgument((ATermAppl)arg, 1);
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_setOptLayoutChars(PTPT_OptLayout arg, PTPT_CHARLIST chars) */

PTPT_OptLayout PTPT_setOptLayoutChars(PTPT_OptLayout arg, PTPT_CHARLIST chars)
{
  if (PTPT_isOptLayoutPresent(arg)) {
    return (PTPT_OptLayout)ATsetArgument((ATermAppl)arg, (ATerm)chars, 1);
  }

  ATabort("OptLayout has no Chars: %t\n", arg);
  return (PTPT_OptLayout)NULL;
}

/*}}}  */

/*}}}  */
/*{{{  sort visitors */

/*{{{  PTPT_NatCon PTPT_visitNatCon(PTPT_NatCon arg, PTPT_CHARLIST (*acceptChars)(PTPT_CHARLIST)) */

PTPT_NatCon PTPT_visitNatCon(PTPT_NatCon arg, PTPT_CHARLIST (*acceptChars)(PTPT_CHARLIST))
{
  if (PTPT_isNatConDigits(arg)) {
    return PTPT_makeNatConDigits(
        acceptChars ? acceptChars(PTPT_getNatConChars(arg)) : PTPT_getNatConChars(arg));
  }
  ATabort("not a NatCon: %t\n", arg);
  return (PTPT_NatCon)NULL;
}

/*}}}  */
/*{{{  PTPT_IntCon PTPT_visitIntCon(PTPT_IntCon arg, PTPT_NatCon (*acceptNatCon)(PTPT_NatCon), PTPT_OptLayout (*acceptWsAfterPos)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterNeg)(PTPT_OptLayout)) */

PTPT_IntCon PTPT_visitIntCon(PTPT_IntCon arg, PTPT_NatCon (*acceptNatCon)(PTPT_NatCon), PTPT_OptLayout (*acceptWsAfterPos)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterNeg)(PTPT_OptLayout))
{
  if (PTPT_isIntConNatural(arg)) {
    return PTPT_makeIntConNatural(
        acceptNatCon ? acceptNatCon(PTPT_getIntConNatCon(arg)) : PTPT_getIntConNatCon(arg));
  }
  if (PTPT_isIntConPositive(arg)) {
    return PTPT_makeIntConPositive(
        acceptWsAfterPos ? acceptWsAfterPos(PTPT_getIntConWsAfterPos(arg)) : PTPT_getIntConWsAfterPos(arg),
        acceptNatCon ? acceptNatCon(PTPT_getIntConNatCon(arg)) : PTPT_getIntConNatCon(arg));
  }
  if (PTPT_isIntConNegative(arg)) {
    return PTPT_makeIntConNegative(
        acceptWsAfterNeg ? acceptWsAfterNeg(PTPT_getIntConWsAfterNeg(arg)) : PTPT_getIntConWsAfterNeg(arg),
        acceptNatCon ? acceptNatCon(PTPT_getIntConNatCon(arg)) : PTPT_getIntConNatCon(arg));
  }
  ATabort("not a IntCon: %t\n", arg);
  return (PTPT_IntCon)NULL;
}

/*}}}  */
/*{{{  PTPT_Tree PTPT_visitTree(PTPT_Tree arg, PTPT_OptLayout (*acceptWsAfterTree)(PTPT_OptLayout), PTPT_Ann (*acceptAnn)(PTPT_Ann), PTPT_OptLayout (*acceptWsAfterAppl)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterParenOpen)(PTPT_OptLayout), PTPT_Production (*acceptProd)(PTPT_Production), PTPT_OptLayout (*acceptWsAfterProd)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterComma)(PTPT_OptLayout), PTPT_Args (*acceptArgs)(PTPT_Args), PTPT_OptLayout (*acceptWsAfterArgs)(PTPT_OptLayout), PTPT_NatCon (*acceptCharacter)(PTPT_NatCon), PTPT_OptLayout (*acceptWsAfterLit)(PTPT_OptLayout), PTPT_QLiteral (*acceptString)(PTPT_QLiteral), PTPT_OptLayout (*acceptWsAfterString)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterAmb)(PTPT_OptLayout)) */

PTPT_Tree PTPT_visitTree(PTPT_Tree arg, PTPT_OptLayout (*acceptWsAfterTree)(PTPT_OptLayout), PTPT_Ann (*acceptAnn)(PTPT_Ann), PTPT_OptLayout (*acceptWsAfterAppl)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterParenOpen)(PTPT_OptLayout), PTPT_Production (*acceptProd)(PTPT_Production), PTPT_OptLayout (*acceptWsAfterProd)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterComma)(PTPT_OptLayout), PTPT_Args (*acceptArgs)(PTPT_Args), PTPT_OptLayout (*acceptWsAfterArgs)(PTPT_OptLayout), PTPT_NatCon (*acceptCharacter)(PTPT_NatCon), PTPT_OptLayout (*acceptWsAfterLit)(PTPT_OptLayout), PTPT_QLiteral (*acceptString)(PTPT_QLiteral), PTPT_OptLayout (*acceptWsAfterString)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterAmb)(PTPT_OptLayout))
{
  if (PTPT_isTreeAnnotated(arg)) {
    return PTPT_makeTreeAnnotated(
        PTPT_visitTree(PTPT_getTreeTree(arg), acceptWsAfterTree, acceptAnn, acceptWsAfterAppl, acceptWsAfterParenOpen, acceptProd, acceptWsAfterProd, acceptWsAfterComma, acceptArgs, acceptWsAfterArgs, acceptCharacter, acceptWsAfterLit, acceptString, acceptWsAfterString, acceptWsAfterAmb),
        acceptWsAfterTree ? acceptWsAfterTree(PTPT_getTreeWsAfterTree(arg)) : PTPT_getTreeWsAfterTree(arg),
        acceptAnn ? acceptAnn(PTPT_getTreeAnn(arg)) : PTPT_getTreeAnn(arg));
  }
  if (PTPT_isTreeAppl(arg)) {
    return PTPT_makeTreeAppl(
        acceptWsAfterAppl ? acceptWsAfterAppl(PTPT_getTreeWsAfterAppl(arg)) : PTPT_getTreeWsAfterAppl(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getTreeWsAfterParenOpen(arg)) : PTPT_getTreeWsAfterParenOpen(arg),
        acceptProd ? acceptProd(PTPT_getTreeProd(arg)) : PTPT_getTreeProd(arg),
        acceptWsAfterProd ? acceptWsAfterProd(PTPT_getTreeWsAfterProd(arg)) : PTPT_getTreeWsAfterProd(arg),
        acceptWsAfterComma ? acceptWsAfterComma(PTPT_getTreeWsAfterComma(arg)) : PTPT_getTreeWsAfterComma(arg),
        acceptArgs ? acceptArgs(PTPT_getTreeArgs(arg)) : PTPT_getTreeArgs(arg),
        acceptWsAfterArgs ? acceptWsAfterArgs(PTPT_getTreeWsAfterArgs(arg)) : PTPT_getTreeWsAfterArgs(arg));
  }
  if (PTPT_isTreeChar(arg)) {
    return PTPT_makeTreeChar(
        acceptCharacter ? acceptCharacter(PTPT_getTreeCharacter(arg)) : PTPT_getTreeCharacter(arg));
  }
  if (PTPT_isTreeLit(arg)) {
    return PTPT_makeTreeLit(
        acceptWsAfterLit ? acceptWsAfterLit(PTPT_getTreeWsAfterLit(arg)) : PTPT_getTreeWsAfterLit(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getTreeWsAfterParenOpen(arg)) : PTPT_getTreeWsAfterParenOpen(arg),
        acceptString ? acceptString(PTPT_getTreeString(arg)) : PTPT_getTreeString(arg),
        acceptWsAfterString ? acceptWsAfterString(PTPT_getTreeWsAfterString(arg)) : PTPT_getTreeWsAfterString(arg));
  }
  if (PTPT_isTreeAmb(arg)) {
    return PTPT_makeTreeAmb(
        acceptWsAfterAmb ? acceptWsAfterAmb(PTPT_getTreeWsAfterAmb(arg)) : PTPT_getTreeWsAfterAmb(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getTreeWsAfterParenOpen(arg)) : PTPT_getTreeWsAfterParenOpen(arg),
        acceptArgs ? acceptArgs(PTPT_getTreeArgs(arg)) : PTPT_getTreeArgs(arg),
        acceptWsAfterArgs ? acceptWsAfterArgs(PTPT_getTreeWsAfterArgs(arg)) : PTPT_getTreeWsAfterArgs(arg));
  }
  ATabort("not a Tree: %t\n", arg);
  return (PTPT_Tree)NULL;
}

/*}}}  */
/*{{{  PTPT_Args PTPT_visitArgs(PTPT_Args arg, PTPT_OptLayout (*acceptWsAfterBracketOpen)(PTPT_OptLayout), PTPT_TreeList (*acceptList)(PTPT_TreeList), PTPT_OptLayout (*acceptWsAfterList)(PTPT_OptLayout)) */

PTPT_Args PTPT_visitArgs(PTPT_Args arg, PTPT_OptLayout (*acceptWsAfterBracketOpen)(PTPT_OptLayout), PTPT_TreeList (*acceptList)(PTPT_TreeList), PTPT_OptLayout (*acceptWsAfterList)(PTPT_OptLayout))
{
  if (PTPT_isArgsList(arg)) {
    return PTPT_makeArgsList(
        acceptWsAfterBracketOpen ? acceptWsAfterBracketOpen(PTPT_getArgsWsAfterBracketOpen(arg)) : PTPT_getArgsWsAfterBracketOpen(arg),
        acceptList ? acceptList(PTPT_getArgsList(arg)) : PTPT_getArgsList(arg),
        acceptWsAfterList ? acceptWsAfterList(PTPT_getArgsWsAfterList(arg)) : PTPT_getArgsWsAfterList(arg));
  }
  ATabort("not a Args: %t\n", arg);
  return (PTPT_Args)NULL;
}

/*}}}  */
/*{{{  PTPT_TreeList PTPT_visitTreeList(PTPT_TreeList arg, PTPT_Tree (*acceptHead)(PTPT_Tree), PTPT_OptLayout (*acceptWsAfterFirst)(PTPT_OptLayout), char * (*acceptSep)(char *), PTPT_OptLayout (*acceptWsAfterSep)(PTPT_OptLayout)) */

PTPT_TreeList PTPT_visitTreeList(PTPT_TreeList arg, PTPT_Tree (*acceptHead)(PTPT_Tree), PTPT_OptLayout (*acceptWsAfterFirst)(PTPT_OptLayout), char * (*acceptSep)(char *), PTPT_OptLayout (*acceptWsAfterSep)(PTPT_OptLayout))
{
  if (PTPT_isTreeListEmpty(arg)) {
    return PTPT_makeTreeListEmpty();
  }
  if (PTPT_isTreeListSingle(arg)) {
    return PTPT_makeTreeListSingle(
        acceptHead ? acceptHead(PTPT_getTreeListHead(arg)) : PTPT_getTreeListHead(arg));
  }
  if (PTPT_isTreeListMany(arg)) {
    return PTPT_makeTreeListMany(
        acceptHead ? acceptHead(PTPT_getTreeListHead(arg)) : PTPT_getTreeListHead(arg),
        acceptWsAfterFirst ? acceptWsAfterFirst(PTPT_getTreeListWsAfterFirst(arg)) : PTPT_getTreeListWsAfterFirst(arg),
        acceptSep ? acceptSep(PTPT_getTreeListSep(arg)) : PTPT_getTreeListSep(arg),
        acceptWsAfterSep ? acceptWsAfterSep(PTPT_getTreeListWsAfterSep(arg)) : PTPT_getTreeListWsAfterSep(arg),
        PTPT_visitTreeList(PTPT_getTreeListTail(arg), acceptHead, acceptWsAfterFirst, acceptSep, acceptWsAfterSep));
  }
  ATabort("not a TreeList: %t\n", arg);
  return (PTPT_TreeList)NULL;
}

/*}}}  */
/*{{{  PTPT_Production PTPT_visitProduction(PTPT_Production arg, PTPT_OptLayout (*acceptWsAfterProd)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterParenOpen)(PTPT_OptLayout), PTPT_Symbols (*acceptLhs)(PTPT_Symbols), PTPT_OptLayout (*acceptWsAfterLhs)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterComma1)(PTPT_OptLayout), PTPT_Symbol (*acceptRhs)(PTPT_Symbol), PTPT_OptLayout (*acceptWsAfterRhs)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterComma2)(PTPT_OptLayout), PTPT_Attributes (*acceptAttributes)(PTPT_Attributes), PTPT_OptLayout (*acceptWsAfterAttributes)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterList)(PTPT_OptLayout)) */

PTPT_Production PTPT_visitProduction(PTPT_Production arg, PTPT_OptLayout (*acceptWsAfterProd)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterParenOpen)(PTPT_OptLayout), PTPT_Symbols (*acceptLhs)(PTPT_Symbols), PTPT_OptLayout (*acceptWsAfterLhs)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterComma1)(PTPT_OptLayout), PTPT_Symbol (*acceptRhs)(PTPT_Symbol), PTPT_OptLayout (*acceptWsAfterRhs)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterComma2)(PTPT_OptLayout), PTPT_Attributes (*acceptAttributes)(PTPT_Attributes), PTPT_OptLayout (*acceptWsAfterAttributes)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterList)(PTPT_OptLayout))
{
  if (PTPT_isProductionDefault(arg)) {
    return PTPT_makeProductionDefault(
        acceptWsAfterProd ? acceptWsAfterProd(PTPT_getProductionWsAfterProd(arg)) : PTPT_getProductionWsAfterProd(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getProductionWsAfterParenOpen(arg)) : PTPT_getProductionWsAfterParenOpen(arg),
        acceptLhs ? acceptLhs(PTPT_getProductionLhs(arg)) : PTPT_getProductionLhs(arg),
        acceptWsAfterLhs ? acceptWsAfterLhs(PTPT_getProductionWsAfterLhs(arg)) : PTPT_getProductionWsAfterLhs(arg),
        acceptWsAfterComma1 ? acceptWsAfterComma1(PTPT_getProductionWsAfterComma1(arg)) : PTPT_getProductionWsAfterComma1(arg),
        acceptRhs ? acceptRhs(PTPT_getProductionRhs(arg)) : PTPT_getProductionRhs(arg),
        acceptWsAfterRhs ? acceptWsAfterRhs(PTPT_getProductionWsAfterRhs(arg)) : PTPT_getProductionWsAfterRhs(arg),
        acceptWsAfterComma2 ? acceptWsAfterComma2(PTPT_getProductionWsAfterComma2(arg)) : PTPT_getProductionWsAfterComma2(arg),
        acceptAttributes ? acceptAttributes(PTPT_getProductionAttributes(arg)) : PTPT_getProductionAttributes(arg),
        acceptWsAfterAttributes ? acceptWsAfterAttributes(PTPT_getProductionWsAfterAttributes(arg)) : PTPT_getProductionWsAfterAttributes(arg));
  }
  if (PTPT_isProductionList(arg)) {
    return PTPT_makeProductionList(
        acceptWsAfterList ? acceptWsAfterList(PTPT_getProductionWsAfterList(arg)) : PTPT_getProductionWsAfterList(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getProductionWsAfterParenOpen(arg)) : PTPT_getProductionWsAfterParenOpen(arg),
        acceptRhs ? acceptRhs(PTPT_getProductionRhs(arg)) : PTPT_getProductionRhs(arg),
        acceptWsAfterRhs ? acceptWsAfterRhs(PTPT_getProductionWsAfterRhs(arg)) : PTPT_getProductionWsAfterRhs(arg));
  }
  ATabort("not a Production: %t\n", arg);
  return (PTPT_Production)NULL;
}

/*}}}  */
/*{{{  PTPT_Symbol PTPT_visitSymbol(PTPT_Symbol arg, PTPT_OptLayout (*acceptWsAfterLit)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterParenOpen)(PTPT_OptLayout), PTPT_QLiteral (*acceptString)(PTPT_QLiteral), PTPT_OptLayout (*acceptWsAfterString)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterCf)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterSymbol)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterLex)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterOpt)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterAlt)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterLhs)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterComma)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterRhs)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterSeq)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterTuple)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterHead)(PTPT_OptLayout), PTPT_Symbols (*acceptRest)(PTPT_Symbols), PTPT_OptLayout (*acceptWsAfterRest)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterSort)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterIter)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterIterStar)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterIterSep)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterSeparator)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterIterStarSep)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterIterN)(PTPT_OptLayout), PTPT_NatCon (*acceptNumber)(PTPT_NatCon), PTPT_OptLayout (*acceptWsAfterNumber)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterIterSepN)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterComma1)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterComma2)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterFunc)(PTPT_OptLayout), PTPT_Symbols (*acceptSymbols)(PTPT_Symbols), PTPT_OptLayout (*acceptWsAfterSymbols)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterVarsym)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterCharClass)(PTPT_OptLayout), PTPT_CharRanges (*acceptCharRanges)(PTPT_CharRanges), PTPT_OptLayout (*acceptWsAfterCharRanges)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterStrategy)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterParametrizedSort)(PTPT_OptLayout), PTPT_QLiteral (*acceptSort)(PTPT_QLiteral), PTPT_Symbols (*acceptParameters)(PTPT_Symbols), PTPT_OptLayout (*acceptWsAfterParameters)(PTPT_OptLayout)) */

PTPT_Symbol PTPT_visitSymbol(PTPT_Symbol arg, PTPT_OptLayout (*acceptWsAfterLit)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterParenOpen)(PTPT_OptLayout), PTPT_QLiteral (*acceptString)(PTPT_QLiteral), PTPT_OptLayout (*acceptWsAfterString)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterCf)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterSymbol)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterLex)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterOpt)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterAlt)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterLhs)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterComma)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterRhs)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterSeq)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterTuple)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterHead)(PTPT_OptLayout), PTPT_Symbols (*acceptRest)(PTPT_Symbols), PTPT_OptLayout (*acceptWsAfterRest)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterSort)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterIter)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterIterStar)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterIterSep)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterSeparator)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterIterStarSep)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterIterN)(PTPT_OptLayout), PTPT_NatCon (*acceptNumber)(PTPT_NatCon), PTPT_OptLayout (*acceptWsAfterNumber)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterIterSepN)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterComma1)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterComma2)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterFunc)(PTPT_OptLayout), PTPT_Symbols (*acceptSymbols)(PTPT_Symbols), PTPT_OptLayout (*acceptWsAfterSymbols)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterVarsym)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterCharClass)(PTPT_OptLayout), PTPT_CharRanges (*acceptCharRanges)(PTPT_CharRanges), PTPT_OptLayout (*acceptWsAfterCharRanges)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterStrategy)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterParametrizedSort)(PTPT_OptLayout), PTPT_QLiteral (*acceptSort)(PTPT_QLiteral), PTPT_Symbols (*acceptParameters)(PTPT_Symbols), PTPT_OptLayout (*acceptWsAfterParameters)(PTPT_OptLayout))
{
  if (PTPT_isSymbolEmpty(arg)) {
    return PTPT_makeSymbolEmpty();
  }
  if (PTPT_isSymbolLit(arg)) {
    return PTPT_makeSymbolLit(
        acceptWsAfterLit ? acceptWsAfterLit(PTPT_getSymbolWsAfterLit(arg)) : PTPT_getSymbolWsAfterLit(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getSymbolWsAfterParenOpen(arg)) : PTPT_getSymbolWsAfterParenOpen(arg),
        acceptString ? acceptString(PTPT_getSymbolString(arg)) : PTPT_getSymbolString(arg),
        acceptWsAfterString ? acceptWsAfterString(PTPT_getSymbolWsAfterString(arg)) : PTPT_getSymbolWsAfterString(arg));
  }
  if (PTPT_isSymbolCf(arg)) {
    return PTPT_makeSymbolCf(
        acceptWsAfterCf ? acceptWsAfterCf(PTPT_getSymbolWsAfterCf(arg)) : PTPT_getSymbolWsAfterCf(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getSymbolWsAfterParenOpen(arg)) : PTPT_getSymbolWsAfterParenOpen(arg),
        PTPT_visitSymbol(PTPT_getSymbolSymbol(arg), acceptWsAfterLit, acceptWsAfterParenOpen, acceptString, acceptWsAfterString, acceptWsAfterCf, acceptWsAfterSymbol, acceptWsAfterLex, acceptWsAfterOpt, acceptWsAfterAlt, acceptWsAfterLhs, acceptWsAfterComma, acceptWsAfterRhs, acceptWsAfterSeq, acceptWsAfterTuple, acceptWsAfterHead, acceptRest, acceptWsAfterRest, acceptWsAfterSort, acceptWsAfterIter, acceptWsAfterIterStar, acceptWsAfterIterSep, acceptWsAfterSeparator, acceptWsAfterIterStarSep, acceptWsAfterIterN, acceptNumber, acceptWsAfterNumber, acceptWsAfterIterSepN, acceptWsAfterComma1, acceptWsAfterComma2, acceptWsAfterFunc, acceptSymbols, acceptWsAfterSymbols, acceptWsAfterVarsym, acceptWsAfterCharClass, acceptCharRanges, acceptWsAfterCharRanges, acceptWsAfterStrategy, acceptWsAfterParametrizedSort, acceptSort, acceptParameters, acceptWsAfterParameters),
        acceptWsAfterSymbol ? acceptWsAfterSymbol(PTPT_getSymbolWsAfterSymbol(arg)) : PTPT_getSymbolWsAfterSymbol(arg));
  }
  if (PTPT_isSymbolLex(arg)) {
    return PTPT_makeSymbolLex(
        acceptWsAfterLex ? acceptWsAfterLex(PTPT_getSymbolWsAfterLex(arg)) : PTPT_getSymbolWsAfterLex(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getSymbolWsAfterParenOpen(arg)) : PTPT_getSymbolWsAfterParenOpen(arg),
        PTPT_visitSymbol(PTPT_getSymbolSymbol(arg), acceptWsAfterLit, acceptWsAfterParenOpen, acceptString, acceptWsAfterString, acceptWsAfterCf, acceptWsAfterSymbol, acceptWsAfterLex, acceptWsAfterOpt, acceptWsAfterAlt, acceptWsAfterLhs, acceptWsAfterComma, acceptWsAfterRhs, acceptWsAfterSeq, acceptWsAfterTuple, acceptWsAfterHead, acceptRest, acceptWsAfterRest, acceptWsAfterSort, acceptWsAfterIter, acceptWsAfterIterStar, acceptWsAfterIterSep, acceptWsAfterSeparator, acceptWsAfterIterStarSep, acceptWsAfterIterN, acceptNumber, acceptWsAfterNumber, acceptWsAfterIterSepN, acceptWsAfterComma1, acceptWsAfterComma2, acceptWsAfterFunc, acceptSymbols, acceptWsAfterSymbols, acceptWsAfterVarsym, acceptWsAfterCharClass, acceptCharRanges, acceptWsAfterCharRanges, acceptWsAfterStrategy, acceptWsAfterParametrizedSort, acceptSort, acceptParameters, acceptWsAfterParameters),
        acceptWsAfterSymbol ? acceptWsAfterSymbol(PTPT_getSymbolWsAfterSymbol(arg)) : PTPT_getSymbolWsAfterSymbol(arg));
  }
  if (PTPT_isSymbolOpt(arg)) {
    return PTPT_makeSymbolOpt(
        acceptWsAfterOpt ? acceptWsAfterOpt(PTPT_getSymbolWsAfterOpt(arg)) : PTPT_getSymbolWsAfterOpt(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getSymbolWsAfterParenOpen(arg)) : PTPT_getSymbolWsAfterParenOpen(arg),
        PTPT_visitSymbol(PTPT_getSymbolSymbol(arg), acceptWsAfterLit, acceptWsAfterParenOpen, acceptString, acceptWsAfterString, acceptWsAfterCf, acceptWsAfterSymbol, acceptWsAfterLex, acceptWsAfterOpt, acceptWsAfterAlt, acceptWsAfterLhs, acceptWsAfterComma, acceptWsAfterRhs, acceptWsAfterSeq, acceptWsAfterTuple, acceptWsAfterHead, acceptRest, acceptWsAfterRest, acceptWsAfterSort, acceptWsAfterIter, acceptWsAfterIterStar, acceptWsAfterIterSep, acceptWsAfterSeparator, acceptWsAfterIterStarSep, acceptWsAfterIterN, acceptNumber, acceptWsAfterNumber, acceptWsAfterIterSepN, acceptWsAfterComma1, acceptWsAfterComma2, acceptWsAfterFunc, acceptSymbols, acceptWsAfterSymbols, acceptWsAfterVarsym, acceptWsAfterCharClass, acceptCharRanges, acceptWsAfterCharRanges, acceptWsAfterStrategy, acceptWsAfterParametrizedSort, acceptSort, acceptParameters, acceptWsAfterParameters),
        acceptWsAfterSymbol ? acceptWsAfterSymbol(PTPT_getSymbolWsAfterSymbol(arg)) : PTPT_getSymbolWsAfterSymbol(arg));
  }
  if (PTPT_isSymbolAlt(arg)) {
    return PTPT_makeSymbolAlt(
        acceptWsAfterAlt ? acceptWsAfterAlt(PTPT_getSymbolWsAfterAlt(arg)) : PTPT_getSymbolWsAfterAlt(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getSymbolWsAfterParenOpen(arg)) : PTPT_getSymbolWsAfterParenOpen(arg),
        PTPT_visitSymbol(PTPT_getSymbolLhs(arg), acceptWsAfterLit, acceptWsAfterParenOpen, acceptString, acceptWsAfterString, acceptWsAfterCf, acceptWsAfterSymbol, acceptWsAfterLex, acceptWsAfterOpt, acceptWsAfterAlt, acceptWsAfterLhs, acceptWsAfterComma, acceptWsAfterRhs, acceptWsAfterSeq, acceptWsAfterTuple, acceptWsAfterHead, acceptRest, acceptWsAfterRest, acceptWsAfterSort, acceptWsAfterIter, acceptWsAfterIterStar, acceptWsAfterIterSep, acceptWsAfterSeparator, acceptWsAfterIterStarSep, acceptWsAfterIterN, acceptNumber, acceptWsAfterNumber, acceptWsAfterIterSepN, acceptWsAfterComma1, acceptWsAfterComma2, acceptWsAfterFunc, acceptSymbols, acceptWsAfterSymbols, acceptWsAfterVarsym, acceptWsAfterCharClass, acceptCharRanges, acceptWsAfterCharRanges, acceptWsAfterStrategy, acceptWsAfterParametrizedSort, acceptSort, acceptParameters, acceptWsAfterParameters),
        acceptWsAfterLhs ? acceptWsAfterLhs(PTPT_getSymbolWsAfterLhs(arg)) : PTPT_getSymbolWsAfterLhs(arg),
        acceptWsAfterComma ? acceptWsAfterComma(PTPT_getSymbolWsAfterComma(arg)) : PTPT_getSymbolWsAfterComma(arg),
        PTPT_visitSymbol(PTPT_getSymbolRhs(arg), acceptWsAfterLit, acceptWsAfterParenOpen, acceptString, acceptWsAfterString, acceptWsAfterCf, acceptWsAfterSymbol, acceptWsAfterLex, acceptWsAfterOpt, acceptWsAfterAlt, acceptWsAfterLhs, acceptWsAfterComma, acceptWsAfterRhs, acceptWsAfterSeq, acceptWsAfterTuple, acceptWsAfterHead, acceptRest, acceptWsAfterRest, acceptWsAfterSort, acceptWsAfterIter, acceptWsAfterIterStar, acceptWsAfterIterSep, acceptWsAfterSeparator, acceptWsAfterIterStarSep, acceptWsAfterIterN, acceptNumber, acceptWsAfterNumber, acceptWsAfterIterSepN, acceptWsAfterComma1, acceptWsAfterComma2, acceptWsAfterFunc, acceptSymbols, acceptWsAfterSymbols, acceptWsAfterVarsym, acceptWsAfterCharClass, acceptCharRanges, acceptWsAfterCharRanges, acceptWsAfterStrategy, acceptWsAfterParametrizedSort, acceptSort, acceptParameters, acceptWsAfterParameters),
        acceptWsAfterRhs ? acceptWsAfterRhs(PTPT_getSymbolWsAfterRhs(arg)) : PTPT_getSymbolWsAfterRhs(arg));
  }
  if (PTPT_isSymbolSeq(arg)) {
    return PTPT_makeSymbolSeq(
        acceptWsAfterSeq ? acceptWsAfterSeq(PTPT_getSymbolWsAfterSeq(arg)) : PTPT_getSymbolWsAfterSeq(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getSymbolWsAfterParenOpen(arg)) : PTPT_getSymbolWsAfterParenOpen(arg),
        PTPT_visitSymbol(PTPT_getSymbolLhs(arg), acceptWsAfterLit, acceptWsAfterParenOpen, acceptString, acceptWsAfterString, acceptWsAfterCf, acceptWsAfterSymbol, acceptWsAfterLex, acceptWsAfterOpt, acceptWsAfterAlt, acceptWsAfterLhs, acceptWsAfterComma, acceptWsAfterRhs, acceptWsAfterSeq, acceptWsAfterTuple, acceptWsAfterHead, acceptRest, acceptWsAfterRest, acceptWsAfterSort, acceptWsAfterIter, acceptWsAfterIterStar, acceptWsAfterIterSep, acceptWsAfterSeparator, acceptWsAfterIterStarSep, acceptWsAfterIterN, acceptNumber, acceptWsAfterNumber, acceptWsAfterIterSepN, acceptWsAfterComma1, acceptWsAfterComma2, acceptWsAfterFunc, acceptSymbols, acceptWsAfterSymbols, acceptWsAfterVarsym, acceptWsAfterCharClass, acceptCharRanges, acceptWsAfterCharRanges, acceptWsAfterStrategy, acceptWsAfterParametrizedSort, acceptSort, acceptParameters, acceptWsAfterParameters),
        acceptWsAfterLhs ? acceptWsAfterLhs(PTPT_getSymbolWsAfterLhs(arg)) : PTPT_getSymbolWsAfterLhs(arg),
        acceptWsAfterComma ? acceptWsAfterComma(PTPT_getSymbolWsAfterComma(arg)) : PTPT_getSymbolWsAfterComma(arg),
        PTPT_visitSymbol(PTPT_getSymbolRhs(arg), acceptWsAfterLit, acceptWsAfterParenOpen, acceptString, acceptWsAfterString, acceptWsAfterCf, acceptWsAfterSymbol, acceptWsAfterLex, acceptWsAfterOpt, acceptWsAfterAlt, acceptWsAfterLhs, acceptWsAfterComma, acceptWsAfterRhs, acceptWsAfterSeq, acceptWsAfterTuple, acceptWsAfterHead, acceptRest, acceptWsAfterRest, acceptWsAfterSort, acceptWsAfterIter, acceptWsAfterIterStar, acceptWsAfterIterSep, acceptWsAfterSeparator, acceptWsAfterIterStarSep, acceptWsAfterIterN, acceptNumber, acceptWsAfterNumber, acceptWsAfterIterSepN, acceptWsAfterComma1, acceptWsAfterComma2, acceptWsAfterFunc, acceptSymbols, acceptWsAfterSymbols, acceptWsAfterVarsym, acceptWsAfterCharClass, acceptCharRanges, acceptWsAfterCharRanges, acceptWsAfterStrategy, acceptWsAfterParametrizedSort, acceptSort, acceptParameters, acceptWsAfterParameters),
        acceptWsAfterRhs ? acceptWsAfterRhs(PTPT_getSymbolWsAfterRhs(arg)) : PTPT_getSymbolWsAfterRhs(arg));
  }
  if (PTPT_isSymbolTuple(arg)) {
    return PTPT_makeSymbolTuple(
        acceptWsAfterTuple ? acceptWsAfterTuple(PTPT_getSymbolWsAfterTuple(arg)) : PTPT_getSymbolWsAfterTuple(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getSymbolWsAfterParenOpen(arg)) : PTPT_getSymbolWsAfterParenOpen(arg),
        PTPT_visitSymbol(PTPT_getSymbolHead(arg), acceptWsAfterLit, acceptWsAfterParenOpen, acceptString, acceptWsAfterString, acceptWsAfterCf, acceptWsAfterSymbol, acceptWsAfterLex, acceptWsAfterOpt, acceptWsAfterAlt, acceptWsAfterLhs, acceptWsAfterComma, acceptWsAfterRhs, acceptWsAfterSeq, acceptWsAfterTuple, acceptWsAfterHead, acceptRest, acceptWsAfterRest, acceptWsAfterSort, acceptWsAfterIter, acceptWsAfterIterStar, acceptWsAfterIterSep, acceptWsAfterSeparator, acceptWsAfterIterStarSep, acceptWsAfterIterN, acceptNumber, acceptWsAfterNumber, acceptWsAfterIterSepN, acceptWsAfterComma1, acceptWsAfterComma2, acceptWsAfterFunc, acceptSymbols, acceptWsAfterSymbols, acceptWsAfterVarsym, acceptWsAfterCharClass, acceptCharRanges, acceptWsAfterCharRanges, acceptWsAfterStrategy, acceptWsAfterParametrizedSort, acceptSort, acceptParameters, acceptWsAfterParameters),
        acceptWsAfterHead ? acceptWsAfterHead(PTPT_getSymbolWsAfterHead(arg)) : PTPT_getSymbolWsAfterHead(arg),
        acceptWsAfterComma ? acceptWsAfterComma(PTPT_getSymbolWsAfterComma(arg)) : PTPT_getSymbolWsAfterComma(arg),
        acceptRest ? acceptRest(PTPT_getSymbolRest(arg)) : PTPT_getSymbolRest(arg),
        acceptWsAfterRest ? acceptWsAfterRest(PTPT_getSymbolWsAfterRest(arg)) : PTPT_getSymbolWsAfterRest(arg));
  }
  if (PTPT_isSymbolSort(arg)) {
    return PTPT_makeSymbolSort(
        acceptWsAfterSort ? acceptWsAfterSort(PTPT_getSymbolWsAfterSort(arg)) : PTPT_getSymbolWsAfterSort(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getSymbolWsAfterParenOpen(arg)) : PTPT_getSymbolWsAfterParenOpen(arg),
        acceptString ? acceptString(PTPT_getSymbolString(arg)) : PTPT_getSymbolString(arg),
        acceptWsAfterString ? acceptWsAfterString(PTPT_getSymbolWsAfterString(arg)) : PTPT_getSymbolWsAfterString(arg));
  }
  if (PTPT_isSymbolIter(arg)) {
    return PTPT_makeSymbolIter(
        acceptWsAfterIter ? acceptWsAfterIter(PTPT_getSymbolWsAfterIter(arg)) : PTPT_getSymbolWsAfterIter(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getSymbolWsAfterParenOpen(arg)) : PTPT_getSymbolWsAfterParenOpen(arg),
        PTPT_visitSymbol(PTPT_getSymbolSymbol(arg), acceptWsAfterLit, acceptWsAfterParenOpen, acceptString, acceptWsAfterString, acceptWsAfterCf, acceptWsAfterSymbol, acceptWsAfterLex, acceptWsAfterOpt, acceptWsAfterAlt, acceptWsAfterLhs, acceptWsAfterComma, acceptWsAfterRhs, acceptWsAfterSeq, acceptWsAfterTuple, acceptWsAfterHead, acceptRest, acceptWsAfterRest, acceptWsAfterSort, acceptWsAfterIter, acceptWsAfterIterStar, acceptWsAfterIterSep, acceptWsAfterSeparator, acceptWsAfterIterStarSep, acceptWsAfterIterN, acceptNumber, acceptWsAfterNumber, acceptWsAfterIterSepN, acceptWsAfterComma1, acceptWsAfterComma2, acceptWsAfterFunc, acceptSymbols, acceptWsAfterSymbols, acceptWsAfterVarsym, acceptWsAfterCharClass, acceptCharRanges, acceptWsAfterCharRanges, acceptWsAfterStrategy, acceptWsAfterParametrizedSort, acceptSort, acceptParameters, acceptWsAfterParameters),
        acceptWsAfterSymbol ? acceptWsAfterSymbol(PTPT_getSymbolWsAfterSymbol(arg)) : PTPT_getSymbolWsAfterSymbol(arg));
  }
  if (PTPT_isSymbolIterStar(arg)) {
    return PTPT_makeSymbolIterStar(
        acceptWsAfterIterStar ? acceptWsAfterIterStar(PTPT_getSymbolWsAfterIterStar(arg)) : PTPT_getSymbolWsAfterIterStar(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getSymbolWsAfterParenOpen(arg)) : PTPT_getSymbolWsAfterParenOpen(arg),
        PTPT_visitSymbol(PTPT_getSymbolSymbol(arg), acceptWsAfterLit, acceptWsAfterParenOpen, acceptString, acceptWsAfterString, acceptWsAfterCf, acceptWsAfterSymbol, acceptWsAfterLex, acceptWsAfterOpt, acceptWsAfterAlt, acceptWsAfterLhs, acceptWsAfterComma, acceptWsAfterRhs, acceptWsAfterSeq, acceptWsAfterTuple, acceptWsAfterHead, acceptRest, acceptWsAfterRest, acceptWsAfterSort, acceptWsAfterIter, acceptWsAfterIterStar, acceptWsAfterIterSep, acceptWsAfterSeparator, acceptWsAfterIterStarSep, acceptWsAfterIterN, acceptNumber, acceptWsAfterNumber, acceptWsAfterIterSepN, acceptWsAfterComma1, acceptWsAfterComma2, acceptWsAfterFunc, acceptSymbols, acceptWsAfterSymbols, acceptWsAfterVarsym, acceptWsAfterCharClass, acceptCharRanges, acceptWsAfterCharRanges, acceptWsAfterStrategy, acceptWsAfterParametrizedSort, acceptSort, acceptParameters, acceptWsAfterParameters),
        acceptWsAfterSymbol ? acceptWsAfterSymbol(PTPT_getSymbolWsAfterSymbol(arg)) : PTPT_getSymbolWsAfterSymbol(arg));
  }
  if (PTPT_isSymbolIterSep(arg)) {
    return PTPT_makeSymbolIterSep(
        acceptWsAfterIterSep ? acceptWsAfterIterSep(PTPT_getSymbolWsAfterIterSep(arg)) : PTPT_getSymbolWsAfterIterSep(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getSymbolWsAfterParenOpen(arg)) : PTPT_getSymbolWsAfterParenOpen(arg),
        PTPT_visitSymbol(PTPT_getSymbolSymbol(arg), acceptWsAfterLit, acceptWsAfterParenOpen, acceptString, acceptWsAfterString, acceptWsAfterCf, acceptWsAfterSymbol, acceptWsAfterLex, acceptWsAfterOpt, acceptWsAfterAlt, acceptWsAfterLhs, acceptWsAfterComma, acceptWsAfterRhs, acceptWsAfterSeq, acceptWsAfterTuple, acceptWsAfterHead, acceptRest, acceptWsAfterRest, acceptWsAfterSort, acceptWsAfterIter, acceptWsAfterIterStar, acceptWsAfterIterSep, acceptWsAfterSeparator, acceptWsAfterIterStarSep, acceptWsAfterIterN, acceptNumber, acceptWsAfterNumber, acceptWsAfterIterSepN, acceptWsAfterComma1, acceptWsAfterComma2, acceptWsAfterFunc, acceptSymbols, acceptWsAfterSymbols, acceptWsAfterVarsym, acceptWsAfterCharClass, acceptCharRanges, acceptWsAfterCharRanges, acceptWsAfterStrategy, acceptWsAfterParametrizedSort, acceptSort, acceptParameters, acceptWsAfterParameters),
        acceptWsAfterSymbol ? acceptWsAfterSymbol(PTPT_getSymbolWsAfterSymbol(arg)) : PTPT_getSymbolWsAfterSymbol(arg),
        acceptWsAfterComma ? acceptWsAfterComma(PTPT_getSymbolWsAfterComma(arg)) : PTPT_getSymbolWsAfterComma(arg),
        PTPT_visitSymbol(PTPT_getSymbolSeparator(arg), acceptWsAfterLit, acceptWsAfterParenOpen, acceptString, acceptWsAfterString, acceptWsAfterCf, acceptWsAfterSymbol, acceptWsAfterLex, acceptWsAfterOpt, acceptWsAfterAlt, acceptWsAfterLhs, acceptWsAfterComma, acceptWsAfterRhs, acceptWsAfterSeq, acceptWsAfterTuple, acceptWsAfterHead, acceptRest, acceptWsAfterRest, acceptWsAfterSort, acceptWsAfterIter, acceptWsAfterIterStar, acceptWsAfterIterSep, acceptWsAfterSeparator, acceptWsAfterIterStarSep, acceptWsAfterIterN, acceptNumber, acceptWsAfterNumber, acceptWsAfterIterSepN, acceptWsAfterComma1, acceptWsAfterComma2, acceptWsAfterFunc, acceptSymbols, acceptWsAfterSymbols, acceptWsAfterVarsym, acceptWsAfterCharClass, acceptCharRanges, acceptWsAfterCharRanges, acceptWsAfterStrategy, acceptWsAfterParametrizedSort, acceptSort, acceptParameters, acceptWsAfterParameters),
        acceptWsAfterSeparator ? acceptWsAfterSeparator(PTPT_getSymbolWsAfterSeparator(arg)) : PTPT_getSymbolWsAfterSeparator(arg));
  }
  if (PTPT_isSymbolIterStarSep(arg)) {
    return PTPT_makeSymbolIterStarSep(
        acceptWsAfterIterStarSep ? acceptWsAfterIterStarSep(PTPT_getSymbolWsAfterIterStarSep(arg)) : PTPT_getSymbolWsAfterIterStarSep(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getSymbolWsAfterParenOpen(arg)) : PTPT_getSymbolWsAfterParenOpen(arg),
        PTPT_visitSymbol(PTPT_getSymbolSymbol(arg), acceptWsAfterLit, acceptWsAfterParenOpen, acceptString, acceptWsAfterString, acceptWsAfterCf, acceptWsAfterSymbol, acceptWsAfterLex, acceptWsAfterOpt, acceptWsAfterAlt, acceptWsAfterLhs, acceptWsAfterComma, acceptWsAfterRhs, acceptWsAfterSeq, acceptWsAfterTuple, acceptWsAfterHead, acceptRest, acceptWsAfterRest, acceptWsAfterSort, acceptWsAfterIter, acceptWsAfterIterStar, acceptWsAfterIterSep, acceptWsAfterSeparator, acceptWsAfterIterStarSep, acceptWsAfterIterN, acceptNumber, acceptWsAfterNumber, acceptWsAfterIterSepN, acceptWsAfterComma1, acceptWsAfterComma2, acceptWsAfterFunc, acceptSymbols, acceptWsAfterSymbols, acceptWsAfterVarsym, acceptWsAfterCharClass, acceptCharRanges, acceptWsAfterCharRanges, acceptWsAfterStrategy, acceptWsAfterParametrizedSort, acceptSort, acceptParameters, acceptWsAfterParameters),
        acceptWsAfterSymbol ? acceptWsAfterSymbol(PTPT_getSymbolWsAfterSymbol(arg)) : PTPT_getSymbolWsAfterSymbol(arg),
        acceptWsAfterComma ? acceptWsAfterComma(PTPT_getSymbolWsAfterComma(arg)) : PTPT_getSymbolWsAfterComma(arg),
        PTPT_visitSymbol(PTPT_getSymbolSeparator(arg), acceptWsAfterLit, acceptWsAfterParenOpen, acceptString, acceptWsAfterString, acceptWsAfterCf, acceptWsAfterSymbol, acceptWsAfterLex, acceptWsAfterOpt, acceptWsAfterAlt, acceptWsAfterLhs, acceptWsAfterComma, acceptWsAfterRhs, acceptWsAfterSeq, acceptWsAfterTuple, acceptWsAfterHead, acceptRest, acceptWsAfterRest, acceptWsAfterSort, acceptWsAfterIter, acceptWsAfterIterStar, acceptWsAfterIterSep, acceptWsAfterSeparator, acceptWsAfterIterStarSep, acceptWsAfterIterN, acceptNumber, acceptWsAfterNumber, acceptWsAfterIterSepN, acceptWsAfterComma1, acceptWsAfterComma2, acceptWsAfterFunc, acceptSymbols, acceptWsAfterSymbols, acceptWsAfterVarsym, acceptWsAfterCharClass, acceptCharRanges, acceptWsAfterCharRanges, acceptWsAfterStrategy, acceptWsAfterParametrizedSort, acceptSort, acceptParameters, acceptWsAfterParameters),
        acceptWsAfterSeparator ? acceptWsAfterSeparator(PTPT_getSymbolWsAfterSeparator(arg)) : PTPT_getSymbolWsAfterSeparator(arg));
  }
  if (PTPT_isSymbolIterN(arg)) {
    return PTPT_makeSymbolIterN(
        acceptWsAfterIterN ? acceptWsAfterIterN(PTPT_getSymbolWsAfterIterN(arg)) : PTPT_getSymbolWsAfterIterN(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getSymbolWsAfterParenOpen(arg)) : PTPT_getSymbolWsAfterParenOpen(arg),
        PTPT_visitSymbol(PTPT_getSymbolSymbol(arg), acceptWsAfterLit, acceptWsAfterParenOpen, acceptString, acceptWsAfterString, acceptWsAfterCf, acceptWsAfterSymbol, acceptWsAfterLex, acceptWsAfterOpt, acceptWsAfterAlt, acceptWsAfterLhs, acceptWsAfterComma, acceptWsAfterRhs, acceptWsAfterSeq, acceptWsAfterTuple, acceptWsAfterHead, acceptRest, acceptWsAfterRest, acceptWsAfterSort, acceptWsAfterIter, acceptWsAfterIterStar, acceptWsAfterIterSep, acceptWsAfterSeparator, acceptWsAfterIterStarSep, acceptWsAfterIterN, acceptNumber, acceptWsAfterNumber, acceptWsAfterIterSepN, acceptWsAfterComma1, acceptWsAfterComma2, acceptWsAfterFunc, acceptSymbols, acceptWsAfterSymbols, acceptWsAfterVarsym, acceptWsAfterCharClass, acceptCharRanges, acceptWsAfterCharRanges, acceptWsAfterStrategy, acceptWsAfterParametrizedSort, acceptSort, acceptParameters, acceptWsAfterParameters),
        acceptWsAfterSymbol ? acceptWsAfterSymbol(PTPT_getSymbolWsAfterSymbol(arg)) : PTPT_getSymbolWsAfterSymbol(arg),
        acceptWsAfterComma ? acceptWsAfterComma(PTPT_getSymbolWsAfterComma(arg)) : PTPT_getSymbolWsAfterComma(arg),
        acceptNumber ? acceptNumber(PTPT_getSymbolNumber(arg)) : PTPT_getSymbolNumber(arg),
        acceptWsAfterNumber ? acceptWsAfterNumber(PTPT_getSymbolWsAfterNumber(arg)) : PTPT_getSymbolWsAfterNumber(arg));
  }
  if (PTPT_isSymbolIterSepN(arg)) {
    return PTPT_makeSymbolIterSepN(
        acceptWsAfterIterSepN ? acceptWsAfterIterSepN(PTPT_getSymbolWsAfterIterSepN(arg)) : PTPT_getSymbolWsAfterIterSepN(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getSymbolWsAfterParenOpen(arg)) : PTPT_getSymbolWsAfterParenOpen(arg),
        PTPT_visitSymbol(PTPT_getSymbolSymbol(arg), acceptWsAfterLit, acceptWsAfterParenOpen, acceptString, acceptWsAfterString, acceptWsAfterCf, acceptWsAfterSymbol, acceptWsAfterLex, acceptWsAfterOpt, acceptWsAfterAlt, acceptWsAfterLhs, acceptWsAfterComma, acceptWsAfterRhs, acceptWsAfterSeq, acceptWsAfterTuple, acceptWsAfterHead, acceptRest, acceptWsAfterRest, acceptWsAfterSort, acceptWsAfterIter, acceptWsAfterIterStar, acceptWsAfterIterSep, acceptWsAfterSeparator, acceptWsAfterIterStarSep, acceptWsAfterIterN, acceptNumber, acceptWsAfterNumber, acceptWsAfterIterSepN, acceptWsAfterComma1, acceptWsAfterComma2, acceptWsAfterFunc, acceptSymbols, acceptWsAfterSymbols, acceptWsAfterVarsym, acceptWsAfterCharClass, acceptCharRanges, acceptWsAfterCharRanges, acceptWsAfterStrategy, acceptWsAfterParametrizedSort, acceptSort, acceptParameters, acceptWsAfterParameters),
        acceptWsAfterSymbol ? acceptWsAfterSymbol(PTPT_getSymbolWsAfterSymbol(arg)) : PTPT_getSymbolWsAfterSymbol(arg),
        acceptWsAfterComma1 ? acceptWsAfterComma1(PTPT_getSymbolWsAfterComma1(arg)) : PTPT_getSymbolWsAfterComma1(arg),
        PTPT_visitSymbol(PTPT_getSymbolSeparator(arg), acceptWsAfterLit, acceptWsAfterParenOpen, acceptString, acceptWsAfterString, acceptWsAfterCf, acceptWsAfterSymbol, acceptWsAfterLex, acceptWsAfterOpt, acceptWsAfterAlt, acceptWsAfterLhs, acceptWsAfterComma, acceptWsAfterRhs, acceptWsAfterSeq, acceptWsAfterTuple, acceptWsAfterHead, acceptRest, acceptWsAfterRest, acceptWsAfterSort, acceptWsAfterIter, acceptWsAfterIterStar, acceptWsAfterIterSep, acceptWsAfterSeparator, acceptWsAfterIterStarSep, acceptWsAfterIterN, acceptNumber, acceptWsAfterNumber, acceptWsAfterIterSepN, acceptWsAfterComma1, acceptWsAfterComma2, acceptWsAfterFunc, acceptSymbols, acceptWsAfterSymbols, acceptWsAfterVarsym, acceptWsAfterCharClass, acceptCharRanges, acceptWsAfterCharRanges, acceptWsAfterStrategy, acceptWsAfterParametrizedSort, acceptSort, acceptParameters, acceptWsAfterParameters),
        acceptWsAfterSeparator ? acceptWsAfterSeparator(PTPT_getSymbolWsAfterSeparator(arg)) : PTPT_getSymbolWsAfterSeparator(arg),
        acceptWsAfterComma2 ? acceptWsAfterComma2(PTPT_getSymbolWsAfterComma2(arg)) : PTPT_getSymbolWsAfterComma2(arg),
        acceptNumber ? acceptNumber(PTPT_getSymbolNumber(arg)) : PTPT_getSymbolNumber(arg),
        acceptWsAfterNumber ? acceptWsAfterNumber(PTPT_getSymbolWsAfterNumber(arg)) : PTPT_getSymbolWsAfterNumber(arg));
  }
  if (PTPT_isSymbolFunc(arg)) {
    return PTPT_makeSymbolFunc(
        acceptWsAfterFunc ? acceptWsAfterFunc(PTPT_getSymbolWsAfterFunc(arg)) : PTPT_getSymbolWsAfterFunc(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getSymbolWsAfterParenOpen(arg)) : PTPT_getSymbolWsAfterParenOpen(arg),
        acceptSymbols ? acceptSymbols(PTPT_getSymbolSymbols(arg)) : PTPT_getSymbolSymbols(arg),
        acceptWsAfterSymbols ? acceptWsAfterSymbols(PTPT_getSymbolWsAfterSymbols(arg)) : PTPT_getSymbolWsAfterSymbols(arg),
        acceptWsAfterComma ? acceptWsAfterComma(PTPT_getSymbolWsAfterComma(arg)) : PTPT_getSymbolWsAfterComma(arg),
        PTPT_visitSymbol(PTPT_getSymbolSymbol(arg), acceptWsAfterLit, acceptWsAfterParenOpen, acceptString, acceptWsAfterString, acceptWsAfterCf, acceptWsAfterSymbol, acceptWsAfterLex, acceptWsAfterOpt, acceptWsAfterAlt, acceptWsAfterLhs, acceptWsAfterComma, acceptWsAfterRhs, acceptWsAfterSeq, acceptWsAfterTuple, acceptWsAfterHead, acceptRest, acceptWsAfterRest, acceptWsAfterSort, acceptWsAfterIter, acceptWsAfterIterStar, acceptWsAfterIterSep, acceptWsAfterSeparator, acceptWsAfterIterStarSep, acceptWsAfterIterN, acceptNumber, acceptWsAfterNumber, acceptWsAfterIterSepN, acceptWsAfterComma1, acceptWsAfterComma2, acceptWsAfterFunc, acceptSymbols, acceptWsAfterSymbols, acceptWsAfterVarsym, acceptWsAfterCharClass, acceptCharRanges, acceptWsAfterCharRanges, acceptWsAfterStrategy, acceptWsAfterParametrizedSort, acceptSort, acceptParameters, acceptWsAfterParameters),
        acceptWsAfterSymbol ? acceptWsAfterSymbol(PTPT_getSymbolWsAfterSymbol(arg)) : PTPT_getSymbolWsAfterSymbol(arg));
  }
  if (PTPT_isSymbolVarsym(arg)) {
    return PTPT_makeSymbolVarsym(
        acceptWsAfterVarsym ? acceptWsAfterVarsym(PTPT_getSymbolWsAfterVarsym(arg)) : PTPT_getSymbolWsAfterVarsym(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getSymbolWsAfterParenOpen(arg)) : PTPT_getSymbolWsAfterParenOpen(arg),
        PTPT_visitSymbol(PTPT_getSymbolSymbol(arg), acceptWsAfterLit, acceptWsAfterParenOpen, acceptString, acceptWsAfterString, acceptWsAfterCf, acceptWsAfterSymbol, acceptWsAfterLex, acceptWsAfterOpt, acceptWsAfterAlt, acceptWsAfterLhs, acceptWsAfterComma, acceptWsAfterRhs, acceptWsAfterSeq, acceptWsAfterTuple, acceptWsAfterHead, acceptRest, acceptWsAfterRest, acceptWsAfterSort, acceptWsAfterIter, acceptWsAfterIterStar, acceptWsAfterIterSep, acceptWsAfterSeparator, acceptWsAfterIterStarSep, acceptWsAfterIterN, acceptNumber, acceptWsAfterNumber, acceptWsAfterIterSepN, acceptWsAfterComma1, acceptWsAfterComma2, acceptWsAfterFunc, acceptSymbols, acceptWsAfterSymbols, acceptWsAfterVarsym, acceptWsAfterCharClass, acceptCharRanges, acceptWsAfterCharRanges, acceptWsAfterStrategy, acceptWsAfterParametrizedSort, acceptSort, acceptParameters, acceptWsAfterParameters),
        acceptWsAfterSymbol ? acceptWsAfterSymbol(PTPT_getSymbolWsAfterSymbol(arg)) : PTPT_getSymbolWsAfterSymbol(arg));
  }
  if (PTPT_isSymbolLayout(arg)) {
    return PTPT_makeSymbolLayout();
  }
  if (PTPT_isSymbolCharClass(arg)) {
    return PTPT_makeSymbolCharClass(
        acceptWsAfterCharClass ? acceptWsAfterCharClass(PTPT_getSymbolWsAfterCharClass(arg)) : PTPT_getSymbolWsAfterCharClass(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getSymbolWsAfterParenOpen(arg)) : PTPT_getSymbolWsAfterParenOpen(arg),
        acceptCharRanges ? acceptCharRanges(PTPT_getSymbolCharRanges(arg)) : PTPT_getSymbolCharRanges(arg),
        acceptWsAfterCharRanges ? acceptWsAfterCharRanges(PTPT_getSymbolWsAfterCharRanges(arg)) : PTPT_getSymbolWsAfterCharRanges(arg));
  }
  if (PTPT_isSymbolStrategy(arg)) {
    return PTPT_makeSymbolStrategy(
        acceptWsAfterStrategy ? acceptWsAfterStrategy(PTPT_getSymbolWsAfterStrategy(arg)) : PTPT_getSymbolWsAfterStrategy(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getSymbolWsAfterParenOpen(arg)) : PTPT_getSymbolWsAfterParenOpen(arg),
        PTPT_visitSymbol(PTPT_getSymbolLhs(arg), acceptWsAfterLit, acceptWsAfterParenOpen, acceptString, acceptWsAfterString, acceptWsAfterCf, acceptWsAfterSymbol, acceptWsAfterLex, acceptWsAfterOpt, acceptWsAfterAlt, acceptWsAfterLhs, acceptWsAfterComma, acceptWsAfterRhs, acceptWsAfterSeq, acceptWsAfterTuple, acceptWsAfterHead, acceptRest, acceptWsAfterRest, acceptWsAfterSort, acceptWsAfterIter, acceptWsAfterIterStar, acceptWsAfterIterSep, acceptWsAfterSeparator, acceptWsAfterIterStarSep, acceptWsAfterIterN, acceptNumber, acceptWsAfterNumber, acceptWsAfterIterSepN, acceptWsAfterComma1, acceptWsAfterComma2, acceptWsAfterFunc, acceptSymbols, acceptWsAfterSymbols, acceptWsAfterVarsym, acceptWsAfterCharClass, acceptCharRanges, acceptWsAfterCharRanges, acceptWsAfterStrategy, acceptWsAfterParametrizedSort, acceptSort, acceptParameters, acceptWsAfterParameters),
        acceptWsAfterLhs ? acceptWsAfterLhs(PTPT_getSymbolWsAfterLhs(arg)) : PTPT_getSymbolWsAfterLhs(arg),
        acceptWsAfterComma ? acceptWsAfterComma(PTPT_getSymbolWsAfterComma(arg)) : PTPT_getSymbolWsAfterComma(arg),
        PTPT_visitSymbol(PTPT_getSymbolRhs(arg), acceptWsAfterLit, acceptWsAfterParenOpen, acceptString, acceptWsAfterString, acceptWsAfterCf, acceptWsAfterSymbol, acceptWsAfterLex, acceptWsAfterOpt, acceptWsAfterAlt, acceptWsAfterLhs, acceptWsAfterComma, acceptWsAfterRhs, acceptWsAfterSeq, acceptWsAfterTuple, acceptWsAfterHead, acceptRest, acceptWsAfterRest, acceptWsAfterSort, acceptWsAfterIter, acceptWsAfterIterStar, acceptWsAfterIterSep, acceptWsAfterSeparator, acceptWsAfterIterStarSep, acceptWsAfterIterN, acceptNumber, acceptWsAfterNumber, acceptWsAfterIterSepN, acceptWsAfterComma1, acceptWsAfterComma2, acceptWsAfterFunc, acceptSymbols, acceptWsAfterSymbols, acceptWsAfterVarsym, acceptWsAfterCharClass, acceptCharRanges, acceptWsAfterCharRanges, acceptWsAfterStrategy, acceptWsAfterParametrizedSort, acceptSort, acceptParameters, acceptWsAfterParameters),
        acceptWsAfterRhs ? acceptWsAfterRhs(PTPT_getSymbolWsAfterRhs(arg)) : PTPT_getSymbolWsAfterRhs(arg));
  }
  if (PTPT_isSymbolParametrizedSort(arg)) {
    return PTPT_makeSymbolParametrizedSort(
        acceptWsAfterParametrizedSort ? acceptWsAfterParametrizedSort(PTPT_getSymbolWsAfterParametrizedSort(arg)) : PTPT_getSymbolWsAfterParametrizedSort(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getSymbolWsAfterParenOpen(arg)) : PTPT_getSymbolWsAfterParenOpen(arg),
        acceptSort ? acceptSort(PTPT_getSymbolSort(arg)) : PTPT_getSymbolSort(arg),
        acceptWsAfterSort ? acceptWsAfterSort(PTPT_getSymbolWsAfterSort(arg)) : PTPT_getSymbolWsAfterSort(arg),
        acceptWsAfterComma ? acceptWsAfterComma(PTPT_getSymbolWsAfterComma(arg)) : PTPT_getSymbolWsAfterComma(arg),
        acceptParameters ? acceptParameters(PTPT_getSymbolParameters(arg)) : PTPT_getSymbolParameters(arg),
        acceptWsAfterParameters ? acceptWsAfterParameters(PTPT_getSymbolWsAfterParameters(arg)) : PTPT_getSymbolWsAfterParameters(arg));
  }
  ATabort("not a Symbol: %t\n", arg);
  return (PTPT_Symbol)NULL;
}

/*}}}  */
/*{{{  PTPT_Symbols PTPT_visitSymbols(PTPT_Symbols arg, PTPT_OptLayout (*acceptWsAfterBracketOpen)(PTPT_OptLayout), PTPT_SymbolList (*acceptList)(PTPT_SymbolList), PTPT_OptLayout (*acceptWsAfterList)(PTPT_OptLayout)) */

PTPT_Symbols PTPT_visitSymbols(PTPT_Symbols arg, PTPT_OptLayout (*acceptWsAfterBracketOpen)(PTPT_OptLayout), PTPT_SymbolList (*acceptList)(PTPT_SymbolList), PTPT_OptLayout (*acceptWsAfterList)(PTPT_OptLayout))
{
  if (PTPT_isSymbolsList(arg)) {
    return PTPT_makeSymbolsList(
        acceptWsAfterBracketOpen ? acceptWsAfterBracketOpen(PTPT_getSymbolsWsAfterBracketOpen(arg)) : PTPT_getSymbolsWsAfterBracketOpen(arg),
        acceptList ? acceptList(PTPT_getSymbolsList(arg)) : PTPT_getSymbolsList(arg),
        acceptWsAfterList ? acceptWsAfterList(PTPT_getSymbolsWsAfterList(arg)) : PTPT_getSymbolsWsAfterList(arg));
  }
  ATabort("not a Symbols: %t\n", arg);
  return (PTPT_Symbols)NULL;
}

/*}}}  */
/*{{{  PTPT_SymbolList PTPT_visitSymbolList(PTPT_SymbolList arg, PTPT_Symbol (*acceptHead)(PTPT_Symbol), PTPT_OptLayout (*acceptWsAfterFirst)(PTPT_OptLayout), char * (*acceptSep)(char *), PTPT_OptLayout (*acceptWsAfterSep)(PTPT_OptLayout)) */

PTPT_SymbolList PTPT_visitSymbolList(PTPT_SymbolList arg, PTPT_Symbol (*acceptHead)(PTPT_Symbol), PTPT_OptLayout (*acceptWsAfterFirst)(PTPT_OptLayout), char * (*acceptSep)(char *), PTPT_OptLayout (*acceptWsAfterSep)(PTPT_OptLayout))
{
  if (PTPT_isSymbolListEmpty(arg)) {
    return PTPT_makeSymbolListEmpty();
  }
  if (PTPT_isSymbolListSingle(arg)) {
    return PTPT_makeSymbolListSingle(
        acceptHead ? acceptHead(PTPT_getSymbolListHead(arg)) : PTPT_getSymbolListHead(arg));
  }
  if (PTPT_isSymbolListMany(arg)) {
    return PTPT_makeSymbolListMany(
        acceptHead ? acceptHead(PTPT_getSymbolListHead(arg)) : PTPT_getSymbolListHead(arg),
        acceptWsAfterFirst ? acceptWsAfterFirst(PTPT_getSymbolListWsAfterFirst(arg)) : PTPT_getSymbolListWsAfterFirst(arg),
        acceptSep ? acceptSep(PTPT_getSymbolListSep(arg)) : PTPT_getSymbolListSep(arg),
        acceptWsAfterSep ? acceptWsAfterSep(PTPT_getSymbolListWsAfterSep(arg)) : PTPT_getSymbolListWsAfterSep(arg),
        PTPT_visitSymbolList(PTPT_getSymbolListTail(arg), acceptHead, acceptWsAfterFirst, acceptSep, acceptWsAfterSep));
  }
  ATabort("not a SymbolList: %t\n", arg);
  return (PTPT_SymbolList)NULL;
}

/*}}}  */
/*{{{  PTPT_CharRanges PTPT_visitCharRanges(PTPT_CharRanges arg, PTPT_OptLayout (*acceptWsAfterBracketOpen)(PTPT_OptLayout), PTPT_CharRangeList (*acceptList)(PTPT_CharRangeList), PTPT_OptLayout (*acceptWsAfterList)(PTPT_OptLayout)) */

PTPT_CharRanges PTPT_visitCharRanges(PTPT_CharRanges arg, PTPT_OptLayout (*acceptWsAfterBracketOpen)(PTPT_OptLayout), PTPT_CharRangeList (*acceptList)(PTPT_CharRangeList), PTPT_OptLayout (*acceptWsAfterList)(PTPT_OptLayout))
{
  if (PTPT_isCharRangesList(arg)) {
    return PTPT_makeCharRangesList(
        acceptWsAfterBracketOpen ? acceptWsAfterBracketOpen(PTPT_getCharRangesWsAfterBracketOpen(arg)) : PTPT_getCharRangesWsAfterBracketOpen(arg),
        acceptList ? acceptList(PTPT_getCharRangesList(arg)) : PTPT_getCharRangesList(arg),
        acceptWsAfterList ? acceptWsAfterList(PTPT_getCharRangesWsAfterList(arg)) : PTPT_getCharRangesWsAfterList(arg));
  }
  ATabort("not a CharRanges: %t\n", arg);
  return (PTPT_CharRanges)NULL;
}

/*}}}  */
/*{{{  PTPT_CharRangeList PTPT_visitCharRangeList(PTPT_CharRangeList arg, PTPT_CharRange (*acceptHead)(PTPT_CharRange), PTPT_OptLayout (*acceptWsAfterFirst)(PTPT_OptLayout), char * (*acceptSep)(char *), PTPT_OptLayout (*acceptWsAfterSep)(PTPT_OptLayout)) */

PTPT_CharRangeList PTPT_visitCharRangeList(PTPT_CharRangeList arg, PTPT_CharRange (*acceptHead)(PTPT_CharRange), PTPT_OptLayout (*acceptWsAfterFirst)(PTPT_OptLayout), char * (*acceptSep)(char *), PTPT_OptLayout (*acceptWsAfterSep)(PTPT_OptLayout))
{
  if (PTPT_isCharRangeListEmpty(arg)) {
    return PTPT_makeCharRangeListEmpty();
  }
  if (PTPT_isCharRangeListSingle(arg)) {
    return PTPT_makeCharRangeListSingle(
        acceptHead ? acceptHead(PTPT_getCharRangeListHead(arg)) : PTPT_getCharRangeListHead(arg));
  }
  if (PTPT_isCharRangeListMany(arg)) {
    return PTPT_makeCharRangeListMany(
        acceptHead ? acceptHead(PTPT_getCharRangeListHead(arg)) : PTPT_getCharRangeListHead(arg),
        acceptWsAfterFirst ? acceptWsAfterFirst(PTPT_getCharRangeListWsAfterFirst(arg)) : PTPT_getCharRangeListWsAfterFirst(arg),
        acceptSep ? acceptSep(PTPT_getCharRangeListSep(arg)) : PTPT_getCharRangeListSep(arg),
        acceptWsAfterSep ? acceptWsAfterSep(PTPT_getCharRangeListWsAfterSep(arg)) : PTPT_getCharRangeListWsAfterSep(arg),
        PTPT_visitCharRangeList(PTPT_getCharRangeListTail(arg), acceptHead, acceptWsAfterFirst, acceptSep, acceptWsAfterSep));
  }
  ATabort("not a CharRangeList: %t\n", arg);
  return (PTPT_CharRangeList)NULL;
}

/*}}}  */
/*{{{  PTPT_CharRange PTPT_visitCharRange(PTPT_CharRange arg, PTPT_NatCon (*acceptInteger)(PTPT_NatCon), PTPT_OptLayout (*acceptWsAfterRange)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterParenOpen)(PTPT_OptLayout), PTPT_NatCon (*acceptStart)(PTPT_NatCon), PTPT_OptLayout (*acceptWsAfterStart)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterComma)(PTPT_OptLayout), PTPT_NatCon (*acceptEnd)(PTPT_NatCon), PTPT_OptLayout (*acceptWsAfterEnd)(PTPT_OptLayout)) */

PTPT_CharRange PTPT_visitCharRange(PTPT_CharRange arg, PTPT_NatCon (*acceptInteger)(PTPT_NatCon), PTPT_OptLayout (*acceptWsAfterRange)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterParenOpen)(PTPT_OptLayout), PTPT_NatCon (*acceptStart)(PTPT_NatCon), PTPT_OptLayout (*acceptWsAfterStart)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterComma)(PTPT_OptLayout), PTPT_NatCon (*acceptEnd)(PTPT_NatCon), PTPT_OptLayout (*acceptWsAfterEnd)(PTPT_OptLayout))
{
  if (PTPT_isCharRangeCharacter(arg)) {
    return PTPT_makeCharRangeCharacter(
        acceptInteger ? acceptInteger(PTPT_getCharRangeInteger(arg)) : PTPT_getCharRangeInteger(arg));
  }
  if (PTPT_isCharRangeRange(arg)) {
    return PTPT_makeCharRangeRange(
        acceptWsAfterRange ? acceptWsAfterRange(PTPT_getCharRangeWsAfterRange(arg)) : PTPT_getCharRangeWsAfterRange(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getCharRangeWsAfterParenOpen(arg)) : PTPT_getCharRangeWsAfterParenOpen(arg),
        acceptStart ? acceptStart(PTPT_getCharRangeStart(arg)) : PTPT_getCharRangeStart(arg),
        acceptWsAfterStart ? acceptWsAfterStart(PTPT_getCharRangeWsAfterStart(arg)) : PTPT_getCharRangeWsAfterStart(arg),
        acceptWsAfterComma ? acceptWsAfterComma(PTPT_getCharRangeWsAfterComma(arg)) : PTPT_getCharRangeWsAfterComma(arg),
        acceptEnd ? acceptEnd(PTPT_getCharRangeEnd(arg)) : PTPT_getCharRangeEnd(arg),
        acceptWsAfterEnd ? acceptWsAfterEnd(PTPT_getCharRangeWsAfterEnd(arg)) : PTPT_getCharRangeWsAfterEnd(arg));
  }
  ATabort("not a CharRange: %t\n", arg);
  return (PTPT_CharRange)NULL;
}

/*}}}  */
/*{{{  PTPT_OptExp PTPT_visitOptExp(PTPT_OptExp arg, PTPT_OptLayout (*acceptWsAfterE)(PTPT_OptLayout), PTPT_IntCon (*acceptIntCon)(PTPT_IntCon)) */

PTPT_OptExp PTPT_visitOptExp(PTPT_OptExp arg, PTPT_OptLayout (*acceptWsAfterE)(PTPT_OptLayout), PTPT_IntCon (*acceptIntCon)(PTPT_IntCon))
{
  if (PTPT_isOptExpPresent(arg)) {
    return PTPT_makeOptExpPresent(
        acceptWsAfterE ? acceptWsAfterE(PTPT_getOptExpWsAfterE(arg)) : PTPT_getOptExpWsAfterE(arg),
        acceptIntCon ? acceptIntCon(PTPT_getOptExpIntCon(arg)) : PTPT_getOptExpIntCon(arg));
  }
  if (PTPT_isOptExpAbsent(arg)) {
    return PTPT_makeOptExpAbsent();
  }
  ATabort("not a OptExp: %t\n", arg);
  return (PTPT_OptExp)NULL;
}

/*}}}  */
/*{{{  PTPT_RealCon PTPT_visitRealCon(PTPT_RealCon arg, PTPT_IntCon (*acceptIntCon)(PTPT_IntCon), PTPT_OptLayout (*acceptWsAfterIntCon)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterPeriod)(PTPT_OptLayout), PTPT_NatCon (*acceptNatCon)(PTPT_NatCon), PTPT_OptLayout (*acceptWsAfterNatCon)(PTPT_OptLayout), PTPT_OptExp (*acceptOptExp)(PTPT_OptExp)) */

PTPT_RealCon PTPT_visitRealCon(PTPT_RealCon arg, PTPT_IntCon (*acceptIntCon)(PTPT_IntCon), PTPT_OptLayout (*acceptWsAfterIntCon)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterPeriod)(PTPT_OptLayout), PTPT_NatCon (*acceptNatCon)(PTPT_NatCon), PTPT_OptLayout (*acceptWsAfterNatCon)(PTPT_OptLayout), PTPT_OptExp (*acceptOptExp)(PTPT_OptExp))
{
  if (PTPT_isRealConRealCon(arg)) {
    return PTPT_makeRealConRealCon(
        acceptIntCon ? acceptIntCon(PTPT_getRealConIntCon(arg)) : PTPT_getRealConIntCon(arg),
        acceptWsAfterIntCon ? acceptWsAfterIntCon(PTPT_getRealConWsAfterIntCon(arg)) : PTPT_getRealConWsAfterIntCon(arg),
        acceptWsAfterPeriod ? acceptWsAfterPeriod(PTPT_getRealConWsAfterPeriod(arg)) : PTPT_getRealConWsAfterPeriod(arg),
        acceptNatCon ? acceptNatCon(PTPT_getRealConNatCon(arg)) : PTPT_getRealConNatCon(arg),
        acceptWsAfterNatCon ? acceptWsAfterNatCon(PTPT_getRealConWsAfterNatCon(arg)) : PTPT_getRealConWsAfterNatCon(arg),
        acceptOptExp ? acceptOptExp(PTPT_getRealConOptExp(arg)) : PTPT_getRealConOptExp(arg));
  }
  ATabort("not a RealCon: %t\n", arg);
  return (PTPT_RealCon)NULL;
}

/*}}}  */
/*{{{  PTPT_ATermList PTPT_visitATermList(PTPT_ATermList arg, PTPT_OptLayout (*acceptWsAfterBracketOpen)(PTPT_OptLayout), PTPT_ATermElems (*acceptElems)(PTPT_ATermElems), PTPT_OptLayout (*acceptWsAfterElems)(PTPT_OptLayout)) */

PTPT_ATermList PTPT_visitATermList(PTPT_ATermList arg, PTPT_OptLayout (*acceptWsAfterBracketOpen)(PTPT_OptLayout), PTPT_ATermElems (*acceptElems)(PTPT_ATermElems), PTPT_OptLayout (*acceptWsAfterElems)(PTPT_OptLayout))
{
  if (PTPT_isATermListNotEmpty(arg)) {
    return PTPT_makeATermListNotEmpty(
        acceptWsAfterBracketOpen ? acceptWsAfterBracketOpen(PTPT_getATermListWsAfterBracketOpen(arg)) : PTPT_getATermListWsAfterBracketOpen(arg),
        acceptElems ? acceptElems(PTPT_getATermListElems(arg)) : PTPT_getATermListElems(arg),
        acceptWsAfterElems ? acceptWsAfterElems(PTPT_getATermListWsAfterElems(arg)) : PTPT_getATermListWsAfterElems(arg));
  }
  ATabort("not a ATermList: %t\n", arg);
  return (PTPT_ATermList)NULL;
}

/*}}}  */
/*{{{  PTPT_ATermElems PTPT_visitATermElems(PTPT_ATermElems arg, PTPT_ATerm (*acceptHead)(PTPT_ATerm), PTPT_OptLayout (*acceptWsAfterFirst)(PTPT_OptLayout), char * (*acceptSep)(char *), PTPT_OptLayout (*acceptWsAfterSep)(PTPT_OptLayout)) */

PTPT_ATermElems PTPT_visitATermElems(PTPT_ATermElems arg, PTPT_ATerm (*acceptHead)(PTPT_ATerm), PTPT_OptLayout (*acceptWsAfterFirst)(PTPT_OptLayout), char * (*acceptSep)(char *), PTPT_OptLayout (*acceptWsAfterSep)(PTPT_OptLayout))
{
  if (PTPT_isATermElemsEmpty(arg)) {
    return PTPT_makeATermElemsEmpty();
  }
  if (PTPT_isATermElemsSingle(arg)) {
    return PTPT_makeATermElemsSingle(
        acceptHead ? acceptHead(PTPT_getATermElemsHead(arg)) : PTPT_getATermElemsHead(arg));
  }
  if (PTPT_isATermElemsMany(arg)) {
    return PTPT_makeATermElemsMany(
        acceptHead ? acceptHead(PTPT_getATermElemsHead(arg)) : PTPT_getATermElemsHead(arg),
        acceptWsAfterFirst ? acceptWsAfterFirst(PTPT_getATermElemsWsAfterFirst(arg)) : PTPT_getATermElemsWsAfterFirst(arg),
        acceptSep ? acceptSep(PTPT_getATermElemsSep(arg)) : PTPT_getATermElemsSep(arg),
        acceptWsAfterSep ? acceptWsAfterSep(PTPT_getATermElemsWsAfterSep(arg)) : PTPT_getATermElemsWsAfterSep(arg),
        PTPT_visitATermElems(PTPT_getATermElemsTail(arg), acceptHead, acceptWsAfterFirst, acceptSep, acceptWsAfterSep));
  }
  ATabort("not a ATermElems: %t\n", arg);
  return (PTPT_ATermElems)NULL;
}

/*}}}  */
/*{{{  PTPT_ACon PTPT_visitACon(PTPT_ACon arg, PTPT_IntCon (*acceptIntCon)(PTPT_IntCon), PTPT_RealCon (*acceptRealCon)(PTPT_RealCon)) */

PTPT_ACon PTPT_visitACon(PTPT_ACon arg, PTPT_IntCon (*acceptIntCon)(PTPT_IntCon), PTPT_RealCon (*acceptRealCon)(PTPT_RealCon))
{
  if (PTPT_isAConInt(arg)) {
    return PTPT_makeAConInt(
        acceptIntCon ? acceptIntCon(PTPT_getAConIntCon(arg)) : PTPT_getAConIntCon(arg));
  }
  if (PTPT_isAConReal(arg)) {
    return PTPT_makeAConReal(
        acceptRealCon ? acceptRealCon(PTPT_getAConRealCon(arg)) : PTPT_getAConRealCon(arg));
  }
  ATabort("not a ACon: %t\n", arg);
  return (PTPT_ACon)NULL;
}

/*}}}  */
/*{{{  PTPT_AFun PTPT_visitAFun(PTPT_AFun arg, PTPT_Literal (*acceptLiteral)(PTPT_Literal)) */

PTPT_AFun PTPT_visitAFun(PTPT_AFun arg, PTPT_Literal (*acceptLiteral)(PTPT_Literal))
{
  if (PTPT_isAFunDefault(arg)) {
    return PTPT_makeAFunDefault(
        acceptLiteral ? acceptLiteral(PTPT_getAFunLiteral(arg)) : PTPT_getAFunLiteral(arg));
  }
  ATabort("not a AFun: %t\n", arg);
  return (PTPT_AFun)NULL;
}

/*}}}  */
/*{{{  PTPT_ATerm PTPT_visitATerm(PTPT_ATerm arg, PTPT_ACon (*acceptACon)(PTPT_ACon), PTPT_ATermList (*acceptList)(PTPT_ATermList), PTPT_AFun (*acceptFun)(PTPT_AFun), PTPT_OptLayout (*acceptWsAfterFun)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterParenOpen)(PTPT_OptLayout), PTPT_ATermArgs (*acceptArgs)(PTPT_ATermArgs), PTPT_OptLayout (*acceptWsAfterArgs)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterACon)(PTPT_OptLayout), PTPT_Ann (*acceptAnn)(PTPT_Ann), PTPT_OptLayout (*acceptWsAfterList)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterParenClose)(PTPT_OptLayout)) */

PTPT_ATerm PTPT_visitATerm(PTPT_ATerm arg, PTPT_ACon (*acceptACon)(PTPT_ACon), PTPT_ATermList (*acceptList)(PTPT_ATermList), PTPT_AFun (*acceptFun)(PTPT_AFun), PTPT_OptLayout (*acceptWsAfterFun)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterParenOpen)(PTPT_OptLayout), PTPT_ATermArgs (*acceptArgs)(PTPT_ATermArgs), PTPT_OptLayout (*acceptWsAfterArgs)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterACon)(PTPT_OptLayout), PTPT_Ann (*acceptAnn)(PTPT_Ann), PTPT_OptLayout (*acceptWsAfterList)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterParenClose)(PTPT_OptLayout))
{
  if (PTPT_isATermConstant(arg)) {
    return PTPT_makeATermConstant(
        acceptACon ? acceptACon(PTPT_getATermACon(arg)) : PTPT_getATermACon(arg));
  }
  if (PTPT_isATermList(arg)) {
    return PTPT_makeATermList(
        acceptList ? acceptList(PTPT_getATermList(arg)) : PTPT_getATermList(arg));
  }
  if (PTPT_isATermFun(arg)) {
    return PTPT_makeATermFun(
        acceptFun ? acceptFun(PTPT_getATermFun(arg)) : PTPT_getATermFun(arg));
  }
  if (PTPT_isATermAppl(arg)) {
    return PTPT_makeATermAppl(
        acceptFun ? acceptFun(PTPT_getATermFun(arg)) : PTPT_getATermFun(arg),
        acceptWsAfterFun ? acceptWsAfterFun(PTPT_getATermWsAfterFun(arg)) : PTPT_getATermWsAfterFun(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getATermWsAfterParenOpen(arg)) : PTPT_getATermWsAfterParenOpen(arg),
        acceptArgs ? acceptArgs(PTPT_getATermArgs(arg)) : PTPT_getATermArgs(arg),
        acceptWsAfterArgs ? acceptWsAfterArgs(PTPT_getATermWsAfterArgs(arg)) : PTPT_getATermWsAfterArgs(arg));
  }
  if (PTPT_isATermAnnotatedConstant(arg)) {
    return PTPT_makeATermAnnotatedConstant(
        acceptACon ? acceptACon(PTPT_getATermACon(arg)) : PTPT_getATermACon(arg),
        acceptWsAfterACon ? acceptWsAfterACon(PTPT_getATermWsAfterACon(arg)) : PTPT_getATermWsAfterACon(arg),
        acceptAnn ? acceptAnn(PTPT_getATermAnn(arg)) : PTPT_getATermAnn(arg));
  }
  if (PTPT_isATermAnnotatedList(arg)) {
    return PTPT_makeATermAnnotatedList(
        acceptList ? acceptList(PTPT_getATermList(arg)) : PTPT_getATermList(arg),
        acceptWsAfterList ? acceptWsAfterList(PTPT_getATermWsAfterList(arg)) : PTPT_getATermWsAfterList(arg),
        acceptAnn ? acceptAnn(PTPT_getATermAnn(arg)) : PTPT_getATermAnn(arg));
  }
  if (PTPT_isATermAnnotatedFun(arg)) {
    return PTPT_makeATermAnnotatedFun(
        acceptFun ? acceptFun(PTPT_getATermFun(arg)) : PTPT_getATermFun(arg),
        acceptWsAfterFun ? acceptWsAfterFun(PTPT_getATermWsAfterFun(arg)) : PTPT_getATermWsAfterFun(arg),
        acceptAnn ? acceptAnn(PTPT_getATermAnn(arg)) : PTPT_getATermAnn(arg));
  }
  if (PTPT_isATermAnnotatedAppl(arg)) {
    return PTPT_makeATermAnnotatedAppl(
        acceptFun ? acceptFun(PTPT_getATermFun(arg)) : PTPT_getATermFun(arg),
        acceptWsAfterFun ? acceptWsAfterFun(PTPT_getATermWsAfterFun(arg)) : PTPT_getATermWsAfterFun(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getATermWsAfterParenOpen(arg)) : PTPT_getATermWsAfterParenOpen(arg),
        acceptArgs ? acceptArgs(PTPT_getATermArgs(arg)) : PTPT_getATermArgs(arg),
        acceptWsAfterArgs ? acceptWsAfterArgs(PTPT_getATermWsAfterArgs(arg)) : PTPT_getATermWsAfterArgs(arg),
        acceptWsAfterParenClose ? acceptWsAfterParenClose(PTPT_getATermWsAfterParenClose(arg)) : PTPT_getATermWsAfterParenClose(arg),
        acceptAnn ? acceptAnn(PTPT_getATermAnn(arg)) : PTPT_getATermAnn(arg));
  }
  ATabort("not a ATerm: %t\n", arg);
  return (PTPT_ATerm)NULL;
}

/*}}}  */
/*{{{  PTPT_ATermArgs PTPT_visitATermArgs(PTPT_ATermArgs arg, PTPT_ATerm (*acceptHead)(PTPT_ATerm), PTPT_OptLayout (*acceptWsAfterFirst)(PTPT_OptLayout), char * (*acceptSep)(char *), PTPT_OptLayout (*acceptWsAfterSep)(PTPT_OptLayout)) */

PTPT_ATermArgs PTPT_visitATermArgs(PTPT_ATermArgs arg, PTPT_ATerm (*acceptHead)(PTPT_ATerm), PTPT_OptLayout (*acceptWsAfterFirst)(PTPT_OptLayout), char * (*acceptSep)(char *), PTPT_OptLayout (*acceptWsAfterSep)(PTPT_OptLayout))
{
  if (PTPT_isATermArgsSingle(arg)) {
    return PTPT_makeATermArgsSingle(
        acceptHead ? acceptHead(PTPT_getATermArgsHead(arg)) : PTPT_getATermArgsHead(arg));
  }
  if (PTPT_isATermArgsMany(arg)) {
    return PTPT_makeATermArgsMany(
        acceptHead ? acceptHead(PTPT_getATermArgsHead(arg)) : PTPT_getATermArgsHead(arg),
        acceptWsAfterFirst ? acceptWsAfterFirst(PTPT_getATermArgsWsAfterFirst(arg)) : PTPT_getATermArgsWsAfterFirst(arg),
        acceptSep ? acceptSep(PTPT_getATermArgsSep(arg)) : PTPT_getATermArgsSep(arg),
        acceptWsAfterSep ? acceptWsAfterSep(PTPT_getATermArgsWsAfterSep(arg)) : PTPT_getATermArgsWsAfterSep(arg),
        PTPT_visitATermArgs(PTPT_getATermArgsTail(arg), acceptHead, acceptWsAfterFirst, acceptSep, acceptWsAfterSep));
  }
  ATabort("not a ATermArgs: %t\n", arg);
  return (PTPT_ATermArgs)NULL;
}

/*}}}  */
/*{{{  PTPT_Ann PTPT_visitAnn(PTPT_Ann arg, PTPT_OptLayout (*acceptWsAfterBraceOpen)(PTPT_OptLayout), PTPT_ATermAnnos (*acceptAnnos)(PTPT_ATermAnnos), PTPT_OptLayout (*acceptWsAfterAnnos)(PTPT_OptLayout)) */

PTPT_Ann PTPT_visitAnn(PTPT_Ann arg, PTPT_OptLayout (*acceptWsAfterBraceOpen)(PTPT_OptLayout), PTPT_ATermAnnos (*acceptAnnos)(PTPT_ATermAnnos), PTPT_OptLayout (*acceptWsAfterAnnos)(PTPT_OptLayout))
{
  if (PTPT_isAnnAnnotation(arg)) {
    return PTPT_makeAnnAnnotation(
        acceptWsAfterBraceOpen ? acceptWsAfterBraceOpen(PTPT_getAnnWsAfterBraceOpen(arg)) : PTPT_getAnnWsAfterBraceOpen(arg),
        acceptAnnos ? acceptAnnos(PTPT_getAnnAnnos(arg)) : PTPT_getAnnAnnos(arg),
        acceptWsAfterAnnos ? acceptWsAfterAnnos(PTPT_getAnnWsAfterAnnos(arg)) : PTPT_getAnnWsAfterAnnos(arg));
  }
  ATabort("not a Ann: %t\n", arg);
  return (PTPT_Ann)NULL;
}

/*}}}  */
/*{{{  PTPT_ATermAnnos PTPT_visitATermAnnos(PTPT_ATermAnnos arg, PTPT_ATerm (*acceptHead)(PTPT_ATerm), PTPT_OptLayout (*acceptWsAfterFirst)(PTPT_OptLayout), char * (*acceptSep)(char *), PTPT_OptLayout (*acceptWsAfterSep)(PTPT_OptLayout)) */

PTPT_ATermAnnos PTPT_visitATermAnnos(PTPT_ATermAnnos arg, PTPT_ATerm (*acceptHead)(PTPT_ATerm), PTPT_OptLayout (*acceptWsAfterFirst)(PTPT_OptLayout), char * (*acceptSep)(char *), PTPT_OptLayout (*acceptWsAfterSep)(PTPT_OptLayout))
{
  if (PTPT_isATermAnnosSingle(arg)) {
    return PTPT_makeATermAnnosSingle(
        acceptHead ? acceptHead(PTPT_getATermAnnosHead(arg)) : PTPT_getATermAnnosHead(arg));
  }
  if (PTPT_isATermAnnosMany(arg)) {
    return PTPT_makeATermAnnosMany(
        acceptHead ? acceptHead(PTPT_getATermAnnosHead(arg)) : PTPT_getATermAnnosHead(arg),
        acceptWsAfterFirst ? acceptWsAfterFirst(PTPT_getATermAnnosWsAfterFirst(arg)) : PTPT_getATermAnnosWsAfterFirst(arg),
        acceptSep ? acceptSep(PTPT_getATermAnnosSep(arg)) : PTPT_getATermAnnosSep(arg),
        acceptWsAfterSep ? acceptWsAfterSep(PTPT_getATermAnnosWsAfterSep(arg)) : PTPT_getATermAnnosWsAfterSep(arg),
        PTPT_visitATermAnnos(PTPT_getATermAnnosTail(arg), acceptHead, acceptWsAfterFirst, acceptSep, acceptWsAfterSep));
  }
  ATabort("not a ATermAnnos: %t\n", arg);
  return (PTPT_ATermAnnos)NULL;
}

/*}}}  */
/*{{{  PTPT_AlphaNumericalEscChar PTPT_visitAlphaNumericalEscChar(PTPT_AlphaNumericalEscChar arg, PTPT_CHARLIST (*acceptChars)(PTPT_CHARLIST)) */

PTPT_AlphaNumericalEscChar PTPT_visitAlphaNumericalEscChar(PTPT_AlphaNumericalEscChar arg, PTPT_CHARLIST (*acceptChars)(PTPT_CHARLIST))
{
  if (PTPT_isAlphaNumericalEscCharDefault(arg)) {
    return PTPT_makeAlphaNumericalEscCharDefault(
        acceptChars ? acceptChars(PTPT_getAlphaNumericalEscCharChars(arg)) : PTPT_getAlphaNumericalEscCharChars(arg));
  }
  ATabort("not a AlphaNumericalEscChar: %t\n", arg);
  return (PTPT_AlphaNumericalEscChar)NULL;
}

/*}}}  */
/*{{{  PTPT_DecimalEscChar PTPT_visitDecimalEscChar(PTPT_DecimalEscChar arg, PTPT_CHARLIST (*acceptChars)(PTPT_CHARLIST)) */

PTPT_DecimalEscChar PTPT_visitDecimalEscChar(PTPT_DecimalEscChar arg, PTPT_CHARLIST (*acceptChars)(PTPT_CHARLIST))
{
  if (PTPT_isDecimalEscCharDec0Underscore199(arg)) {
    return PTPT_makeDecimalEscCharDec0Underscore199(
        acceptChars ? acceptChars(PTPT_getDecimalEscCharChars(arg)) : PTPT_getDecimalEscCharChars(arg));
  }
  if (PTPT_isDecimalEscCharDec200Underscore249(arg)) {
    return PTPT_makeDecimalEscCharDec200Underscore249(
        acceptChars ? acceptChars(PTPT_getDecimalEscCharChars(arg)) : PTPT_getDecimalEscCharChars(arg));
  }
  if (PTPT_isDecimalEscCharDec250Underscore255(arg)) {
    return PTPT_makeDecimalEscCharDec250Underscore255(
        acceptChars ? acceptChars(PTPT_getDecimalEscCharChars(arg)) : PTPT_getDecimalEscCharChars(arg));
  }
  ATabort("not a DecimalEscChar: %t\n", arg);
  return (PTPT_DecimalEscChar)NULL;
}

/*}}}  */
/*{{{  PTPT_EscChar PTPT_visitEscChar(PTPT_EscChar arg, PTPT_CHARLIST (*acceptChars)(PTPT_CHARLIST)) */

PTPT_EscChar PTPT_visitEscChar(PTPT_EscChar arg, PTPT_CHARLIST (*acceptChars)(PTPT_CHARLIST))
{
  if (PTPT_isEscCharAlphaNumeric(arg)) {
    return PTPT_makeEscCharAlphaNumeric(
        acceptChars ? acceptChars(PTPT_getEscCharChars(arg)) : PTPT_getEscCharChars(arg));
  }
  if (PTPT_isEscCharDecimal(arg)) {
    return PTPT_makeEscCharDecimal(
        acceptChars ? acceptChars(PTPT_getEscCharChars(arg)) : PTPT_getEscCharChars(arg));
  }
  ATabort("not a EscChar: %t\n", arg);
  return (PTPT_EscChar)NULL;
}

/*}}}  */
/*{{{  PTPT_LChar PTPT_visitLChar(PTPT_LChar arg, PTPT_CHARLIST (*acceptChars)(PTPT_CHARLIST)) */

PTPT_LChar PTPT_visitLChar(PTPT_LChar arg, PTPT_CHARLIST (*acceptChars)(PTPT_CHARLIST))
{
  if (PTPT_isLCharNormal(arg)) {
    return PTPT_makeLCharNormal(
        acceptChars ? acceptChars(PTPT_getLCharChars(arg)) : PTPT_getLCharChars(arg));
  }
  if (PTPT_isLCharEscaped(arg)) {
    return PTPT_makeLCharEscaped(
        acceptChars ? acceptChars(PTPT_getLCharChars(arg)) : PTPT_getLCharChars(arg));
  }
  ATabort("not a LChar: %t\n", arg);
  return (PTPT_LChar)NULL;
}

/*}}}  */
/*{{{  PTPT_QLiteral PTPT_visitQLiteral(PTPT_QLiteral arg, PTPT_CHARLIST (*acceptChars)(PTPT_CHARLIST)) */

PTPT_QLiteral PTPT_visitQLiteral(PTPT_QLiteral arg, PTPT_CHARLIST (*acceptChars)(PTPT_CHARLIST))
{
  if (PTPT_isQLiteralQuoted(arg)) {
    return PTPT_makeQLiteralQuoted(
        acceptChars ? acceptChars(PTPT_getQLiteralChars(arg)) : PTPT_getQLiteralChars(arg));
  }
  ATabort("not a QLiteral: %t\n", arg);
  return (PTPT_QLiteral)NULL;
}

/*}}}  */
/*{{{  PTPT_UQLiteral PTPT_visitUQLiteral(PTPT_UQLiteral arg, PTPT_CHARLIST (*acceptChars)(PTPT_CHARLIST)) */

PTPT_UQLiteral PTPT_visitUQLiteral(PTPT_UQLiteral arg, PTPT_CHARLIST (*acceptChars)(PTPT_CHARLIST))
{
  if (PTPT_isUQLiteralOneChar(arg)) {
    return PTPT_makeUQLiteralOneChar(
        acceptChars ? acceptChars(PTPT_getUQLiteralChars(arg)) : PTPT_getUQLiteralChars(arg));
  }
  if (PTPT_isUQLiteralMoreChars(arg)) {
    return PTPT_makeUQLiteralMoreChars(
        acceptChars ? acceptChars(PTPT_getUQLiteralChars(arg)) : PTPT_getUQLiteralChars(arg));
  }
  ATabort("not a UQLiteral: %t\n", arg);
  return (PTPT_UQLiteral)NULL;
}

/*}}}  */
/*{{{  PTPT_Literal PTPT_visitLiteral(PTPT_Literal arg, PTPT_QLiteral (*acceptQLiteral)(PTPT_QLiteral), PTPT_UQLiteral (*acceptUQLiteral)(PTPT_UQLiteral)) */

PTPT_Literal PTPT_visitLiteral(PTPT_Literal arg, PTPT_QLiteral (*acceptQLiteral)(PTPT_QLiteral), PTPT_UQLiteral (*acceptUQLiteral)(PTPT_UQLiteral))
{
  if (PTPT_isLiteralQlit(arg)) {
    return PTPT_makeLiteralQlit(
        acceptQLiteral ? acceptQLiteral(PTPT_getLiteralQLiteral(arg)) : PTPT_getLiteralQLiteral(arg));
  }
  if (PTPT_isLiteralUqlit(arg)) {
    return PTPT_makeLiteralUqlit(
        acceptUQLiteral ? acceptUQLiteral(PTPT_getLiteralUQLiteral(arg)) : PTPT_getLiteralUQLiteral(arg));
  }
  ATabort("not a Literal: %t\n", arg);
  return (PTPT_Literal)NULL;
}

/*}}}  */
/*{{{  PTPT_Attributes PTPT_visitAttributes(PTPT_Attributes arg, PTPT_OptLayout (*acceptWsAfterAttrs)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterParenOpen)(PTPT_OptLayout), PTPT_Attrs (*acceptAttributes)(PTPT_Attrs), PTPT_OptLayout (*acceptWsAfterAttributes)(PTPT_OptLayout)) */

PTPT_Attributes PTPT_visitAttributes(PTPT_Attributes arg, PTPT_OptLayout (*acceptWsAfterAttrs)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterParenOpen)(PTPT_OptLayout), PTPT_Attrs (*acceptAttributes)(PTPT_Attrs), PTPT_OptLayout (*acceptWsAfterAttributes)(PTPT_OptLayout))
{
  if (PTPT_isAttributesNoAttrs(arg)) {
    return PTPT_makeAttributesNoAttrs();
  }
  if (PTPT_isAttributesAttrs(arg)) {
    return PTPT_makeAttributesAttrs(
        acceptWsAfterAttrs ? acceptWsAfterAttrs(PTPT_getAttributesWsAfterAttrs(arg)) : PTPT_getAttributesWsAfterAttrs(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getAttributesWsAfterParenOpen(arg)) : PTPT_getAttributesWsAfterParenOpen(arg),
        acceptAttributes ? acceptAttributes(PTPT_getAttributesAttributes(arg)) : PTPT_getAttributesAttributes(arg),
        acceptWsAfterAttributes ? acceptWsAfterAttributes(PTPT_getAttributesWsAfterAttributes(arg)) : PTPT_getAttributesWsAfterAttributes(arg));
  }
  ATabort("not a Attributes: %t\n", arg);
  return (PTPT_Attributes)NULL;
}

/*}}}  */
/*{{{  PTPT_Attrs PTPT_visitAttrs(PTPT_Attrs arg, PTPT_OptLayout (*acceptWsAfterBracketOpen)(PTPT_OptLayout), PTPT_AttrList (*acceptList)(PTPT_AttrList), PTPT_OptLayout (*acceptWsAfterList)(PTPT_OptLayout)) */

PTPT_Attrs PTPT_visitAttrs(PTPT_Attrs arg, PTPT_OptLayout (*acceptWsAfterBracketOpen)(PTPT_OptLayout), PTPT_AttrList (*acceptList)(PTPT_AttrList), PTPT_OptLayout (*acceptWsAfterList)(PTPT_OptLayout))
{
  if (PTPT_isAttrsMany(arg)) {
    return PTPT_makeAttrsMany(
        acceptWsAfterBracketOpen ? acceptWsAfterBracketOpen(PTPT_getAttrsWsAfterBracketOpen(arg)) : PTPT_getAttrsWsAfterBracketOpen(arg),
        acceptList ? acceptList(PTPT_getAttrsList(arg)) : PTPT_getAttrsList(arg),
        acceptWsAfterList ? acceptWsAfterList(PTPT_getAttrsWsAfterList(arg)) : PTPT_getAttrsWsAfterList(arg));
  }
  ATabort("not a Attrs: %t\n", arg);
  return (PTPT_Attrs)NULL;
}

/*}}}  */
/*{{{  PTPT_AttrList PTPT_visitAttrList(PTPT_AttrList arg, PTPT_Attr (*acceptHead)(PTPT_Attr), PTPT_OptLayout (*acceptWsAfterFirst)(PTPT_OptLayout), char * (*acceptSep)(char *), PTPT_OptLayout (*acceptWsAfterSep)(PTPT_OptLayout)) */

PTPT_AttrList PTPT_visitAttrList(PTPT_AttrList arg, PTPT_Attr (*acceptHead)(PTPT_Attr), PTPT_OptLayout (*acceptWsAfterFirst)(PTPT_OptLayout), char * (*acceptSep)(char *), PTPT_OptLayout (*acceptWsAfterSep)(PTPT_OptLayout))
{
  if (PTPT_isAttrListSingle(arg)) {
    return PTPT_makeAttrListSingle(
        acceptHead ? acceptHead(PTPT_getAttrListHead(arg)) : PTPT_getAttrListHead(arg));
  }
  if (PTPT_isAttrListMany(arg)) {
    return PTPT_makeAttrListMany(
        acceptHead ? acceptHead(PTPT_getAttrListHead(arg)) : PTPT_getAttrListHead(arg),
        acceptWsAfterFirst ? acceptWsAfterFirst(PTPT_getAttrListWsAfterFirst(arg)) : PTPT_getAttrListWsAfterFirst(arg),
        acceptSep ? acceptSep(PTPT_getAttrListSep(arg)) : PTPT_getAttrListSep(arg),
        acceptWsAfterSep ? acceptWsAfterSep(PTPT_getAttrListWsAfterSep(arg)) : PTPT_getAttrListWsAfterSep(arg),
        PTPT_visitAttrList(PTPT_getAttrListTail(arg), acceptHead, acceptWsAfterFirst, acceptSep, acceptWsAfterSep));
  }
  ATabort("not a AttrList: %t\n", arg);
  return (PTPT_AttrList)NULL;
}

/*}}}  */
/*{{{  PTPT_Attr PTPT_visitAttr(PTPT_Attr arg, PTPT_OptLayout (*acceptWsAfterAssoc)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterParenOpen)(PTPT_OptLayout), PTPT_Associativity (*acceptAssociativity)(PTPT_Associativity), PTPT_OptLayout (*acceptWsAfterAssociativity)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterTerm)(PTPT_OptLayout), PTPT_ATerm (*acceptAterm)(PTPT_ATerm), PTPT_OptLayout (*acceptWsAfterAterm)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterId)(PTPT_OptLayout), PTPT_QLiteral (*acceptModuleName)(PTPT_QLiteral), PTPT_OptLayout (*acceptWsAfterModuleName)(PTPT_OptLayout)) */

PTPT_Attr PTPT_visitAttr(PTPT_Attr arg, PTPT_OptLayout (*acceptWsAfterAssoc)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterParenOpen)(PTPT_OptLayout), PTPT_Associativity (*acceptAssociativity)(PTPT_Associativity), PTPT_OptLayout (*acceptWsAfterAssociativity)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterTerm)(PTPT_OptLayout), PTPT_ATerm (*acceptAterm)(PTPT_ATerm), PTPT_OptLayout (*acceptWsAfterAterm)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterId)(PTPT_OptLayout), PTPT_QLiteral (*acceptModuleName)(PTPT_QLiteral), PTPT_OptLayout (*acceptWsAfterModuleName)(PTPT_OptLayout))
{
  if (PTPT_isAttrAssoc(arg)) {
    return PTPT_makeAttrAssoc(
        acceptWsAfterAssoc ? acceptWsAfterAssoc(PTPT_getAttrWsAfterAssoc(arg)) : PTPT_getAttrWsAfterAssoc(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getAttrWsAfterParenOpen(arg)) : PTPT_getAttrWsAfterParenOpen(arg),
        acceptAssociativity ? acceptAssociativity(PTPT_getAttrAssociativity(arg)) : PTPT_getAttrAssociativity(arg),
        acceptWsAfterAssociativity ? acceptWsAfterAssociativity(PTPT_getAttrWsAfterAssociativity(arg)) : PTPT_getAttrWsAfterAssociativity(arg));
  }
  if (PTPT_isAttrTerm(arg)) {
    return PTPT_makeAttrTerm(
        acceptWsAfterTerm ? acceptWsAfterTerm(PTPT_getAttrWsAfterTerm(arg)) : PTPT_getAttrWsAfterTerm(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getAttrWsAfterParenOpen(arg)) : PTPT_getAttrWsAfterParenOpen(arg),
        acceptAterm ? acceptAterm(PTPT_getAttrAterm(arg)) : PTPT_getAttrAterm(arg),
        acceptWsAfterAterm ? acceptWsAfterAterm(PTPT_getAttrWsAfterAterm(arg)) : PTPT_getAttrWsAfterAterm(arg));
  }
  if (PTPT_isAttrId(arg)) {
    return PTPT_makeAttrId(
        acceptWsAfterId ? acceptWsAfterId(PTPT_getAttrWsAfterId(arg)) : PTPT_getAttrWsAfterId(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getAttrWsAfterParenOpen(arg)) : PTPT_getAttrWsAfterParenOpen(arg),
        acceptModuleName ? acceptModuleName(PTPT_getAttrModuleName(arg)) : PTPT_getAttrModuleName(arg),
        acceptWsAfterModuleName ? acceptWsAfterModuleName(PTPT_getAttrWsAfterModuleName(arg)) : PTPT_getAttrWsAfterModuleName(arg));
  }
  if (PTPT_isAttrBracket(arg)) {
    return PTPT_makeAttrBracket();
  }
  if (PTPT_isAttrReject(arg)) {
    return PTPT_makeAttrReject();
  }
  if (PTPT_isAttrPrefer(arg)) {
    return PTPT_makeAttrPrefer();
  }
  if (PTPT_isAttrAvoid(arg)) {
    return PTPT_makeAttrAvoid();
  }
  ATabort("not a Attr: %t\n", arg);
  return (PTPT_Attr)NULL;
}

/*}}}  */
/*{{{  PTPT_Associativity PTPT_visitAssociativity(PTPT_Associativity arg) */

PTPT_Associativity PTPT_visitAssociativity(PTPT_Associativity arg)
{
  if (PTPT_isAssociativityLeft(arg)) {
    return PTPT_makeAssociativityLeft();
  }
  if (PTPT_isAssociativityRight(arg)) {
    return PTPT_makeAssociativityRight();
  }
  if (PTPT_isAssociativityAssoc(arg)) {
    return PTPT_makeAssociativityAssoc();
  }
  if (PTPT_isAssociativityNonAssoc(arg)) {
    return PTPT_makeAssociativityNonAssoc();
  }
  ATabort("not a Associativity: %t\n", arg);
  return (PTPT_Associativity)NULL;
}

/*}}}  */
/*{{{  PTPT_ParseTree PTPT_visitParseTree(PTPT_ParseTree arg, PTPT_OptLayout (*acceptWsAfterParsetree)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterParenOpen)(PTPT_OptLayout), PTPT_Tree (*acceptTop)(PTPT_Tree), PTPT_OptLayout (*acceptWsAfterTop)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterComma)(PTPT_OptLayout), PTPT_NatCon (*acceptAmbCnt)(PTPT_NatCon), PTPT_OptLayout (*acceptWsAfterAmbCnt)(PTPT_OptLayout)) */

PTPT_ParseTree PTPT_visitParseTree(PTPT_ParseTree arg, PTPT_OptLayout (*acceptWsAfterParsetree)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterParenOpen)(PTPT_OptLayout), PTPT_Tree (*acceptTop)(PTPT_Tree), PTPT_OptLayout (*acceptWsAfterTop)(PTPT_OptLayout), PTPT_OptLayout (*acceptWsAfterComma)(PTPT_OptLayout), PTPT_NatCon (*acceptAmbCnt)(PTPT_NatCon), PTPT_OptLayout (*acceptWsAfterAmbCnt)(PTPT_OptLayout))
{
  if (PTPT_isParseTreeTop(arg)) {
    return PTPT_makeParseTreeTop(
        acceptWsAfterParsetree ? acceptWsAfterParsetree(PTPT_getParseTreeWsAfterParsetree(arg)) : PTPT_getParseTreeWsAfterParsetree(arg),
        acceptWsAfterParenOpen ? acceptWsAfterParenOpen(PTPT_getParseTreeWsAfterParenOpen(arg)) : PTPT_getParseTreeWsAfterParenOpen(arg),
        acceptTop ? acceptTop(PTPT_getParseTreeTop(arg)) : PTPT_getParseTreeTop(arg),
        acceptWsAfterTop ? acceptWsAfterTop(PTPT_getParseTreeWsAfterTop(arg)) : PTPT_getParseTreeWsAfterTop(arg),
        acceptWsAfterComma ? acceptWsAfterComma(PTPT_getParseTreeWsAfterComma(arg)) : PTPT_getParseTreeWsAfterComma(arg),
        acceptAmbCnt ? acceptAmbCnt(PTPT_getParseTreeAmbCnt(arg)) : PTPT_getParseTreeAmbCnt(arg),
        acceptWsAfterAmbCnt ? acceptWsAfterAmbCnt(PTPT_getParseTreeWsAfterAmbCnt(arg)) : PTPT_getParseTreeWsAfterAmbCnt(arg));
  }
  ATabort("not a ParseTree: %t\n", arg);
  return (PTPT_ParseTree)NULL;
}

/*}}}  */
/*{{{  PTPT_Start PTPT_visitStart(PTPT_Start arg, PTPT_OptLayout (*acceptWsBefore)(PTPT_OptLayout), PTPT_ParseTree (*acceptTopParseTree)(PTPT_ParseTree), PTPT_OptLayout (*acceptWsAfter)(PTPT_OptLayout), int (*acceptAmbCnt)(int), PTPT_Associativity (*acceptTopAssociativity)(PTPT_Associativity), PTPT_Attr (*acceptTopAttr)(PTPT_Attr), PTPT_Attrs (*acceptTopAttrs)(PTPT_Attrs), PTPT_Attributes (*acceptTopAttributes)(PTPT_Attributes), PTPT_QLiteral (*acceptTopQLiteral)(PTPT_QLiteral), PTPT_UQLiteral (*acceptTopUQLiteral)(PTPT_UQLiteral), PTPT_Literal (*acceptTopLiteral)(PTPT_Literal), PTPT_Ann (*acceptTopAnn)(PTPT_Ann), PTPT_ATerm (*acceptTopATerm)(PTPT_ATerm), PTPT_AFun (*acceptTopAFun)(PTPT_AFun), PTPT_ACon (*acceptTopACon)(PTPT_ACon), PTPT_ATermList (*acceptTopATermList)(PTPT_ATermList), PTPT_RealCon (*acceptTopRealCon)(PTPT_RealCon), PTPT_OptExp (*acceptTopOptExp)(PTPT_OptExp), PTPT_CharRanges (*acceptTopCharRanges)(PTPT_CharRanges), PTPT_CharRange (*acceptTopCharRange)(PTPT_CharRange), PTPT_Symbols (*acceptTopSymbols)(PTPT_Symbols), PTPT_Symbol (*acceptTopSymbol)(PTPT_Symbol), PTPT_Production (*acceptTopProduction)(PTPT_Production), PTPT_Args (*acceptTopArgs)(PTPT_Args), PTPT_Tree (*acceptTopTree)(PTPT_Tree), PTPT_IntCon (*acceptTopIntCon)(PTPT_IntCon), PTPT_NatCon (*acceptTopNatCon)(PTPT_NatCon)) */

PTPT_Start PTPT_visitStart(PTPT_Start arg, PTPT_OptLayout (*acceptWsBefore)(PTPT_OptLayout), PTPT_ParseTree (*acceptTopParseTree)(PTPT_ParseTree), PTPT_OptLayout (*acceptWsAfter)(PTPT_OptLayout), int (*acceptAmbCnt)(int), PTPT_Associativity (*acceptTopAssociativity)(PTPT_Associativity), PTPT_Attr (*acceptTopAttr)(PTPT_Attr), PTPT_Attrs (*acceptTopAttrs)(PTPT_Attrs), PTPT_Attributes (*acceptTopAttributes)(PTPT_Attributes), PTPT_QLiteral (*acceptTopQLiteral)(PTPT_QLiteral), PTPT_UQLiteral (*acceptTopUQLiteral)(PTPT_UQLiteral), PTPT_Literal (*acceptTopLiteral)(PTPT_Literal), PTPT_Ann (*acceptTopAnn)(PTPT_Ann), PTPT_ATerm (*acceptTopATerm)(PTPT_ATerm), PTPT_AFun (*acceptTopAFun)(PTPT_AFun), PTPT_ACon (*acceptTopACon)(PTPT_ACon), PTPT_ATermList (*acceptTopATermList)(PTPT_ATermList), PTPT_RealCon (*acceptTopRealCon)(PTPT_RealCon), PTPT_OptExp (*acceptTopOptExp)(PTPT_OptExp), PTPT_CharRanges (*acceptTopCharRanges)(PTPT_CharRanges), PTPT_CharRange (*acceptTopCharRange)(PTPT_CharRange), PTPT_Symbols (*acceptTopSymbols)(PTPT_Symbols), PTPT_Symbol (*acceptTopSymbol)(PTPT_Symbol), PTPT_Production (*acceptTopProduction)(PTPT_Production), PTPT_Args (*acceptTopArgs)(PTPT_Args), PTPT_Tree (*acceptTopTree)(PTPT_Tree), PTPT_IntCon (*acceptTopIntCon)(PTPT_IntCon), PTPT_NatCon (*acceptTopNatCon)(PTPT_NatCon))
{
  if (PTPT_isStartParseTree(arg)) {
    return PTPT_makeStartParseTree(
        acceptWsBefore ? acceptWsBefore(PTPT_getStartWsBefore(arg)) : PTPT_getStartWsBefore(arg),
        acceptTopParseTree ? acceptTopParseTree(PTPT_getStartTopParseTree(arg)) : PTPT_getStartTopParseTree(arg),
        acceptWsAfter ? acceptWsAfter(PTPT_getStartWsAfter(arg)) : PTPT_getStartWsAfter(arg),
        acceptAmbCnt ? acceptAmbCnt(PTPT_getStartAmbCnt(arg)) : PTPT_getStartAmbCnt(arg));
  }
  if (PTPT_isStartAssociativity(arg)) {
    return PTPT_makeStartAssociativity(
        acceptWsBefore ? acceptWsBefore(PTPT_getStartWsBefore(arg)) : PTPT_getStartWsBefore(arg),
        acceptTopAssociativity ? acceptTopAssociativity(PTPT_getStartTopAssociativity(arg)) : PTPT_getStartTopAssociativity(arg),
        acceptWsAfter ? acceptWsAfter(PTPT_getStartWsAfter(arg)) : PTPT_getStartWsAfter(arg),
        acceptAmbCnt ? acceptAmbCnt(PTPT_getStartAmbCnt(arg)) : PTPT_getStartAmbCnt(arg));
  }
  if (PTPT_isStartAttr(arg)) {
    return PTPT_makeStartAttr(
        acceptWsBefore ? acceptWsBefore(PTPT_getStartWsBefore(arg)) : PTPT_getStartWsBefore(arg),
        acceptTopAttr ? acceptTopAttr(PTPT_getStartTopAttr(arg)) : PTPT_getStartTopAttr(arg),
        acceptWsAfter ? acceptWsAfter(PTPT_getStartWsAfter(arg)) : PTPT_getStartWsAfter(arg),
        acceptAmbCnt ? acceptAmbCnt(PTPT_getStartAmbCnt(arg)) : PTPT_getStartAmbCnt(arg));
  }
  if (PTPT_isStartAttrs(arg)) {
    return PTPT_makeStartAttrs(
        acceptWsBefore ? acceptWsBefore(PTPT_getStartWsBefore(arg)) : PTPT_getStartWsBefore(arg),
        acceptTopAttrs ? acceptTopAttrs(PTPT_getStartTopAttrs(arg)) : PTPT_getStartTopAttrs(arg),
        acceptWsAfter ? acceptWsAfter(PTPT_getStartWsAfter(arg)) : PTPT_getStartWsAfter(arg),
        acceptAmbCnt ? acceptAmbCnt(PTPT_getStartAmbCnt(arg)) : PTPT_getStartAmbCnt(arg));
  }
  if (PTPT_isStartAttributes(arg)) {
    return PTPT_makeStartAttributes(
        acceptWsBefore ? acceptWsBefore(PTPT_getStartWsBefore(arg)) : PTPT_getStartWsBefore(arg),
        acceptTopAttributes ? acceptTopAttributes(PTPT_getStartTopAttributes(arg)) : PTPT_getStartTopAttributes(arg),
        acceptWsAfter ? acceptWsAfter(PTPT_getStartWsAfter(arg)) : PTPT_getStartWsAfter(arg),
        acceptAmbCnt ? acceptAmbCnt(PTPT_getStartAmbCnt(arg)) : PTPT_getStartAmbCnt(arg));
  }
  if (PTPT_isStartQLiteral(arg)) {
    return PTPT_makeStartQLiteral(
        acceptWsBefore ? acceptWsBefore(PTPT_getStartWsBefore(arg)) : PTPT_getStartWsBefore(arg),
        acceptTopQLiteral ? acceptTopQLiteral(PTPT_getStartTopQLiteral(arg)) : PTPT_getStartTopQLiteral(arg),
        acceptWsAfter ? acceptWsAfter(PTPT_getStartWsAfter(arg)) : PTPT_getStartWsAfter(arg),
        acceptAmbCnt ? acceptAmbCnt(PTPT_getStartAmbCnt(arg)) : PTPT_getStartAmbCnt(arg));
  }
  if (PTPT_isStartUQLiteral(arg)) {
    return PTPT_makeStartUQLiteral(
        acceptWsBefore ? acceptWsBefore(PTPT_getStartWsBefore(arg)) : PTPT_getStartWsBefore(arg),
        acceptTopUQLiteral ? acceptTopUQLiteral(PTPT_getStartTopUQLiteral(arg)) : PTPT_getStartTopUQLiteral(arg),
        acceptWsAfter ? acceptWsAfter(PTPT_getStartWsAfter(arg)) : PTPT_getStartWsAfter(arg),
        acceptAmbCnt ? acceptAmbCnt(PTPT_getStartAmbCnt(arg)) : PTPT_getStartAmbCnt(arg));
  }
  if (PTPT_isStartLiteral(arg)) {
    return PTPT_makeStartLiteral(
        acceptWsBefore ? acceptWsBefore(PTPT_getStartWsBefore(arg)) : PTPT_getStartWsBefore(arg),
        acceptTopLiteral ? acceptTopLiteral(PTPT_getStartTopLiteral(arg)) : PTPT_getStartTopLiteral(arg),
        acceptWsAfter ? acceptWsAfter(PTPT_getStartWsAfter(arg)) : PTPT_getStartWsAfter(arg),
        acceptAmbCnt ? acceptAmbCnt(PTPT_getStartAmbCnt(arg)) : PTPT_getStartAmbCnt(arg));
  }
  if (PTPT_isStartAnn(arg)) {
    return PTPT_makeStartAnn(
        acceptWsBefore ? acceptWsBefore(PTPT_getStartWsBefore(arg)) : PTPT_getStartWsBefore(arg),
        acceptTopAnn ? acceptTopAnn(PTPT_getStartTopAnn(arg)) : PTPT_getStartTopAnn(arg),
        acceptWsAfter ? acceptWsAfter(PTPT_getStartWsAfter(arg)) : PTPT_getStartWsAfter(arg),
        acceptAmbCnt ? acceptAmbCnt(PTPT_getStartAmbCnt(arg)) : PTPT_getStartAmbCnt(arg));
  }
  if (PTPT_isStartATerm(arg)) {
    return PTPT_makeStartATerm(
        acceptWsBefore ? acceptWsBefore(PTPT_getStartWsBefore(arg)) : PTPT_getStartWsBefore(arg),
        acceptTopATerm ? acceptTopATerm(PTPT_getStartTopATerm(arg)) : PTPT_getStartTopATerm(arg),
        acceptWsAfter ? acceptWsAfter(PTPT_getStartWsAfter(arg)) : PTPT_getStartWsAfter(arg),
        acceptAmbCnt ? acceptAmbCnt(PTPT_getStartAmbCnt(arg)) : PTPT_getStartAmbCnt(arg));
  }
  if (PTPT_isStartAFun(arg)) {
    return PTPT_makeStartAFun(
        acceptWsBefore ? acceptWsBefore(PTPT_getStartWsBefore(arg)) : PTPT_getStartWsBefore(arg),
        acceptTopAFun ? acceptTopAFun(PTPT_getStartTopAFun(arg)) : PTPT_getStartTopAFun(arg),
        acceptWsAfter ? acceptWsAfter(PTPT_getStartWsAfter(arg)) : PTPT_getStartWsAfter(arg),
        acceptAmbCnt ? acceptAmbCnt(PTPT_getStartAmbCnt(arg)) : PTPT_getStartAmbCnt(arg));
  }
  if (PTPT_isStartACon(arg)) {
    return PTPT_makeStartACon(
        acceptWsBefore ? acceptWsBefore(PTPT_getStartWsBefore(arg)) : PTPT_getStartWsBefore(arg),
        acceptTopACon ? acceptTopACon(PTPT_getStartTopACon(arg)) : PTPT_getStartTopACon(arg),
        acceptWsAfter ? acceptWsAfter(PTPT_getStartWsAfter(arg)) : PTPT_getStartWsAfter(arg),
        acceptAmbCnt ? acceptAmbCnt(PTPT_getStartAmbCnt(arg)) : PTPT_getStartAmbCnt(arg));
  }
  if (PTPT_isStartATermList(arg)) {
    return PTPT_makeStartATermList(
        acceptWsBefore ? acceptWsBefore(PTPT_getStartWsBefore(arg)) : PTPT_getStartWsBefore(arg),
        acceptTopATermList ? acceptTopATermList(PTPT_getStartTopATermList(arg)) : PTPT_getStartTopATermList(arg),
        acceptWsAfter ? acceptWsAfter(PTPT_getStartWsAfter(arg)) : PTPT_getStartWsAfter(arg),
        acceptAmbCnt ? acceptAmbCnt(PTPT_getStartAmbCnt(arg)) : PTPT_getStartAmbCnt(arg));
  }
  if (PTPT_isStartRealCon(arg)) {
    return PTPT_makeStartRealCon(
        acceptWsBefore ? acceptWsBefore(PTPT_getStartWsBefore(arg)) : PTPT_getStartWsBefore(arg),
        acceptTopRealCon ? acceptTopRealCon(PTPT_getStartTopRealCon(arg)) : PTPT_getStartTopRealCon(arg),
        acceptWsAfter ? acceptWsAfter(PTPT_getStartWsAfter(arg)) : PTPT_getStartWsAfter(arg),
        acceptAmbCnt ? acceptAmbCnt(PTPT_getStartAmbCnt(arg)) : PTPT_getStartAmbCnt(arg));
  }
  if (PTPT_isStartOptExp(arg)) {
    return PTPT_makeStartOptExp(
        acceptWsBefore ? acceptWsBefore(PTPT_getStartWsBefore(arg)) : PTPT_getStartWsBefore(arg),
        acceptTopOptExp ? acceptTopOptExp(PTPT_getStartTopOptExp(arg)) : PTPT_getStartTopOptExp(arg),
        acceptWsAfter ? acceptWsAfter(PTPT_getStartWsAfter(arg)) : PTPT_getStartWsAfter(arg),
        acceptAmbCnt ? acceptAmbCnt(PTPT_getStartAmbCnt(arg)) : PTPT_getStartAmbCnt(arg));
  }
  if (PTPT_isStartCharRanges(arg)) {
    return PTPT_makeStartCharRanges(
        acceptWsBefore ? acceptWsBefore(PTPT_getStartWsBefore(arg)) : PTPT_getStartWsBefore(arg),
        acceptTopCharRanges ? acceptTopCharRanges(PTPT_getStartTopCharRanges(arg)) : PTPT_getStartTopCharRanges(arg),
        acceptWsAfter ? acceptWsAfter(PTPT_getStartWsAfter(arg)) : PTPT_getStartWsAfter(arg),
        acceptAmbCnt ? acceptAmbCnt(PTPT_getStartAmbCnt(arg)) : PTPT_getStartAmbCnt(arg));
  }
  if (PTPT_isStartCharRange(arg)) {
    return PTPT_makeStartCharRange(
        acceptWsBefore ? acceptWsBefore(PTPT_getStartWsBefore(arg)) : PTPT_getStartWsBefore(arg),
        acceptTopCharRange ? acceptTopCharRange(PTPT_getStartTopCharRange(arg)) : PTPT_getStartTopCharRange(arg),
        acceptWsAfter ? acceptWsAfter(PTPT_getStartWsAfter(arg)) : PTPT_getStartWsAfter(arg),
        acceptAmbCnt ? acceptAmbCnt(PTPT_getStartAmbCnt(arg)) : PTPT_getStartAmbCnt(arg));
  }
  if (PTPT_isStartSymbols(arg)) {
    return PTPT_makeStartSymbols(
        acceptWsBefore ? acceptWsBefore(PTPT_getStartWsBefore(arg)) : PTPT_getStartWsBefore(arg),
        acceptTopSymbols ? acceptTopSymbols(PTPT_getStartTopSymbols(arg)) : PTPT_getStartTopSymbols(arg),
        acceptWsAfter ? acceptWsAfter(PTPT_getStartWsAfter(arg)) : PTPT_getStartWsAfter(arg),
        acceptAmbCnt ? acceptAmbCnt(PTPT_getStartAmbCnt(arg)) : PTPT_getStartAmbCnt(arg));
  }
  if (PTPT_isStartSymbol(arg)) {
    return PTPT_makeStartSymbol(
        acceptWsBefore ? acceptWsBefore(PTPT_getStartWsBefore(arg)) : PTPT_getStartWsBefore(arg),
        acceptTopSymbol ? acceptTopSymbol(PTPT_getStartTopSymbol(arg)) : PTPT_getStartTopSymbol(arg),
        acceptWsAfter ? acceptWsAfter(PTPT_getStartWsAfter(arg)) : PTPT_getStartWsAfter(arg),
        acceptAmbCnt ? acceptAmbCnt(PTPT_getStartAmbCnt(arg)) : PTPT_getStartAmbCnt(arg));
  }
  if (PTPT_isStartProduction(arg)) {
    return PTPT_makeStartProduction(
        acceptWsBefore ? acceptWsBefore(PTPT_getStartWsBefore(arg)) : PTPT_getStartWsBefore(arg),
        acceptTopProduction ? acceptTopProduction(PTPT_getStartTopProduction(arg)) : PTPT_getStartTopProduction(arg),
        acceptWsAfter ? acceptWsAfter(PTPT_getStartWsAfter(arg)) : PTPT_getStartWsAfter(arg),
        acceptAmbCnt ? acceptAmbCnt(PTPT_getStartAmbCnt(arg)) : PTPT_getStartAmbCnt(arg));
  }
  if (PTPT_isStartArgs(arg)) {
    return PTPT_makeStartArgs(
        acceptWsBefore ? acceptWsBefore(PTPT_getStartWsBefore(arg)) : PTPT_getStartWsBefore(arg),
        acceptTopArgs ? acceptTopArgs(PTPT_getStartTopArgs(arg)) : PTPT_getStartTopArgs(arg),
        acceptWsAfter ? acceptWsAfter(PTPT_getStartWsAfter(arg)) : PTPT_getStartWsAfter(arg),
        acceptAmbCnt ? acceptAmbCnt(PTPT_getStartAmbCnt(arg)) : PTPT_getStartAmbCnt(arg));
  }
  if (PTPT_isStartTree(arg)) {
    return PTPT_makeStartTree(
        acceptWsBefore ? acceptWsBefore(PTPT_getStartWsBefore(arg)) : PTPT_getStartWsBefore(arg),
        acceptTopTree ? acceptTopTree(PTPT_getStartTopTree(arg)) : PTPT_getStartTopTree(arg),
        acceptWsAfter ? acceptWsAfter(PTPT_getStartWsAfter(arg)) : PTPT_getStartWsAfter(arg),
        acceptAmbCnt ? acceptAmbCnt(PTPT_getStartAmbCnt(arg)) : PTPT_getStartAmbCnt(arg));
  }
  if (PTPT_isStartIntCon(arg)) {
    return PTPT_makeStartIntCon(
        acceptWsBefore ? acceptWsBefore(PTPT_getStartWsBefore(arg)) : PTPT_getStartWsBefore(arg),
        acceptTopIntCon ? acceptTopIntCon(PTPT_getStartTopIntCon(arg)) : PTPT_getStartTopIntCon(arg),
        acceptWsAfter ? acceptWsAfter(PTPT_getStartWsAfter(arg)) : PTPT_getStartWsAfter(arg),
        acceptAmbCnt ? acceptAmbCnt(PTPT_getStartAmbCnt(arg)) : PTPT_getStartAmbCnt(arg));
  }
  if (PTPT_isStartNatCon(arg)) {
    return PTPT_makeStartNatCon(
        acceptWsBefore ? acceptWsBefore(PTPT_getStartWsBefore(arg)) : PTPT_getStartWsBefore(arg),
        acceptTopNatCon ? acceptTopNatCon(PTPT_getStartTopNatCon(arg)) : PTPT_getStartTopNatCon(arg),
        acceptWsAfter ? acceptWsAfter(PTPT_getStartWsAfter(arg)) : PTPT_getStartWsAfter(arg),
        acceptAmbCnt ? acceptAmbCnt(PTPT_getStartAmbCnt(arg)) : PTPT_getStartAmbCnt(arg));
  }
  ATabort("not a Start: %t\n", arg);
  return (PTPT_Start)NULL;
}

/*}}}  */
/*{{{  PTPT_OptLayout PTPT_visitOptLayout(PTPT_OptLayout arg, PTPT_CHARLIST (*acceptChars)(PTPT_CHARLIST)) */

PTPT_OptLayout PTPT_visitOptLayout(PTPT_OptLayout arg, PTPT_CHARLIST (*acceptChars)(PTPT_CHARLIST))
{
  if (PTPT_isOptLayoutAbsent(arg)) {
    return PTPT_makeOptLayoutAbsent();
  }
  if (PTPT_isOptLayoutPresent(arg)) {
    return PTPT_makeOptLayoutPresent(
        acceptChars ? acceptChars(PTPT_getOptLayoutChars(arg)) : PTPT_getOptLayoutChars(arg));
  }
  ATabort("not a OptLayout: %t\n", arg);
  return (PTPT_OptLayout)NULL;
}

/*}}}  */

/*}}}  */
