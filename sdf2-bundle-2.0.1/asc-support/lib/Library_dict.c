/*
 * Generated at Thu Sep 25 11:16:48 2003
 */

#include "Library_dict.h"

AFun CO_afun0;
AFun CO_afun1;
AFun CO_afun2;
AFun CO_afun3;
AFun CO_afun4;
AFun CO_afun5;
AFun CO_afun6;
AFun CO_afun7;
AFun CO_afun8;
AFun CO_afun9;
AFun CO_afun10;
AFun CO_afun11;
AFun CO_afun12;
AFun CO_afun13;
AFun CO_afun14;
AFun CO_afun15;
AFun CO_afun16;
AFun CO_afun17;
AFun CO_afun18;
AFun CO_afun19;
AFun CO_afun20;
AFun CO_afun21;
AFun CO_afun22;
AFun CO_afun23;
AFun CO_afun24;
AFun CO_afun25;
AFun CO_afun26;
AFun CO_afun27;
AFun CO_afun28;
AFun CO_afun29;
AFun CO_afun30;
AFun CO_afun31;
AFun CO_afun32;
AFun CO_afun33;

ATerm CO_patternOptLayoutPresent = NULL;
ATerm CO_patternOptLayoutAbsent = NULL;
ATerm CO_patternStartBoolean = NULL;
ATerm CO_patternStartBoolCon = NULL;
ATerm CO_patternStartMeasure = NULL;
ATerm CO_patternMeasureEqual = NULL;
ATerm CO_patternMeasureGreater = NULL;
ATerm CO_patternMeasureLess = NULL;
ATerm CO_patternBooleanBracket = NULL;
ATerm CO_patternBooleanNot = NULL;
ATerm CO_patternBooleanAnd = NULL;
ATerm CO_patternBooleanOr = NULL;
ATerm CO_patternBooleanConstant = NULL;
ATerm CO_patternBoolConFalse = NULL;
ATerm CO_patternBoolConTrue = NULL;

/*
 * CO_afun0 = appl(x,x)
 * CO_afun1 = prod(x,x,x)
 * CO_afun2 = lit(x)
 * CO_afun3 = "true"
 * CO_afun4 = cf(x)
 * CO_afun5 = sort(x)
 * CO_afun6 = "BoolCon"
 * CO_afun7 = attrs(x)
 * CO_afun8 = term(x)
 * CO_afun9 = cons(x)
 * CO_afun10 = constructor
 * CO_afun11 = "false"
 * CO_afun12 = "Boolean"
 * CO_afun13 = "constant"
 * CO_afun14 = opt(x)
 * CO_afun15 = layout
 * CO_afun16 = "|"
 * CO_afun17 = "or"
 * CO_afun18 = assoc(x)
 * CO_afun19 = left
 * CO_afun20 = "&"
 * CO_afun21 = "and"
 * CO_afun22 = ")"
 * CO_afun23 = "("
 * CO_afun24 = "not"
 * CO_afun25 = "bracket"
 * CO_afun26 = bracket
 * CO_afun27 = "less"
 * CO_afun28 = "Measure"
 * CO_afun29 = "greater"
 * CO_afun30 = "equal"
 * CO_afun31 = parsetree(x,x)
 * CO_afun32 = "<START>"
 * CO_afun33 = no-attrs
 *
 * CO_patternOptLayoutPresent = appl(prod([cf(layout)],cf(opt(layout)),no-attrs),<term>)
 * CO_patternOptLayoutAbsent = appl(prod([],cf(opt(layout)),no-attrs),[])
 * CO_patternStartBoolean = parsetree(appl(prod([cf(opt(layout)),cf(sort("Boolean")),cf(opt(layout))],sort("<START>"),no-attrs),[<term>,<term>,<term>]),<int>)
 * CO_patternStartBoolCon = parsetree(appl(prod([cf(opt(layout)),cf(sort("BoolCon")),cf(opt(layout))],sort("<START>"),no-attrs),[<term>,<term>,<term>]),<int>)
 * CO_patternStartMeasure = parsetree(appl(prod([cf(opt(layout)),cf(sort("Measure")),cf(opt(layout))],sort("<START>"),no-attrs),[<term>,<term>,<term>]),<int>)
 * CO_patternMeasureEqual = appl(prod([lit("equal")],cf(sort("Measure")),attrs([term(cons("equal"))])),[lit("equal")])
 * CO_patternMeasureGreater = appl(prod([lit("greater")],cf(sort("Measure")),attrs([term(cons("greater"))])),[lit("greater")])
 * CO_patternMeasureLess = appl(prod([lit("less")],cf(sort("Measure")),attrs([term(cons("less"))])),[lit("less")])
 * CO_patternBooleanBracket = appl(prod([lit("("),cf(opt(layout)),cf(sort("Boolean")),cf(opt(layout)),lit(")")],cf(sort("Boolean")),attrs([bracket,term(cons("bracket"))])),[lit("("),<term>,<term>,<term>,lit(")")])
 * CO_patternBooleanNot = appl(prod([lit("not"),cf(opt(layout)),lit("("),cf(opt(layout)),cf(sort("Boolean")),cf(opt(layout)),lit(")")],cf(sort("Boolean")),attrs([term(cons("not"))])),[lit("not"),<term>,lit("("),<term>,<term>,<term>,lit(")")])
 * CO_patternBooleanAnd = appl(prod([cf(sort("Boolean")),cf(opt(layout)),lit("&"),cf(opt(layout)),cf(sort("Boolean"))],cf(sort("Boolean")),attrs([assoc(left),term(cons("and"))])),[<term>,<term>,lit("&"),<term>,<term>])
 * CO_patternBooleanOr = appl(prod([cf(sort("Boolean")),cf(opt(layout)),lit("|"),cf(opt(layout)),cf(sort("Boolean"))],cf(sort("Boolean")),attrs([assoc(left),term(cons("or"))])),[<term>,<term>,lit("|"),<term>,<term>])
 * CO_patternBooleanConstant = appl(prod([cf(sort("BoolCon"))],cf(sort("Boolean")),attrs([term(cons("constant"))])),[<term>])
 * CO_patternBoolConFalse = appl(prod([lit("false")],cf(sort("BoolCon")),attrs([term(constructor),term(cons("false"))])),[lit("false")])
 * CO_patternBoolConTrue = appl(prod([lit("true")],cf(sort("BoolCon")),attrs([term(constructor),term(cons("true"))])),[lit("true")])
 *
 */

static ATermList _Library_dict = NULL;

#define _Library_dict_LEN 799

static char _Library_dict_baf[_Library_dict_LEN] = {
0x00,0x8B,0xAF,0x83,0x00,0x28,0x80,0xEB,0x03,0x3C,0x5F,0x3E,0x01,0x00,0x02,0x02,
0x03,0x04,0x05,0x5B,0x5F,0x2C,0x5F,0x5D,0x02,0x00,0x6E,0x24,0x01,0x00,0x06,0x07,
0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,
0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F,0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,
0x02,0x01,0x02,0x02,0x5B,0x5D,0x00,0x00,0x01,0x03,0x69,0x6E,0x74,0x00,0x00,0x01,
0x04,0x74,0x65,0x72,0x6D,0x00,0x00,0x01,0x01,0x78,0x00,0x00,0x01,0x04,0x61,0x70,
0x70,0x6C,0x02,0x00,0x10,0x02,0x07,0x05,0x04,0x01,0x02,0x00,0x05,0x04,0x70,0x72,
0x6F,0x64,0x03,0x00,0x10,0x03,0x02,0x01,0x05,0x03,0x0B,0x0A,0x05,0x03,0x0D,0x27,
0x05,0x03,0x6C,0x69,0x74,0x01,0x00,0x0B,0x0B,0x09,0x11,0x16,0x1A,0x1E,0x1C,0x1D,
0x21,0x23,0x24,0x05,0x04,0x74,0x72,0x75,0x65,0x00,0x01,0x01,0x02,0x63,0x66,0x01,
0x00,0x06,0x04,0x0B,0x14,0x15,0x05,0x04,0x73,0x6F,0x72,0x74,0x01,0x00,0x05,0x05,
0x22,0x0C,0x26,0x12,0x05,0x07,0x42,0x6F,0x6F,0x6C,0x43,0x6F,0x6E,0x00,0x01,0x01,
0x05,0x61,0x74,0x74,0x72,0x73,0x01,0x00,0x0B,0x02,0x01,0x05,0x04,0x74,0x65,0x72,
0x6D,0x01,0x00,0x0C,0x03,0x10,0x0F,0x05,0x04,0x63,0x6F,0x6E,0x73,0x01,0x00,0x0B,
0x0B,0x09,0x11,0x13,0x17,0x1B,0x1E,0x1F,0x21,0x23,0x24,0x05,0x0B,0x63,0x6F,0x6E,
0x73,0x74,0x72,0x75,0x63,0x74,0x6F,0x72,0x00,0x00,0x01,0x05,0x66,0x61,0x6C,0x73,
0x65,0x00,0x01,0x01,0x07,0x42,0x6F,0x6F,0x6C,0x65,0x61,0x6E,0x00,0x01,0x01,0x08,
0x63,0x6F,0x6E,0x73,0x74,0x61,0x6E,0x74,0x00,0x01,0x01,0x03,0x6F,0x70,0x74,0x01,
0x00,0x02,0x02,0x15,0x05,0x06,0x6C,0x61,0x79,0x6F,0x75,0x74,0x00,0x00,0x01,0x01,
0x7C,0x00,0x01,0x01,0x02,0x6F,0x72,0x00,0x01,0x01,0x05,0x61,0x73,0x73,0x6F,0x63,
0x01,0x00,0x02,0x02,0x19,0x05,0x04,0x6C,0x65,0x66,0x74,0x00,0x00,0x01,0x01,0x26,
0x00,0x01,0x01,0x03,0x61,0x6E,0x64,0x00,0x01,0x01,0x01,0x29,0x00,0x01,0x01,0x01,
0x28,0x00,0x01,0x01,0x03,0x6E,0x6F,0x74,0x00,0x01,0x01,0x07,0x62,0x72,0x61,0x63,
0x6B,0x65,0x74,0x00,0x01,0x01,0x07,0x62,0x72,0x61,0x63,0x6B,0x65,0x74,0x00,0x00,
0x01,0x04,0x6C,0x65,0x73,0x73,0x00,0x01,0x01,0x07,0x4D,0x65,0x61,0x73,0x75,0x72,
0x65,0x00,0x01,0x01,0x07,0x67,0x72,0x65,0x61,0x74,0x65,0x72,0x00,0x01,0x01,0x05,
0x65,0x71,0x75,0x61,0x6C,0x00,0x01,0x01,0x09,0x70,0x61,0x72,0x73,0x65,0x74,0x72,
0x65,0x65,0x02,0x00,0x04,0x02,0x06,0x05,0x02,0x00,0x05,0x07,0x3C,0x53,0x54,0x41,
0x52,0x54,0x3E,0x00,0x01,0x01,0x08,0x6E,0x6F,0x2D,0x61,0x74,0x74,0x72,0x73,0x00,
0x00,0x01,0x01,0x02,0x12,0x00,0xB0,0x0B,0x00,0x2A,0x7C,0x20,0x14,0x79,0x41,0x71,
0x81,0x83,0x9C,0x02,0x6C,0x10,0x59,0x20,0x44,0xC5,0x01,0x06,0x68,0x0A,0x74,0x30,
0x69,0x61,0x51,0xC0,0xA7,0x86,0x40,0x88,0x49,0x11,0x11,0x20,0x26,0x47,0x82,0x88,
0x71,0x51,0x61,0xA0,0xC7,0x46,0x81,0x85,0x13,0x12,0x16,0x04,0x6C,0x70,0x38,0x61,
0x71,0x41,0xE0,0x87,0xC6,0x00,0x44,0x10,0x8A,0x20,0x22,0x00,0xC6,0x0D,0x81,0xAD,
0x08,0x10,0x91,0x31,0x15,0x29,0x12,0x21,0x5A,0x08,0x10,0x4A,0x84,0xB8,0x61,0x83,
0x15,0x26,0x10,0x49,0x8C,0x26,0x31,0x30,0xA1,0x28,0x0A,0x80,0x39,0x40,0x0C,0xA0,
0x22,0x01,0x70,0xA0,0x81,0x25,0x4C,0x22,0x53,0x04,0x68,0x62,0x14,0x05,0x11,0xCE,
0x1C,0x50,0xA4,0x69,0x84,0x6A,0x62,0x82,0x0C,0x42,0x80,0xA2,0x19,0xA0,0xC1,0x95,
0xA2,0x22,0x6A,0x90,0x3A,0x52,0x28,0x98,0x5A,0x29,0xA1,0xC3,0x97,0xA2,0x10,0x6A,
0x88,0x06,0x51,0x24,0x18,0x7A,0x09,0xA0,0x20,0x54,0x62,0x33,0xAA,0x98,0x26,0x53,
0x2C,0xE8,0x46,0x31,0xA1,0x22,0x57,0x62,0x09,0x86,0xCC,0x22,0xB3,0x18,0x2C,0xC2,
0x33,0x11,0x55,0x58,0x22,0x71,0xC0,0xE5,0x0A,0x26,0x85,0xE2,0x08,0x3D,0x00,0x6E,
0x80,0x17,0x40,0x19,0x84,0x68,0x28,0x55,0xF8,0x86,0x21,0xF3,0x08,0xED,0x62,0x80,
0x2A,0x55,0x55,0x04,0x24,0x31,0x06,0x00,0xBC,0x43,0x43,0x46,0xAE,0x2C,0x61,0x8B,
0x08,0xA2,0x47,0x60,0x45,0x84,0x61,0x63,0x56,0x18,0x92,0x54,0x02,0x54,0x69,0x8A,
0x06,0x60,0x0D,0x30,0x02,0x92,0x38,0xE4,0x03,0x40,0xC1,0xAF,0x2C,0x61,0xCB,0x08,
0xB2,0x40,0xA0,0x46,0xC7,0x11,0x4A,0x80,0xAA,0x75,0xD9,0x02,0xB0,0x06,0x58,0x01,
0x29,0x02,0x72,0x3E,0xA1,0x62,0xD5,0x56,0x0D,0x60,0x46,0xAA,0x0C,0x29,0x06,0x43,
0xD4,0x1C,0x3A,0xEA,0x44,0xC5,0x12,0x49,0xAA,0x48,0x0D,0x51,0x69,0x88,0x75,0x2E,
0xA1,0xE3,0xD2,0xD2,0x14,0x28,0x8A,0x1D,0x52,0x4D,0xAA,0x6C,0xA1,0x05,0xB4
};

void init_Library_dict()
{
  ATermList afuns, terms;

  _Library_dict = (ATermList)ATreadFromBinaryString(_Library_dict_baf, _Library_dict_LEN);

  ATprotect((ATerm *)&_Library_dict);

  afuns = (ATermList)ATelementAt(_Library_dict, 0);

  CO_afun0 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun1 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun2 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun3 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun4 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun5 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun6 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun7 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun8 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun9 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun10 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun11 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun12 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun13 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun14 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun15 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun16 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun17 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun18 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun19 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun20 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun21 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun22 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun23 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun24 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun25 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun26 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun27 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun28 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun29 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun30 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun31 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun32 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);
  CO_afun33 = ATgetAFun((ATermAppl)ATgetFirst(afuns));
  afuns = ATgetNext(afuns);

  terms = (ATermList)ATelementAt(_Library_dict, 1);

  CO_patternOptLayoutPresent = ATgetFirst(terms);
  terms = ATgetNext(terms);
  CO_patternOptLayoutAbsent = ATgetFirst(terms);
  terms = ATgetNext(terms);
  CO_patternStartBoolean = ATgetFirst(terms);
  terms = ATgetNext(terms);
  CO_patternStartBoolCon = ATgetFirst(terms);
  terms = ATgetNext(terms);
  CO_patternStartMeasure = ATgetFirst(terms);
  terms = ATgetNext(terms);
  CO_patternMeasureEqual = ATgetFirst(terms);
  terms = ATgetNext(terms);
  CO_patternMeasureGreater = ATgetFirst(terms);
  terms = ATgetNext(terms);
  CO_patternMeasureLess = ATgetFirst(terms);
  terms = ATgetNext(terms);
  CO_patternBooleanBracket = ATgetFirst(terms);
  terms = ATgetNext(terms);
  CO_patternBooleanNot = ATgetFirst(terms);
  terms = ATgetNext(terms);
  CO_patternBooleanAnd = ATgetFirst(terms);
  terms = ATgetNext(terms);
  CO_patternBooleanOr = ATgetFirst(terms);
  terms = ATgetNext(terms);
  CO_patternBooleanConstant = ATgetFirst(terms);
  terms = ATgetNext(terms);
  CO_patternBoolConFalse = ATgetFirst(terms);
  terms = ATgetNext(terms);
  CO_patternBoolConTrue = ATgetFirst(terms);
  terms = ATgetNext(terms);
}