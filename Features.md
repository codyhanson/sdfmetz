SdfMetz is a command line tool. For details about its usage, type `SdfMetz -h` at the command line prompt.

## Input ##

SdfMetz accepts one of more Antlr, Bison, SDF or DMS grammar files as input.

## Output ##

The metric values computed by SdfMetz can be exported as a nicely formatted textual report, or as comma-separated-value files, for easy import into e.g. Excel or SPSS.

SdfMetz calculates immediate and transitive successor graphs to compute structure metrics. These graphs can also be exported, in the _dot_ format of AT&T's GraphViz'.

In addition, SdfMetz can export the _non-singleton levels_ of the input grammars. These are groups of non-terminals that are mutually reachable from each other in the grammar's successor graph.

## Computed metrics ##

The suite of metrics computed by SdfMetz is defined and explained in the technical report _Metrication of SDF grammars_. They include counters for non-terminals, terminals, productions, and disambiguations constructs, McCabe's cyclometeric complexity, and structure metrics such as tree impurity and normalized count of levels.