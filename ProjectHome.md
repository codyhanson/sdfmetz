SdfMetz computes metrics for Antlr, Bison, SDF and DMS grammars.
Among the supported metrics are counters of terminals, non-terminals, productions, and disambiguation constructs, McCabe's cyclometric complexity, and structure metrics such as tree impurity and normalized count of grammar levels.

The metrics implemented by SdfMetz are explained and illustrated in:

  * Tiago Alves and Joost Visser, _Metrication of SDF Grammars_. Technical Report, DI-Research.PURe-05.05.01, Departamento de Informática, Universidade do Minho, May 2005. [pdf](http://wiki.di.uminho.pt/twiki/pub/Personal/Tiago/Publications/DI-PURe-05-05-01.pdf)

This technical report also shows metric values for 27 non-trivial grammars, and some discussion of the interpretation of this data. Some exerpts from the report are included below. A companion tool SdfCoverage, for measuring grammar coverage is also mentioned in the report.

## More information ##

For more information see:
  * [Background](Background.md)
  * [Implementation](Implementation.md)
  * [Features](Features.md)
  * [Experiments](Experiments.md)


## Future work ##

Apart from adding more metrics to the repertoire of SdfMetz, we intend to do the following:

  * Collect more grammars for experimentation
  * Perform more comprehensive statistical analysis on the collected data

## Credits ##

The main developers of the SdfMetz tool are:

  * Tiago Alves
  * Joost Viiser