## A grammar comparison experiment ##

As an initial experimant in grammar metrication, we have applied SdfMetz to collect metrics data for a range of grammars from various origins. Full details about this experiment can be found in the Section **Data Collection** in:

  * Tiago Alves and Joost Visser, _Metrication of SDF Grammars_. Technical Report, DI-Research.PURe-05.05.01, Departamento de Informática, Universidade do Minho, May 2005. [pdf](http://wiki.di.uminho.pt/twiki/pub/Personal/Tiago/Publications/DI-PURe-05-05-01.pdf)

### Sampled grammars ###

The sampled grammars include 18 SDF grammars for languages such as Yacc, BibTex, Fortran, Toolbus, Stratego, SDF itself, Java, SDL, C, Cobol, DB2, PL/SQL, VDM-SL, and VB.Net. These grammars were obtained from the following sources:

  * **sfi**  - Grammars freely available in the [Strafunski](http://www.cs.vu.nl/Strafunski/) bundle
  * **pg**  - Grammars freely available in the [PGen](http://www.cwi.nl/htbin/sen1/twiki/bin/view/SEN1/ParsetableGenerator)
  * **gb**  - Grammars freely available from the [Grammar Base](http://www.cs.uu.nl/~mdejonge/grammar-base/) **(unavailable)**
  * **sig** - Grammars gently provided by Dr. Tobias Kuipers from [Software Improvement Group](http://www.sig.nl)

Furthermore, five DMS grammars for ECMAScript, PHP, Java, Verilog, and C++ were sampled:

  * **sd**  - Grammars gently provided by [Dr. Ira Baxter](http://www.semdesigns.com/Company/People/idbaxter) from [Semantic Designs Inc.](http://www.semdesigns.com)

In the report, metrics calculated by us are presented side by side with those of four BNF grammars from the paper  _A metrics suite for grammar-based software_ of Power and Malloy.

### Raw data ###

The technical report presents the collected metrics data in a series of tables, one for each category of metrics. The raw data of the experiment is available upon request in a single comma-separated-value file.


## A grammar monitoring experiment ##

The initial motivation for the development of the SdfMetz tool came from a grammar development project where an SDF grammar for the VDM-SL language was recovered from an ISO reference manual. A description of the use of SdfMetz for monitoring that grammar development process can be found in:

  * Tiago Alves and Joost Visser, _Grammar-centered Development of VDM Support_. In proceedings of the Overture Workshop, colocated with Formal Methods 2005. Technical Report of the University of Newcastle, to appear, 2005. [pdf](http://www.di.uminho.pt/~joost.visser/publications/GrammarCenteredDevelopmentOfVdmSupport.pdf)

  * Tiago Alves and Joost Visser, _Development of an Industrial Strength Grammar for VDM_. Technical Report, DI-Research.PURe-05.04.29, Departamento de Informática, Universidade do Minho, April 2005. [pdf](http://www.di.uminho.pt/~joost.visser/publications/DI-Research.PURe-05.04.29.pdf)