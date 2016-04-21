SdfMetz is intended to be an instrument for Grammar Engineering. An overview of research into grammar engineering can be found in:
  * P. Klint and R. Lämmel and C. Verhoef, [Towards an engineering discipline for grammarware](http://www.cs.vu.nl/grammarware/agenda/), [(PDF)](http://www.cs.vu.nl/grammarware/agenda/paper.pdf).

In particular, SdfMetz is inspired by
  * James F. Power and Brian A. Malloy, [A metrics suite for grammar-based software](http://www.cs.nuim.ie/~jpower/Research/Papers/2004), [(PDF)](http://www.cs.nuim.ie/~jpower/Research/Papers/2004/jsme04.pdf).

We started by adapting their suite of metrics to the SDF grammar notation, and extended it with a few SDF-specific metrics. Also, the SdfMetz tool supports generation of immediate and transitive successor graphs from SDF grammars. For details, see the technical report Metrication of SDF grammars.

SDF stands for Syntax Definition Formalism. It is a purely declarative (no semantic actions) and modular grammar notations which is supported by Generalized LR Parsing. For information and resources regarding SDF, see [SDF page at program-transformation.org](http://www.program-transformation.org/Sdf/WebHome).

The initial motivation for the development of the SdfMetz tool came from a grammar development project where an SDF grammar for the VDM-SL language was recovered from an ISO reference manual. A description of the use of SdfMetz for monitoring that grammar development process can be found in:
  * Tiago Alves and Joost Visser, Grammar-centered Development of VDM Support. In proceedings of the Overture Workshop, colocated with Formal Methods 2005. Technical Report of the University of Newcastle, to appear, 2005. [pdf](http://www.di.uminho.pt/~joostvisser/publications/GrammarCenteredDevelopmentOfVdmSupport.pdf).
  * Tiago Alves and Joost Visser, Development of an Industrial Strength Grammar for VDM. Technical Report, DI-Research.PURe-05.04.29, Departamento de Informática, Universidade do Minho, April 2005. [pdf](http://wiki.di.uminho.pt/twiki/pub/Personal/Tiago/Publications/DI-PURe-05-04-29.pdf)

The project further developed to support other grammar formalims such as Antlr, Bison and DMS grammars. These formalisms are first translated to SDF (which acts as super-formalism for grammars) from which metrics and visualizations are computed.