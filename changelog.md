-*-change-log-*-

v0.5.1.1: May 2016
 * Fix: GHC 8.0 compatibility

v0.5.1: March 2016
 * Fix: serialization of multi criteria css selector.

v0.5: March 2016:
 * Adding: preserveAspectRatio attribute
 * Fix: Application of CSS rules with indirect parent/child relation.

v0.4.2: March 2016
 * Enhancement: avoiding serializatinon of empty class attribute
 * Fix: incorrect deserialization of complex CSS
 * Fix: Really fixing duplicate ID with serialization

v0.4.1: February 2016
 * Fix: fixing duplicate ID with serialization

v0.4: February 2016
 * Breaking change: viewbox types are no longer Int
   but double, sneakingly passed in v0.3.2.2. This
   version acknoweledge this change

V0.3.2.2 February 2016 (Deprecated)
 * Fix: Bad serialization of some None constructors.

v0.3.2.1 October 2015
 * Fix: Don't add '#' for <img> serialization

v0.3.2 August 2015
 * Fix: allow compilation with GHC 7.4

v0.3.1 May 2015
 * Fix: Bumping lens dependency and removing upper bound.

v0.3 April 2015
 * Breaking change: Switching all the numeric types associated to geometry
   to Double precision (thx to Kasbah)

v0.2 April 2015
 * Fix: Differentiating opacity & fill-opacity, as they are
   semantically deferent (BREAKING CHANGE!)

v0.1.1 April 2015
 * Fix: Bumping lens bounds

v0.1.0.2 March 2015
 * Fix: Bumping lens bounds

v0.1.0.1
 * Fix: Lowering some lower bounds

v0.1
 * Initial release

