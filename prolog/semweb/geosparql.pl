:- module(
  geosparql,
  [
    'http://www.opengis.net/def/function/geosparql/boundary'/2,      % +Wkt, -Boundary
    'http://www.opengis.net/def/function/geosparql/convexHull'/2,    % +Wkt, -ConvexHull
    'http://www.opengis.net/def/function/geosparql/difference'/3,    % +Wkt1, +Wkt2, -Difference
    'http://www.opengis.net/def/function/geosparql/distance'/3,      % +Wkt1, +Wkt2, -Distance
    'http://www.opengis.net/def/function/geosparql/envelope'/2,      % +Wkt, -Envelope
    'http://www.opengis.net/def/function/geosparql/intersection'/3,  % +Wkt1, +Wkt2, -Intersection
    'http://www.opengis.net/def/function/geosparql/sfContains'/2,    % +Wkt1, +Wkt2
    'http://www.opengis.net/def/function/geosparql/sfCrosses'/2,     % +Wkt1, +Wkt2
    'http://www.opengis.net/def/function/geosparql/sfDisjoint'/2,    % +Wkt1, +Wkt2
    'http://www.opengis.net/def/function/geosparql/sfEquals'/2,      % +Wkt1, +Wkt2
    'http://www.opengis.net/def/function/geosparql/sfIntersects'/2,  % +Wkt1, +Wkt2
    'http://www.opengis.net/def/function/geosparql/sfOverlaps'/2,    % +Wkt1, +Wkt2
    'http://www.opengis.net/def/function/geosparql/sfTouches'/2,     % +Wkt1, +Wkt2
    'http://www.opengis.net/def/function/geosparql/sfWithin'/2,      % +Wkt1, +Wkt2
    'http://www.opengis.net/def/function/geosparql/symDifference'/3, % +Wkt1, +Wkt2, -Difference
    'http://www.opengis.net/def/function/geosparql/union'/3          % +Wkt1, +Wkt2, -Union
  ]
).

/** <module> GeoSPARQL

@author Wouter Beek
@tbd geof:buffer
@tbd geof:getSRID
@tbd geof:relate
@version 2018
*/

:- use_module(library(gis/gis)).
:- use_module(library(semweb/rdf_term)).

:- meta_predicate
    geof_bool(2, +, +).

:- rdf_meta
   'http://www.opengis.net/def/function/geosparql/boundary'(o, o),
   'http://www.opengis.net/def/function/geosparql/convexHull'(o, o),
   'http://www.opengis.net/def/function/geosparql/difference'(o, o, o),
   'http://www.opengis.net/def/function/geosparql/distance'(o, o, o),
   'http://www.opengis.net/def/function/geosparql/envelope'(o, o),
   'http://www.opengis.net/def/function/geosparql/intersection'(o, o, o),
   'http://www.opengis.net/def/function/geosparql/sfContains'(o, o),
   'http://www.opengis.net/def/function/geosparql/sfCrosses'(o, o),
   'http://www.opengis.net/def/function/geosparql/sfDisjoint'(o, o),
   'http://www.opengis.net/def/function/geosparql/sfEquals'(o, o),
   'http://www.opengis.net/def/function/geosparql/sfIntersects'(o, o),
   'http://www.opengis.net/def/function/geosparql/sfOverlaps'(o, o),
   'http://www.opengis.net/def/function/geosparql/sfTouches'(o, o),
   'http://www.opengis.net/def/function/geosparql/sfWithin'(o, o),
   'http://www.opengis.net/def/function/geosparql/symDifference'(o, o, o),
   'http://www.opengis.net/def/function/geosparql/union'(o, o, o).





'http://www.opengis.net/def/function/geosparql/boundary'(Wkt, Boundary) :-
  rdf_typed_literal(geo:wktLiteral, Lex1, Wkt),
  gis_boundary(Lex1, Lex2),
  rdf_typed_literal(geo:wktLiteral, Lex2, Boundary).



'http://www.opengis.net/def/function/geosparql/convexHull'(Wkt, ConvexHull) :-
  rdf_typed_literal(geo:wktLiteral, Lex1, Wkt),
  gis_convex_hull(Lex1, Lex2),
  rdf_typed_literal(geo:wktLiteral, Lex2, ConvexHull).



'http://www.opengis.net/def/function/geosparql/difference'(Wkt1, Wkt2, Difference) :-
  rdf_typed_literal(geo:wktLiteral, Lex1, Wkt1),
  rdf_typed_literal(geo:wktLiteral, Lex2, Wkt2),
  gis_difference(Lex1, Lex2, Lex3),
  rdf_typed_literal(xsd:double, Lex3, Difference).



'http://www.opengis.net/def/function/geosparql/distance'(Wkt1, Wkt2, Distance) :-
  rdf_typed_literal(geo:wktLiteral, Lex1, Wkt1),
  rdf_typed_literal(geo:wktLiteral, Lex2, Wkt2),
  gis_distance(Lex1, Lex2, Lex3),
  rdf_typed_literal(xsd:double, Lex3, Distance).



'http://www.opengis.net/def/function/geosparql/envelope'(Wkt, Envelope) :-
  rdf_typed_literal(geo:wktLiteral, Lex1, Wkt),
  gis_envelope(Lex1, Lex2),
  rdf_typed_literal(geo:wktLiteral, Lex2, Envelope).



'http://www.opengis.net/def/function/geosparql/intersection'(Wkt1, Wkt2, Intersection) :-
  rdf_typed_literal(geo:wktLiteral, Lex1, Wkt1),
  rdf_typed_literal(geo:wktLiteral, Lex2, Wkt2),
  gis_intersection(Lex1, Lex2, Lex3),
  rdf_typed_literal(geo:wktLiteral, Lex3, Intersection).



'http://www.opengis.net/def/function/geosparql/sfContains'(Wkt1, Wkt2) :-
  geof_bool(gis_contains, Wkt1, Wkt2).



'http://www.opengis.net/def/function/geosparql/sfCrosses'(Wkt1, Wkt2) :-
  geof_bool(gis_crosses, Wkt1, Wkt2).



'http://www.opengis.net/def/function/geosparql/sfDisjoint'(Wkt1, Wkt2) :-
  geof_bool(gis_disjoint, Wkt1, Wkt2).



'http://www.opengis.net/def/function/geosparql/sfEquals'(Wkt1, Wkt2) :-
  geof_bool(gis_equals, Wkt1, Wkt2).



'http://www.opengis.net/def/function/geosparql/sfIntersects'(Wkt1, Wkt2) :-
  geof_bool(gis_intersects, Wkt1, Wkt2).



'http://www.opengis.net/def/function/geosparql/sfOverlaps'(Wkt1, Wkt2) :-
  geof_bool(gis_overlaps, Wkt1, Wkt2).



'http://www.opengis.net/def/function/geosparql/sfTouches'(Wkt1, Wkt2) :-
  geof_bool(gis_touches, Wkt1, Wkt2).



'http://www.opengis.net/def/function/geosparql/sfWithin'(Wkt1, Wkt2) :-
  debug(sparql, "~w within?", [Wkt1]),
  geof_bool(gis_within, Wkt1, Wkt2).



'http://www.opengis.net/def/function/geosparql/symDifference'(Wkt1, Wkt2, Difference) :-
  rdf_typed_literal(geo:wktLiteral, Lex1, Wkt1),
  rdf_typed_literal(geo:wktLiteral, Lex2, Wkt2),
  gis_symmetric_difference(Lex1, Lex2, Lex3),
  rdf_typed_literal(xsd:double, Lex3, Difference).



'http://www.opengis.net/def/function/geosparql/union'(Wkt1, Wkt2, Union) :-
  rdf_typed_literal(geo:wktLiteral, Lex1, Wkt1),
  rdf_typed_literal(geo:wktLiteral, Lex2, Wkt2),
  gis_intersection(Lex1, Lex2, Lex3),
  rdf_typed_literal(geo:wktLiteral, Lex3, Union).





% GENERICS %

%! geof_bool(:Goal_2, +Wkt1:rdf_literal, +Wkt2:rdf_literal) is semidet.

geof_bool(Goal_2, Wkt1, Wkt2) :-
  rdf_typed_literal(geo:wktLiteral, Lex1, Wkt1),
  rdf_typed_literal(geo:wktLiteral, Lex2, Wkt2),
  call(Goal_2, Lex1, Lex2).
