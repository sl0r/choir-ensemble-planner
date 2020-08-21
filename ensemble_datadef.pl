:- module(ensemble_datadef,[
              is_available/4,
              clear_data/0,
              populate_abwesend/0,
              init_member_datelist/0,
              update_participation/2,
              randomize_data/0,
              select_person/2,
              select_person/3,
              init_sessioncounter/1,
              inc_sessioncounter/3
          ]).

/** <module> Ensemble Data Definitions

Contains definitions of the dynamic predicates chormitglied/3, termin/3
and abwesend/4 and operations on these predicates.

@author Sebastian Lorenz
@license MIT

*/


:- dynamic chormitglied/3.
:- dynamic termin/3.
:- dynamic abwesend/4.


/** is_available(+Name:atom, +Year:int, +Month:int, +Day:int) is semidet
 *
 * Checks if the person with Name is available on the date specified
 * by Year, Month and Day.
 */

is_available(Name, Year, Month, Day) :- not(abwesend(Name, Year, Month, Day)).


/** select_person(-Person:atom, +Voice)
 *
 * Outputs a person from the dynamic predicate chormitglied/3.
 * If voice is var, select any member. If voice is bound to _bass_,
 * _tenor_, _sopran_ or _alt_, select a member in the corresponding
 * voice group.
*/

select_person(Person, Voice) :-
  var(Voice),
  !,
  chormitglied(Person,_,_).

select_person(Person, Voice) :-
  chormitglied(Person, Voice,_).

%!   select_person(-Person, +Voice, -Dates)
%    Same as select_person/2, but includes the list of dates.

select_person(Person, Stimmgruppe,Termine) :-
  var(Stimmgruppe),                           %if Stimmgruppe not specified, just find a person
  !,
  chormitglied(Person,_,Termine).

select_person(Person, Stimmgruppe,Termine) :-
  chormitglied(Person,Stimmgruppe,Termine).


%!  clear_data
% Retract all facts from the dynamic predicates termin/3, chormitglied/3
% and abwesend/4.

clear_data :-
  retractall(termin(_,_,_)),
  retractall(chormitglied(_,_,_)),
  retractall(abwesend(_,_,_,_)).


%!  populate_abwesend
%
%   Initializes the dynamic predicate abwesend/4 based on the data found
%   in the list of dates in chormitglied/3.
%
populate_abwesend :-
   forall(chormitglied(Person,_,Terminliste),ppa(Person,Terminliste)).

ppa(_,[]).
ppa(Person,[termin(Jahr,Monat,Tag,abw)|T]) :-
   assertz(abwesend(Person,Jahr,Monat,Tag)),
   ppa(Person,T).
ppa(Person,[_|T]) :- ppa(Person,T).


%!  init_member_datelist
%
%   Initializes a list of dates for all persons that are listed in
%   chormitglied/3. Retracts the old predicates without the date list
%   and asserts new chormitglied/3 facts with the corresponding list
%   |=chormitglied(_,_,[termin(2020,7,12,anw),termin(2020,7,19,abw),..])|=.
%

init_member_datelist :-
  findall(P,chormitglied(P,_,_),Personen),
  init_all(Personen).


init_all([]).
init_all([Person|PT]) :-
  init_pl(Person),
  init_all(PT).

init_pl(P) :-
  chormitglied(P,Stimmgruppe,_),
  findall(T,init_termin(P,T),L),
  !,
  retract(chormitglied(P,_,_)),
  asserta(chormitglied(P,Stimmgruppe,L)).

init_termin(P,termin(J,M,T,abw)) :- termin(J,M,T),abwesend(P,J,M,T).
init_termin(P,termin(J,M,T,anw)) :- termin(J,M,T),not(abwesend(P,J,M,T)).



%change element from list: find element "Old" in L1 and exchange with "New" in L2
chl([],_,_,[]).
chl([Old|T1],Old,New,[New|T2]) :- chl(T1,Old,New,T2).
chl([L1|T1],Old,New,[L1|T2]) :- chl(T1,Old,New,T2).


%!  update_participation(+Dates:list, +Solution:list)
%
%   Updates for all dates in Dates and the list of lists of
%   persons found in Solution the dates in chormitglied/3.
%   Retracts old chormitglied/3 facts and asserts new ones with
%   the corresponding dates. The dates in chormitglied/3 are used for
%   outputting the solution to a CSV file.

update_participation([],[]).
update_participation([Termin|TT],[PH|PT]) :-
  update_terminteilnahme(Termin,PH),
  update_participation(TT,PT).

update_chormitglied_terminteilnahme(Name,Termin) :-
  chormitglied(Name,Stimmgruppe,Termine),
  Termin = termin(J,M,T,_),
  chl(Termine,termin(J,M,T,_),Termin,NeueTermine),
  retract(chormitglied(Name,_,_)),
  asserta(chormitglied(Name,Stimmgruppe,NeueTermine)).

%Aktualisierung der Datenbank für ALLE Personen eines Termins
update_terminteilnahme(_,[]).
update_terminteilnahme(Termin,[Person|PT]) :-
  Termin = termin(Jahr, Monat, Tag),
  update_chormitglied_terminteilnahme(Person,termin(Jahr,Monat,Tag,pln)),
  update_terminteilnahme(Termin,PT).



%!  randomize_date
%
%   Shuffle facts from chormitglied/3

randomize_data :-
  findall(chormitglied(X,Y,Z),chormitglied(X,Y,Z),CM),
  retractall(chormitglied(_,_,_)), %abolish(chormitglied/3),
  new_fact_order(CM).

new_fact_order([]).
new_fact_order([H|T]) :-
  X is random(2),
  X = 0,
  !,
  asserta(H),
  new_fact_order(T)
  ;
  assertz(H),
  new_fact_order(T).


%!  init_sessioncounter(-Dict:dict) is det
%
%   Creates a dictionary for counting the number of sessions each choir
%   member is scheduled for. Initializes all values to 0.

init_sessioncounter(Dict) :-
  findall(P:0, chormitglied(P,_,_),L),
  dict_create(Dict,einsaetze,L).


%!  inc_sessioncounter(+Person:atom, +Dict:dict, -Dict_new:dict)
%
%   Increases the number of session counts for Person in Dict by 1 and
%   outputs the updated dict in Dict_new.

inc_sessioncounter(Person, Dict, Dict_neu) :-
  X = Dict.Person,
  XplusOne is X + 1,
  Dict_neu = Dict.put(Person,XplusOne).














