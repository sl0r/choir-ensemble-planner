:- module(ensemble_fast_sol,[fast_sol/2]).

/** <module> Ensemble Fast Solution

This module implements a rather simple approach for generating
schedules for choir ensembles. Instead of checking all possible
solutions, it generates just one solution with a simple trick: it
logs the number of sessions a choir member is scheduled for and
always prefers members that have less sessions. In this way you get a
plan that is not the best, but quite balanced.

@author Sebastian Lorenz
@license MIT

*/


:- use_module(ensemble_csv_io).
:- use_module(ensemble_datadef).


/**
 * fast_sol(+File:atom, +Voices:list)
 *
 * Reads data from CSV file File and generates a solution according
 * to Voices. Voices is a list which contains the desired cast. E.g.
 * =|[bass,tenor,alt,alt,sopran,sopran,_,_,_,_]|= for an ensemble of 10
 * singers which must include at least one bass, one tenor, two alto and
 * two soprano. The solution is saved in a new CSV file with the ending
 * =|generated.csv|=.
 *
*/


fast_sol(File,Voices) :-
  import_csv(File),
  findall(termin(J,M,T),ensemble_datadef:termin(J,M,T),Dates),
  writeln('Check if there is a solution for every date...'),
  check_casts(Dates,Voices),nl,
  writeln('Generating Solution...'),
  gen_plan(Dates, Voices, Sol),
  print_plan(Dates,Sol),
  update_participation(Dates,Sol),                           %Prepare data from solution for export
  sub_string(File,_,_,4,Filename),                       %Extract name (without ".csv")
  string_concat(Filename,'_generated.csv',NewFile),
  export_csv(NewFile),
  format('~nSolution exported to ~w~n', [NewFile]),
  !.


/** gen_plan(+Dates:list, +Voices:list, -Plan:list)
 *
 * Takes a list of dates and desired voices and generates a plan.
 * Initializes a dictionary for logging the number of sessions for each
 * persond.
*/

gen_plan(Termine,Stimmgruppen,Personen) :-
  init_sessioncounter(Dic),
  all_dates_plan(Termine,Stimmgruppen,Personen,Dic).


/** ensemblesk(+Dates:list, +Voices:list, -Plan:list, +Dic)
 *
 * Generates a plan for all dates.
*/

all_dates_plan([],_,[],_).
all_dates_plan([Termin|TT],Stimmgruppe,[Personen|PT],Dic) :-
  one_date_plan(Termin,Stimmgruppe,Personen,[],Dic,Dic_n),
  all_dates_plan(TT,Stimmgruppe,PT,Dic_n).


/** ensk(+Date:compound, +Voices:list, -Plan:list, Temp:list, Dic:dict, DicNew:dict)
 *
 * Generates a plan for one date. Temp is a temporary list for checking
 * that no one is scheduled twice for the same day.
*/

one_date_plan(_,[],[],_,Dic_n,Dic_n).
one_date_plan(Termin,[Stimmgruppe|ST],[Person|PT],L,Dic,Dic_n) :-
  Termin = termin(Jahr, Monat, Tag),
  select_person(Person, Stimmgruppe),
  not(member(Person, L)),
  is_available(Person, Jahr, Monat, Tag),
  not(better_alternative(Jahr, Monat, Tag, Stimmgruppe, Person, L, Dic)),
  append(L,[Person], L2),
  inc_sessioncounter(Person, Dic, Dic_neu),
  one_date_plan(Termin, ST, PT, L2, Dic_neu, Dic_n).


/** chk_alternative(+Year:atom, +Month:atom, +Day:atom, +Voice:atom, +Person:atom, +TempPersonList:list, +SessionDic) is semidet
 *
 * Checks whether there is another suitable choir member of the same
 * vocal group who is assigned less frequently.
*/

better_alternative(Jahr,Monat,Tag,Stimmgruppe,Person,L,EDict) :-
  select_person(P_alt,Stimmgruppe),
  not(member(P_alt,L)),
  is_available(P_alt,Jahr,Monat,Tag),
  Person_anz = EDict.Person,
  P_alt_anz  = EDict.P_alt,
  P_alt_anz < Person_anz.



/** check_casts(+Dates, +Voices)
 *
 *  Check if the voice default can be complied with on all dates.
*/

check_casts([],_).
check_casts([Termin|TT],St) :-
  Termin = termin(J,M,T),
  check_cast(J,M,T,[],St),
  !,
  format('~w.~w.~w. ok, ',[T,M,J]),
  check_casts(TT,St).

check_cast(_,_,_,_,[]).
check_cast(J,M,T,Temp,[LH|LT]) :-
  select_person(P,LH,Termine),
  member(termin(J,M,T,anw),Termine),
  not(member(P,Temp)),
  append(Temp,[P],Temp_new),
  check_cast(J,M,T,Temp_new,LT).















