:- module(ensemble_best_sol,[best_sol/3]).

/** <module> Ensemble Best Solution

This module is rather experimental to date. It implements a more
complete approach for generating schedules for choir ensembles. It
looks at all solutions (which is in practice onnly usable for small
solution spaces.

The best solution is determined in this way: We want a fair distribution
of the dates. Everyone should participate in as equal a number of
appointments as possible. So first, we calculate the ideal average. If
the ensembles consist of 10 persons and 8 dates are scheduled, there are
80 free slots. Assuming the choir consists of 20 persons. In this case,
if the distribution is as fair as possible, each person would be
assigned four times. Then we look at how much the assignment numbers in
the generated plan differ from the ideal number. The generated plan with
the lowest difference is the fairest, best plan.

@author Sebastian Lorenz
@license MIT

*/


:- use_module(ensemble_csv_io).
:- use_module(ensemble_datadef).


% datesols/2 is used for saving partial solutions for several dates.
% E.g. datesols(termin(2020,9,20),[Sol1])
%      datesols(termin(2020,9,20),[Sol2])

:- dynamic datesols/2.

% bestsol/2 is used for temporarily storing the best solution while
% traversing all different valid constellations

:- dynamic bestsol/2.

% avg/1 stores the ideal average number of participation. Basically for
% better performance. It has to be calculated only once.

:- dynamic(avg/1).

%! best_sol(+File:atom, +Voices:list, -Solution:list)
%
% Imports the CSV file File, generates partial solutions for every date
% and then generates full solutions for all dates. May take indefinitely
% for large solution spaces.

best_sol(File, Voices, Sol) :-
  import_csv(File),
  init_bestsol,
  ideal_average(Voices),
  find_partsols(Voices),
  findall(termin(Y,M,D),ensemble_datadef:termin(Y,M,D),Dates),
  !,
  find_best(Dates,Sol),
  print_plan(Dates, Sol).


%! find_best(-BestSol:list)
%
% Looks at all full solutions via backtracking and gives back the best
% solution.

find_best(Dates,BestSol) :-
  fullsol(Dates,Sol),
  init_sessioncounter(Dic),
  fill_session_numbers(Sol, Dic, NewDic),
  quality_of_schedule(NewDic, Rating),
  update_bestsol(Rating,Sol),
  fail
  ;
  bestsol(_, BestSol).


%! fullsol(+Dates:list, -Sol:list)
%
% Generates a full solution, based on already generated partial
% solutions in the dynamic predicate datesols/2.

fullsol([],[]).
fullsol([Date|DT],[Sol|ST]) :-
  datesols(Date, Sol),
  fullsol(DT,ST).

%! find_partsols(+Voices:list)
%
% Finds all valid constellations of people for all dates for the
% corresponding Voices selection and stores these partial solutions in
% the dynamic predicate datesols/2.

find_partsols(Voices) :-
  init_datesols,
  findall(termin(Y,M,D),ensemble_datadef:termin(Y,M,D),Termine),
  all_partsols_all_dates(Voices, Termine),
  print_datesols,
  !.

%! all_partsols_all_dates(+Voices:list, +Dates:list)
%
% Finds ALL partial solutions for ALL dates and stores them in the
% dynamic predicate datesols/2.

all_partsols_all_dates(_,[]).
all_partsols_all_dates(Voices,[Termin|Rest]) :-
  all_partsols_date(Termin, Voices),
  all_partsols_all_dates(Voices,Rest).

%! all_partsols_date(+Date:compound, +Voices:list)
%
% Finds ALL partial solutions for a date via backtracking on
% partsol_date/4. Updates the dynamic predicate datesols/2 via
% update_datesol/2.

all_partsols_date(Termin, Voices) :-
  partsol_date(Termin, Voices, Sol, []),
  update_datesol(Termin, Sol),
  fail
  ;
  true.

%! partsol_date(+Date:compound, +Voices:list, -Plan:list, +Temp:list)
%
% Finds a valid constellation of persons for the specified Date and the
% specified Voices (a partial plan). Temp has to be an empty list. It's
% used to make sure that no one is assigned more than once.

partsol_date(_,[],[],_).
partsol_date(Termin,[Stimmgruppe|ST],[Person|PT],L) :-
  Termin = termin(Jahr, Monat, Tag),
  select_person(Person, Stimmgruppe),
  not(member(Person, L)),                 %check if the person is in the plan already
  is_available(Person, Jahr, Monat, Tag),       %check if the person is not absent
  append(L,[Person], L2),
  partsol_date(Termin, ST, PT, L2).


fill_session_numbers(Sol, Dic, NewDic) :-
  flatten(Sol, Sol_flat),
  fill_sn_r(Sol_flat,Dic,NewDic).

fill_sn_r([], Final, Final).
fill_sn_r([Person|T],Temp,Final) :-
  inc_sessioncounter(Person, Temp, NewTemp),
  fill_sn_r(T, NewTemp, Final).


/** quality_of_schedule(+Dict,-Rating)
 *
 * Transforms the dict to a list and calculates the
 * difference for each date to the ideal distribution
 * Rating is the sum of these difference signifiers for all dates.
*/

quality_of_schedule(Dict,Rating) :-
  is_dict(Dict),
  var(Rating),
  dict_pairs(Dict,_,Pairs),
  pairs_values(Pairs,Val),
  calc_rating(Val,0,Rating).


/** calc_rating(+Val:list, +Temp:int, -Rating:float)
 *
 * Takes a list of values which represent the number of sessions all
 * choire members are scheduled for. Calculates for all values how
 * they differ from the ideal average number. Rating is the sum of
 * all these calculated differences. Temp is intented to be initiallized
 * with 0.
 *
*/

calc_rating([],Rating,Rating).
calc_rating([H|T],TempSol,Sol) :-
  avg(Avg),
  R is abs(H - Avg),
  TempSolNew is TempSol + R,
  calc_rating(T,TempSolNew,Sol).



/** ideal_average
 *
 * Calculates the ideal average number of participation
 * and saves it in the dynamic predicate avg/1.
*/

ideal_average(Voices) :-
  aggregate_all(count,ensemble_datadef:chormitglied(_,_,_),Mitgliederanzahl),
  aggregate_all(count,ensemble_datadef:termin(_,_,_),Terminanzahl),
  length(Voices, Voices_len),
  Ds is ((Terminanzahl * Voices_len / Mitgliederanzahl)),
  retractall(avg(_)),
  assertz(avg(Ds)).


%! init_datesols
%
% Retract all facts from datesols/2

init_datesols :-
  retractall(datesols(_,_)).

%! update_datesol(+Date:compound, +Sol:list)
%
% If the solution is not already present as a fact, assert a new fact
% with the solution and the corresponding date.

update_datesol(Termin, Sol) :-
  msort(Sol, Sol_sorted),
  not(datesols(Termin, Sol_sorted)),
  !,
  asserta(datesols(Termin, Sol_sorted))
  ;
  true.

init_bestsol :-
  retractall(bestsol(_,_)),
  assertz(bestsol(99999,[])).

update_bestsol(X,L) :-
  bestsol(Y,_),
  X < Y,
  !,
  retractall(bestsol(_,_)),
  assertz(bestsol(X,L)),
  format('Better solution found: ~w -- ~w~n',[X,L])
  ;
  true.


print_datesols :-
  findall(termin(Y,M,D),ensemble_datadef:termin(Y,M,D),Termine),
  pp_dsol(Termine,1,AllSolCount),
  format('----------------------~n'),
  format('~w solution(s)~n',[AllSolCount]).

pp_dsol([],AllSolCount,AllSolCount).
pp_dsol([Date|Tail],Temp,AllSolCount) :-
  Date = termin(Y,M,D),
  aggregate_all(count,datesols(Date,_),DateCount),
  Temp_new is Temp * DateCount,
  format('~|~t~w.~w.~w~10+:~| ~w partial solution(s)~n',[D,M,Y,DateCount]),
  pp_dsol(Tail,Temp_new,AllSolCount).
















