:- module(ensemble_csv_io,[export_csv/1,
                           import_csv/1,
                           print_plan/2]).

/** <module> Ensemble CSV IO

Helper module for the _Ensemble Planner_. It imports data from a CSV
file and populates the dynamic predicates chormitglied/3, termin/3 and
abwesend/4. It also provides the means to export data from these
predicates back to a CSV file and to output the contents of these
predicates to the terminal.

@author Sebastian Lorenz
@license MIT

*/

:- use_module(library(csv)).
:- use_module(ensemble_datadef).


/** export_csv(+Filename:string) is det
 *
 * Exports a generated plan to a new CSV file named Filename.
*/

export_csv(Dateiname) :-
  csv_table(Output),
  setup_call_cleanup(
       open(Dateiname, write, Out),
       csv_write_file(Dateiname,Output,[separator(0';)]),
       close(Out)).


/** csv_table(-T:list) is det
 *
 * Gives back data from the dynamic predicates termin/3 and
 * chormitglied/3. Basically provides a table which is ready to be
 * output as a CSV file.
*/

csv_table(T) :-
  csv_header(Z1),
  findall(Z,csv_row(_,Z),Zeilen),
  append([Z1],Zeilen,T).


/** csv_header(-Header:list) is det
 *
 * Gives back the header row for CSV output.
*/

csv_header(Z1term) :-
  Zeilenbeginn = [z,"Name","Stimmgruppe"],
  findall(S,(ensemble_datadef:termin(J,M,T),atomics_to_string([T,M,J],'.',S)),Terminliste),
  append(Zeilenbeginn,Terminliste,Ztemp),
  append(Ztemp,["Einsaetze"],Zeile1),
  Z1term =.. Zeile1.


/** csv_row(+Person:atom, -Row:list)
 *
 * Gives back a row which contains the schedule of a Person for all
 * dates.
*/

csv_row(P,Zeilenterm) :-
  ensemble_datadef:chormitglied(P,Stimmgruppe,TListe),
  csv_stimme(Stimmgruppe,StimmString),Templ = [z,P,StimmString],
  findall(StatusString,(ensemble_datadef:termin(J,M,T),member(termin(J,M,T,Status),TListe),csv_status(Status,StatusString)),StatusListe),
  append(Templ,StatusListe,Templ2),
  aggregate_all(count,member(termin(_,_,_,pln),TListe),PLN),
  append(Templ2,[PLN],Templ3),
  Zeilenterm=..Templ3.

csv_status(anw,"").
csv_status(abw,"-").
csv_status(pln,"X").

csv_stimme(bass,'Bass').
csv_stimme(tenor,'Tenor').
csv_stimme(alt,'Alt').
csv_stimme(sopran,'Sopran').


/** import_csv(+Filename:atom)
 *
 * Imports data from the CSV file Filename and populates the dynamic
 * predicates termin/3, chormitglied/3 and abwesend/4.
 *
*/

import_csv(Dateiname) :-
  %writeln('Checking if file exists...'),
  exists_file(Dateiname),
  %writeln('Importing CSV...'),
  clear_data,
  read_data(Dateiname,Tabelle),
  read_table(Tabelle),
  populate_abwesend,
  print_import_stats,
  !.


/** read_data(+File:atom, -Table:list) is det
 *
 * Gives back a table which represents the content from the CSV file
 * File.
 *
*/

read_data(File,Tabelle) :-
   setup_call_cleanup(
       open(File, read, In),
       csv_read_file(File,Tabelle,[separator(0';)]),
       close(In)).

/** read_table(+Table:list) is det
 *
*/

read_table([H|T]) :-
   H =.. HeaderList,
   extract_header(HeaderList),
   read_lines(T).

/** read_lines(+Table:list) is det
 *
*/

read_lines([]).
read_lines([H|T]) :-
   H =.. HList,
   extract_row(HList),
   read_lines(T).


/** extract_row(+Row:list) is det
 *
*/

extract_row([]).
extract_row(Row) :-
   Row = [_|T],           %functor ignorieren
   T = [Name|Rest1],
   Rest1 = [Stimmgruppe|Rest2],
   extract_status_list(Rest2, Statusliste),
   atom_string(AName,Name),
   atom_string(AStimmgruppe,Stimmgruppe),
   csv_stimme(ASt,AStimmgruppe),
   assertz(ensemble_datadef:chormitglied(AName,ASt,Statusliste)).

extract_status_list(L,StL) :-
   findall(termin(X,Y,Z),ensemble_datadef:termin(X,Y,Z),Terminliste),
   extract_status_list_rec(L,Terminliste,[],StL).

extract_status_list_rec([],[],Erg,Erg).
extract_status_list_rec([L|LT],[T|TT],Zw,Erg) :-
   atom_string(L,LString),
   csv_status(Status,LString),                    %String in Statuscode umwandeln
   T =.. TListe,                                  %termin-Functor in Liste umwandeln
   append(TListe,[Status],TListe2),               %Status in Liste ergänzen
   T2 =.. TListe2,                                %neue Liste wieder in Functor umwandeln
   append(Zw,[T2],Zw2),
   extract_status_list_rec(LT,TT,Zw2,Erg).


extract_header([]).
extract_header([H|T]) :-
   atom_string(H,S),                              %Listenelement zu String konvertieren
   re_match("\\d\\d\\.\\d\\d.\\d\\d\\d\\d",S),    %bei der Form dd.mm.yyyy...
   split_string(S, ".", "", Erg),                 %String aufteilen
   Erg = [Tag,Monat,Jahr],
   atom_number(Tag,TagN),                         %Aufgeteilte Strings zu Atoms konvertieren
   atom_number(Monat,MonatN),
   atom_number(Jahr,JahrN),
   assertz(ensemble_datadef:termin(JahrN,MonatN,TagN)),
   extract_header(T).
extract_header([_|T]) :-
   extract_header(T).


print_import_stats :-
   findall(termin(J,M,T),ensemble_datadef:termin(J,M,T),Dates),
   length(Dates,Number_of_Dates),
   format('- ~w dates found~n',[Number_of_Dates]),
   print_termin_stats(Dates),
   aggregate_all(count,ensemble_datadef:chormitglied(_,_,_),Personenanzahl),
   format('- ~w persons found~n',[Personenanzahl]),
   aggregate_all(count,ensemble_datadef:abwesend(_,_,_,_),Abwesenheiten),
   format('- ~w absences found~n', [Abwesenheiten]),
   writeln('Import completed.').

print_termin_stats([]).
print_termin_stats([H|Tail]) :-
  H = termin(J,M,T),
  aggregate_all(count,(ensemble_datadef:chormitglied(_,_,Termine),member(termin(J,M,T,anw),Termine)),Available),
  aggregate_all(count, (ensemble_datadef:chormitglied(_,bass,Termine), member(termin(J,M,T,anw), Termine)), Bass),
  aggregate_all(count, (ensemble_datadef:chormitglied(_,tenor,Termine), member(termin(J,M,T,anw), Termine)), Tenor),
  aggregate_all(count, (ensemble_datadef:chormitglied(_,alt,Termine), member(termin(J,M,T,anw), Termine)), Alt),
  aggregate_all(count, (ensemble_datadef:chormitglied(_,sopran,Termine), member(termin(J,M,T,anw), Termine)), Sopran),
  format('--> ~|~t~w.~w.~w.~10+: ~|~t~w~3+ persons available (~|~t~w~3+ Bass | ~|~t~w~3+ Tenor | ~|~t~w~3+ Alt | ~|~t~w~3+ Sopran)~n',[T,M,J,Available,Bass,Tenor,Alt,Sopran]),
  print_termin_stats(Tail).


/** print_plan(+Dates, +Plan) is det
 *
 * Outputs a table which represents a calculated plan to the terminal.
*/

print_plan(Termine,Personenplan) :-
  init_member_datelist,
  update_participation(Termine,Personenplan),
  findall(P,ensemble_datadef:chormitglied(P,_,_),P_L),
  msort(P_L, P_L_sorted),
  print_plz2(P_L_sorted),
  !.

print_plz2([]).
print_plz2([P|PT]) :-
  ensemble_datadef:chormitglied(P,Stimmgruppe,TListe),nl,
  format('~|~t~w~20+',[P]),pp_stimme(Stimmgruppe),
  print_tm(TListe),
  !,
  print_plz2(PT).

print_tm(TListe) :-
  ensemble_datadef:termin(J,M,T),
  member(termin(J,M,T,Status),TListe),
  pp_status(Status),
  ist_letzter_termin(J,M,T),
  aggregate_all(count,member(termin(_,_,_,pln),TListe),PLN),
  write(PLN).

pp_status(anw) :- write('     ').
pp_status(abw) :- write(' -   ').
pp_status(pln) :- write(' X   ').

pp_stimme(bass) :- write(' [B] ').
pp_stimme(tenor) :- write(' [T] ').
pp_stimme(alt) :- write(' [A] ').
pp_stimme(sopran) :- write(' [S] ').

ist_letzter_termin(J,_,_) :-
  ensemble_datadef:termin(J2,_,_),J2 > J,!,fail.
ist_letzter_termin(J,M,_) :-
  ensemble_datadef:termin(J2,M2,_),
  J = J2,
  M2 > M,
  !,fail.
ist_letzter_termin(J,M,T) :-
  ensemble_datadef:termin(J2,M2,T2),
  J = J2,
  M = M2,
  T2 > T,!,fail.
ist_letzter_termin(_,_,_).






















