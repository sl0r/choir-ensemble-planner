# choir-ensemble-planner
A small prolog application for generating rehearsal plans for ensembles

## What is this?

The Choir Ensemble Planner is intended as an aid for choirs to quickly create rehearsal plans for small groups. The prologue console application reads a CSV file with necessary data about the singers, their absences and the planned dates. The file can be created in Excel, for example. The program then generates a rehearsal plan with a valid (and mostly fair) distribution. It takes absences and a specification of which voices must be represented into account. SWI-Prolog is required for running the application (https://www.swi-prolog.org/Download.html).

## Usage

##### 1. Prepare a CSV file with input data. Take a look at the example files. You can do this in Excel or a simple text editor.

This is what it looks like in a text editor:
```
Name;Stimmgruppe;07.07.2020;14.07.2020;21.07.2020;28.07.2020;01.08.2020;07.08.2020
Abigail;Sopran;-;;-;;;
Jonesy;Alt;;;;-;;
Marta;Sopran;;-;;;-;
Judith;Sopran;;;-;-;;
Norbert;Bass;;;;;;-
Markus;Tenor;;-;;;;
Troy;Bass;;;;;;
Helge;Tenor;-;;;;;
Maria;Alt;;-;;;;
Sina;Alt;-;-;-;;;
```

* Dates must take the form `DD.MM.YYYY`
* The Value for *Stimmgruppe* (voice group) has to be `Bass`, `Alt`, `Tenor` or `Sopran`
* Absences are marked by `-`
*	Use Semicolons `;` as seperators (this is the Excel standard) 

##### 2. Load the application

Open the SWI Prolog Console, navigate to the directory where the application is and run `consult('start.pl').`
(Or just doubleclick on `start.pl` if you have swipl in your path...)

##### 3. Run the application

`fast_sol('File.csv', [bass, tenor, sopran, alt]).`

* Replace `File.csv` with your prepared file. It has to be in the same directory as the application.
* Replace `[bass, tenor, sopran, alt]` with your choice of voices for the ensembles. Examples:
  * `[bass, bass, tenor, tenor, sopran, sopran, alt, alt, _, _]`: Ensemble of 10 singers with two bass, two tenor, two sopranos, two altos and two arbitrary voices.
  * `[bass, tenor, sopran, sopran, alt, alt]`: Ensemble of 6 singers with exactly one bass, one tenor, two sopranos and two altos.
  * `[_,_,_,_,_]`: Ensemble of 5 singers with arbitrary voices.

The resulting plan is exported to `File_generated.csv`.

You can also try the predicate `best_sol`. It's rather experimental to date. It looks at all possible solutions and displays the best on screen. This is nice in theory, but not always in practice because of the combinatorial explosion of possible solutions. It's fun to play with for small solution spaces, however.

`best_sol('File.csv', [bass, tenor, sopran, alt], Solution).`
