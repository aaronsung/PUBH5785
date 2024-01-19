*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
* Program: IALHD - Ex3                                                ; 
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
* Date:   10-07-07		                                              ;
* Author: M.Burmas      											  ;
* Edited by Melanie Greenland 14-09-17								  ;
* Purpose: Training Session 3 Syntax Solutions                        ;
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;





*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
** PART A .
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;



*---------------------------------------------------------------------;
* Q1. Open vaslinks4 data file using your preferred stats software    ;

* Set up libraries                                                    ;

* In data ;
libname dlcourse "F:\IALHD SAS Data";
* Out data ;
libname dlout "F:\Out Data";

* Double click on 'Libraries' and then 'dlout' in the SAS explorer    ;
* window to examine the datasets.    						    	  ; 

* Make a copy of the raw dataset in your work library to use     	  ;
data vaslinks4;
	set dlout.vaslinks4;
run;



*---------------------------------------------------------------------;
* Q2. Select records where yearsep >= 1985 and delete the remaining   ;
*     records. Create new morbseq variable and select records where   ;
*     morbseq = 1. Generate a frequency distribution of these records ;
*     by yearsep.                                                     ;


* This is the step-by-step way ie. the long way                       ;

* Select post 1985 records ;
data vaslinks_3a;
	* Get rid of the morbseq variable, we'll be creating a new one ;
	set vaslinks4 (drop = morbseq);
	* Select records where yearsep >= 1985 ;
	where yearsep >= 1985;
run;

* Sort dataset ;
proc sort data = vaslinks_3a;
	by rootlpno;
run;

* Create a new morbseq variable ;
data vaslinks_3b;
	set vaslinks_3a;
	by rootlpno;
	* Assign morbseq attributes ;
	attrib morbseq length = 8.
	             label = "Morbidity Sequence";
	retain morbseq;
	if first.rootlpno then morbseq = 1;
	else morbseq = morbseq + 1;
run;

* Now select records where morbseq = 1;
data vaslinks_3c;
	set vaslinks_3b;
	where morbseq = 1;
run;

* Alternative method to the above syntax to perform all in one step     ;
data vaslinks_3c;
	* Get rid of the morbseq variable ;
	set vaslinks4 (drop = morbseq);
	* Select records post 1985 ;
	where yearsep >= 1985;
	by rootlpno;
	* Don't need to create morbseq because the first record for each root ;
	* will have morbseq = 1, so just select on first.rootlpno ;
	if first.rootlpno then output;
run;



*---------------------------------------------------------------------;
* Q3. Get frequency of number of records by yearsep ;

proc freq data = vaslinks_3c;
	tables yearsep / missing;
	title "Frequency - Year of Separation - from 1985";
run;



*---------------------------------------------------------------------;
* Q4. Discard the above datasets without saving 					  ;
*     Reselect records where morbseq = 1 and yearsep >=1985 but do not;
*     delete any records this time. 								  ;			

* delete datasets;
proc datasets library = work nolist;
	delete vaslinks_3a vaslinks_3b vaslinks_3c;
quit;
run;


* Reselect records 													  ;
* This is the step-by-step way                                        ;
* First select the records where morbseq = 1 and yearsep >= 1985;
* and put into a dataset ;
data vaslinks_3a;
	set vaslinks4;
	where morbseq = 1 and yearsep >= 1985;
run;



*---------------------------------------------------------------------;
* Q5. Run a frequency on these records by yearsep ;

proc freq data = vaslinks_3a;
	tables yearsep / missing;
	title "Frequency - Year of Separation";
run;


* Alternatively this is the quick way                                 ;
* Put the where statement straight into the frequency 				  ;
* Don't need to select the records and put them into a dataset 		  ;
proc freq data = vaslinks4;
	where morbseq = 1 and yearsep >= 1985;
	tables yearsep / missing;
	title "Frequency - Year of Separation";
run;


* Reselect all the records ie. delete vaslinks_3a and continue to use ;
* dataset called vaslinks4											  ;
proc datasets library = work nolist;
	delete vaslinks_3a;
quit;
run;




*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
** PART B .
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;

*---------------------------------------------------------------------;
* Q6. Create a transfer sequence variable, transseq.                  ;

* First sort the data ;
proc sort data = vaslinks4;
	by rootlpno morbseq;
run;

data vaslinks_3a (drop = lagsep lagstype);
	set vaslinks4;
	by rootlpno;
	* Create lag variables - separation date and separation type ;
	* Define their type ;
	attrib lagsep length = 8. format = ddmmyy10.
	     lagstype length = 8.
		 transseq length = 8. label = "Transfer Set Sequence";
	retain transseq;
	lagsep   = lag(sepdate);
	lagstype = lag(septype);
	* Identify records with an admission date that falls before the sepdate ;
	* of the previous record for the same patient or on the same date provided ;
	* that septype of previous record was a transfer ;
	if first.rootlpno then transseq = 0;
	else do;
		if admdate < lagsep then transseq = transseq + 1;
		else if (admdate = lagsep) and (lagstype = 2) then transseq = transseq + 1;
		else transseq = 0;
	end;
run;

proc freq data = vaslinks_3a;
	tables transseq / missing;
	title "Frequency of Transfer Sequence Variable";
run;



*---------------------------------------------------------------------;
* Q7. Get last date of separation in a transfer set back to earlier   ;
*      records in the set                                             ;


* Turn the file upside down ;
proc sort data = vaslinks_3a;
	by rootlpno descending morbseq;
run;

data vaslinks_3b /*(drop = lag_transseq)*/;
	set vaslinks_3a;
	by rootlpno;
	length lag_transseq findate 8.;
	format findate ddmmyy10.;
	* Retain the findate over rows ;
	retain findate;
	* Create lag variable for transseq ; 
	lag_transseq = lag(transseq);
	* Initialise findate for each root ;
	if first.rootlpno or lag_transseq = 0 then findate = sepdate;
run;

* Sort file so it's the right way up ;
proc sort data = vaslinks_3b;
	by rootlpno morbseq;
run;
* check patients who have multiple transfers to see how this works    ;


*---------------------------------------------------------------------;
* Q8. Part 1 Create length of stay, los                               ;
*     Part 2 Create totlos variable, totlos                           ;

data vaslinks_3c;
	set vaslinks_3b;
	length los totlos 8.;
	* Code length of stay ;
	* If same day admission, re-code length of stay to 1 ;
	if admdate = sepdate then los = 1;
	else los = datdif(admdate, sepdate, 'actual');
	* Calculate total length of stay ;
	if admdate = findate then totlos = 1;
	else totlos = datdif(admdate, findate, 'actual');
run;



*---------------------------------------------------------------------;
* Q9. Get mean, SD and range for los and totlos for non-transfer recs ;

proc means data = vaslinks_3c n mean std min max maxdec = 4 /* maximum decimal places is 4 */;
	* only apply to index record for each person ;
	* Select non-transfer records ;
	where transseq = 0;
	var los totlos;
	title "Stats for length of stay and total length of stay variables" ;
run;

