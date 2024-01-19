*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
* Program: IALHD - Ex5                                                ; 
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
* Date:   15-07-07		                                              ;
* Author: M.Burmas                                                    ;
* Edited by Melanie Greenland 15-09-17								  ;
* Purpose: Training Session 5 Syntax Solutions                        ;
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;





*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
** PART A .
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;



*---------------------------------------------------------------------;
* Q1. Open phsurgery data file using your preferred stats software    ;

* Set up libraries                                                    ;

* In data ;
libname dlcourse "F:\IALHD SAS Data";
* Out data ;
libname dlout "F:\Out Data";

* Double click on 'Libraries' and then 'Dlcourse' in the SAS explorer ;
* window to examine the datasets.    						    	  ; 

* Make a copy of the raw dataset in your work library to use     	  ;
data bphsurgery;
	set dlcourse.bphsurgery;
run;



*---------------------------------------------------------------------;
* Q2. bphsurg: 1 for mention of TURP, 0 for mention of OP, else blank ;


data bphsurgery_5a (drop = i);
	set bphsurgery;
	* Define bphsurg and yearsep type ;
	attrib bphsurg length = 3.
	             label = "Surgery for BPH"
		 yearsep length = 8.
	             label = "Year of Separation";
	* Define yearsep ;
	yearsep = year(sepdate);
	* Set up array to search through procedures for relevant ICD codes ;
	array procs{3} proc1-proc3;
	* search through proc1-proc3 and look for vasectomy ;
	* or vasovasostomy procedure code ;
	do i = 1 to 3;
		* If sepdate pre 1998 - ICPM ;
		if yearsep < 1988 then do;
		  * TURP ;
		  if procs{i} = 56.01 then bphsurg = 1;
		  * OP ;
		  else if procs{i} >= 56.02 and procs{i} < 56.06 then bphsurg = 0;
		end;
		* If sepdate post 1998 - ICD-9-CM ;
		else if yearsep >= 1988 then do;
		  * TURP ;
		  if procs{i} >= 60.20 and procs{i} < 60.30 then bphsurg = 1;
		  * OP ;
		  else if procs{i} >= 60.30 and procs{i} <= 60.70 then bphsurg = 0;
		end;
	end;
run;



*---------------------------------------------------------------------;
* Q3. Create fileseq, morbseq and indexseq variables                  ;


* Data is already sorted ;
data bphsurgery_5b;
	set bphsurgery_5a;
	by rootlpno;
	* Specify sequence variable attributes ;
	attrib fileseq length = 8.
	               label = "File Sequence"
	       morbseq length = 8.
	               label = "Morbidity Sequence"
	      indexseq length = 8.
	               label = "Index Sequence"; 
	* Retain over rows ;
	retain morbseq indexseq;
	* Use SAS auto variable for fileseq ;
	fileseq = _n_;
	if first.rootlpno then do;
		* Initialise morbseq ;
		morbseq = 1;
		* Initialise indexseq ;
		if bphsurg in (0, 1) then indexseq = 1;
		else indexseq = 0;
	end;
	else do;
		* Increment morbseq ;
		morbseq = morbseq + 1;
		* Increment index seq if already have TURP/OP or if first mention of TURP/OP ;
		if indexseq >= 1 or bphsurg in (0, 1) then indexseq = indexseq + 1;
	end;
run;



*---------------------------------------------------------------------;
* Q4. Followup date and survival time and dead indicator              ;

data bphsurgery_5c;
	set bphsurgery_5b;
	* Specify variable attributes ;
	attrib dead   length = 3.
		          label = "Death Flag"
	     followup length = 8.
	              label = "Followup Date"
				  format = ddmmyy10.
		 survival length = 8.
		          label = "Survival Time in Days";
	* Determine followup date ;
	if dthdate ne . and dthdate < "31Dec1996"d then do;
		followup = dthdate;
		dead = 1;
	end;
	else do;
		followup = "31Dec1996"d;
		dead = 0;
	end;
	survival = datdif(admdate, followup, 'actual');
run;



*---------------------------------------------------------------------;
* Q5. Survival Analysis and Cox Regression                            ;

* Formats ;
proc format;
	value bphsurg 0 = "OP"
	              1 = "TURP"
				  ;
	value dead    0 = "Alive"
	              1 = "Death"
				  ;
run;

* Survival Analysis on index admissions (indexseq = 1);
proc lifetest data = bphsurgery_5c (where = (indexseq = 1)) plots = (s)
	graphics width = 365.25 method = act outsurv = surv1;
	time survival * dead (0);
	symbol1 v = none;
	strata bphsurg;
	format bphsurg bphsurg.
	       dead    dead.;
run;

* Cox regression on index admissions (indexseq = 1);
proc phreg data = bphsurgery_5c;
	where indexseq = 1;
	model survival * dead (0) = bphsurg / rl;
	format bphsurg bphsurg.
	       dead    dead.;
run;

* alternative syntax - the model already knows the censor variable is ;
* categorical so does not need to be included on class statement      ;
proc phreg data = bphsurgery_5c;
	where indexseq = 1;
	class bphsurg (ref = '0') / param =glm ;
	model survival * dead (0) = bphsurg / rl;
run;


*---------------------------------------------------------------------;
* Q6. Compare the means for OP and TURP                               ;


proc sort data = bphsurgery_5c;
	by bphsurg;
run;

proc means data = bphsurgery_5c N MEAN MAXDEC = 2;
	where indexseq = 1;
	class bphsurg;
	var age yearsep;
	types () bphsurg ;
	format bphsurg bphsurg.;
run;


* alternative syntax;
proc means data = bphsurgery_5c N MEAN MAXDEC = 2;
	where indexseq = 1;
	var age yearsep;
run;
proc means data = bphsurgery_5c N MEAN MAXDEC = 2;
	where indexseq = 1;
	by bphsurg;
	var age yearsep;
	format bphsurg bphsurg.;
run;



*---------------------------------------------------------------------;
* Q7. Cox regression - adjust for age and calendar year as continuous ;


* Calculate age variables ;
data bphsurgery_5d;
	set bphsurgery_5c;
	length age2 age2ln 8.;
	age2 = age * age;
	age2ln = (age2) * log(age);
run;

* Cox regression with age and calendar year as continous vars in model;
proc phreg data = bphsurgery_5d;
	where indexseq = 1;
	class bphsurg (ref = '0') / param =glm ;
	model survival * dead (0) = bphsurg yearsep age2 age2ln / rl;
run;



*---------------------------------------------------------------------;
* Q8. Cox regression - Charlson Index                                 ;


* Calculate Charlson covariates (don't forget to add 0.01 to value)   ;
data bphsurgery_5e;
	set bphsurgery_5d;
	length chmsqr 8. chln 8. anycom 3.;
	chmsqr = 1/(sqrt(0.01 + charlson));
	chln = log(0.01 + charlson);
	if charlson > 0 then anycom = 1;
	else anycom = 0;
run;

* Cox regression with age, age squared, calendar year, charlson and binary indicator variable;
* use a class statement for categorical/binary variables and set the reference group for each;
proc phreg data = bphsurgery_5e;
	where indexseq = 1;
	class bphsurg (ref = '0') anycom (ref = '0') / param =glm ;
	model survival * dead (0) = bphsurg yearsep age2 age2ln chmsqr chln anycom / rl;
run;




