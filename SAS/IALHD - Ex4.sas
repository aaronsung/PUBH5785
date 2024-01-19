*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
* Program: IALHD - Ex4                                                ; 
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
* Date:   14-07-07		                                              ;
* Author: M.Burmas                                                    ;
* Edited by Melanie Greenland 14-09-17								  ;
* Purpose: Training Session 4 Syntax Solutions                        ;
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

* Double click on 'Libraries' and then 'dlout' in the SAS explorer ;
* window to examine the datasets.    						    	  ; 

* Make a copy of the raw dataset in your work library to use     	  ;
data vaslinks4;
	set dlout.vaslinks4;
run;



*---------------------------------------------------------------------;
* Q2. Create survival variables with values loaded to then end of     ;
*     each vasectomy index record:                                    ;
*     (i)  reverse: set to 1 if man had a subsequent vaso by 31Dec96  ;
*     (ii) timerev ...                                                ;


* Get follow up date ;
data vaslinks_4a;
	set vaslinks4;
	* Specify variable attributes ;
	attrib followup length = 8.
	                format = ddmmyy10.
	                label = "Date of Last Follow-up";
	if dthdate ne . and dthdate < "31Dec1996"d then followup = dthdate;
	else followup = "31Dec1996"d;
run;


* Option 1. Use merge ;

	* Get date of first vasovasostomy ;
	data vaslinks_4b (drop = treat);
		set vaslinks_4a (keep = rootlpno admdate treat rename = (admdate = vasodate));
		* Keep vaso records only ;  
		where treat = 2;
		by rootlpno;
		* Label vasodate ;
		label vasodate = "Vasovasostomy Date";
		* Get first vaso for each root ;
		if first.rootlpno;
	run;


	* Now merge this onto the vaslinks file and assign values to reverse variable ;
	data vaslinks_4c;
		merge vaslinks_4a (in = invasf) vaslinks_4b (in = invasdate);
		by rootlpno;
		if invasf then do;
			attrib reverse length = 3.
			               label = "Vasectomy Reversal";
			* If have had vaso ; 
			if vasodate ne . then do;
			  * and this record is before the vaso and the vaso was before followup ;
			  if admdate <= vasodate and vasodate < followup then reverse = 1;
			  else reverse = 0;
			end;
			else reverse = 0;
			output;
		end;
	run;



* Option 2. Using upside down file ;

	* Startwith upside down file ;
	proc sort data = vaslinks_4a;
		by rootlpno descending morbseq;
	run;

	* Create reverse variable ;
	* Also calculate vaso date ; 
	data vaslinks_4b;
		set vaslinks_4a;
		by rootlpno;
		* Vasodate_a is date of the vasovasostomy ;
		attrib reverse length = 3.
		               label = "Vasectomy Reversal"
			 vasodate_a length = 8.
			            format = ddmmyy10.;
		* Retain values across rows ;
		retain reverse vasodate_a;
		* Initialise variables for each root ;
		if first.rootlpno then do;
			* If have vaso ;
			if treat = 2 then do;
			  vasodate_a = admdate;	
			  if admdate <= followup then reverse = 1;
			  else reverse = 0;
			end;
			else do; 
			  vasodate_a = .;
			  reverse = 0;
			end;
		end;
		else do;
			if treat = 2 then do;
			  vasodate_a = admdate;
			  if admdate <= followup then reverse = 1;
			  else reverse = 0;
			end;
		end;
	run;

	* Turn back the right way ;
	proc sort data = vaslinks_4b;
		by rootlpno morbseq;
	run;

	data vaslinks_4c (drop = vasodate_a);
		set vaslinks_4b;
		by rootlpno;
		* Specify attributes ;
		attrib vasodate length = 8.
		                format = ddmmyy10.
					    label = "Vasovasostomy Date";
		retain vasodate;
		if first.rootlpno then vasodate = vasodate_a;
	run;



* Calculate time to reversal - timerev;
data vaslinks_4d;
	set vaslinks_4c;
	attrib timerev length = 8.
	               label = "Survival Time to Vasovasostomy in Days";
	* POST VASO ;
	if revseq > 1 then timerev = datdif(admdate, followup, 'actual');
	* VASO ;
	else if revseq = 1 then timerev = datdif(admdate, vasodate, 'actual'); 
	else do;
	  * NO VASO ;
	  if vasodate = . then timerev = datdif(admdate, followup, 'actual');
	  * POST VASO ;
	  else timerev = datdif(admdate, vasodate, 'actual');
	end;
run;



*---------------------------------------------------------------------;
* Q3. Life Table analysis of vasectomy index case, reverse as status  ;
*      variable and timerev as time variable                          ;


* Where statement selects the vasectomy index case ;
proc lifetest data = vaslinks_4d (where = (vasseq = 1)) plots = (s)
	graphics width = 1 method = act outsurv = surv1;
	time timerev * reverse (0);
	symbol1 v = none;
run;



*---------------------------------------------------------------------;
* Q4. create an age30 binary indicator variable for agegroup 		  ;

data vaslinks_4e;
	set vaslinks_4d;
	length age30 8.;
	if age < 30 then age30 = 1;
	else age30 = 0;
run;



*---------------------------------------------------------------------;
* Q5. perform life table analysis again using age30 as stratified var ;

proc lifetest data = vaslinks_4e (where = (vasseq = 1)) plots = (s)
	graphics width = 1 method = act outsurv = surv2;
	time timerev * reverse (0) ;
	symbol1 v = none;
	strata age30;
run;

proc phreg data = vaslinks_4e;
	where vasseq = 1;
	model timerev * reverse (0) = age30 /rl;
run;



*---------------------------------------------------------------------;
* Q6. Cumulative incidence of paternity in men with vaso              ;


* Create patern indicator ;
data vaslinks_4f;
	set vaslinks_4e;
	* specify patern and timepat type ;
	attrib patern length = 8.
	              label = "Paternity on Birth Certificate";
	* Calculate the patern variable ;
	if bthdate ne . and bthdate <= followup then patern = 1;
	else patern = 0;
run;

* Calculate timepat - date of admission to birthdate, dthdate or 31/12/96 ;
data vaslinks_4g;
	set vaslinks_4f;
	attrib timepat length = 8.
	               label = "Survival Time to Paternity on Birth Certificate";
	if patern = 0 then timepat = datdif(admdate, followup, 'actual');
	else timepat = datdif(admdate, bthdate, 'actual');
run;



*---------------------------------------------------------------------;
* Q7. Lifetable analysis for vasovasostomy index records              ;

proc lifetest data = vaslinks_4g (where = (revseq = 1)) plots = (s)
	graphics width = 1 method = act outsurv = surv3;
	time timepat * patern (0);
	symbol1 v = none;
run;



*---------------------------------------------------------------------;
* Q8. Create two new covariates year1990 and elapse6                  ;


* Get vasectomy date;
* Create year1990 var;
data vaslinks_4h;
	set vaslinks_4g;
	by rootlpno;
	attrib year1990 length = 3.
	      vasdate_a length = 8.
		            format = ddmmyy10.;
	retain vasdate_a;
	if first.rootlpno then do;
		if yearsep < 1990 then year1990 = 0;
		else year1990 = 1;
		if vasseq < 1 then vasdate_a = .;
		else if vasseq = 1 then vasdate_a = admdate;
	end;
	else do;
		if yearsep < 1990 then year1990 = 0;
		else year1990 = 1;
		if vasseq = 1 or vasseq > 1 and treat = 1 then vasdate_a = admdate;
	end;
run;

* Option 1. Use merge ;

	data vaslinks_4i (drop = treat);
		set vaslinks_4h (keep = rootlpno treat admdate rename = (admdate = vasdate));
		where treat = 1;
		by rootlpno;
		* label for vasdate ;
		label vasdate = "Vasectomy Date";
		if first.rootlpno;
	run;

data vaslinks_4j;
	merge vaslinks_4h (in = invasf) vaslinks_4i (in = invasdate);
	by rootlpno;
	if invasf;
run;

* Option 2. Use upside down file ; 

	* Apply the first vasectomy date to all variables;
	proc sort data = vaslinks_4h;
		by rootlpno descending morbseq;
	run;

	data vaslinks_4j (drop = vasdate_a);
		set vaslinks_4h;
		by rootlpno;
		length vasdate 8.;
		format vasdate ddmmyy10.;
		retain vasdate;
		if first.rootlpno then vasdate = vasdate_a;
		else do;
			if vasdate_a = . then vasdate = vasdate;
			else vasdate = vasdate_a;
		end;
	run;

	proc sort data = vaslinks_4j;
		by rootlpno morbseq;
	run;
   

* Calculate elapse6 ;
data vaslinks_4k;
	set vaslinks_4j;
	where revseq = 1; 
	length elapse6 3.;
	if vasseq >= 1 then do;
		if 0 <= yrdif(vasdate, vasodate, 'act/act') < 6 then elapse6 = 1;
		else if yrdif(vasdate, vasodate, 'act/act') >= 6 then elapse6 = 0;
	end;
	else elapse6 = 0;
run;



*---------------------------------------------------------------------;
* Q9. Multivariate cox regression                                     ;


* create subset of data containing the 43 vasovasostomies (43 records);
data vaslinks_4l;
	set vaslinks_4k;
	if revseq = 1 and vasseq >= 1;
run;

* Select cases where vaso followed vasectomy;
proc phreg data = vaslinks_4l ;
	model timepat * patern (0) = age30 year1990 elapse6 /rl;
run;

proc phreg data = vaslinks_4l ;
	model timepat * patern (0) = age30  /rl;
run;

proc phreg data = vaslinks_4l ;
	model timepat * patern (0) = year1990 /rl;
run;

proc phreg data = vaslinks_4l ;
	model timepat * patern (0) = elapse6 /rl;
run;

