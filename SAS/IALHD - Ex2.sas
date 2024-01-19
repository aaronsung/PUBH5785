*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
* Program: IALHD - Ex2                                                ; 
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
* Date:   07-07-07		                                              ;
* Author: M.Burmas                                                    ;
* Edited by Melanie Greenland 13-09-17								  ;
* Purpose: Training Session 2 Syntax Solutions                        ;
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;





*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
** PART A .
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;



*---------------------------------------------------------------------;
* Q1. Open vashmds data file using your preferred stats software      ;

* Set up libraries                                                    ;

* In data ;
libname dlcourse "F:\IALHD SAS Data";
* Out data ;
libname dlout "F:\Out Data";

* Double click on 'Libraries' and then 'Dlcourse' in the SAS explorer ;
* window to examine the datasets.    						    	  ; 

* Make a copy of the raw dataset in your work library to use     	  ;
data vashmds;
	set dlcourse.vashmds;
run;



*---------------------------------------------------------------------;
* Q2. Open vasdeath and vasbirth files and examine layouts            ;

* Double click on 'Libraries' and then 'Dlcourse' in the SAS explorer ;
* window to examine the datasets.    								  ; 

* Make a copy of the raw datasets in your work library to use     	  ;
data vasdeath;
	set dlcourse.vasdeath;
run;

data vasbirth;
	set dlcourse.vasbirth;
run;



*---------------------------------------------------------------------;
* Q3. Merge dthdate and dthcode from vasdeath onto end of each record ;
*     in vashmds                                                      ;

* Note: Usually we'd sort data, check for duplicates etc. first, but  ;
*       that's part of the next question. In this question we'll see  ;
* 	    what happens when we don't do this.                           ;

data vashmds_dth;
	merge vashmds (in = mor) vasdeath (in = dth);
	by rootlpno;
	if mor then output;
run;


* We don't get any warning errors in the log, but if we check the log,;
* we see that the vashmds file has 9406 observations, but the result- ;
* ing file has 9407, both should have the same no. of observations.   ;

* Note: There are other ways to merge in SAS eg. proc sql.            ;



*---------------------------------------------------------------------;
* Q4. Why doesn't step 3 work at first?                              ;


* Because we haven't sorted the datasets first and checked for dupes  ;

* Note: One would expect a 'death' file to have only one record per   ;
* rootlpno (person), but this is not the case, there are legitimate   ;
* circumstances when a person's death is registered more than once.   ;
* Dupes may also be bad/incorrect links so should be investigated     ;

* Note: Can also use indexes to speed up merges (no sorting req'd) ...;
* depends on file size and the type of merge/join as to whether using ;
* indexes is more efficient than the standard sort then merge process.;


* Sort the vasdeath file by rootlpno ;
proc sort data = vasdeath;
	by rootlpno;
run;


* Investigate the people with more than one death record ;

* If only one record for a rootlpno their first. and last. obs        ;
* would be the same record, these rootlpnos are OK.					  ; 
* We want to look at people with more than one record                 ;
data vasdeath_dups;
	set vasdeath;
	by rootlpno;
	if not (first.rootlpno and last.rootlpno) then output;
run;


* There are two observations in the vasdeath_dups file                ;
* rootlpno 14171943 with two different dates of death and causes of   ;
* death. Because the dthdates for the two records are quite different ;
* this looks like it might be a bad link and should be investigated.  ;

* Compare to records in other files ... hmds to help determine which  ;
* record to keep.                                                     ;       

* Write code to remove the record that we don't want to keep 		  ;
data vasdeath_nodupes;
	set vasdeath;
	if rootlpno = 14171943 and dthdate = "09Nov1982"d then delete;
run;
* Check in the log that the number of observations is fewer (35 now)  ;

* Now we can merge ;
* Sort files first ;
proc sort data = vashmds;
	by rootlpno;
run;

proc sort data = vasdeath_nodupes;
	by rootlpno;
run;

data vaslinks;
	merge vashmds (in = mor) vasdeath_nodupes (in = dth);
	by rootlpno;
	if mor then output;
run;
* Check the log, 9406 observations in merged file ;



*---------------------------------------------------------------------;
* Q5. Add cancer info (vascancer2) to the end of the vaslinks file    ;

* We know that vascancer2 has one record per rootlpno so we don't need;
* to look for any duplicates.                                         ;

* Make a copy of the raw dataset in your work library to use     	  ;
data vascancer2;
	set dlout.vascancer2;
run;

* Sort data first ;
proc sort data = vascancer2;
	by rootlpno;
run;

proc sort data = vaslinks;
	by rootlpno;
run;

data vaslinks_a;
  merge vaslinks (in = lin) vascancer2 (in = can);
  by rootlpno;
  if lin;
run;
* Check log, 9406 observations in new vaslinks_a file ;



*---------------------------------------------------------------------;
* Q6. Add birth reg. data (vasbirth) to the vaslinks_a file           ;

* Again, we know that we should sort and check for dupes, but that's  ;
* part of the next question so we won't do it yet.                    ;

* Merge vaslinka_a file and vasbirth file;
data vaslinks_b;
	merge vaslinks_a (in = lin) vasbirth (in = bir);
	by rootlpno;
	if lin;
run;
* Error in log, by variables not properly sorted in dataset vasbirth  ;



*---------------------------------------------------------------------;
* Q7. Why does step 6 fail the first time?                            ; 

* Because we haven't sorted the data.                                 ;

* Now sort the datasets by the same variable before merging together;
proc sort data = vasbirth;
	by rootlpno;
run;

* Note: The birth data doesn't have dupes in our dataset, but similar to;
*       deaths it is legitimate for a birth to be registered twice, so  ;
*       when working with your own births data, you should check for    ;
*       duplicates.                                                     ;

proc sort data = vaslinks_a;
	by rootlpno;
run;

* Now merge by the sorted variable;
data vaslinks_c;
	merge vaslinks_a (in = lin) vasbirth (in = bir);
	by rootlpno;
	if lin then output;
run;
* Check log, all OK, inspect dataset, all OK ;




*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
** PART B .
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;

*---------------------------------------------------------------------;
* Q8. Add a year of separation variable, yearsep (from sepdate)       ;
*     Create a variable, treat, to 1 if record mentions vasectomy and ;
*     to 2 if the record mentions a vasovasostomy (with default set   ;
*     to 0.                                                           ;


data vaslinks_d (drop = i);
	set vaslinks_c;

	* Specify variables attributes ;
	attrib yearsep length = 8. label = "Year of Separation"
	     treat length = 3.;

	* Create yearsep variable ;
	yearsep = year(sepdate);

	* Set up an array to search through procedures for the ICD code ;
	array procs{3} proc1-proc3;

	* Search through proc1-proc3 and look for vasectomy ;
	* or vasovasostomy procedure code ;
	do i = 1 to 3 until (treat > 0);
		* If sepdate pre 1998 - ICPM ;
		if yearsep < 1988 then do;
		    * Vasectomy ;
		    if procs{i} in (56.36, 59.81) then treat = 1;
		    * Vasovasostomy ;
		    else if procs{i} in (56.34, 56.37) then treat = 2;
		    * Other ;
		    else treat = 0;
		end;
		* If sepdate post 1998 - ICD-9-CM ;
		else if yearsep >= 1988 then do;
			* Vasectomy ;
			if procs{i} >= 63.70 and procs{i} <= 63.79 then treat = 1;
			* Vasovasostomy ;
			else if procs{i} >= 63.80 and procs{i} <= 63.89 then treat = 2;
			* Other ;
			else treat = 0;
		end;
	end;
run;

* Get frequencies;
proc freq data = vaslinks_d;
	tables treat / missing;
	title "Count of men with vasectomies and vasovasostomies";
run;



*---------------------------------------------------------------------;
* Q9. Create filseq variable numbering all values from 1 to 9406      ;

proc sort data = vaslinks_d;
	by rootlpno sepdate;
run;

data vaslinks_e;
	set vaslinks_d;
	* Specify variable attributes ;
	attrib fileseq length = 8. 
				label = "File Sequence";
	* use the automatic sas variable _n_ to assign value to fileseq ;
	fileseq = _n_;
run;



*---------------------------------------------------------------------;
* Q10. Create a morbseq variable at the end of each record            ;

* data is already sorted;
data vaslinks_f;
	set vaslinks_e;
	by rootlpno;
	* Specify variable attributes ;
	attrib morbseq length = 8.
	             label = "Morbidity Sequence";
	retain morbseq;
	if first.rootlpno then morbseq = 1;
	else morbseq = morbseq + 1;
run;



*---------------------------------------------------------------------;
* Q11. Create vasseq variable. Value 0 before vasectomy, 1 for        ;
*       vasectomy record and increment post vasectomy                 ;

* data is already sorted;
data vaslinks_g;
	set vaslinks_f;
	by rootlpno;
	* Specify variable attributes ;
	attrib vasseq length = 8.
	            label = "Vasectomy Sequence";
	retain vasseq;
	* start with first record for each person ;
	* if this record is a vasectomy record then vasseq is 1 ;
	* otherwise it's 0 ;
	if first.rootlpno then do;
		if treat = 1 then vasseq = 1;
		else vasseq = 0;
	end;
	* look at subsequent records;
	* if have already had vasectomy (ie. vasseq >= 1) ;
	* then increment or if this is the first vasectomy record ;
	* increment 0 to 1 ;
	else if treat = 1 or vasseq >=1 then vasseq = vasseq + 1;
run;



*---------------------------------------------------------------------;
* Q12. Create revseq variable. Value of 0 before vasovasostomy, 1     ;
*        for vasovasostomy record and increment post vasovasostomy    ;


data vaslinks_h;
	set vaslinks_g;
	by rootlpno;
	attrib revseq length = 8.
	            label = "Vasovasostomy Sequence";
	retain revseq ;
	* start with first record for each person ;
	* if this record is a vaso record then revseq is 1 ;
	* otherwise it's 0 ;
	if first.rootlpno then do;
	if treat = 2 then revseq = 1;
	else revseq = 0;
	end;
	* look at subsequent records;
	* if have already had vaso (ie. revseq >= 1) ;
	* then increment or if this is the first vaso record ;
	* increment 0 to 1 ;
	else if treat = 2 or revseq >= 1 then revseq = revseq + 1;
run;



*---------------------------------------------------------------------;
* Q13. Create dead variable. Value of 0 for records for living indiv- ;
*       iduals and 1 if dead.                                         ;

data vaslinks_i;
	set vaslinks_h;
	attrib dead length = 3.
	          label = "Death Flag";
	* If they haven't died, they don't have a dthdate ;
	if dthdate = . then dead = 0;
	else dead = 1;
run;



*---------------------------------------------------------------------;
* Q14 save vaslinks file.             								  ;

* save dataset as vaslinks4 in dlout library;
data dlout.vaslinks4;
	set work.vaslinks_i;
run;



*---------------------------------------------------------------------;
* Q15. Complete table in book              							  ;


* Stats for men who underwent a vasectomy ;

* Use formats rather than creating new variables ;

* Set up formats ;
proc format;
  * Set up a format for to use for candate1 and candate2. This will help;
  * to identify number of ppl with first time and second time cancers  ;
  * without having to create two new variables ;
  value can1date   .    = "No cancer"
                  other = "1st time cancer"
				  ;
  value	can2date   .    = "No cancer"
		          other = "2nd time cancer"
				  ;
  * Set up format to determine if have had prior vaso (at time of vas index) ;
  value revcheck  0      = "No vaso yet"
                  1-high = "Prior vaso"
				  ;
  * set up format to determine if have had prior vas (at time of vaso index) ;
  value vascheck  0      = "No vas yet"
                  1-high = "Prior vas"
				  ;
  * set up format to determine if have had paternity registration ;
  value patcheck   .   = "No paternity registration "
                  other = "Paternity registration"
				  ;
  * set up format for dead variable ;
  value dead 	  0 = "Alive"
             	  1 = "Dead"
			 	  ;
run; 

* Use the index vasectomy record (vasseq = 1)             ;
* Get frequencies of: number of men undergoing vasectomy  ;
*                     number who died                     ;
*                     number with a first cancer reg      ;
*                     number with a second cancer reg     ;
*                     number with a prior vaso            ;

proc freq data = vaslinks_i;
	* only apply to index record for each person ;
	where  vasseq = 1;  
	tables vasseq dead candate1 candate2 revseq / missing;
	title1 "Frequencies for men who underwent a vasectomy";
	title2 "Stats relating to index vasectomy record";
	format candate1 can1date.
	       candate2 can2date.
	       revseq   revcheck.
	       dead     dead.;
run;

* Get mean age in years and standard deviation;
proc means data = vaslinks_i mean std;
	* only apply to index record for each person ;
	where vasseq = 1;
	var age;
	title "Average age of men who underwent vasectomy" ;
run;

* Stats for men who underwent vasovasostomy ;
* Use the index vaso record                               ;
* Get frequencies of: number of men undergoing vaso       ;
*                     number who died                     ;
*                     number with a first cancer reg      ;
*                     number with a second cancer reg     ;
*                     number with vasectomy prior         ;
*                     number with paternity registration  ;   

proc freq data = vaslinks_i;
	* only apply to index record for each person ;
	where  revseq = 1;  
	tables revseq dead candate1 candate2 vasseq bthdate / missing;
	title1 "Frequencies for men who underwent a vasovasostomy";
	title2 "Stats relating to index vasovasostomy record";
	format candate1 Can1date.
	       candate2 Can2date.
	       vasseq   vascheck.
	       bthdate  patcheck.
	       dead     dead.;
run;

* Get mean age in years and standard deviation;
proc means data = vaslinks_i mean std;
	* only apply to index record for each person ;
	where revseq = 1;
	var age;
	title "Average age of men who underwent vasovasostomy" ;
run;



*---------------------------------------------------------------------;
* Save final vaslinks file                                            ;
*---------------------------------------------------------------------;

* Delete the existing vaslinks4 file in the output library ;
proc datasets library = dlout nolist;
	delete vaslinks4;
quit;
run;

* Now set the lastest vaslinks file, vaslinks_i to be the vaslinks4 file;
* This new file will have formats associated to the variables			; 
data dlout.vaslinks4;
	set vaslinks_i;
run;

