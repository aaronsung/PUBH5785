*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
* Program: IALHD - Ex1                                                ; 
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
* Date:   03-07-07		                                              ;
* Author: M.Burmas                                                    ;
* Edited by Melanie Greenland 13-09-17								  ;
* Purpose: Training Session 1 Syntax Solutions                        ;
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;





*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
** PART A .
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;


*---------------------------------------------------------------------;
* Q1. Open vashmds data file using your preferred stats software    ;

* Set up a SAS library to point to where the data is stored           ;
* ie. on the CD, on the c drive or flash/usb drive etc.               ;

* Format is: libname libraryname "path to data";

* This points to my flash drive ;
libname dlcourse "F:\IALHD SAS Data";

* To open the dataset open the appropriate library in the explorer    ;
* window then double click on the vascancer dataset.                  ;

* Note: remember to close the dataset before performing any datasteps ;
* or procedures														  ;

* Create a copy of the dataset in the 'work' library so as not to 	  ;
* edit the raw dataset:												  ;
data work.vashmds;
	set dlcourse.vashmds;
run;
* To view the dataset click on explorer, work and you will see it here;


*---------------------------------------------------------------------;
* Q2. Provide total row count for data set;
proc contents data = vashmds;
run;


*---------------------------------------------------------------------;
* Q3. Number of records by year.-seperations ;
* create a new dataset so as not to overwrite the last one;
data vashmds1;
	set vashmds;
	yearsep=year(sepdate);
run;

proc freq data=vashmds1;
	tables yearsep;
run;


*---------------------------------------------------------------------;
* Q4. Create morbseq variable;
proc sort data = vashmds1;
	by rootlpno admdate;
run;

* create a new dataset so as not to overwrite the last one;
data vashmds2;
	set vashmds1;
	by rootlpno;

	* Specify morbseq attributes ;
	attrib morbseq length = 8.
	               label = "Morbidity Sequence";

	* Retain value of moreseq across iterations ;
	retain morbseq;

	* Reset the value of morbseq for each root ;
	if first.rootlpno then morbseq = 1;

	* Add one for each root's subsequent cancer ;
	else morbseq = morbseq + 1;
run;
* visually inspect your new dataset vashmds2;


*---------------------------------------------------------------------;
*Q5. How many patients in the data set.;
data vashmds3;
	set vashmds2;
	if morbseq=1 then first_hospitalisation=1;
run;

proc freq data=vashmds3;
	tables first_hospitalisation / missing ; * note: '/missing' will count the frequency of missing records too;
run;

* there are 2933 patients with at least one record;

*Number of patients with mulitple records .;
data vashmds4;
	set vashmds3;
	if morbseq=2 then multiplerecords=1;
run;

proc freq data=vashmds4;
	tables multiplerecords / missing ;
run;

* there are 1772 patients with mutliple records;
* 2933 - 1772 = 1161 patients have a single record;


*---------------------------------------------------------------------;
*Q6. Descriptives for inpatient data set.

* Mean age at first hospitalisation.;
proc sort data = vashmds4;
	by first_hospitalisation;
run;

proc means data=vashmds4; * this will produce: n, mean, standard deviation, minimum and maximum;
	by first_hospitalisation;
	var age;
run;
* if you want to also calculate the median, lower quartile and upper quartile ;
* you can specify which summary statistics you wish to produce by listing 	  ;
* them on the top line of the procedure after the dataset name like this: 	  ;
* proc means data = vashmds4 n mean stddev min max median q1 q3				  ; 


*---------------------------------------------------------------------;
*Q7. Max number of records per patient;

proc sort data=vashmds4;
	by rootlpno descending morbseq;
run;

* create a new variable called totrec which will be the total number of records by patient;
data vashmds5; 
	set vashmds4;
	by rootlpno;
	retain totrec;
	if first.rootlpno then totrec = morbseq;
	else totrec = totrec;
run;

proc sort data = vashmds5;
	by first_hospitalisation rootlpno;
run;

proc univariate data = vashmds5;
	by first_hospitalisation;
	var totrec;
run;

* alternative code to calculate sumamry statistics for a variable;
proc means data = vashmds5 n mean median max;
	where first_hospitalisation = 1;
	var totrec;
run;





*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
** PART B .
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;

*---------------------------------------------------------------------;
* Q8. Open vascaner data file using your preferred stats software    ;

* Set up a SAS library to point to where the data is stored           ;
* ie. on the CD, on the c drive or flash/usb drive etc.               ;

* Format is: libname libraryname "path to data";

* This points to my flash drive ;
libname dlcourse "F:\IALHD SAS Data";

* Note: remember to close the dataset before performing any datasteps ;
* or procedures														  ;

* Create a copy of the dataset in the 'work' library so as not to 	  ;
* edit the raw dataset:												  ;
data work.vascancer;
	set dlcourse.vascancer;
run;
* To view the dataset click on explorer, work and you will see it here;



*---------------------------------------------------------------------;
* Q9. Create morbseq variable  ;
* Make sure data is sorted correctly, by rootlpno and in date order ;
proc sort data = vascancer;
	by rootlpno candate;
run;

data vascancer_a;
	set vascancer;
	by rootlpno;

	* Specify morbseq attributes ;
	attrib morbseq length = 8.
	               label = "Morbidity Sequence";

	* Retain value of moreseq across iterations ;
	retain morbseq;

	* Reset the value of morbseq for each root ;
	if first.rootlpno then morbseq = 1;

	* Add one for each root's subsequent cancer ;
	else morbseq = morbseq + 1;
run;
* visually inspect your new dataset vascancer_a;


*---------------------------------------------------------------------;
* Q10. Run a frequency on morbseq ;
proc freq data = vascancer_a;
	tables morbseq / missing;
	title "Frequency of Morbseq";
run;

* Note: we can use the output delivery system (ODS) in SAS to output this ;
* to a file eg. text, excel, rtf etc. Here is an example				  ;
* You can set various options like page orientation and size etc.         ;
* The folder must already exist if you save your file to a folder name    ;

ods rtf close;
options pageno=1 nocenter nodate orientation = portrait papersize = a4; 
ods rtf  file = "Frequency - Morbseq.rtf" 	/* Name of file */
         path = "F:\IALHD\Output" 			/* Where to put the output */
         style = styles.rtf; 				/* Style */
proc freq data = vascancer_a;
  tables morbseq / missing;
  title "Frequency of Morbseq";
run;
ods rtf close;



*---------------------------------------------------------------------;
* Q11. Reconstruct file so that one record per person, containing all  ;
*     cancer data ie. long to wide file transformation                 ;

* Data is already sorted by rootlpno and candate ;
* From the frequency run previously we know that the max number of ;
* cancers per patient (aka root) is 2 ;
data vascancer_b (drop = cansite cantis candate morbseq);
	set vascancer_a;
	* Specify new variable length and type;
	length cansite1 cantis1 candate1 cansite2 cantis2 candate2 8.;
	* Specify the way we want date vars to appear when they are output to dataset;
	format candate1 candate2 ddmmyy10.;

	by rootlpno;
	* Retain the values of these variables over records (rows) for each root ;
	retain cansite1 cantis1 candate1 cansite2 cantis2 candate2;
	if first.rootlpno then do;
		* Reset the values for each rootlpno;
		cansite1 = cansite;
		cantis1 = cantis;
		candate1 = candate;
		cansite2 = .;
		cantis2 = .;
		candate2 = .;
	end;
	else do;
		cansite2 = cansite;
		cantis2 = cantis;
		candate2 = candate;
	end;
	* The last record for each root contains full info for each root ;
	if last.rootlpno then output;
run;




* The generic syntax for this kind of procedure would be something ;
* like below ;

* Assign max number of cancers to a macro variable using %let function ;
* Could get this by running a frequency on morbseq as we did above     ;
* or could set up some syntax to automatically set to a macro variable ;

***********************************************************************;
%let maxcan = 2; * max number of cancer records is 2 in this example ;

data vascancer_b (drop = cansite cantis candate morbseq i);
	set vascancer_a;
	by rootlpno;

	* Specify new variable type ;
	length cansite1-cansite&maxcan. cantis1-cantis&maxcan. candate1-candate&maxcan. 8.;

	* Specify the way we want date vars to appear when they are output;
	format candate1-candate&maxcan. ddmmyy10.;

	* Retain variable values over records for each root ;
	retain cansite1-cansite&maxcan. 
	       cantis1-cantis&maxcan. 
	       candate1-candate&maxcan.;

	* Set up arrays to store the values cansite cantis and candate    ;
	* &maxcan. references our macro variable defined above ie. maxcan ;
	* will be replaced by the value of 2 as that's what we've defined ;
	* it to be above.                                                 ;

	* Note: array can not have same name as a existing variable ;
	array can_site{&maxcan.} cansite1-cansite&maxcan.;
	array can_tis{&maxcan.}  cantis1-cantis&maxcan.;
	array can_date{&maxcan.} candate1-candate&maxcan.;

	if first.rootlpno then do;
		* Reset the values for each root;
		can_site{1} = cansite;
		can_tis{1}  = cantis;
		can_date{1} = candate;
		* The first record for each root contains details of the first cancer ;
		* All other cansite, cantis and candate values should be null         ;
			do i = 2 to &maxcan;
			  can_site{i} = .;
			  can_tis{i}  = .;
			  can_date{i} = .;
			end;
	end;
	else do;
		* Assign values for details of each subsequent cancer ;
		can_site{morbseq} = cansite;
		can_tis{morbseq}  = cantis;
		can_date{morbseq} = candate;
	end;

	* The last record will contain the summary data for each root ;
	* Discard other records ;
	if last.rootlpno then output;

run;
***********************************************************************;





*---------------------------------------------------------------------;
* Save the reconstructed file using a new filename e.g. vascancer2    ;
*---------------------------------------------------------------------;
* Save the dataset to a permanent library first set up a    		  ;
* library to save the data to ie. c drive, usb drive etc.             ;

* This points to a new folder for my new datasets on my flash drive   ;
* Remeber the folder must already exist before running this step      ; 
libname dlout "F:\Out Data";

data dlout.vascancer2;
  set work.vascancer_b;
run;
*---------------------------------------------------------------------;

