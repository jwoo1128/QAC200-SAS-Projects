/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Tuesday, January 13, 2015     TIME: 4:45:35 PM
PROJECT: WooJ_SAS_project_011315
PROJECT PATH: P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp
---------------------------------------- */

/* Library assignment for Local.JWOODATA */
Libname JWOODATA BASE 'P:\QAC\qac200\students\jwoo' ;
/* Library assignment for Local.JWOODATA */
Libname JWOODATA BASE 'P:\QAC\qac200\students\jwoo' ;
/* Library assignment for Local.MYDATA */
Libname MYDATA BASE 'P:\QAC\qac200\students\jwoo' ;
/* Library assignment for Local.MYDATA */
Libname MYDATA BASE 'P:\QAC\qac200\students\jwoo' ;


/* Conditionally delete set of tables or views, if they exists          */
/* If the member does not exist, then no action is performed   */
%macro _eg_conditional_dropds /parmbuff;
	
   	%local num;
   	%local stepneeded;
   	%local stepstarted;
   	%local dsname;
	%local name;

   	%let num=1;
	/* flags to determine whether a PROC SQL step is needed */
	/* or even started yet                                  */
	%let stepneeded=0;
	%let stepstarted=0;
   	%let dsname= %qscan(&syspbuff,&num,',()');
	%do %while(&dsname ne);	
		%let name = %sysfunc(left(&dsname));
		%if %qsysfunc(exist(&name)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;

			%end;
				drop table &name;
		%end;

		%if %sysfunc(exist(&name,view)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;
			%end;
				drop view &name;
		%end;
		%let num=%eval(&num+1);
      	%let dsname=%qscan(&syspbuff,&num,',()');
	%end;
	%if &stepstarted %then %do;
		quit;
	%end;
%mend _eg_conditional_dropds;


/* Build where clauses from stored process parameters */

%macro _eg_WhereParam( COLUMN, PARM, OPERATOR, TYPE=S, MATCHALL=_ALL_VALUES_, MATCHALL_CLAUSE=1, MAX= , IS_EXPLICIT=0);

  %local q1 q2 sq1 sq2;
  %local isEmpty;
  %local isEqual isNotEqual;
  %local isIn isNotIn;
  %local isString;
  %local isBetween;

  %let isEqual = ("%QUPCASE(&OPERATOR)" = "EQ" OR "&OPERATOR" = "=");
  %let isNotEqual = ("%QUPCASE(&OPERATOR)" = "NE" OR "&OPERATOR" = "<>");
  %let isIn = ("%QUPCASE(&OPERATOR)" = "IN");
  %let isNotIn = ("%QUPCASE(&OPERATOR)" = "NOT IN");
  %let isString = (%QUPCASE(&TYPE) eq S or %QUPCASE(&TYPE) eq STRING );
  %if &isString %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%");
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq D or %QUPCASE(&TYPE) eq DATE %then 
  %do;
    %let q1=%str(%");
    %let q2=%str(%"d);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq T or %QUPCASE(&TYPE) eq TIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"t);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq DT or %QUPCASE(&TYPE) eq DATETIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"dt);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else
  %do;
    %let q1=;
    %let q2=;
	%let sq1=;
    %let sq2=;
  %end;
  
  %if "&PARM" = "" %then %let PARM=&COLUMN;

  %let isBetween = ("%QUPCASE(&OPERATOR)"="BETWEEN" or "%QUPCASE(&OPERATOR)"="NOT BETWEEN");

  %if "&MAX" = "" %then %do;
    %let MAX = &parm._MAX;
    %if &isBetween %then %let PARM = &parm._MIN;
  %end;

  %if not %symexist(&PARM) or (&isBetween and not %symexist(&MAX)) %then %do;
    %if &IS_EXPLICIT=0 %then %do;
		not &MATCHALL_CLAUSE
	%end;
	%else %do;
	    not 1=1
	%end;
  %end;
  %else %if "%qupcase(&&&PARM)" = "%qupcase(&MATCHALL)" %then %do;
    %if &IS_EXPLICIT=0 %then %do;
	    &MATCHALL_CLAUSE
	%end;
	%else %do;
	    1=1
	%end;	
  %end;
  %else %if (not %symexist(&PARM._count)) or &isBetween %then %do;
    %let isEmpty = ("&&&PARM" = "");
    %if (&isEqual AND &isEmpty AND &isString) %then
       &COLUMN is null;
    %else %if (&isNotEqual AND &isEmpty AND &isString) %then
       &COLUMN is not null;
    %else %do;
	   %if &IS_EXPLICIT=0 %then %do;
           &COLUMN &OPERATOR %unquote(&q1)&&&PARM%unquote(&q2)
	   %end;
	   %else %do;
	       &COLUMN &OPERATOR %unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2))
	   %end;
       %if &isBetween %then 
          AND %unquote(&q1)&&&MAX%unquote(&q2);
    %end;
  %end;
  %else 
  %do;
	%local emptyList;
  	%let emptyList = %symexist(&PARM._count);
  	%if &emptyList %then %let emptyList = &&&PARM._count = 0;
	%if (&emptyList) %then
	%do;
		%if (&isNotin) %then
		   1;
		%else
			0;
	%end;
	%else %if (&&&PARM._count = 1) %then 
    %do;
      %let isEmpty = ("&&&PARM" = "");
      %if (&isIn AND &isEmpty AND &isString) %then
        &COLUMN is null;
      %else %if (&isNotin AND &isEmpty AND &isString) %then
        &COLUMN is not null;
      %else %do;
	    %if &IS_EXPLICIT=0 %then %do;
            &COLUMN &OPERATOR (%unquote(&q1)&&&PARM%unquote(&q2))
	    %end;
		%else %do;
		    &COLUMN &OPERATOR (%unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2)))
		%end;
	  %end;
    %end;
    %else 
    %do;
       %local addIsNull addIsNotNull addComma;
       %let addIsNull = %eval(0);
       %let addIsNotNull = %eval(0);
       %let addComma = %eval(0);
       (&COLUMN &OPERATOR ( 
       %do i=1 %to &&&PARM._count; 
          %let isEmpty = ("&&&PARM&i" = "");
          %if (&isString AND &isEmpty AND (&isIn OR &isNotIn)) %then
          %do;
             %if (&isIn) %then %let addIsNull = 1;
             %else %let addIsNotNull = 1;
          %end;
          %else
          %do;		     
            %if &addComma %then %do;,%end;
			%if &IS_EXPLICIT=0 %then %do;
                %unquote(&q1)&&&PARM&i%unquote(&q2) 
			%end;
			%else %do;
			    %unquote(%nrstr(&sq1))&&&PARM&i%unquote(%nrstr(&sq2)) 
			%end;
            %let addComma = %eval(1);
          %end;
       %end;) 
       %if &addIsNull %then OR &COLUMN is null;
       %else %if &addIsNotNull %then AND &COLUMN is not null;
       %do;)
       %end;
    %end;
  %end;
%mend;

/* save the current settings of XPIXELS and YPIXELS */
/* so that they can be restored later               */
%macro _sas_pushchartsize(new_xsize, new_ysize);
	%global _savedxpixels _savedypixels;
	options nonotes;
	proc sql noprint;
	select setting into :_savedxpixels
	from sashelp.vgopt
	where optname eq "XPIXELS";
	select setting into :_savedypixels
	from sashelp.vgopt
	where optname eq "YPIXELS";
	quit;
	options notes;
	GOPTIONS XPIXELS=&new_xsize YPIXELS=&new_ysize;
%mend;

/* restore the previous values for XPIXELS and YPIXELS */
%macro _sas_popchartsize;
	%if %symexist(_savedxpixels) %then %do;
		GOPTIONS XPIXELS=&_savedxpixels YPIXELS=&_savedypixels;
		%symdel _savedxpixels / nowarn;
		%symdel _savedypixels / nowarn;
	%end;
%mend;

/* ---------------------------------- */
/* MACRO: enterpriseguide             */
/* PURPOSE: define a macro variable   */
/*   that contains the file system    */
/*   path of the WORK library on the  */
/*   server.  Note that different     */
/*   logic is needed depending on the */
/*   server type.                     */
/* ---------------------------------- */
%macro enterpriseguide;
%global sasworklocation;
%local tempdsn unique_dsn path;

%if &sysscp=OS %then %do; /* MVS Server */
	%if %sysfunc(getoption(filesystem))=MVS %then %do;
        /* By default, physical file name will be considered a classic MVS data set. */
	    /* Construct dsn that will be unique for each concurrent session under a particular account: */
		filename egtemp '&egtemp' disp=(new,delete); /* create a temporary data set */
 		%let tempdsn=%sysfunc(pathname(egtemp)); /* get dsn */
		filename egtemp clear; /* get rid of data set - we only wanted its name */
		%let unique_dsn=".EGTEMP.%substr(&tempdsn, 1, 16).PDSE"; 
		filename egtmpdir &unique_dsn
			disp=(new,delete,delete) space=(cyl,(5,5,50))
			dsorg=po dsntype=library recfm=vb
			lrecl=8000 blksize=8004 ;
		options fileext=ignore ;
	%end; 
 	%else %do; 
        /* 
		By default, physical file name will be considered an HFS 
		(hierarchical file system) file. 
		*/
		%if "%sysfunc(getoption(filetempdir))"="" %then %do;
			filename egtmpdir '/tmp';
		%end;
		%else %do;
			filename egtmpdir "%sysfunc(getoption(filetempdir))";
		%end;
	%end; 
	%let path=%sysfunc(pathname(egtmpdir));
    %let sasworklocation=%sysfunc(quote(&path));  
%end; /* MVS Server */
%else %do;
	%let sasworklocation = "%sysfunc(getoption(work))/";
%end;
%if &sysscp=VMS_AXP %then %do; /* Alpha VMS server */
	%let sasworklocation = "%sysfunc(getoption(work))";                         
%end;
%if &sysscp=CMS %then %do; 
	%let path = %sysfunc(getoption(work));                         
	%let sasworklocation = "%substr(&path, %index(&path,%str( )))";
%end;
%mend enterpriseguide;

%enterpriseguide

ODS PROCTITLE;
OPTIONS DEV=ACTIVEX;
GOPTIONS XPIXELS=0 YPIXELS=0;
FILENAME EGRTFX TEMP;
ODS RTF(ID=EGRTFX) FILE=EGRTFX
    ENCODING='utf-8'
    STYLE=Rtf
    NOGTITLE
    NOGFOOTNOTE
;
FILENAME EGSRX TEMP;
ODS tagsets.sasreport13(ID=EGSRX) FILE=EGSRX
    STYLE=HtmlBlue
    STYLESHEET=(URL="file:///C:/Program%20Files/SASHome/SASEnterpriseGuide/6.1/Styles/HtmlBlue.css")
    NOGTITLE
    NOGFOOTNOTE
    GPATH=&sasworklocation
    ENCODING=UTF8
    options(rolap="on")
;

/*   START OF NODE: Assign Project Library (JWOODATA)   */
%LET _CLIENTTASKLABEL='Assign Project Library (JWOODATA)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
LIBNAME JWOODATA BASE "P:\QAC\qac200\students\jwoo" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data and Observ.   */
%LET _CLIENTTASKLABEL='Data and Observ.';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(JWOODATA.MEPS_FULLYR_2012_SUBSET);

PROC SQL;
   CREATE TABLE JWOODATA.MEPS_FULLYR_2012_SUBSET(label="MEPS_FULLYR_2012_SUBSET") AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.UNEMP12X, 
          t1.UNEIMP12, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADL3MO42, 
          t1.ADLANG42, 
          t1.ADLHLP42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.PHQ242, 
          t1.HPRAP12, 
          t1.HPRAU12, 
          t1.HPRDE12, 
          t1.HPRFE12, 
          t1.HPRJA12, 
          t1.HPRJL12, 
          t1.HPRJU12, 
          t1.HPRMA12, 
          t1.HPRMY12, 
          t1.HPRNO12, 
          t1.HPROC12, 
          t1.HPRSE12, 
          t1.MDDLPR42, 
          t1.MDDLRS42, 
          t1.MDUNAB42, 
          t1.MDUNPR42, 
          t1.MDUNRS42, 
          t1.MDDLAY42, 
          t1.DNUNAB42, 
          t1.DNUNPR42, 
          t1.DNUNRS42, 
          t1.DNDLAY42, 
          t1.DNDLPR42, 
          t1.DNDLRS42, 
          t1.RTHLTH31, 
          t1.RTHLTH42, 
          t1.RTHLTH53, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.ADHDADDX, 
          t1.ADHDAGED, 
          t1.DSDIA53, 
          t1.MIAGED, 
          t1.MIDX, 
          t1.HIBPAGED, 
          t1.HIBPDX, 
          t1.LEUKAGED, 
          t1.LEUKREMS, 
          t1.LUNGAGED, 
          t1.LUNGREMS, 
          t1.LYMPAGED, 
          t1.LYMPREMS, 
          t1.MELAAGED, 
          t1.MELAREMS, 
          t1.OTHRAGED, 
          t1.OHRTAGED, 
          t1.OHRTDX, 
          t1.SKDKAGED, 
          t1.SKDKREMS, 
          t1.SKNMAGED, 
          t1.STRKAGED, 
          t1.STRKDX, 
          t1.THRTAGED, 
          t1.THRTREMS, 
          t1.THYRAGED, 
          t1.THYRREMS, 
          t1.AIDHLP31, 
          t1.AIDHLP53, 
          t1.SSECP12X, 
          t1.BUSIMP12, 
          t1.BUSNP12X, 
          t1.BRAIAGED, 
          t1.BRAIREMS, 
          t1.BRSTAGED, 
          t1.BRSTEX53, 
          t1.BRSTREMS, 
          t1.CABLADDR, 
          t1.CABRAIN, 
          t1.CABREAST, 
          t1.CACERVIX, 
          t1.CACOLON, 
          t1.CALEUKEM, 
          t1.CALUNG, 
          t1.CALYMPH, 
          t1.CAMELANO, 
          t1.CANCERDX, 
          t1.CAOTHER, 
          t1.CAPROSTA, 
          t1.CARECO42, 
          t1.CASHP12X, 
          t1.CASKINDK, 
          t1.CASKINNM, 
          t1.CATHROAT, 
          t1.CATHYROD, 
          t1.CERVAGED, 
          t1.CERVREMS, 
          t1.EDRECODE
      FROM EC100005.meps_fullyr_2012 t1
      WHERE t1.AGE12X >= 18
      ORDER BY t1.DUPERSID;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: SAS Program Code new   */
%LET SYSLAST=JWOODATA.MEPS_FULLYR_2012_SUBSET;
%LET _CLIENTTASKLABEL='SAS Program Code new';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';
%LET _SASPROGRAMFILE='P:\QAC\qac200\students\jwoo\SAS Program Code new.sas';

GOPTIONS ACCESSIBLE;

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Wednesday, January 07, 2015 at 4:38:21 PM
   By task: Data Set Attributes1

   Input Data: Local:JWOODATA.MEPS_FULLYR_2012_SUBSET
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsForMEPS_FULLYR_2012_);
TITLE "Data set attributes for subset data set";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";


PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=JWOODATA.MEPS_FULLYR_2012_SUBSET ;

RUN;



GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;


/*   START OF NODE: Recode Variables   */
%LET _CLIENTTASKLABEL='Recode Variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_FULLYR_2012_SUBSET_MANAGED);

PROC SQL;
   CREATE TABLE WORK."MEPS_FULLYR_2012_SUBSET_MANAGED"n AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EDRECODE, 
          t1.UNEMP12X, 
          t1.UNEIMP12, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADL3MO42, 
          t1.ADLANG42, 
          t1.ADLHLP42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.PHQ242, 
          t1.HPRAP12, 
          t1.HPRAU12, 
          t1.HPRDE12, 
          t1.HPRFE12, 
          t1.HPRJA12, 
          t1.HPRJL12, 
          t1.HPRJU12, 
          t1.HPRMA12, 
          t1.HPRMY12, 
          t1.HPRNO12, 
          t1.HPROC12, 
          t1.HPRSE12, 
          t1.MDDLPR42, 
          t1.MDDLRS42, 
          t1.MDUNAB42, 
          t1.MDUNPR42, 
          t1.MDUNRS42, 
          t1.MDDLAY42, 
          t1.DNUNAB42, 
          t1.DNUNPR42, 
          t1.DNUNRS42, 
          t1.DNDLAY42, 
          t1.DNDLPR42, 
          t1.DNDLRS42, 
          t1.RTHLTH31, 
          t1.RTHLTH42, 
          t1.RTHLTH53, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.ADHDADDX, 
          t1.ADHDAGED, 
          t1.DSDIA53, 
          t1.MIAGED, 
          t1.MIDX, 
          t1.HIBPAGED, 
          t1.HIBPDX, 
          t1.LEUKAGED, 
          t1.LEUKREMS, 
          t1.LUNGAGED, 
          t1.LUNGREMS, 
          t1.LYMPAGED, 
          t1.LYMPREMS, 
          t1.MELAAGED, 
          t1.MELAREMS, 
          t1.OTHRAGED, 
          t1.OHRTAGED, 
          t1.OHRTDX, 
          t1.SKDKAGED, 
          t1.SKDKREMS, 
          t1.SKNMAGED, 
          t1.STRKAGED, 
          t1.STRKDX, 
          t1.THRTAGED, 
          t1.THRTREMS, 
          t1.THYRAGED, 
          t1.THYRREMS, 
          t1.AIDHLP31, 
          t1.AIDHLP53, 
          t1.SSECP12X, 
          t1.BUSIMP12, 
          t1.BUSNP12X, 
          t1.BRAIAGED, 
          t1.BRAIREMS, 
          t1.BRSTAGED, 
          t1.BRSTEX53, 
          t1.BRSTREMS, 
          t1.CABLADDR, 
          t1.CABRAIN, 
          t1.CABREAST, 
          t1.CACERVIX, 
          t1.CACOLON, 
          t1.CALEUKEM, 
          t1.CALUNG, 
          t1.CALYMPH, 
          t1.CAMELANO, 
          t1.CANCERDX, 
          t1.CAOTHER, 
          t1.CAPROSTA, 
          t1.CARECO42, 
          t1.CASHP12X, 
          t1.CASKINDK, 
          t1.CASKINNM, 
          t1.CATHROAT, 
          t1.CATHYROD, 
          t1.CERVAGED, 
          t1.CERVREMS, 
          /* SAQ_GENH */
            (CASE 
               WHEN -1 = t1.ADGENH42 THEN .
               WHEN -8 = t1.ADGENH42 THEN .
               WHEN -9 = t1.ADGENH42 THEN .
               ELSE t1.ADGENH42
            END) LABEL="SAQ General Health (recoded missing)" AS SAQ_GENH, 
          /* SAQ_HEALIMACT */
            (CASE 
               WHEN -1 = t1.ADDAYA42 THEN .
               WHEN -9 = t1.ADDAYA42 THEN .
               ELSE t1.ADDAYA42
            END) LABEL="Health Limiting Activities" AS SAQ_HEALIMACT, 
          /* SAQ_HEALIMSTRS */
            (CASE 
               WHEN -1 = t1.ADCLIM42 THEN .
               WHEN -8 = t1.ADCLIM42 THEN .
               WHEN -9 = t1.ADCLIM42 THEN .
               ELSE t1.ADCLIM42
            END) LABEL="Health Limits Climbing Stairs" AS SAQ_HEALIMSTRS, 
          /* SAQ_ALPHYS */
            (CASE 
               WHEN -1 = t1.ADPALS42 THEN .
               WHEN -9 = t1.ADPALS42 THEN .
               ELSE t1.ADPALS42
            END) LABEL="Accompany less because Phys PBRS" AS SAQ_ALPHYS, 
          /* SAQ_WORKLIM */
            (CASE 
               WHEN -1 = t1.ADPWLM42 THEN .
               WHEN -9 = t1.ADPWLM42 THEN .
               ELSE t1.ADPWLM42
            END) LABEL="Work Limits because of physical problems" AS SAQ_WORKLIM, 
          /* SAQ_ALMENT */
            (CASE 
               WHEN -1 = t1.ADMALS42 THEN .
               WHEN -9 = t1.ADMALS42 THEN .
               ELSE t1.ADMALS42
            END) LABEL="Accmp less because of Mental Problems" AS SAQ_ALMENT, 
          /* SAQ_WLMENT */
            (CASE 
               WHEN -1 = t1.ADMWLM42 THEN .
               WHEN -7 = t1.ADMWLM42 THEN .
               WHEN -8 = t1.ADMWLM42 THEN .
               WHEN -9 = t1.ADMWLM42 THEN .
               ELSE t1.ADMWLM42
            END) LABEL="Work Limit because of Mental Problems" AS SAQ_WLMENT, 
          /* SAQ_FELTPEACE */
            (CASE 
               WHEN -1 = t1.ADCAPE42 THEN .
               WHEN -8 = t1.ADCAPE42 THEN .
               WHEN -9 = t1.ADCAPE42 THEN .
               ELSE t1.ADCAPE42
            END) LABEL="Felt peace and calm 4 weeks" AS SAQ_FELTPEACE, 
          /* SAQ_PAINLIM */
            (CASE 
               WHEN -1 = t1.ADPAIN42 THEN .
               WHEN -9 = t1.ADPAIN42 THEN .
               ELSE t1.ADPAIN42
            END) LABEL="Pain limits normal work" AS SAQ_PAINLIM, 
          /* SAQ_ENERGY */
            (CASE 
               WHEN -1 = t1.ADNRGY42 THEN .
               WHEN -9 = t1.ADNRGY42 THEN .
               ELSE t1.ADNRGY42
            END) LABEL="Had a lot of energy 4 weeks" AS SAQ_ENERGY, 
          /* SAQ_DOWNHEART */
            (CASE 
               WHEN -1 = t1.ADDOWN42 THEN .
               WHEN -8 = t1.ADDOWN42 THEN .
               WHEN -9 = t1.ADDOWN42 THEN .
               ELSE t1.ADDOWN42
            END) LABEL="Felt downhearted/depressed" AS SAQ_DOWNHEART, 
          /* SAQ_SOCACT */
            (CASE 
               WHEN -1 = t1.ADSOCA42 THEN .
               WHEN -9 = t1.ADSOCA42 THEN .
               ELSE t1.ADSOCA42
            END) LABEL="Health stops social activity 4 weeks" AS SAQ_SOCACT, 
          /* Marital_Status */
            (CASE 
               WHEN -7 = t1.MARRY12X THEN .
               WHEN -9 = t1.MARRY12X THEN .
               ELSE t1.MARRY12X
            END) LABEL="Marital Status" AS Marital_Status, 
          /* Education_Level */
            (CASE 
               WHEN -1 = t1.EDUYRDEG THEN .
               WHEN -7 = t1.EDUYRDEG THEN .
               WHEN -8 = t1.EDUYRDEG THEN .
               WHEN -9 = t1.EDUYRDEG THEN .
               ELSE t1.EDUYRDEG
            END) LABEL="Highest Degree/Level of Education" AS Education_Level, 
          /* GOT_CARE */
            (CASE 
               WHEN -1 = t1.ADILWW42 THEN .
               WHEN -9 = t1.ADILWW42 THEN .
               ELSE t1.ADILWW42
            END) LABEL="Got care when needed ill/injured" AS GOT_CARE, 
          /* NUMB_VISIT_MED */
            (CASE 
               WHEN -1 = t1.ADAPPT42 THEN .
               WHEN -8 = t1.ADAPPT42 THEN .
               WHEN -9 = t1.ADAPPT42 THEN .
               ELSE t1.ADAPPT42
            END) LABEL="Number of visits to medical officer for care" AS NUMB_VISIT_MED, 
          /* EASY_MED_CARE */
            (CASE 
               WHEN -1 = t1.ADEGMC42 THEN .
               WHEN -9 = t1.ADEGMC42 THEN .
               ELSE t1.ADEGMC42
            END) LABEL="Easy getting medical care" AS EASY_MED_CARE, 
          /* HEALTH_CARE_RATING */
            (CASE 
               WHEN -1 = t1.ADHECR42 THEN .
               WHEN -9 = t1.ADHECR42 THEN .
               ELSE t1.ADHECR42
            END) LABEL="Rating of health care" AS HEALTH_CARE_RATING, 
          /* SMOKING */
            (CASE 
               WHEN -1 = t1.ADSMOK42 THEN .
               WHEN -9 = t1.ADSMOK42 THEN .
               ELSE t1.ADSMOK42
            END) LABEL="Currently smoking" AS SMOKING, 
          /* HEALTH_STAT1 */
            (CASE 
               WHEN -1 = t1.RTHLTH31 THEN .
               WHEN -7 = t1.RTHLTH31 THEN .
               WHEN -8 = t1.RTHLTH31 THEN .
               WHEN -9 = t1.RTHLTH31 THEN .
               ELSE t1.RTHLTH31
            END) LABEL="Percieved Health Status 3/1" AS HEALTH_STAT1, 
          /* HEALTH_STAT2 */
            (CASE 
               WHEN -1 = t1.RTHLTH42 THEN .
               WHEN -7 = t1.RTHLTH42 THEN .
               WHEN -8 = t1.RTHLTH42 THEN .
               WHEN -9 = t1.RTHLTH42 THEN .
               ELSE t1.RTHLTH42
            END) LABEL="Percieved Health Status 4/2" AS HEALTH_STAT2, 
          /* HEALTH_STAT3 */
            (CASE 
               WHEN -1 = t1.RTHLTH53 THEN .
               WHEN -7 = t1.RTHLTH53 THEN .
               WHEN -8 = t1.RTHLTH53 THEN .
               WHEN -9 = t1.RTHLTH53 THEN .
               ELSE t1.RTHLTH53
            END) LABEL="Percieved health status 5/3" AS HEALTH_STAT3, 
          /* FEEL_NERV */
            (CASE 
               WHEN -1 = t1.ADNERV42 THEN .
               WHEN -7 = t1.ADNERV42 THEN .
               WHEN -8 = t1.ADNERV42 THEN .
               WHEN -9 = t1.ADNERV42 THEN .
               ELSE t1.ADNERV42
            END) LABEL="How often respondent felt nervous 30 days" AS FEEL_NERV, 
          /* FEEL_WORTHLESS */
            (CASE 
               WHEN -1 = t1.ADWRTH42 THEN .
               WHEN -7 = t1.ADWRTH42 THEN .
               WHEN -8 = t1.ADWRTH42 THEN .
               WHEN -9 = t1.ADWRTH42 THEN .
               ELSE t1.ADWRTH42
            END) LABEL="How often respondent felt nervous" AS FEEL_WORTHLESS, 
          /* FEEL_RESTLESS */
            (CASE 
               WHEN -1 = t1.ADREST42 THEN .
               WHEN -7 = t1.ADREST42 THEN .
               WHEN -8 = t1.ADREST42 THEN .
               WHEN -9 = t1.ADREST42 THEN .
               ELSE t1.ADREST42
            END) LABEL="How often the respondent felt restless" AS FEEL_RESTLESS
      FROM JWOODATA.MEPS_FULLYR_2012_SUBSET t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Reverse Code Variables   */
%LET _CLIENTTASKLABEL='Reverse Code Variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_FULLYR_2012_REVERSE);

PROC SQL;
   CREATE TABLE WORK."MEPS_FULLYR_2012_REVERSE"n(label="QUERY_FOR_MEPS_FULLYR_2012_RE") AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EDRECODE, 
          t1.UNEMP12X, 
          t1.UNEIMP12, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADL3MO42, 
          t1.ADLANG42, 
          t1.ADLHLP42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.PHQ242, 
          t1.HPRAP12, 
          t1.HPRAU12, 
          t1.HPRDE12, 
          t1.HPRFE12, 
          t1.HPRJA12, 
          t1.HPRJL12, 
          t1.HPRJU12, 
          t1.HPRMA12, 
          t1.HPRMY12, 
          t1.HPRNO12, 
          t1.HPROC12, 
          t1.HPRSE12, 
          t1.MDDLPR42, 
          t1.MDDLRS42, 
          t1.MDUNAB42, 
          t1.MDUNPR42, 
          t1.MDUNRS42, 
          t1.MDDLAY42, 
          t1.DNUNAB42, 
          t1.DNUNPR42, 
          t1.DNUNRS42, 
          t1.DNDLAY42, 
          t1.DNDLPR42, 
          t1.DNDLRS42, 
          t1.RTHLTH31, 
          t1.RTHLTH42, 
          t1.RTHLTH53, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.ADHDADDX, 
          t1.ADHDAGED, 
          t1.DSDIA53, 
          t1.MIAGED, 
          t1.MIDX, 
          t1.HIBPAGED, 
          t1.HIBPDX, 
          t1.LEUKAGED, 
          t1.LEUKREMS, 
          t1.LUNGAGED, 
          t1.LUNGREMS, 
          t1.LYMPAGED, 
          t1.LYMPREMS, 
          t1.MELAAGED, 
          t1.MELAREMS, 
          t1.OTHRAGED, 
          t1.OHRTAGED, 
          t1.OHRTDX, 
          t1.SKDKAGED, 
          t1.SKDKREMS, 
          t1.SKNMAGED, 
          t1.STRKAGED, 
          t1.STRKDX, 
          t1.THRTAGED, 
          t1.THRTREMS, 
          t1.THYRAGED, 
          t1.THYRREMS, 
          t1.AIDHLP31, 
          t1.AIDHLP53, 
          t1.SSECP12X, 
          t1.BUSIMP12, 
          t1.BUSNP12X, 
          t1.BRAIAGED, 
          t1.BRAIREMS, 
          t1.BRSTAGED, 
          t1.BRSTEX53, 
          t1.BRSTREMS, 
          t1.CABLADDR, 
          t1.CABRAIN, 
          t1.CABREAST, 
          t1.CACERVIX, 
          t1.CACOLON, 
          t1.CALEUKEM, 
          t1.CALUNG, 
          t1.CALYMPH, 
          t1.CAMELANO, 
          t1.CANCERDX, 
          t1.CAOTHER, 
          t1.CAPROSTA, 
          t1.CARECO42, 
          t1.CASHP12X, 
          t1.CASKINDK, 
          t1.CASKINNM, 
          t1.CATHROAT, 
          t1.CATHYROD, 
          t1.CERVAGED, 
          t1.CERVREMS, 
          t1.SAQ_GENH, 
          t1.SAQ_HEALIMACT, 
          t1.SAQ_HEALIMSTRS, 
          t1.SAQ_ALPHYS, 
          t1.SAQ_WORKLIM, 
          t1.SAQ_ALMENT, 
          t1.SAQ_WLMENT, 
          t1.SAQ_FELTPEACE, 
          t1.SAQ_PAINLIM, 
          t1.SAQ_ENERGY, 
          t1.SAQ_DOWNHEART, 
          t1.SAQ_SOCACT, 
          t1.Marital_Status, 
          t1.Education_Level, 
          t1.GOT_CARE, 
          t1.NUMB_VISIT_MED, 
          t1.EASY_MED_CARE, 
          t1.HEALTH_CARE_RATING, 
          t1.SMOKING, 
          t1.HEALTH_STAT1, 
          t1.HEALTH_STAT2, 
          t1.HEALTH_STAT3, 
          t1.FEEL_NERV, 
          t1.FEEL_WORTHLESS, 
          t1.FEEL_RESTLESS, 
          /* SAQ_ENERGY_rev */
            (6-t1.SAQ_ENERGY) LABEL="Had a lot of energy recoded reverse" AS SAQ_ENERGY_rev, 
          /* SAQ_FELTPEACE_rev */
            (6-t1.SAQ_FELTPEACE) LABEL="SAQ felt calm/peace recoded reverse" AS SAQ_FELTPEACE_rev, 
          /* SAQ_GENH_rev */
            (6-t1.SAQ_GENH) LABEL="SAQ General Health recoded rev" AS SAQ_GENH_rev, 
          /* SAQ_PAINLIM_rev */
            (6-t1.SAQ_PAINLIM) LABEL="SAQ Pain limits normal work recoded reverse" AS SAQ_PAINLIM_rev
      FROM WORK.MEPS_FULLYR_2012_SUBSET_MANAGED t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: SUM variable SF-12   */
%LET _CLIENTTASKLABEL='SUM variable SF-12';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_FULLYR_2012_REVER_SUM);

PROC SQL;
   CREATE TABLE WORK."MEPS_FULLYR_2012_REVER_SUM"n AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EDRECODE, 
          t1.UNEMP12X, 
          t1.UNEIMP12, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADL3MO42, 
          t1.ADLANG42, 
          t1.ADLHLP42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.PHQ242, 
          t1.HPRAP12, 
          t1.HPRAU12, 
          t1.HPRDE12, 
          t1.HPRFE12, 
          t1.HPRJA12, 
          t1.HPRJL12, 
          t1.HPRJU12, 
          t1.HPRMA12, 
          t1.HPRMY12, 
          t1.HPRNO12, 
          t1.HPROC12, 
          t1.HPRSE12, 
          t1.MDDLPR42, 
          t1.MDDLRS42, 
          t1.MDUNAB42, 
          t1.MDUNPR42, 
          t1.MDUNRS42, 
          t1.MDDLAY42, 
          t1.DNUNAB42, 
          t1.DNUNPR42, 
          t1.DNUNRS42, 
          t1.DNDLAY42, 
          t1.DNDLPR42, 
          t1.DNDLRS42, 
          t1.RTHLTH31, 
          t1.RTHLTH42, 
          t1.RTHLTH53, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.ADHDADDX, 
          t1.ADHDAGED, 
          t1.DSDIA53, 
          t1.MIAGED, 
          t1.MIDX, 
          t1.HIBPAGED, 
          t1.HIBPDX, 
          t1.LEUKAGED, 
          t1.LEUKREMS, 
          t1.LUNGAGED, 
          t1.LUNGREMS, 
          t1.LYMPAGED, 
          t1.LYMPREMS, 
          t1.MELAAGED, 
          t1.MELAREMS, 
          t1.OTHRAGED, 
          t1.OHRTAGED, 
          t1.OHRTDX, 
          t1.SKDKAGED, 
          t1.SKDKREMS, 
          t1.SKNMAGED, 
          t1.STRKAGED, 
          t1.STRKDX, 
          t1.THRTAGED, 
          t1.THRTREMS, 
          t1.THYRAGED, 
          t1.THYRREMS, 
          t1.AIDHLP31, 
          t1.AIDHLP53, 
          t1.SSECP12X, 
          t1.BUSIMP12, 
          t1.BUSNP12X, 
          t1.BRAIAGED, 
          t1.BRAIREMS, 
          t1.BRSTAGED, 
          t1.BRSTEX53, 
          t1.BRSTREMS, 
          t1.CABLADDR, 
          t1.CABRAIN, 
          t1.CABREAST, 
          t1.CACERVIX, 
          t1.CACOLON, 
          t1.CALEUKEM, 
          t1.CALUNG, 
          t1.CALYMPH, 
          t1.CAMELANO, 
          t1.CANCERDX, 
          t1.CAOTHER, 
          t1.CAPROSTA, 
          t1.CARECO42, 
          t1.CASHP12X, 
          t1.CASKINDK, 
          t1.CASKINNM, 
          t1.CATHROAT, 
          t1.CATHYROD, 
          t1.CERVAGED, 
          t1.CERVREMS, 
          t1.SAQ_GENH, 
          t1.SAQ_HEALIMACT, 
          t1.SAQ_HEALIMSTRS, 
          t1.SAQ_ALPHYS, 
          t1.SAQ_WORKLIM, 
          t1.SAQ_ALMENT, 
          t1.SAQ_WLMENT, 
          t1.SAQ_FELTPEACE, 
          t1.SAQ_PAINLIM, 
          t1.SAQ_ENERGY, 
          t1.SAQ_DOWNHEART, 
          t1.SAQ_SOCACT, 
          t1.Marital_Status, 
          t1.Education_Level, 
          t1.GOT_CARE, 
          t1.NUMB_VISIT_MED, 
          t1.EASY_MED_CARE, 
          t1.HEALTH_CARE_RATING, 
          t1.SMOKING, 
          t1.HEALTH_STAT1, 
          t1.HEALTH_STAT2, 
          t1.HEALTH_STAT3, 
          t1.FEEL_NERV, 
          t1.FEEL_WORTHLESS, 
          t1.FEEL_RESTLESS, 
          t1.SAQ_ENERGY_rev, 
          t1.SAQ_FELTPEACE_rev, 
          t1.SAQ_GENH_rev, 
          t1.SAQ_PAINLIM_rev, 
          /* SUM_SAQ_SF12 */
            
            (t1.SAQ_ENERGY_rev+t1.SAQ_FELTPEACE_rev+t1.SAQ_GENH_rev+t1.SAQ_PAINLIM_rev+t1.SAQ_HEALIMACT+t1.SAQ_HEALIMSTRS+t1.SAQ_ALPHYS+t1.SAQ_WORKLIM+t1.SAQ_ALMENT+t1.SAQ_WLMENT+t1.SAQ_DOWNHEART+t1.SAQ_SOCACT) 
            LABEL="Sum Variable - Aggregate Overall Health Score from SF-12" AS SUM_SAQ_SF12
      FROM WORK.MEPS_FULLYR_2012_REVERSE t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: SUM Categorical for Sum1   */
%LET _CLIENTTASKLABEL='SUM Categorical for Sum1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_FULLYR_2012_SUMCATE);

PROC SQL;
   CREATE TABLE WORK."MEPS_FULLYR_2012_SUMCATE"n AS 
   SELECT /* Quartiles_Categorical */
            (CASE  
               WHEN t1.SUM_SAQ_SF12 >=2 and t1.SUM_SAQ_SF12 <= 41
               THEN 1
               WHEN t1.SUM_SAQ_SF12 >= 42  and t1.SUM_SAQ_SF12 <= 48
               THEN 2
               WHEN t1.SUM_SAQ_SF12 >= 49  and t1.SUM_SAQ_SF12 <= 52
               THEN 3
               WHEN t1.SUM_SAQ_SF12 >= 53  and t1.SUM_SAQ_SF12 <= 56
               THEN 4
            END) LABEL="Categorical variable from SF-12 Sum Variable" AS Quartiles_Categorical, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EDRECODE, 
          t1.UNEMP12X, 
          t1.UNEIMP12, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADL3MO42, 
          t1.ADLANG42, 
          t1.ADLHLP42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.PHQ242, 
          t1.HPRAP12, 
          t1.HPRAU12, 
          t1.HPRDE12, 
          t1.HPRFE12, 
          t1.HPRJA12, 
          t1.HPRJL12, 
          t1.HPRJU12, 
          t1.HPRMA12, 
          t1.HPRMY12, 
          t1.HPRNO12, 
          t1.HPROC12, 
          t1.HPRSE12, 
          t1.MDDLPR42, 
          t1.MDDLRS42, 
          t1.MDUNAB42, 
          t1.MDUNPR42, 
          t1.MDUNRS42, 
          t1.MDDLAY42, 
          t1.DNUNAB42, 
          t1.DNUNPR42, 
          t1.DNUNRS42, 
          t1.DNDLAY42, 
          t1.DNDLPR42, 
          t1.DNDLRS42, 
          t1.RTHLTH31, 
          t1.RTHLTH42, 
          t1.RTHLTH53, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.ADHDADDX, 
          t1.ADHDAGED, 
          t1.DSDIA53, 
          t1.MIAGED, 
          t1.MIDX, 
          t1.HIBPAGED, 
          t1.HIBPDX, 
          t1.LEUKAGED, 
          t1.LEUKREMS, 
          t1.LUNGAGED, 
          t1.LUNGREMS, 
          t1.LYMPAGED, 
          t1.LYMPREMS, 
          t1.MELAAGED, 
          t1.MELAREMS, 
          t1.OTHRAGED, 
          t1.OHRTAGED, 
          t1.OHRTDX, 
          t1.SKDKAGED, 
          t1.SKDKREMS, 
          t1.SKNMAGED, 
          t1.STRKAGED, 
          t1.STRKDX, 
          t1.THRTAGED, 
          t1.THRTREMS, 
          t1.THYRAGED, 
          t1.THYRREMS, 
          t1.AIDHLP31, 
          t1.AIDHLP53, 
          t1.SSECP12X, 
          t1.BUSIMP12, 
          t1.BUSNP12X, 
          t1.BRAIAGED, 
          t1.BRAIREMS, 
          t1.BRSTAGED, 
          t1.BRSTEX53, 
          t1.BRSTREMS, 
          t1.CABLADDR, 
          t1.CABRAIN, 
          t1.CABREAST, 
          t1.CACERVIX, 
          t1.CACOLON, 
          t1.CALEUKEM, 
          t1.CALUNG, 
          t1.CALYMPH, 
          t1.CAMELANO, 
          t1.CANCERDX, 
          t1.CAOTHER, 
          t1.CAPROSTA, 
          t1.CARECO42, 
          t1.CASHP12X, 
          t1.CASKINDK, 
          t1.CASKINNM, 
          t1.CATHROAT, 
          t1.CATHYROD, 
          t1.CERVAGED, 
          t1.CERVREMS, 
          t1.SAQ_GENH, 
          t1.SAQ_HEALIMACT, 
          t1.SAQ_HEALIMSTRS, 
          t1.SAQ_ALPHYS, 
          t1.SAQ_WORKLIM, 
          t1.SAQ_ALMENT, 
          t1.SAQ_WLMENT, 
          t1.SAQ_FELTPEACE, 
          t1.SAQ_PAINLIM, 
          t1.SAQ_ENERGY, 
          t1.SAQ_DOWNHEART, 
          t1.SAQ_SOCACT, 
          t1.Marital_Status, 
          t1.Education_Level, 
          t1.GOT_CARE, 
          t1.NUMB_VISIT_MED, 
          t1.EASY_MED_CARE, 
          t1.HEALTH_CARE_RATING, 
          t1.SMOKING, 
          t1.HEALTH_STAT1, 
          t1.HEALTH_STAT2, 
          t1.HEALTH_STAT3, 
          t1.FEEL_NERV, 
          t1.FEEL_WORTHLESS, 
          t1.FEEL_RESTLESS, 
          t1.SAQ_ENERGY_rev, 
          t1.SAQ_FELTPEACE_rev, 
          t1.SAQ_GENH_rev, 
          t1.SAQ_PAINLIM_rev, 
          t1.SUM_SAQ_SF12
      FROM WORK.MEPS_FULLYR_2012_REVER_SUM t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Miss var. Doc-Pat   */
%LET _CLIENTTASKLABEL='Miss var. Doc-Pat';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_FULLYR_2012_Miss);

PROC SQL;
   CREATE TABLE WORK."MEPS_FULLYR_2012_Miss"n AS 
   SELECT t1.Quartiles_Categorical, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EDRECODE, 
          t1.UNEMP12X, 
          t1.UNEIMP12, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADL3MO42, 
          t1.ADLANG42, 
          t1.ADLHLP42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.PHQ242, 
          t1.HPRAP12, 
          t1.HPRAU12, 
          t1.HPRDE12, 
          t1.HPRFE12, 
          t1.HPRJA12, 
          t1.HPRJL12, 
          t1.HPRJU12, 
          t1.HPRMA12, 
          t1.HPRMY12, 
          t1.HPRNO12, 
          t1.HPROC12, 
          t1.HPRSE12, 
          t1.MDDLPR42, 
          t1.MDDLRS42, 
          t1.MDUNAB42, 
          t1.MDUNPR42, 
          t1.MDUNRS42, 
          t1.MDDLAY42, 
          t1.DNUNAB42, 
          t1.DNUNPR42, 
          t1.DNUNRS42, 
          t1.DNDLAY42, 
          t1.DNDLPR42, 
          t1.DNDLRS42, 
          t1.RTHLTH31, 
          t1.RTHLTH42, 
          t1.RTHLTH53, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.ADHDADDX, 
          t1.ADHDAGED, 
          t1.DSDIA53, 
          t1.MIAGED, 
          t1.MIDX, 
          t1.HIBPAGED, 
          t1.HIBPDX, 
          t1.LEUKAGED, 
          t1.LEUKREMS, 
          t1.LUNGAGED, 
          t1.LUNGREMS, 
          t1.LYMPAGED, 
          t1.LYMPREMS, 
          t1.MELAAGED, 
          t1.MELAREMS, 
          t1.OTHRAGED, 
          t1.OHRTAGED, 
          t1.OHRTDX, 
          t1.SKDKAGED, 
          t1.SKDKREMS, 
          t1.SKNMAGED, 
          t1.STRKAGED, 
          t1.STRKDX, 
          t1.THRTAGED, 
          t1.THRTREMS, 
          t1.THYRAGED, 
          t1.THYRREMS, 
          t1.AIDHLP31, 
          t1.AIDHLP53, 
          t1.SSECP12X, 
          t1.BUSIMP12, 
          t1.BUSNP12X, 
          t1.BRAIAGED, 
          t1.BRAIREMS, 
          t1.BRSTAGED, 
          t1.BRSTEX53, 
          t1.BRSTREMS, 
          t1.CABLADDR, 
          t1.CABRAIN, 
          t1.CABREAST, 
          t1.CACERVIX, 
          t1.CACOLON, 
          t1.CALEUKEM, 
          t1.CALUNG, 
          t1.CALYMPH, 
          t1.CAMELANO, 
          t1.CANCERDX, 
          t1.CAOTHER, 
          t1.CAPROSTA, 
          t1.CARECO42, 
          t1.CASHP12X, 
          t1.CASKINDK, 
          t1.CASKINNM, 
          t1.CATHROAT, 
          t1.CATHYROD, 
          t1.CERVAGED, 
          t1.CERVREMS, 
          t1.SAQ_GENH, 
          t1.SAQ_HEALIMACT, 
          t1.SAQ_HEALIMSTRS, 
          t1.SAQ_ALPHYS, 
          t1.SAQ_WORKLIM, 
          t1.SAQ_ALMENT, 
          t1.SAQ_WLMENT, 
          t1.SAQ_FELTPEACE, 
          t1.SAQ_PAINLIM, 
          t1.SAQ_ENERGY, 
          t1.SAQ_DOWNHEART, 
          t1.SAQ_SOCACT, 
          t1.Marital_Status, 
          t1.Education_Level, 
          t1.GOT_CARE, 
          t1.NUMB_VISIT_MED, 
          t1.EASY_MED_CARE, 
          t1.HEALTH_CARE_RATING, 
          t1.SMOKING, 
          t1.HEALTH_STAT1, 
          t1.HEALTH_STAT2, 
          t1.HEALTH_STAT3, 
          t1.FEEL_NERV, 
          t1.FEEL_WORTHLESS, 
          t1.FEEL_RESTLESS, 
          t1.SAQ_ENERGY_rev, 
          t1.SAQ_FELTPEACE_rev, 
          t1.SAQ_GENH_rev, 
          t1.SAQ_PAINLIM_rev, 
          t1.SUM_SAQ_SF12, 
          /* SAQ2_DOCLIST */
            (CASE 
               WHEN -1 = t1.ADLIST42 THEN .
               WHEN -7 = t1.ADLIST42 THEN .
               WHEN -9 = t1.ADLIST42 THEN .
               ELSE t1.ADLIST42
            END) LABEL="Doctor listened to patient" AS SAQ2_DOCLIST, 
          /* SAQ2_DOCUND */
            (CASE 
               WHEN -1 = t1.ADEXPL42 THEN .
               WHEN -9 = t1.ADEXPL42 THEN .
               ELSE t1.ADEXPL42
            END) LABEL="Doctor explained it so understood" AS SAQ2_DOCUND, 
          /* SAQ2_DOCRESP */
            (CASE 
               WHEN -1 = t1.ADRESP42 THEN .
               WHEN -9 = t1.ADRESP42 THEN .
               ELSE t1.ADRESP42
            END) LABEL="Doctor Showed Respect" AS SAQ2_DOCRESP, 
          /* SAQ2_DOCTIME */
            (CASE 
               WHEN -1 = t1.ADPRTM42 THEN .
               WHEN -9 = t1.ADPRTM42 THEN .
               ELSE t1.ADPRTM42
            END) LABEL="Doctor spent enough time with patient" AS SAQ2_DOCTIME, 
          /* SAQ2_DOCSPEC */
            (CASE 
               WHEN -1 = t1.ADINST42 THEN .
               WHEN -8 = t1.ADINST42 THEN .
               WHEN -9 = t1.ADINST42 THEN .
               ELSE t1.ADINST42
            END) LABEL="Doctor gave specific instructions" AS SAQ2_DOCSPEC, 
          /* SAQ2_EZUN */
            (CASE 
               WHEN -1 = t1.ADEZUN42 THEN .
               WHEN -9 = t1.ADEZUN42 THEN .
               ELSE t1.ADEZUN42
            END) LABEL="Doctor given instruction was easy to understand" AS SAQ2_EZUN, 
          /* SAQ2_DOCFOLLOW */
            (CASE 
               WHEN -1 = t1.ADTLHW42 THEN .
               WHEN -8 = t1.ADTLHW42 THEN .
               WHEN -9 = t1.ADTLHW42 THEN .
               ELSE t1.ADTLHW42
            END) LABEL="Doctor asked patient to describe how to follow" AS SAQ2_DOCFOLLOW
      FROM WORK.MEPS_FULLYR_2012_SUMCATE t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Reverse Code Doc-Pat   */
%LET _CLIENTTASKLABEL='Reverse Code Doc-Pat';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_FULLYR_2012_missrev);

PROC SQL;
   CREATE TABLE WORK."MEPS_FULLYR_2012_missrev"n AS 
   SELECT t1.Quartiles_Categorical, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EDRECODE, 
          t1.UNEMP12X, 
          t1.UNEIMP12, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADL3MO42, 
          t1.ADLANG42, 
          t1.ADLHLP42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.PHQ242, 
          t1.HPRAP12, 
          t1.HPRAU12, 
          t1.HPRDE12, 
          t1.HPRFE12, 
          t1.HPRJA12, 
          t1.HPRJL12, 
          t1.HPRJU12, 
          t1.HPRMA12, 
          t1.HPRMY12, 
          t1.HPRNO12, 
          t1.HPROC12, 
          t1.HPRSE12, 
          t1.MDDLPR42, 
          t1.MDDLRS42, 
          t1.MDUNAB42, 
          t1.MDUNPR42, 
          t1.MDUNRS42, 
          t1.MDDLAY42, 
          t1.DNUNAB42, 
          t1.DNUNPR42, 
          t1.DNUNRS42, 
          t1.DNDLAY42, 
          t1.DNDLPR42, 
          t1.DNDLRS42, 
          t1.RTHLTH31, 
          t1.RTHLTH42, 
          t1.RTHLTH53, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.ADHDADDX, 
          t1.ADHDAGED, 
          t1.DSDIA53, 
          t1.MIAGED, 
          t1.MIDX, 
          t1.HIBPAGED, 
          t1.HIBPDX, 
          t1.LEUKAGED, 
          t1.LEUKREMS, 
          t1.LUNGAGED, 
          t1.LUNGREMS, 
          t1.LYMPAGED, 
          t1.LYMPREMS, 
          t1.MELAAGED, 
          t1.MELAREMS, 
          t1.OTHRAGED, 
          t1.OHRTAGED, 
          t1.OHRTDX, 
          t1.SKDKAGED, 
          t1.SKDKREMS, 
          t1.SKNMAGED, 
          t1.STRKAGED, 
          t1.STRKDX, 
          t1.THRTAGED, 
          t1.THRTREMS, 
          t1.THYRAGED, 
          t1.THYRREMS, 
          t1.AIDHLP31, 
          t1.AIDHLP53, 
          t1.SSECP12X, 
          t1.BUSIMP12, 
          t1.BUSNP12X, 
          t1.BRAIAGED, 
          t1.BRAIREMS, 
          t1.BRSTAGED, 
          t1.BRSTEX53, 
          t1.BRSTREMS, 
          t1.CABLADDR, 
          t1.CABRAIN, 
          t1.CABREAST, 
          t1.CACERVIX, 
          t1.CACOLON, 
          t1.CALEUKEM, 
          t1.CALUNG, 
          t1.CALYMPH, 
          t1.CAMELANO, 
          t1.CANCERDX, 
          t1.CAOTHER, 
          t1.CAPROSTA, 
          t1.CARECO42, 
          t1.CASHP12X, 
          t1.CASKINDK, 
          t1.CASKINNM, 
          t1.CATHROAT, 
          t1.CATHYROD, 
          t1.CERVAGED, 
          t1.CERVREMS, 
          t1.SAQ_GENH, 
          t1.SAQ_HEALIMACT, 
          t1.SAQ_HEALIMSTRS, 
          t1.SAQ_ALPHYS, 
          t1.SAQ_WORKLIM, 
          t1.SAQ_ALMENT, 
          t1.SAQ_WLMENT, 
          t1.SAQ_FELTPEACE, 
          t1.SAQ_PAINLIM, 
          t1.SAQ_ENERGY, 
          t1.SAQ_DOWNHEART, 
          t1.SAQ_SOCACT, 
          t1.Marital_Status, 
          t1.Education_Level, 
          t1.GOT_CARE, 
          t1.NUMB_VISIT_MED, 
          t1.EASY_MED_CARE, 
          t1.HEALTH_CARE_RATING, 
          t1.SMOKING, 
          t1.HEALTH_STAT1, 
          t1.HEALTH_STAT2, 
          t1.HEALTH_STAT3, 
          t1.FEEL_NERV, 
          t1.FEEL_WORTHLESS, 
          t1.FEEL_RESTLESS, 
          t1.SAQ_ENERGY_rev, 
          t1.SAQ_FELTPEACE_rev, 
          t1.SAQ_GENH_rev, 
          t1.SAQ_PAINLIM_rev, 
          t1.SUM_SAQ_SF12, 
          t1.SAQ2_DOCLIST, 
          t1.SAQ2_DOCUND, 
          t1.SAQ2_DOCRESP, 
          t1.SAQ2_DOCTIME, 
          t1.SAQ2_DOCSPEC, 
          t1.SAQ2_EZUN, 
          t1.SAQ2_DOCFOLLOW, 
          /* SAQ2_DOCSPEC_rev */
            (3-t1.SAQ2_DOCSPEC) LABEL="Doctor gave specific instruction recode" AS SAQ2_DOCSPEC_rev
      FROM WORK.MEPS_FULLYR_2012_MISS t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: SUM Variable Doc Pat   */
%LET _CLIENTTASKLABEL='SUM Variable Doc Pat';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_FULLYR_2012_sum2);

PROC SQL;
   CREATE TABLE WORK."MEPS_FULLYR_2012_sum2"n AS 
   SELECT t1.Quartiles_Categorical, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EDRECODE, 
          t1.UNEMP12X, 
          t1.UNEIMP12, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADL3MO42, 
          t1.ADLANG42, 
          t1.ADLHLP42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.PHQ242, 
          t1.HPRAP12, 
          t1.HPRAU12, 
          t1.HPRDE12, 
          t1.HPRFE12, 
          t1.HPRJA12, 
          t1.HPRJL12, 
          t1.HPRJU12, 
          t1.HPRMA12, 
          t1.HPRMY12, 
          t1.HPRNO12, 
          t1.HPROC12, 
          t1.HPRSE12, 
          t1.MDDLPR42, 
          t1.MDDLRS42, 
          t1.MDUNAB42, 
          t1.MDUNPR42, 
          t1.MDUNRS42, 
          t1.MDDLAY42, 
          t1.DNUNAB42, 
          t1.DNUNPR42, 
          t1.DNUNRS42, 
          t1.DNDLAY42, 
          t1.DNDLPR42, 
          t1.DNDLRS42, 
          t1.RTHLTH31, 
          t1.RTHLTH42, 
          t1.RTHLTH53, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.ADHDADDX, 
          t1.ADHDAGED, 
          t1.DSDIA53, 
          t1.MIAGED, 
          t1.MIDX, 
          t1.HIBPAGED, 
          t1.HIBPDX, 
          t1.LEUKAGED, 
          t1.LEUKREMS, 
          t1.LUNGAGED, 
          t1.LUNGREMS, 
          t1.LYMPAGED, 
          t1.LYMPREMS, 
          t1.MELAAGED, 
          t1.MELAREMS, 
          t1.OTHRAGED, 
          t1.OHRTAGED, 
          t1.OHRTDX, 
          t1.SKDKAGED, 
          t1.SKDKREMS, 
          t1.SKNMAGED, 
          t1.STRKAGED, 
          t1.STRKDX, 
          t1.THRTAGED, 
          t1.THRTREMS, 
          t1.THYRAGED, 
          t1.THYRREMS, 
          t1.AIDHLP31, 
          t1.AIDHLP53, 
          t1.SSECP12X, 
          t1.BUSIMP12, 
          t1.BUSNP12X, 
          t1.BRAIAGED, 
          t1.BRAIREMS, 
          t1.BRSTAGED, 
          t1.BRSTEX53, 
          t1.BRSTREMS, 
          t1.CABLADDR, 
          t1.CABRAIN, 
          t1.CABREAST, 
          t1.CACERVIX, 
          t1.CACOLON, 
          t1.CALEUKEM, 
          t1.CALUNG, 
          t1.CALYMPH, 
          t1.CAMELANO, 
          t1.CANCERDX, 
          t1.CAOTHER, 
          t1.CAPROSTA, 
          t1.CARECO42, 
          t1.CASHP12X, 
          t1.CASKINDK, 
          t1.CASKINNM, 
          t1.CATHROAT, 
          t1.CATHYROD, 
          t1.CERVAGED, 
          t1.CERVREMS, 
          t1.SAQ_GENH, 
          t1.SAQ_HEALIMACT, 
          t1.SAQ_HEALIMSTRS, 
          t1.SAQ_ALPHYS, 
          t1.SAQ_WORKLIM, 
          t1.SAQ_ALMENT, 
          t1.SAQ_WLMENT, 
          t1.SAQ_FELTPEACE, 
          t1.SAQ_PAINLIM, 
          t1.SAQ_ENERGY, 
          t1.SAQ_DOWNHEART, 
          t1.SAQ_SOCACT, 
          t1.Marital_Status, 
          t1.Education_Level, 
          t1.GOT_CARE, 
          t1.NUMB_VISIT_MED, 
          t1.EASY_MED_CARE, 
          t1.HEALTH_CARE_RATING, 
          t1.SMOKING, 
          t1.HEALTH_STAT1, 
          t1.HEALTH_STAT2, 
          t1.HEALTH_STAT3, 
          t1.FEEL_NERV, 
          t1.FEEL_WORTHLESS, 
          t1.FEEL_RESTLESS, 
          t1.SAQ_ENERGY_rev, 
          t1.SAQ_FELTPEACE_rev, 
          t1.SAQ_GENH_rev, 
          t1.SAQ_PAINLIM_rev, 
          t1.SUM_SAQ_SF12, 
          t1.SAQ2_DOCLIST, 
          t1.SAQ2_DOCUND, 
          t1.SAQ2_DOCRESP, 
          t1.SAQ2_DOCTIME, 
          t1.SAQ2_DOCSPEC, 
          t1.SAQ2_EZUN, 
          t1.SAQ2_DOCFOLLOW, 
          t1.SAQ2_DOCSPEC_rev, 
          /* SAQ2_DocPat_SUM */
            
            (t1.SAQ2_DOCLIST+t1.SAQ2_DOCUND+t1.SAQ2_DOCRESP+t1.SAQ2_DOCTIME+t1.SAQ2_EZUN+t1.SAQ2_DOCSPEC_rev+t1.SAQ2_DOCFOLLOW) 
            LABEL="Sum variable for aggregate doctor patient score" AS SAQ2_DocPat_SUM
      FROM WORK.MEPS_FULLYR_2012_MISSREV t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Categorical for Sum2   */
%LET _CLIENTTASKLABEL='Categorical for Sum2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_FULLYR_2012_SUM2_cate);

PROC SQL;
   CREATE TABLE WORK."MEPS_FULLYR_2012_SUM2_cate"n AS 
   SELECT /* SAQ2_DocPatRel_sumcate */
            (CASE  
               WHEN  t1.SAQ2_DocPat_SUM >= 6 and t1.SAQ2_DocPat_SUM <= 17
               THEN 1
               WHEN  t1.SAQ2_DocPat_SUM >= 18  and t1.SAQ2_DocPat_SUM <= 21
               THEN 2
               WHEN t1.SAQ2_DocPat_SUM >= 22 and  t1.SAQ2_DocPat_SUM <= 24
               THEN 3
               WHEN t1.SAQ2_DocPat_SUM >= 25  and t1.SAQ2_DocPat_SUM <= 26
               THEN 4
            END) LABEL="Summary categorical variable for Doctor Patient Relationship Score" AS SAQ2_DocPatRel_sumcate, 
          t1.Quartiles_Categorical, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EDRECODE, 
          t1.UNEMP12X, 
          t1.UNEIMP12, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADL3MO42, 
          t1.ADLANG42, 
          t1.ADLHLP42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.PHQ242, 
          t1.HPRAP12, 
          t1.HPRAU12, 
          t1.HPRDE12, 
          t1.HPRFE12, 
          t1.HPRJA12, 
          t1.HPRJL12, 
          t1.HPRJU12, 
          t1.HPRMA12, 
          t1.HPRMY12, 
          t1.HPRNO12, 
          t1.HPROC12, 
          t1.HPRSE12, 
          t1.MDDLPR42, 
          t1.MDDLRS42, 
          t1.MDUNAB42, 
          t1.MDUNPR42, 
          t1.MDUNRS42, 
          t1.MDDLAY42, 
          t1.DNUNAB42, 
          t1.DNUNPR42, 
          t1.DNUNRS42, 
          t1.DNDLAY42, 
          t1.DNDLPR42, 
          t1.DNDLRS42, 
          t1.RTHLTH31, 
          t1.RTHLTH42, 
          t1.RTHLTH53, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.ADHDADDX, 
          t1.ADHDAGED, 
          t1.DSDIA53, 
          t1.MIAGED, 
          t1.MIDX, 
          t1.HIBPAGED, 
          t1.HIBPDX, 
          t1.LEUKAGED, 
          t1.LEUKREMS, 
          t1.LUNGAGED, 
          t1.LUNGREMS, 
          t1.LYMPAGED, 
          t1.LYMPREMS, 
          t1.MELAAGED, 
          t1.MELAREMS, 
          t1.OTHRAGED, 
          t1.OHRTAGED, 
          t1.OHRTDX, 
          t1.SKDKAGED, 
          t1.SKDKREMS, 
          t1.SKNMAGED, 
          t1.STRKAGED, 
          t1.STRKDX, 
          t1.THRTAGED, 
          t1.THRTREMS, 
          t1.THYRAGED, 
          t1.THYRREMS, 
          t1.AIDHLP31, 
          t1.AIDHLP53, 
          t1.SSECP12X, 
          t1.BUSIMP12, 
          t1.BUSNP12X, 
          t1.BRAIAGED, 
          t1.BRAIREMS, 
          t1.BRSTAGED, 
          t1.BRSTEX53, 
          t1.BRSTREMS, 
          t1.CABLADDR, 
          t1.CABRAIN, 
          t1.CABREAST, 
          t1.CACERVIX, 
          t1.CACOLON, 
          t1.CALEUKEM, 
          t1.CALUNG, 
          t1.CALYMPH, 
          t1.CAMELANO, 
          t1.CANCERDX, 
          t1.CAOTHER, 
          t1.CAPROSTA, 
          t1.CARECO42, 
          t1.CASHP12X, 
          t1.CASKINDK, 
          t1.CASKINNM, 
          t1.CATHROAT, 
          t1.CATHYROD, 
          t1.CERVAGED, 
          t1.CERVREMS, 
          t1.SAQ_GENH, 
          t1.SAQ_HEALIMACT, 
          t1.SAQ_HEALIMSTRS, 
          t1.SAQ_ALPHYS, 
          t1.SAQ_WORKLIM, 
          t1.SAQ_ALMENT, 
          t1.SAQ_WLMENT, 
          t1.SAQ_FELTPEACE, 
          t1.SAQ_PAINLIM, 
          t1.SAQ_ENERGY, 
          t1.SAQ_DOWNHEART, 
          t1.SAQ_SOCACT, 
          t1.Marital_Status, 
          t1.Education_Level, 
          t1.GOT_CARE, 
          t1.NUMB_VISIT_MED, 
          t1.EASY_MED_CARE, 
          t1.HEALTH_CARE_RATING, 
          t1.SMOKING, 
          t1.HEALTH_STAT1, 
          t1.HEALTH_STAT2, 
          t1.HEALTH_STAT3, 
          t1.FEEL_NERV, 
          t1.FEEL_WORTHLESS, 
          t1.FEEL_RESTLESS, 
          t1.SAQ_ENERGY_rev, 
          t1.SAQ_FELTPEACE_rev, 
          t1.SAQ_GENH_rev, 
          t1.SAQ_PAINLIM_rev, 
          t1.SUM_SAQ_SF12, 
          t1.SAQ2_DOCLIST, 
          t1.SAQ2_DOCUND, 
          t1.SAQ2_DOCRESP, 
          t1.SAQ2_DOCTIME, 
          t1.SAQ2_DOCSPEC, 
          t1.SAQ2_EZUN, 
          t1.SAQ2_DOCFOLLOW, 
          t1.SAQ2_DOCSPEC_rev, 
          t1.SAQ2_DocPat_SUM
      FROM WORK.MEPS_FULLYR_2012_SUM2 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Marry and Edurcode Missing   */
%LET _CLIENTTASKLABEL='Marry and Edurcode Missing';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_FULLYR_2012_Marry_Edu);

PROC SQL;
   CREATE TABLE WORK."MEPS_FULLYR_2012_Marry_Edu"n AS 
   SELECT t1.SAQ2_DocPatRel_sumcate, 
          t1.Quartiles_Categorical, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EDRECODE, 
          t1.UNEMP12X, 
          t1.UNEIMP12, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADL3MO42, 
          t1.ADLANG42, 
          t1.ADLHLP42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.PHQ242, 
          t1.HPRAP12, 
          t1.HPRAU12, 
          t1.HPRDE12, 
          t1.HPRFE12, 
          t1.HPRJA12, 
          t1.HPRJL12, 
          t1.HPRJU12, 
          t1.HPRMA12, 
          t1.HPRMY12, 
          t1.HPRNO12, 
          t1.HPROC12, 
          t1.HPRSE12, 
          t1.MDDLPR42, 
          t1.MDDLRS42, 
          t1.MDUNAB42, 
          t1.MDUNPR42, 
          t1.MDUNRS42, 
          t1.MDDLAY42, 
          t1.DNUNAB42, 
          t1.DNUNPR42, 
          t1.DNUNRS42, 
          t1.DNDLAY42, 
          t1.DNDLPR42, 
          t1.DNDLRS42, 
          t1.RTHLTH31, 
          t1.RTHLTH42, 
          t1.RTHLTH53, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.ADHDADDX, 
          t1.ADHDAGED, 
          t1.DSDIA53, 
          t1.MIAGED, 
          t1.MIDX, 
          t1.HIBPAGED, 
          t1.HIBPDX, 
          t1.LEUKAGED, 
          t1.LEUKREMS, 
          t1.LUNGAGED, 
          t1.LUNGREMS, 
          t1.LYMPAGED, 
          t1.LYMPREMS, 
          t1.MELAAGED, 
          t1.MELAREMS, 
          t1.OTHRAGED, 
          t1.OHRTAGED, 
          t1.OHRTDX, 
          t1.SKDKAGED, 
          t1.SKDKREMS, 
          t1.SKNMAGED, 
          t1.STRKAGED, 
          t1.STRKDX, 
          t1.THRTAGED, 
          t1.THRTREMS, 
          t1.THYRAGED, 
          t1.THYRREMS, 
          t1.AIDHLP31, 
          t1.AIDHLP53, 
          t1.SSECP12X, 
          t1.BUSIMP12, 
          t1.BUSNP12X, 
          t1.BRAIAGED, 
          t1.BRAIREMS, 
          t1.BRSTAGED, 
          t1.BRSTEX53, 
          t1.BRSTREMS, 
          t1.CABLADDR, 
          t1.CABRAIN, 
          t1.CABREAST, 
          t1.CACERVIX, 
          t1.CACOLON, 
          t1.CALEUKEM, 
          t1.CALUNG, 
          t1.CALYMPH, 
          t1.CAMELANO, 
          t1.CANCERDX, 
          t1.CAOTHER, 
          t1.CAPROSTA, 
          t1.CARECO42, 
          t1.CASHP12X, 
          t1.CASKINDK, 
          t1.CASKINNM, 
          t1.CATHROAT, 
          t1.CATHYROD, 
          t1.CERVAGED, 
          t1.CERVREMS, 
          t1.SAQ_GENH, 
          t1.SAQ_HEALIMACT, 
          t1.SAQ_HEALIMSTRS, 
          t1.SAQ_ALPHYS, 
          t1.SAQ_WORKLIM, 
          t1.SAQ_ALMENT, 
          t1.SAQ_WLMENT, 
          t1.SAQ_FELTPEACE, 
          t1.SAQ_PAINLIM, 
          t1.SAQ_ENERGY, 
          t1.SAQ_DOWNHEART, 
          t1.SAQ_SOCACT, 
          t1.Marital_Status, 
          t1.Education_Level, 
          t1.GOT_CARE, 
          t1.NUMB_VISIT_MED, 
          t1.EASY_MED_CARE, 
          t1.HEALTH_CARE_RATING, 
          t1.SMOKING, 
          t1.HEALTH_STAT1, 
          t1.HEALTH_STAT2, 
          t1.HEALTH_STAT3, 
          t1.FEEL_NERV, 
          t1.FEEL_WORTHLESS, 
          t1.FEEL_RESTLESS, 
          t1.SAQ_ENERGY_rev, 
          t1.SAQ_FELTPEACE_rev, 
          t1.SAQ_GENH_rev, 
          t1.SAQ_PAINLIM_rev, 
          t1.SUM_SAQ_SF12, 
          t1.SAQ2_DOCLIST, 
          t1.SAQ2_DOCUND, 
          t1.SAQ2_DOCRESP, 
          t1.SAQ2_DOCTIME, 
          t1.SAQ2_DOCSPEC, 
          t1.SAQ2_EZUN, 
          t1.SAQ2_DOCFOLLOW, 
          t1.SAQ2_DOCSPEC_rev, 
          t1.SAQ2_DocPat_SUM, 
          /* Marry12x_Recode */
            (CASE 
               WHEN -7 = t1.MARRY12X THEN .
               WHEN -9 = t1.MARRY12X THEN .
               ELSE t1.MARRY12X
            END) LABEL="Recoded Marry12x variable" AS Marry12x_Recode, 
          /* EDURecode_Missing */
            (CASE 
               WHEN -7 = t1.EDRECODE THEN .
               WHEN -8 = t1.EDRECODE THEN .
               WHEN -9 = t1.EDRECODE THEN .
               ELSE t1.EDRECODE
            END) LABEL="Eudcation recode missing" AS EDURecode_Missing
      FROM WORK.MEPS_FULLYR_2012_SUM2_CATE t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Marry Categorical variable   */
%LET _CLIENTTASKLABEL='Marry Categorical variable';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_FULLYR_2012_MARRY_cate);

PROC SQL;
   CREATE TABLE WORK."MEPS_FULLYR_2012_MARRY_cate"n AS 
   SELECT /* Marry12x_Recode_cate */
            (CASE  
               WHEN t1.Marry12x_Recode = 1
               THEN 2
               WHEN t1.Marry12x_Recode = 2 or t1.Marry12x_Recode = 3 or  t1.Marry12x_Recode = 4 or t1.Marry12x_Recode = 
            5
               THEN 1
            END) LABEL="Marry recoded categorical variable" AS Marry12x_Recode_cate, 
          t1.SAQ2_DocPatRel_sumcate, 
          t1.Quartiles_Categorical, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EDRECODE, 
          t1.UNEMP12X, 
          t1.UNEIMP12, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADL3MO42, 
          t1.ADLANG42, 
          t1.ADLHLP42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.PHQ242, 
          t1.HPRAP12, 
          t1.HPRAU12, 
          t1.HPRDE12, 
          t1.HPRFE12, 
          t1.HPRJA12, 
          t1.HPRJL12, 
          t1.HPRJU12, 
          t1.HPRMA12, 
          t1.HPRMY12, 
          t1.HPRNO12, 
          t1.HPROC12, 
          t1.HPRSE12, 
          t1.MDDLPR42, 
          t1.MDDLRS42, 
          t1.MDUNAB42, 
          t1.MDUNPR42, 
          t1.MDUNRS42, 
          t1.MDDLAY42, 
          t1.DNUNAB42, 
          t1.DNUNPR42, 
          t1.DNUNRS42, 
          t1.DNDLAY42, 
          t1.DNDLPR42, 
          t1.DNDLRS42, 
          t1.RTHLTH31, 
          t1.RTHLTH42, 
          t1.RTHLTH53, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.ADHDADDX, 
          t1.ADHDAGED, 
          t1.DSDIA53, 
          t1.MIAGED, 
          t1.MIDX, 
          t1.HIBPAGED, 
          t1.HIBPDX, 
          t1.LEUKAGED, 
          t1.LEUKREMS, 
          t1.LUNGAGED, 
          t1.LUNGREMS, 
          t1.LYMPAGED, 
          t1.LYMPREMS, 
          t1.MELAAGED, 
          t1.MELAREMS, 
          t1.OTHRAGED, 
          t1.OHRTAGED, 
          t1.OHRTDX, 
          t1.SKDKAGED, 
          t1.SKDKREMS, 
          t1.SKNMAGED, 
          t1.STRKAGED, 
          t1.STRKDX, 
          t1.THRTAGED, 
          t1.THRTREMS, 
          t1.THYRAGED, 
          t1.THYRREMS, 
          t1.AIDHLP31, 
          t1.AIDHLP53, 
          t1.SSECP12X, 
          t1.BUSIMP12, 
          t1.BUSNP12X, 
          t1.BRAIAGED, 
          t1.BRAIREMS, 
          t1.BRSTAGED, 
          t1.BRSTEX53, 
          t1.BRSTREMS, 
          t1.CABLADDR, 
          t1.CABRAIN, 
          t1.CABREAST, 
          t1.CACERVIX, 
          t1.CACOLON, 
          t1.CALEUKEM, 
          t1.CALUNG, 
          t1.CALYMPH, 
          t1.CAMELANO, 
          t1.CANCERDX, 
          t1.CAOTHER, 
          t1.CAPROSTA, 
          t1.CARECO42, 
          t1.CASHP12X, 
          t1.CASKINDK, 
          t1.CASKINNM, 
          t1.CATHROAT, 
          t1.CATHYROD, 
          t1.CERVAGED, 
          t1.CERVREMS, 
          t1.SAQ_GENH, 
          t1.SAQ_HEALIMACT, 
          t1.SAQ_HEALIMSTRS, 
          t1.SAQ_ALPHYS, 
          t1.SAQ_WORKLIM, 
          t1.SAQ_ALMENT, 
          t1.SAQ_WLMENT, 
          t1.SAQ_FELTPEACE, 
          t1.SAQ_PAINLIM, 
          t1.SAQ_ENERGY, 
          t1.SAQ_DOWNHEART, 
          t1.SAQ_SOCACT, 
          t1.Marital_Status, 
          t1.Education_Level, 
          t1.GOT_CARE, 
          t1.NUMB_VISIT_MED, 
          t1.EASY_MED_CARE, 
          t1.HEALTH_CARE_RATING, 
          t1.SMOKING, 
          t1.HEALTH_STAT1, 
          t1.HEALTH_STAT2, 
          t1.HEALTH_STAT3, 
          t1.FEEL_NERV, 
          t1.FEEL_WORTHLESS, 
          t1.FEEL_RESTLESS, 
          t1.SAQ_ENERGY_rev, 
          t1.SAQ_FELTPEACE_rev, 
          t1.SAQ_GENH_rev, 
          t1.SAQ_PAINLIM_rev, 
          t1.SUM_SAQ_SF12, 
          t1.SAQ2_DOCLIST, 
          t1.SAQ2_DOCUND, 
          t1.SAQ2_DOCRESP, 
          t1.SAQ2_DOCTIME, 
          t1.SAQ2_DOCSPEC, 
          t1.SAQ2_EZUN, 
          t1.SAQ2_DOCFOLLOW, 
          t1.SAQ2_DOCSPEC_rev, 
          t1.SAQ2_DocPat_SUM, 
          t1.Marry12x_Recode, 
          /* EDURECODE Categorical */
            (CASE  
               WHEN t1.EDURecode_Missing >= 0 and t1.EDURecode_Missing <= 12
               THEN 1
               WHEN  t1.EDURecode_Missing = 13
               THEN 2
               WHEN t1.EDURecode_Missing = 14
               THEN 3
               WHEN t1.EDURecode_Missing = 15
               THEN 4
               WHEN t1.EDURecode_Missing = 16
               THEN 5
            END) LABEL="EDURECODE Missing categorical variable" AS 'EDURECODE Categorical'n
      FROM WORK.MEPS_FULLYR_2012_MARRY_EDU t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Assign Project Library (MYDATA)   */
%LET _CLIENTTASKLABEL='Assign Project Library (MYDATA)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
LIBNAME MYDATA BASE "P:\QAC\qac200\students\jwoo" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Create INFULLYR=1   */
LIBNAME EC100036 "P:\QAC\qac200\students\jwoo";


%LET _CLIENTTASKLABEL='Create INFULLYR=1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_FULLYR_2012_A5DAT);

PROC SQL;
   CREATE TABLE WORK."MEPS_FULLYR_2012_A5DAT"n AS 
   SELECT t1.Marry12x_Recode_cate, 
          t1.SAQ2_DocPatRel_sumcate, 
          t1.Quartiles_Categorical, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EDRECODE, 
          t1.UNEMP12X, 
          t1.UNEIMP12, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADL3MO42, 
          t1.ADLANG42, 
          t1.ADLHLP42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.PHQ242, 
          t1.HPRAP12, 
          t1.HPRAU12, 
          t1.HPRDE12, 
          t1.HPRFE12, 
          t1.HPRJA12, 
          t1.HPRJL12, 
          t1.HPRJU12, 
          t1.HPRMA12, 
          t1.HPRMY12, 
          t1.HPRNO12, 
          t1.HPROC12, 
          t1.HPRSE12, 
          t1.MDDLPR42, 
          t1.MDDLRS42, 
          t1.MDUNAB42, 
          t1.MDUNPR42, 
          t1.MDUNRS42, 
          t1.MDDLAY42, 
          t1.DNUNAB42, 
          t1.DNUNPR42, 
          t1.DNUNRS42, 
          t1.DNDLAY42, 
          t1.DNDLPR42, 
          t1.DNDLRS42, 
          t1.RTHLTH31, 
          t1.RTHLTH42, 
          t1.RTHLTH53, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.ADHDADDX, 
          t1.ADHDAGED, 
          t1.DSDIA53, 
          t1.MIAGED, 
          t1.MIDX, 
          t1.HIBPAGED, 
          t1.HIBPDX, 
          t1.LEUKAGED, 
          t1.LEUKREMS, 
          t1.LUNGAGED, 
          t1.LUNGREMS, 
          t1.LYMPAGED, 
          t1.LYMPREMS, 
          t1.MELAAGED, 
          t1.MELAREMS, 
          t1.OTHRAGED, 
          t1.OHRTAGED, 
          t1.OHRTDX, 
          t1.SKDKAGED, 
          t1.SKDKREMS, 
          t1.SKNMAGED, 
          t1.STRKAGED, 
          t1.STRKDX, 
          t1.THRTAGED, 
          t1.THRTREMS, 
          t1.THYRAGED, 
          t1.THYRREMS, 
          t1.AIDHLP31, 
          t1.AIDHLP53, 
          t1.SSECP12X, 
          t1.BUSIMP12, 
          t1.BUSNP12X, 
          t1.BRAIAGED, 
          t1.BRAIREMS, 
          t1.BRSTAGED, 
          t1.BRSTEX53, 
          t1.BRSTREMS, 
          t1.CABLADDR, 
          t1.CABRAIN, 
          t1.CABREAST, 
          t1.CACERVIX, 
          t1.CACOLON, 
          t1.CALEUKEM, 
          t1.CALUNG, 
          t1.CALYMPH, 
          t1.CAMELANO, 
          t1.CANCERDX, 
          t1.CAOTHER, 
          t1.CAPROSTA, 
          t1.CARECO42, 
          t1.CASHP12X, 
          t1.CASKINDK, 
          t1.CASKINNM, 
          t1.CATHROAT, 
          t1.CATHYROD, 
          t1.CERVAGED, 
          t1.CERVREMS, 
          t1.SAQ_GENH, 
          t1.SAQ_HEALIMACT, 
          t1.SAQ_HEALIMSTRS, 
          t1.SAQ_ALPHYS, 
          t1.SAQ_WORKLIM, 
          t1.SAQ_ALMENT, 
          t1.SAQ_WLMENT, 
          t1.SAQ_FELTPEACE, 
          t1.SAQ_PAINLIM, 
          t1.SAQ_ENERGY, 
          t1.SAQ_DOWNHEART, 
          t1.SAQ_SOCACT, 
          t1.Marital_Status, 
          t1.Education_Level, 
          t1.GOT_CARE, 
          t1.NUMB_VISIT_MED, 
          t1.EASY_MED_CARE, 
          t1.HEALTH_CARE_RATING, 
          t1.SMOKING, 
          t1.HEALTH_STAT1, 
          t1.HEALTH_STAT2, 
          t1.HEALTH_STAT3, 
          t1.FEEL_NERV, 
          t1.FEEL_WORTHLESS, 
          t1.FEEL_RESTLESS, 
          t1.SAQ_ENERGY_rev, 
          t1.SAQ_FELTPEACE_rev, 
          t1.SAQ_GENH_rev, 
          t1.SAQ_PAINLIM_rev, 
          t1.SUM_SAQ_SF12, 
          t1.SAQ2_DOCLIST, 
          t1.SAQ2_DOCUND, 
          t1.SAQ2_DOCRESP, 
          t1.SAQ2_DOCTIME, 
          t1.SAQ2_DOCSPEC, 
          t1.SAQ2_EZUN, 
          t1.SAQ2_DOCFOLLOW, 
          t1.SAQ2_DOCSPEC_rev, 
          t1.SAQ2_DocPat_SUM, 
          t1.Marry12x_Recode, 
          t1.'EDURECODE Categorical'n, 
          /* INFULLYR */
            (1) AS INFULLYR
      FROM EC100036.meps_fullyr_2012_A5data t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Creat INER=1   */
LIBNAME EC100038 "P:\QAC\qac200\Data\MEPS";


%LET _CLIENTTASKLABEL='Creat INER=1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_ER_2012_er2012);

PROC SQL;
   CREATE TABLE WORK."MEPS_ER_2012_er2012"n AS 
   SELECT t1.DUID, 
          t1.PID, 
          t1.DUPERSID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          /* INER */
            (1) AS INER
      FROM EC100038.meps_er_2012 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: INNER JOIN for FULLYR and ER   */
%LET _CLIENTTASKLABEL='INNER JOIN for FULLYR and ER';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.MEPS_FULLYR_AND_ER);

PROC SQL;
   CREATE TABLE MYDATA.MEPS_FULLYR_AND_ER(label="MEPS_FULLYR_AND_ER") AS 
   SELECT t1.Marry12x_Recode_cate, 
          t1.SAQ2_DocPatRel_sumcate, 
          t1.Quartiles_Categorical, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EDRECODE, 
          t1.UNEMP12X, 
          t1.UNEIMP12, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADL3MO42, 
          t1.ADLANG42, 
          t1.ADLHLP42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.PHQ242, 
          t1.HPRAP12, 
          t1.HPRAU12, 
          t1.HPRDE12, 
          t1.HPRFE12, 
          t1.HPRJA12, 
          t1.HPRJL12, 
          t1.HPRJU12, 
          t1.HPRMA12, 
          t1.HPRMY12, 
          t1.HPRNO12, 
          t1.HPROC12, 
          t1.HPRSE12, 
          t1.MDDLPR42, 
          t1.MDDLRS42, 
          t1.MDUNAB42, 
          t1.MDUNPR42, 
          t1.MDUNRS42, 
          t1.MDDLAY42, 
          t1.DNUNAB42, 
          t1.DNUNPR42, 
          t1.DNUNRS42, 
          t1.DNDLAY42, 
          t1.DNDLPR42, 
          t1.DNDLRS42, 
          t1.RTHLTH31, 
          t1.RTHLTH42, 
          t1.RTHLTH53, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.ADHDADDX, 
          t1.ADHDAGED, 
          t1.DSDIA53, 
          t1.MIAGED, 
          t1.MIDX, 
          t1.HIBPAGED, 
          t1.HIBPDX, 
          t1.LEUKAGED, 
          t1.LEUKREMS, 
          t1.LUNGAGED, 
          t1.LUNGREMS, 
          t1.LYMPAGED, 
          t1.LYMPREMS, 
          t1.MELAAGED, 
          t1.MELAREMS, 
          t1.OTHRAGED, 
          t1.OHRTAGED, 
          t1.OHRTDX, 
          t1.SKDKAGED, 
          t1.SKDKREMS, 
          t1.SKNMAGED, 
          t1.STRKAGED, 
          t1.STRKDX, 
          t1.THRTAGED, 
          t1.THRTREMS, 
          t1.THYRAGED, 
          t1.THYRREMS, 
          t1.AIDHLP31, 
          t1.AIDHLP53, 
          t1.SSECP12X, 
          t1.BUSIMP12, 
          t1.BUSNP12X, 
          t1.BRAIAGED, 
          t1.BRAIREMS, 
          t1.BRSTAGED, 
          t1.BRSTEX53, 
          t1.BRSTREMS, 
          t1.CABLADDR, 
          t1.CABRAIN, 
          t1.CABREAST, 
          t1.CACERVIX, 
          t1.CACOLON, 
          t1.CALEUKEM, 
          t1.CALUNG, 
          t1.CALYMPH, 
          t1.CAMELANO, 
          t1.CANCERDX, 
          t1.CAOTHER, 
          t1.CAPROSTA, 
          t1.CARECO42, 
          t1.CASHP12X, 
          t1.CASKINDK, 
          t1.CASKINNM, 
          t1.CATHROAT, 
          t1.CATHYROD, 
          t1.CERVAGED, 
          t1.CERVREMS, 
          t1.SAQ_GENH, 
          t1.SAQ_HEALIMACT, 
          t1.SAQ_HEALIMSTRS, 
          t1.SAQ_ALPHYS, 
          t1.SAQ_WORKLIM, 
          t1.SAQ_ALMENT, 
          t1.SAQ_WLMENT, 
          t1.SAQ_FELTPEACE, 
          t1.SAQ_PAINLIM, 
          t1.SAQ_ENERGY, 
          t1.SAQ_DOWNHEART, 
          t1.SAQ_SOCACT, 
          t1.Marital_Status, 
          t1.Education_Level, 
          t1.GOT_CARE, 
          t1.NUMB_VISIT_MED, 
          t1.EASY_MED_CARE, 
          t1.HEALTH_CARE_RATING, 
          t1.SMOKING, 
          t1.HEALTH_STAT1, 
          t1.HEALTH_STAT2, 
          t1.HEALTH_STAT3, 
          t1.FEEL_NERV, 
          t1.FEEL_WORTHLESS, 
          t1.FEEL_RESTLESS, 
          t1.SAQ_ENERGY_rev, 
          t1.SAQ_FELTPEACE_rev, 
          t1.SAQ_GENH_rev, 
          t1.SAQ_PAINLIM_rev, 
          t1.SUM_SAQ_SF12, 
          t1.SAQ2_DOCLIST, 
          t1.SAQ2_DOCUND, 
          t1.SAQ2_DOCRESP, 
          t1.SAQ2_DOCTIME, 
          t1.SAQ2_DOCSPEC, 
          t1.SAQ2_EZUN, 
          t1.SAQ2_DOCFOLLOW, 
          t1.SAQ2_DOCSPEC_rev, 
          t1.SAQ2_DocPat_SUM, 
          t1.Marry12x_Recode, 
          t1.'EDURECODE Categorical'n, 
          t1.INFULLYR, 
          t2.DUID, 
          t2.PID, 
          t2.DUPERSID AS DUPERSID1, 
          t2.EVNTIDX, 
          t2.EVENTRN, 
          t2.ERHEVIDX, 
          t2.FFEEIDX, 
          t2.PANEL, 
          t2.MPCDATA, 
          t2.ERDATEYR, 
          t2.ERDATEMM, 
          t2.ERDATEDD, 
          t2.SEEDOC, 
          t2.VSTCTGRY, 
          t2.VSTRELCN, 
          t2.LABTEST, 
          t2.SONOGRAM, 
          t2.XRAYS, 
          t2.MAMMOG, 
          t2.MRI, 
          t2.EKG, 
          t2.EEG, 
          t2.RCVVAC, 
          t2.ANESTH, 
          t2.THRTSWAB, 
          t2.OTHSVCE, 
          t2.SURGPROC, 
          t2.MEDPRESC, 
          t2.ERICD1X, 
          t2.ERICD2X, 
          t2.ERICD3X, 
          t2.ERPRO1X, 
          t2.ERCCC1X, 
          t2.ERCCC2X, 
          t2.ERCCC3X, 
          t2.FFERTYPE, 
          t2.FFBEF12, 
          t2.ERXP12X, 
          t2.ERTC12X, 
          t2.ERFSF12X, 
          t2.ERFMR12X, 
          t2.ERFMD12X, 
          t2.ERFPV12X, 
          t2.ERFVA12X, 
          t2.ERFTR12X, 
          t2.ERFOF12X, 
          t2.ERFSL12X, 
          t2.ERFWC12X, 
          t2.ERFOR12X, 
          t2.ERFOU12X, 
          t2.ERFOT12X, 
          t2.ERFXP12X, 
          t2.ERFTC12X, 
          t2.ERDSF12X, 
          t2.ERDMR12X, 
          t2.ERDMD12X, 
          t2.ERDPV12X, 
          t2.ERDVA12X, 
          t2.ERDTR12X, 
          t2.ERDOF12X, 
          t2.ERDSL12X, 
          t2.ERDWC12X, 
          t2.ERDOR12X, 
          t2.ERDOU12X, 
          t2.ERDOT12X, 
          t2.ERDXP12X, 
          t2.ERDTC12X, 
          t2.IMPFLAG, 
          t2.PERWT12F, 
          t2.VARSTR, 
          t2.VARPSU, 
          t2.INER
      FROM WORK.MEPS_FULLYR_2012_A5DAT t1
           FULL JOIN WORK.MEPS_ER_2012_ER2012 t2 ON (t1.DUPERSID = t2.DUPERSID)
      WHERE t2.INER = 1 AND t1.INFULLYR = 1;
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data   */
%LET _CLIENTTASKLABEL='List Data';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 4:44:53 PM
   By task: List Data

   Input Data: Local:MYDATA.MEPS_FULLYR_AND_ER
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.MEPS_FULLYR_AND_ER
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.INER, T.DUPERSID1, T.DUPERSID
	FROM MYDATA.MEPS_FULLYR_AND_ER as T
;
QUIT;
TITLE;
TITLE1 "Report Listing";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=100)
	OBS="Row number"
	LABEL
	;
	VAR INER DUPERSID1 DUPERSID;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 4:44:53 PM
   By task: Data Set Attributes

   Input Data: Local:MYDATA.MEPS_FULLYR_AND_ER
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsForMEPS_FULLYR_AND_E);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=MYDATA.MEPS_FULLYR_AND_ER OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTContentsForMEPS_FULLYR_AND_E(LABEL="Contents Details for MEPS_FULLYR_AND_ER");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM WORK.CONTContentsForMEPS_FULLYR_AND_E
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='MEPS_FULLYR_AND_ER';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTContentsForMEPS_FULLYR_AND_E OUT=WORK.CONTContentsForMEPS_FULLYR_AND_E;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTContentsForMEPS_FULLYR_AND_E
		WHERE memname='MEPS_FULLYR_AND_ER';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder   */
%LET _CLIENTTASKLABEL='Query Builder';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR_AND_ER);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR_AND_ER AS 
   SELECT t1.Marry12x_Recode_cate, 
          t1.SAQ2_DocPatRel_sumcate, 
          t1.Quartiles_Categorical, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EDRECODE, 
          t1.UNEMP12X, 
          t1.UNEIMP12, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADL3MO42, 
          t1.ADLANG42, 
          t1.ADLHLP42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.PHQ242, 
          t1.HPRAP12, 
          t1.HPRAU12, 
          t1.HPRDE12, 
          t1.HPRFE12, 
          t1.HPRJA12, 
          t1.HPRJL12, 
          t1.HPRJU12, 
          t1.HPRMA12, 
          t1.HPRMY12, 
          t1.HPRNO12, 
          t1.HPROC12, 
          t1.HPRSE12, 
          t1.MDDLPR42, 
          t1.MDDLRS42, 
          t1.MDUNAB42, 
          t1.MDUNPR42, 
          t1.MDUNRS42, 
          t1.MDDLAY42, 
          t1.DNUNAB42, 
          t1.DNUNPR42, 
          t1.DNUNRS42, 
          t1.DNDLAY42, 
          t1.DNDLPR42, 
          t1.DNDLRS42, 
          t1.RTHLTH31, 
          t1.RTHLTH42, 
          t1.RTHLTH53, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.ADHDADDX, 
          t1.ADHDAGED, 
          t1.DSDIA53, 
          t1.MIAGED, 
          t1.MIDX, 
          t1.HIBPAGED, 
          t1.HIBPDX, 
          t1.LEUKAGED, 
          t1.LEUKREMS, 
          t1.LUNGAGED, 
          t1.LUNGREMS, 
          t1.LYMPAGED, 
          t1.LYMPREMS, 
          t1.MELAAGED, 
          t1.MELAREMS, 
          t1.OTHRAGED, 
          t1.OHRTAGED, 
          t1.OHRTDX, 
          t1.SKDKAGED, 
          t1.SKDKREMS, 
          t1.SKNMAGED, 
          t1.STRKAGED, 
          t1.STRKDX, 
          t1.THRTAGED, 
          t1.THRTREMS, 
          t1.THYRAGED, 
          t1.THYRREMS, 
          t1.AIDHLP31, 
          t1.AIDHLP53, 
          t1.SSECP12X, 
          t1.BUSIMP12, 
          t1.BUSNP12X, 
          t1.BRAIAGED, 
          t1.BRAIREMS, 
          t1.BRSTAGED, 
          t1.BRSTEX53, 
          t1.BRSTREMS, 
          t1.CABLADDR, 
          t1.CABRAIN, 
          t1.CABREAST, 
          t1.CACERVIX, 
          t1.CACOLON, 
          t1.CALEUKEM, 
          t1.CALUNG, 
          t1.CALYMPH, 
          t1.CAMELANO, 
          t1.CANCERDX, 
          t1.CAOTHER, 
          t1.CAPROSTA, 
          t1.CARECO42, 
          t1.CASHP12X, 
          t1.CASKINDK, 
          t1.CASKINNM, 
          t1.CATHROAT, 
          t1.CATHYROD, 
          t1.CERVAGED, 
          t1.CERVREMS, 
          t1.SAQ_GENH, 
          t1.SAQ_HEALIMACT, 
          t1.SAQ_HEALIMSTRS, 
          t1.SAQ_ALPHYS, 
          t1.SAQ_WORKLIM, 
          t1.SAQ_ALMENT, 
          t1.SAQ_WLMENT, 
          t1.SAQ_FELTPEACE, 
          t1.SAQ_PAINLIM, 
          t1.SAQ_ENERGY, 
          t1.SAQ_DOWNHEART, 
          t1.SAQ_SOCACT, 
          t1.Marital_Status, 
          t1.Education_Level, 
          t1.GOT_CARE, 
          t1.NUMB_VISIT_MED, 
          t1.EASY_MED_CARE, 
          t1.HEALTH_CARE_RATING, 
          t1.SMOKING, 
          t1.HEALTH_STAT1, 
          t1.HEALTH_STAT2, 
          t1.HEALTH_STAT3, 
          t1.FEEL_NERV, 
          t1.FEEL_WORTHLESS, 
          t1.FEEL_RESTLESS, 
          t1.SAQ_ENERGY_rev, 
          t1.SAQ_FELTPEACE_rev, 
          t1.SAQ_GENH_rev, 
          t1.SAQ_PAINLIM_rev, 
          t1.SUM_SAQ_SF12, 
          t1.SAQ2_DOCLIST, 
          t1.SAQ2_DOCUND, 
          t1.SAQ2_DOCRESP, 
          t1.SAQ2_DOCTIME, 
          t1.SAQ2_DOCSPEC, 
          t1.SAQ2_EZUN, 
          t1.SAQ2_DOCFOLLOW, 
          t1.SAQ2_DOCSPEC_rev, 
          t1.SAQ2_DocPat_SUM, 
          t1.Marry12x_Recode, 
          t1.'EDURECODE Categorical'n, 
          t1.INFULLYR, 
          t1.DUID, 
          t1.PID, 
          t1.DUPERSID1, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.INER, 
          /* MRI_Miss */
            (CASE 
               WHEN -7 = t1.MRI THEN .
               WHEN -8 = t1.MRI THEN .
               WHEN -9 = t1.MRI THEN .
               ELSE t1.MRI
            END) LABEL="Whether had catscan or MRI missing" AS MRI_Miss, 
          /* XRAYS_Miss */
            (CASE 
               WHEN -7 = t1.XRAYS THEN .
               WHEN -8 = t1.XRAYS THEN .
               WHEN -9 = t1.XRAYS THEN .
               ELSE t1.XRAYS
            END) LABEL="Xray visit Missing Variables" AS XRAYS_Miss
      FROM MYDATA.MEPS_FULLYR_AND_ER t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Recode_NoServ   */
%LET _CLIENTTASKLABEL='Recode_NoServ';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_FULLYR_Recode);

PROC SQL;
   CREATE TABLE WORK."MEPS_FULLYR_Recode"n AS 
   SELECT /* MRI_recode */
            (CASE  
               WHEN t1.MRI_Miss = 95
               THEN 2
               ELSE t1.MRI_Miss
            END) LABEL="Recoded MRI variable" AS MRI_recode, 
          /* XRAYS_recode */
            (CASE  
               WHEN t1.XRAYS_Miss =95
               THEN 2
               ELSE t1.XRAYS_Miss
            END) LABEL="X-rays recode" AS XRAYS_recode, 
          t1.Marry12x_Recode_cate, 
          t1.SAQ2_DocPatRel_sumcate, 
          t1.Quartiles_Categorical, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EDRECODE, 
          t1.UNEMP12X, 
          t1.UNEIMP12, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADL3MO42, 
          t1.ADLANG42, 
          t1.ADLHLP42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.PHQ242, 
          t1.HPRAP12, 
          t1.HPRAU12, 
          t1.HPRDE12, 
          t1.HPRFE12, 
          t1.HPRJA12, 
          t1.HPRJL12, 
          t1.HPRJU12, 
          t1.HPRMA12, 
          t1.HPRMY12, 
          t1.HPRNO12, 
          t1.HPROC12, 
          t1.HPRSE12, 
          t1.MDDLPR42, 
          t1.MDDLRS42, 
          t1.MDUNAB42, 
          t1.MDUNPR42, 
          t1.MDUNRS42, 
          t1.MDDLAY42, 
          t1.DNUNAB42, 
          t1.DNUNPR42, 
          t1.DNUNRS42, 
          t1.DNDLAY42, 
          t1.DNDLPR42, 
          t1.DNDLRS42, 
          t1.RTHLTH31, 
          t1.RTHLTH42, 
          t1.RTHLTH53, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.ADHDADDX, 
          t1.ADHDAGED, 
          t1.DSDIA53, 
          t1.MIAGED, 
          t1.MIDX, 
          t1.HIBPAGED, 
          t1.HIBPDX, 
          t1.LEUKAGED, 
          t1.LEUKREMS, 
          t1.LUNGAGED, 
          t1.LUNGREMS, 
          t1.LYMPAGED, 
          t1.LYMPREMS, 
          t1.MELAAGED, 
          t1.MELAREMS, 
          t1.OTHRAGED, 
          t1.OHRTAGED, 
          t1.OHRTDX, 
          t1.SKDKAGED, 
          t1.SKDKREMS, 
          t1.SKNMAGED, 
          t1.STRKAGED, 
          t1.STRKDX, 
          t1.THRTAGED, 
          t1.THRTREMS, 
          t1.THYRAGED, 
          t1.THYRREMS, 
          t1.AIDHLP31, 
          t1.AIDHLP53, 
          t1.SSECP12X, 
          t1.BUSIMP12, 
          t1.BUSNP12X, 
          t1.BRAIAGED, 
          t1.BRAIREMS, 
          t1.BRSTAGED, 
          t1.BRSTEX53, 
          t1.BRSTREMS, 
          t1.CABLADDR, 
          t1.CABRAIN, 
          t1.CABREAST, 
          t1.CACERVIX, 
          t1.CACOLON, 
          t1.CALEUKEM, 
          t1.CALUNG, 
          t1.CALYMPH, 
          t1.CAMELANO, 
          t1.CANCERDX, 
          t1.CAOTHER, 
          t1.CAPROSTA, 
          t1.CARECO42, 
          t1.CASHP12X, 
          t1.CASKINDK, 
          t1.CASKINNM, 
          t1.CATHROAT, 
          t1.CATHYROD, 
          t1.CERVAGED, 
          t1.CERVREMS, 
          t1.SAQ_GENH, 
          t1.SAQ_HEALIMACT, 
          t1.SAQ_HEALIMSTRS, 
          t1.SAQ_ALPHYS, 
          t1.SAQ_WORKLIM, 
          t1.SAQ_ALMENT, 
          t1.SAQ_WLMENT, 
          t1.SAQ_FELTPEACE, 
          t1.SAQ_PAINLIM, 
          t1.SAQ_ENERGY, 
          t1.SAQ_DOWNHEART, 
          t1.SAQ_SOCACT, 
          t1.Marital_Status, 
          t1.Education_Level, 
          t1.GOT_CARE, 
          t1.NUMB_VISIT_MED, 
          t1.EASY_MED_CARE, 
          t1.HEALTH_CARE_RATING, 
          t1.SMOKING, 
          t1.HEALTH_STAT1, 
          t1.HEALTH_STAT2, 
          t1.HEALTH_STAT3, 
          t1.FEEL_NERV, 
          t1.FEEL_WORTHLESS, 
          t1.FEEL_RESTLESS, 
          t1.SAQ_ENERGY_rev, 
          t1.SAQ_FELTPEACE_rev, 
          t1.SAQ_GENH_rev, 
          t1.SAQ_PAINLIM_rev, 
          t1.SUM_SAQ_SF12, 
          t1.SAQ2_DOCLIST, 
          t1.SAQ2_DOCUND, 
          t1.SAQ2_DOCRESP, 
          t1.SAQ2_DOCTIME, 
          t1.SAQ2_DOCSPEC, 
          t1.SAQ2_EZUN, 
          t1.SAQ2_DOCFOLLOW, 
          t1.SAQ2_DOCSPEC_rev, 
          t1.SAQ2_DocPat_SUM, 
          t1.Marry12x_Recode, 
          t1.'EDURECODE Categorical'n, 
          t1.INFULLYR, 
          t1.DUID, 
          t1.PID, 
          t1.DUPERSID1, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.INER, 
          t1.MRI_Miss, 
          t1.XRAYS_Miss
      FROM WORK.QUERY_FOR_MEPS_FULLYR_AND_ER t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies   */
%LET _CLIENTTASKLABEL='One-Way Frequencies';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 4:44:54 PM
   By task: One-Way Frequencies

   Input Data: Local:WORK.MEPS_FULLYR_RECODE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_RECODE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.XRAYS_recode, T.XRAYS
	FROM WORK.MEPS_FULLYR_RECODE as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies for MRI";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Julian Woo";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES XRAYS_recode /  SCORES=TABLE;
	TABLES XRAYS /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Make Count Variable   */
%LET _CLIENTTASKLABEL='Make Count Variable';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_FULLYR_Count);

PROC SQL;
   CREATE TABLE WORK."MEPS_FULLYR_Count"n AS 
   SELECT t1.DUPERSID, 
          /* COUNT_of_DUPERSID */
            (COUNT(t1.DUPERSID)) AS COUNT_of_DUPERSID
      FROM WORK.MEPS_FULLYR_RECODE t1
      GROUP BY t1.DUPERSID;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Join Count Variable   */
%LET _CLIENTTASKLABEL='Join Count Variable';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_FULLYR_Join_Count);

PROC SQL;
   CREATE TABLE WORK."MEPS_FULLYR_Join_Count"n AS 
   SELECT t1.MRI_recode, 
          t1.XRAYS_recode, 
          t1.Marry12x_Recode_cate, 
          t1.SAQ2_DocPatRel_sumcate, 
          t1.Quartiles_Categorical, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t2.DUPERSID AS DUPERSID2, 
          t2.COUNT_of_DUPERSID, 
          t1.EDUYRDEG, 
          t1.EDRECODE, 
          t1.UNEMP12X, 
          t1.UNEIMP12, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADL3MO42, 
          t1.ADLANG42, 
          t1.ADLHLP42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.PHQ242, 
          t1.HPRAP12, 
          t1.HPRAU12, 
          t1.HPRDE12, 
          t1.HPRFE12, 
          t1.HPRJA12, 
          t1.HPRJL12, 
          t1.HPRJU12, 
          t1.HPRMA12, 
          t1.HPRMY12, 
          t1.HPRNO12, 
          t1.HPROC12, 
          t1.HPRSE12, 
          t1.MDDLPR42, 
          t1.MDDLRS42, 
          t1.MDUNAB42, 
          t1.MDUNPR42, 
          t1.MDUNRS42, 
          t1.MDDLAY42, 
          t1.DNUNAB42, 
          t1.DNUNPR42, 
          t1.DNUNRS42, 
          t1.DNDLAY42, 
          t1.DNDLPR42, 
          t1.DNDLRS42, 
          t1.RTHLTH31, 
          t1.RTHLTH42, 
          t1.RTHLTH53, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.ADHDADDX, 
          t1.ADHDAGED, 
          t1.DSDIA53, 
          t1.MIAGED, 
          t1.MIDX, 
          t1.HIBPAGED, 
          t1.HIBPDX, 
          t1.LEUKAGED, 
          t1.LEUKREMS, 
          t1.LUNGAGED, 
          t1.LUNGREMS, 
          t1.LYMPAGED, 
          t1.LYMPREMS, 
          t1.MELAAGED, 
          t1.MELAREMS, 
          t1.OTHRAGED, 
          t1.OHRTAGED, 
          t1.OHRTDX, 
          t1.SKDKAGED, 
          t1.SKDKREMS, 
          t1.SKNMAGED, 
          t1.STRKAGED, 
          t1.STRKDX, 
          t1.THRTAGED, 
          t1.THRTREMS, 
          t1.THYRAGED, 
          t1.THYRREMS, 
          t1.AIDHLP31, 
          t1.AIDHLP53, 
          t1.SSECP12X, 
          t1.BUSIMP12, 
          t1.BUSNP12X, 
          t1.BRAIAGED, 
          t1.BRAIREMS, 
          t1.BRSTAGED, 
          t1.BRSTEX53, 
          t1.BRSTREMS, 
          t1.CABLADDR, 
          t1.CABRAIN, 
          t1.CABREAST, 
          t1.CACERVIX, 
          t1.CACOLON, 
          t1.CALEUKEM, 
          t1.CALUNG, 
          t1.CALYMPH, 
          t1.CAMELANO, 
          t1.CANCERDX, 
          t1.CAOTHER, 
          t1.CAPROSTA, 
          t1.CARECO42, 
          t1.CASHP12X, 
          t1.CASKINDK, 
          t1.CASKINNM, 
          t1.CATHROAT, 
          t1.CATHYROD, 
          t1.CERVAGED, 
          t1.CERVREMS, 
          t1.SAQ_GENH, 
          t1.SAQ_HEALIMACT, 
          t1.SAQ_HEALIMSTRS, 
          t1.SAQ_ALPHYS, 
          t1.SAQ_WORKLIM, 
          t1.SAQ_ALMENT, 
          t1.SAQ_WLMENT, 
          t1.SAQ_FELTPEACE, 
          t1.SAQ_PAINLIM, 
          t1.SAQ_ENERGY, 
          t1.SAQ_DOWNHEART, 
          t1.SAQ_SOCACT, 
          t1.Marital_Status, 
          t1.Education_Level, 
          t1.GOT_CARE, 
          t1.NUMB_VISIT_MED, 
          t1.EASY_MED_CARE, 
          t1.HEALTH_CARE_RATING, 
          t1.SMOKING, 
          t1.HEALTH_STAT1, 
          t1.HEALTH_STAT2, 
          t1.HEALTH_STAT3, 
          t1.FEEL_NERV, 
          t1.FEEL_WORTHLESS, 
          t1.FEEL_RESTLESS, 
          t1.SAQ_ENERGY_rev, 
          t1.SAQ_FELTPEACE_rev, 
          t1.SAQ_GENH_rev, 
          t1.SAQ_PAINLIM_rev, 
          t1.SUM_SAQ_SF12, 
          t1.SAQ2_DOCLIST, 
          t1.SAQ2_DOCUND, 
          t1.SAQ2_DOCRESP, 
          t1.SAQ2_DOCTIME, 
          t1.SAQ2_DOCSPEC, 
          t1.SAQ2_EZUN, 
          t1.SAQ2_DOCFOLLOW, 
          t1.SAQ2_DOCSPEC_rev, 
          t1.SAQ2_DocPat_SUM, 
          t1.Marry12x_Recode, 
          t1.'EDURECODE Categorical'n, 
          t1.INFULLYR, 
          t1.DUID, 
          t1.PID, 
          t1.DUPERSID1, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.INER, 
          t1.MRI_Miss, 
          t1.XRAYS_Miss
      FROM WORK.MEPS_FULLYR_RECODE t1
           INNER JOIN WORK.MEPS_FULLYR_COUNT t2 ON (t1.DUPERSID = t2.DUPERSID);
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis   */
%LET _CLIENTTASKLABEL='Distribution Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 4:44:54 PM
   By task: Distribution Analysis

   Input Data: Local:WORK.MEPS_FULLYR_JOIN_COUNT
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_JOIN_COUNT
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.COUNT_of_DUPERSID
	FROM WORK.MEPS_FULLYR_JOIN_COUNT as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of DUPERSID Count (or Number of ER visits)";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS QUANTILES;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR COUNT_of_DUPERSID;
	HISTOGRAM   COUNT_of_DUPERSID / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies1   */
%LET _CLIENTTASKLABEL='One-Way Frequencies1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 4:44:54 PM
   By task: One-Way Frequencies1

   Input Data: Local:WORK.MEPS_FULLYR_JOIN_COUNT
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_JOIN_COUNT
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.COUNT_of_DUPERSID
	FROM WORK.MEPS_FULLYR_JOIN_COUNT as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES COUNT_of_DUPERSID /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Summary Statistics   */
%LET _CLIENTTASKLABEL='Summary Statistics';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 4:44:54 PM
   By task: Summary Statistics

   Input Data: Local:WORK.MEPS_FULLYR_JOIN_COUNT
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_JOIN_COUNT
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.COUNT_of_DUPERSID
	FROM WORK.MEPS_FULLYR_JOIN_COUNT as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR COUNT_of_DUPERSID;

RUN;
ODS GRAPHICS ON;
TITLE;
/*-----------------------------------------------------
 * Use PROC UNIVARIATE to generate the histograms.
 */

TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Histograms";
PROC UNIVARIATE DATA=WORK.SORTTempTableSorted	NOPRINT	;
	VAR COUNT_of_DUPERSID;

			HISTOGRAM ;

RUN; QUIT;
ODS GRAPHICS OFF;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder1   */
%LET _CLIENTTASKLABEL='Query Builder1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR_JOIN_COUNT);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR_JOIN_COUNT AS 
   SELECT /* NUM_ER_cate */
            (CASE  
               WHEN t1.COUNT_of_DUPERSID = 1
               THEN 1
               WHEN t1.COUNT_of_DUPERSID =2
               THEN 2
               WHEN t1.COUNT_of_DUPERSID >= 3 and  t1.COUNT_of_DUPERSID <= 5
               THEN 3
                WHEN t1.COUNT_of_DUPERSID >= 6 and  t1.COUNT_of_DUPERSID <= 15
               THEN 4
            END) LABEL="Number of Visits to the ER Categorical variable" AS NUM_ER_cate, 
          t1.MRI_recode, 
          t1.XRAYS_recode, 
          t1.Marry12x_Recode_cate, 
          t1.SAQ2_DocPatRel_sumcate, 
          t1.Quartiles_Categorical, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.DUPERSID2, 
          t1.COUNT_of_DUPERSID, 
          t1.EDUYRDEG, 
          t1.EDRECODE, 
          t1.UNEMP12X, 
          t1.UNEIMP12, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADL3MO42, 
          t1.ADLANG42, 
          t1.ADLHLP42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.PHQ242, 
          t1.HPRAP12, 
          t1.HPRAU12, 
          t1.HPRDE12, 
          t1.HPRFE12, 
          t1.HPRJA12, 
          t1.HPRJL12, 
          t1.HPRJU12, 
          t1.HPRMA12, 
          t1.HPRMY12, 
          t1.HPRNO12, 
          t1.HPROC12, 
          t1.HPRSE12, 
          t1.MDDLPR42, 
          t1.MDDLRS42, 
          t1.MDUNAB42, 
          t1.MDUNPR42, 
          t1.MDUNRS42, 
          t1.MDDLAY42, 
          t1.DNUNAB42, 
          t1.DNUNPR42, 
          t1.DNUNRS42, 
          t1.DNDLAY42, 
          t1.DNDLPR42, 
          t1.DNDLRS42, 
          t1.RTHLTH31, 
          t1.RTHLTH42, 
          t1.RTHLTH53, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.ADHDADDX, 
          t1.ADHDAGED, 
          t1.DSDIA53, 
          t1.MIAGED, 
          t1.MIDX, 
          t1.HIBPAGED, 
          t1.HIBPDX, 
          t1.LEUKAGED, 
          t1.LEUKREMS, 
          t1.LUNGAGED, 
          t1.LUNGREMS, 
          t1.LYMPAGED, 
          t1.LYMPREMS, 
          t1.MELAAGED, 
          t1.MELAREMS, 
          t1.OTHRAGED, 
          t1.OHRTAGED, 
          t1.OHRTDX, 
          t1.SKDKAGED, 
          t1.SKDKREMS, 
          t1.SKNMAGED, 
          t1.STRKAGED, 
          t1.STRKDX, 
          t1.THRTAGED, 
          t1.THRTREMS, 
          t1.THYRAGED, 
          t1.THYRREMS, 
          t1.AIDHLP31, 
          t1.AIDHLP53, 
          t1.SSECP12X, 
          t1.BUSIMP12, 
          t1.BUSNP12X, 
          t1.BRAIAGED, 
          t1.BRAIREMS, 
          t1.BRSTAGED, 
          t1.BRSTEX53, 
          t1.BRSTREMS, 
          t1.CABLADDR, 
          t1.CABRAIN, 
          t1.CABREAST, 
          t1.CACERVIX, 
          t1.CACOLON, 
          t1.CALEUKEM, 
          t1.CALUNG, 
          t1.CALYMPH, 
          t1.CAMELANO, 
          t1.CANCERDX, 
          t1.CAOTHER, 
          t1.CAPROSTA, 
          t1.CARECO42, 
          t1.CASHP12X, 
          t1.CASKINDK, 
          t1.CASKINNM, 
          t1.CATHROAT, 
          t1.CATHYROD, 
          t1.CERVAGED, 
          t1.CERVREMS, 
          t1.SAQ_GENH, 
          t1.SAQ_HEALIMACT, 
          t1.SAQ_HEALIMSTRS, 
          t1.SAQ_ALPHYS, 
          t1.SAQ_WORKLIM, 
          t1.SAQ_ALMENT, 
          t1.SAQ_WLMENT, 
          t1.SAQ_FELTPEACE, 
          t1.SAQ_PAINLIM, 
          t1.SAQ_ENERGY, 
          t1.SAQ_DOWNHEART, 
          t1.SAQ_SOCACT, 
          t1.Marital_Status, 
          t1.Education_Level, 
          t1.GOT_CARE, 
          t1.NUMB_VISIT_MED, 
          t1.EASY_MED_CARE, 
          t1.HEALTH_CARE_RATING, 
          t1.SMOKING, 
          t1.HEALTH_STAT1, 
          t1.HEALTH_STAT2, 
          t1.HEALTH_STAT3, 
          t1.FEEL_NERV, 
          t1.FEEL_WORTHLESS, 
          t1.FEEL_RESTLESS, 
          t1.SAQ_ENERGY_rev, 
          t1.SAQ_FELTPEACE_rev, 
          t1.SAQ_GENH_rev, 
          t1.SAQ_PAINLIM_rev, 
          t1.SUM_SAQ_SF12, 
          t1.SAQ2_DOCLIST, 
          t1.SAQ2_DOCUND, 
          t1.SAQ2_DOCRESP, 
          t1.SAQ2_DOCTIME, 
          t1.SAQ2_DOCSPEC, 
          t1.SAQ2_EZUN, 
          t1.SAQ2_DOCFOLLOW, 
          t1.SAQ2_DOCSPEC_rev, 
          t1.SAQ2_DocPat_SUM, 
          t1.Marry12x_Recode, 
          t1.'EDURECODE Categorical'n, 
          t1.INFULLYR, 
          t1.DUID, 
          t1.PID, 
          t1.DUPERSID1, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.INER, 
          t1.MRI_Miss, 
          t1.XRAYS_Miss
      FROM WORK.MEPS_FULLYR_JOIN_COUNT t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies2   */
%LET _CLIENTTASKLABEL='One-Way Frequencies2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 4:44:54 PM
   By task: One-Way Frequencies2

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_JOIN_COUNT
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_JOIN_COUNT
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.NUM_ER_cate
	FROM WORK.QUERY_FOR_MEPS_FULLYR_JOIN_COUNT as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies of Number of ER visits Categorical Variable";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES NUM_ER_cate /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis   */
%LET _CLIENTTASKLABEL='Table Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 4:44:55 PM
   By task: Table Analysis

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_JOIN_COUNT
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_JOIN_COUNT
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.COUNT_of_DUPERSID, T.NUM_ER_cate
	FROM WORK.QUERY_FOR_MEPS_FULLYR_JOIN_COUNT as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis For Number of ER Visits";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Julian Woo";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES COUNT_of_DUPERSID * NUM_ER_cate /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Program   */
%LET _CLIENTTASKLABEL='Program';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011315.egp';
%LET _SASPROGRAMFILE=;

GOPTIONS ACCESSIBLE;
PROC SQL;
   CREATE TABLE JWOODATA.MEPS_FULLYR_2012_SUBSET(label="MEPS_FULLYR_2012_SUBSET") AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.UNEMP12X, 
          t1.UNEIMP12, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADL3MO42, 
          t1.ADLANG42, 
          t1.ADLHLP42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADUPRO42, 
          t1.ADWRTH42, 
          t1.PHQ242, 
          t1.HPRAP12, 
          t1.HPRAU12, 
          t1.HPRDE12, 
          t1.HPRFE12, 
          t1.HPRJA12, 
          t1.HPRJL12, 
          t1.HPRJU12, 
          t1.HPRMA12, 
          t1.HPRMY12, 
          t1.HPRNO12, 
          t1.HPROC12, 
          t1.HPRSE12, 
          t1.MDDLPR42, 
          t1.MDDLRS42, 
          t1.MDUNAB42, 
          t1.MDUNPR42, 
          t1.MDUNRS42, 
          t1.MDDLAY42, 
          t1.DNUNAB42, 
          t1.DNUNPR42, 
          t1.DNUNRS42, 
          t1.DNDLAY42, 
          t1.DNDLPR42, 
          t1.DNDLRS42, 
          t1.RTHLTH31, 
          t1.RTHLTH42, 
          t1.RTHLTH53, 
          t1.ARTHAGED, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASTHAGED, 
          t1.ASTHDX, 
          t1.ADHDADDX, 
          t1.ADHDAGED, 
          t1.DSDIA53, 
          t1.MIAGED, 
          t1.MIDX, 
          t1.HIBPAGED, 
          t1.HIBPDX, 
          t1.LEUKAGED, 
          t1.LEUKREMS, 
          t1.LUNGAGED, 
          t1.LUNGREMS, 
          t1.LYMPAGED, 
          t1.LYMPREMS, 
          t1.MELAAGED, 
          t1.MELAREMS, 
          t1.OTHRAGED, 
          t1.OHRTAGED, 
          t1.OHRTDX, 
          t1.SKDKAGED, 
          t1.SKDKREMS, 
          t1.SKNMAGED, 
          t1.STRKAGED, 
          t1.STRKDX, 
          t1.THRTAGED, 
          t1.THRTREMS, 
          t1.THYRAGED, 
          t1.THYRREMS, 
          t1.AIDHLP31, 
          t1.AIDHLP53, 
          t1.SSECP12X, 
          t1.BUSIMP12, 
          t1.BUSNP12X, 
          t1.BRAIAGED, 
          t1.BRAIREMS, 
          t1.BRSTAGED, 
          t1.BRSTEX53, 
          t1.BRSTREMS, 
          t1.CABLADDR, 
          t1.CABRAIN, 
          t1.CABREAST, 
          t1.CACERVIX, 
          t1.CACOLON, 
          t1.CALEUKEM, 
          t1.CALUNG, 
          t1.CALYMPH, 
          t1.CAMELANO, 
          t1.CANCERDX, 
          t1.CAOTHER, 
          t1.CAPROSTA, 
          t1.CARECO42, 
          t1.CASHP12X, 
          t1.CASKINDK, 
          t1.CASKINNM, 
          t1.CATHROAT, 
          t1.CATHYROD, 
          t1.CERVAGED, 
          t1.CERVREMS, 
          t1.EDRECODE
      FROM EC100005.meps_fullyr_2012 t1
      WHERE t1.AGE12X >= 18
      ORDER BY t1.DUPERSID;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
