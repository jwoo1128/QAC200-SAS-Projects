/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Monday, January 12, 2015     TIME: 9:02:48 PM
PROJECT: WooJ_SAS_project_011215
PROJECT PATH: P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp
---------------------------------------- */

/* Unable to determine code to assign library JWOODATA on Local */


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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
LIBNAME JWOODATA BASE "P:\QAC\qac200\students\jwoo" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data and Observ.   */
%LET _CLIENTTASKLABEL='Data and Observ.';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

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
      FROM EC100002.meps_fullyr_2012 t1
      WHERE t1.AGE12X >= 18
      ORDER BY t1.DUPERSID;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: SAS Program Code new   */
%LET _CLIENTTASKLABEL='SAS Program Code new';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
%put ERROR: Unable to get SAS code. Unable to open input data;


GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: SUM variable SF-12   */
%LET _CLIENTTASKLABEL='SUM variable SF-12';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
%put ERROR: Unable to get SAS code. Unable to open input data;


GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data   */
%LET _CLIENTTASKLABEL='List Data';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 8:30:57 PM
   By task: List Data

   Input Data: Local:WORK.MEPS_FULLYR_2012_REVER_SUM
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_REVER_SUM
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SAQ_ENERGY_rev, T.SAQ_FELTPEACE_rev, T.SAQ_GENH_rev, T.SAQ_PAINLIM_rev, T.SAQ_HEALIMACT, T.SAQ_HEALIMSTRS, T.SAQ_ALPHYS, T.SAQ_WORKLIM, T.SAQ_ALMENT, T.SAQ_WLMENT, T.SAQ_DOWNHEART, T.SAQ_SOCACT
	FROM WORK.MEPS_FULLYR_2012_REVER_SUM as T
;
QUIT;
TITLE;
TITLE1 "Check Aggregate Variable Codding";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=50)
	OBS="Row number"
	LABEL
	;
	VAR SAQ_ENERGY_rev SAQ_FELTPEACE_rev SAQ_GENH_rev SAQ_PAINLIM_rev SAQ_HEALIMACT SAQ_HEALIMSTRS SAQ_ALPHYS SAQ_WORKLIM SAQ_ALMENT SAQ_WLMENT SAQ_DOWNHEART SAQ_SOCACT;
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


/*   START OF NODE: One-Way Frequencies   */
%LET _CLIENTTASKLABEL='One-Way Frequencies';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 8:30:58 PM
   By task: One-Way Frequencies

   Input Data: Local:WORK.MEPS_FULLYR_2012_REVER_SUM
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_REVER_SUM
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.SUM_SAQ_SF12
	FROM WORK.MEPS_FULLYR_2012_REVER_SUM as T
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
	TABLES SUM_SAQ_SF12 /  SCORES=TABLE;
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


/*   START OF NODE: CHECK_2   */
%LET _CLIENTTASKLABEL='CHECK_2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.CHECK_2_SUM);

PROC SQL;
   CREATE TABLE WORK.CHECK_2_SUM AS 
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
      FROM WORK.MEPS_FULLYR_2012_REVER_SUM t1
      WHERE t1.SUM_SAQ_SF12 = 2;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data1   */
%LET _CLIENTTASKLABEL='List Data1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 8:31:10 PM
   By task: List Data1

   Input Data: Local:WORK.CHECK_2_SUM
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.CHECK_2_SUM
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SAQ_ENERGY_rev, T.SAQ_FELTPEACE_rev, T.SAQ_GENH_rev, T.SAQ_PAINLIM_rev, T.SAQ_HEALIMACT, T.SAQ_HEALIMSTRS, T.SAQ_ALPHYS, T.SAQ_WORKLIM, T.SAQ_DOWNHEART, T.SAQ_SOCACT, T.SAQ_ALMENT, T.SAQ_WLMENT
	FROM WORK.CHECK_2_SUM as T
;
QUIT;
TITLE;
TITLE1 "Report Listing";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SORTTempTableSorted
	OBS="Row number"
	LABEL
	;
	VAR SAQ_ENERGY_rev SAQ_FELTPEACE_rev SAQ_GENH_rev SAQ_PAINLIM_rev SAQ_HEALIMACT SAQ_HEALIMSTRS SAQ_ALPHYS SAQ_WORKLIM SAQ_DOWNHEART SAQ_SOCACT SAQ_ALMENT SAQ_WLMENT;
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


/*   START OF NODE: Summary Statistics   */
%LET _CLIENTTASKLABEL='Summary Statistics';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 8:31:00 PM
   By task: Summary Statistics

   Input Data: Local:WORK.MEPS_FULLYR_2012_REVER_SUM
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_REVER_SUM
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SUM_SAQ_SF12
	FROM WORK.MEPS_FULLYR_2012_REVER_SUM as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics for Sum Variable";
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
	VAR SUM_SAQ_SF12;

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


/*   START OF NODE: Distribution Analysis for SF-12   */
%LET _CLIENTTASKLABEL='Distribution Analysis for SF-12';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 8:31:02 PM
   By task: Distribution Analysis for SF-12

   Input Data: Local:WORK.MEPS_FULLYR_2012_REVER_SUM
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_REVER_SUM
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SUM_SAQ_SF12
	FROM WORK.MEPS_FULLYR_2012_REVER_SUM(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of Aggregate Health Score SF-12";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))  by Julian Woo";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR SUM_SAQ_SF12;
	HISTOGRAM   SUM_SAQ_SF12 / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
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


/*   START OF NODE: SUM Categorical   */
%LET _CLIENTTASKLABEL='SUM Categorical';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
%put ERROR: Unable to get SAS code. Unable to open input data;


GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies1   */
%LET _CLIENTTASKLABEL='One-Way Frequencies1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 8:31:10 PM
   By task: One-Way Frequencies1

   Input Data: Local:WORK.MEPS_FULLYR_2012_SUMCATE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SUMCATE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.Quartiles_Categorical, T.SUM_SAQ_SF12
	FROM WORK.MEPS_FULLYR_2012_SUMCATE as T
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
	TABLES Quartiles_Categorical /  SCORES=TABLE;
	TABLES SUM_SAQ_SF12 /  SCORES=TABLE;
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 8:31:11 PM
   By task: Table Analysis

   Input Data: Local:WORK.MEPS_FULLYR_2012_SUMCATE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SUMCATE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SUM_SAQ_SF12, T.Quartiles_Categorical
	FROM WORK.MEPS_FULLYR_2012_SUMCATE as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis Sum Quantitative vs. Sum Categorical";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Julian Woo";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES SUM_SAQ_SF12 * Quartiles_Categorical /
		NOROW
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


/*   START OF NODE: Recode Variable 2   */
%LET _CLIENTTASKLABEL='Recode Variable 2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_FULLYR_2012_SUBSET_DOCPAT);

PROC SQL;
   CREATE TABLE WORK."MEPS_FULLYR_2012_SUBSET_DOCPAT"n AS 
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
          /* SAQ2_DOCLIST */
            (CASE 
               WHEN -1 = t1.ADLIST42 THEN .
               WHEN -7 = t1.ADLIST42 THEN .
               WHEN -9 = t1.ADLIST42 THEN .
               ELSE t1.ADLIST42
            END) LABEL="Doctor listened to patient" AS SAQ2_DOCLIST, 
          /* SAQ2_DOCEXPL */
            (CASE 
               WHEN -1 = t1.ADEXPL42 THEN .
               WHEN -9 = t1.ADEXPL42 THEN .
               ELSE t1.ADEXPL42
            END) LABEL="Doctor Explained so understood" AS SAQ2_DOCEXPL, 
          /* SAQ2_DOCRESP */
            (CASE 
               WHEN -1 = t1.ADRESP42 THEN .
               WHEN -9 = t1.ADRESP42 THEN .
               ELSE t1.ADRESP42
            END) LABEL="Doctor Showed Respect" AS SAQ2_DOCRESP, 
          /* SAQ2_DOCENUF */
            (CASE 
               WHEN -1 = t1.ADPRTM42 THEN .
               WHEN -9 = t1.ADPRTM42 THEN .
               ELSE t1.ADPRTM42
            END) LABEL="Doctor Spent enough time with patient" AS SAQ2_DOCENUF, 
          /* SAQ2_DOCSPEC */
            (CASE 
               WHEN -1 = t1.ADINST42 THEN .
               WHEN -8 = t1.ADINST42 THEN .
               WHEN -9 = t1.ADINST42 THEN .
               ELSE t1.ADINST42
            END) LABEL="Doctor gave specific instructions" AS SAQ2_DOCSPEC, 
          /* SAQ2_DOCEZUN */
            (CASE 
               WHEN -1 = t1.ADEZUN42 THEN .
               WHEN -9 = t1.ADEZUN42 THEN .
               ELSE t1.ADEZUN42
            END) LABEL="Doctor gave instruction that was easy to understand" AS SAQ2_DOCEZUN, 
          /* SAQ2_DOCDESFOL */
            (CASE 
               WHEN -1 = t1.ADTLHW42 THEN .
               WHEN -8 = t1.ADTLHW42 THEN .
               WHEN -9 = t1.ADTLHW42 THEN .
               ELSE t1.ADTLHW42
            END) LABEL="Doctor asked to describe how to follow" AS SAQ2_DOCDESFOL
      FROM JWOODATA.MEPS_FULLYR_2012_SUBSET t1;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Reverse Code2   */
%LET _CLIENTTASKLABEL='Reverse Code2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
%put ERROR: Unable to get SAS code. Unable to open input data;


GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: SUM Variable Doc-Pat Rela   */
%LET _CLIENTTASKLABEL='SUM Variable Doc-Pat Rela';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
%put ERROR: Unable to get SAS code. Unable to open input data;


GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data2   */
%LET _CLIENTTASKLABEL='List Data2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 8:31:05 PM
   By task: List Data2

   Input Data: Local:WORK.MEPS_FULLYR_2012_SUM
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SUM
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SAQ2_DOCEZUN, T.SAQ2_DOCDESFOL, T.SAQ2_DOCLIST, T.SAQ2_DOCEXPL, T.SAQ2_DOCRESP, T.SAQ2_DOCENUF, T.SAQ2_DOCSPEC_rev
	FROM WORK.MEPS_FULLYR_2012_SUM as T
;
QUIT;
TITLE;
TITLE1 "Report Listing for Sum Variable";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Julian Woo";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=50)
	OBS="Row number"
	LABEL
	;
	VAR SAQ2_DOCEZUN SAQ2_DOCDESFOL SAQ2_DOCLIST SAQ2_DOCEXPL SAQ2_DOCRESP SAQ2_DOCENUF SAQ2_DOCSPEC_rev;
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


/*   START OF NODE: One-Way Frequencies2   */
%LET _CLIENTTASKLABEL='One-Way Frequencies2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 8:31:06 PM
   By task: One-Way Frequencies2

   Input Data: Local:WORK.MEPS_FULLYR_2012_SUM
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SUM
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.SUM_DOCPATREL
	FROM WORK.MEPS_FULLYR_2012_SUM as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies for Sum Variable";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Julian Woo";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES SUM_DOCPATREL /  SCORES=TABLE;
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


/*   START OF NODE: Summary Statistics1   */
%LET _CLIENTTASKLABEL='Summary Statistics1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 8:31:07 PM
   By task: Summary Statistics1

   Input Data: Local:WORK.MEPS_FULLYR_2012_SUM
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SUM
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SUM_DOCPATREL
	FROM WORK.MEPS_FULLYR_2012_SUM as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics for Sum Variable - Doc-Patient Relationship";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Julian Woo";
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
		NMISS	
		Q1 
		MEDIAN 
		Q3	;
	VAR SUM_DOCPATREL;

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


/*   START OF NODE: Distribution Analysis   */
%LET _CLIENTTASKLABEL='Distribution Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 8:31:08 PM
   By task: Distribution Analysis

   Input Data: Local:WORK.MEPS_FULLYR_2012_SUM
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SUM
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SUM_DOCPATREL
	FROM WORK.MEPS_FULLYR_2012_SUM as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of Sum Variable Doctor-Patient Relationship";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Julian Woo";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS QUANTILES;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR SUM_DOCPATREL;
	HISTOGRAM   SUM_DOCPATREL / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
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


/*   START OF NODE: SUM categorical var   */
%LET _CLIENTTASKLABEL='SUM categorical var';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
%put ERROR: Unable to get SAS code. Unable to open input data;


GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies3   */
%LET _CLIENTTASKLABEL='One-Way Frequencies3';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 8:31:12 PM
   By task: One-Way Frequencies3

   Input Data: Local:WORK.MEPS_FULLYR_2012_SUM_CAT
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SUM_CAT
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.SUM_DOCPATREL, T.SUM_DOCPATREL_cat
	FROM WORK.MEPS_FULLYR_2012_SUM_CAT as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies Sum Var. Categorical";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Julian Woo";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES SUM_DOCPATREL /  SCORES=TABLE;
	TABLES SUM_DOCPATREL_cat /  SCORES=TABLE;
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


/*   START OF NODE: Table Analysis1   */
%LET _CLIENTTASKLABEL='Table Analysis1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 8:31:13 PM
   By task: Table Analysis1

   Input Data: Local:WORK.MEPS_FULLYR_2012_SUM_CAT
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SUM_CAT
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SUM_DOCPATREL_cat, T.SUM_DOCPATREL
	FROM WORK.MEPS_FULLYR_2012_SUM_CAT as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis for Sum quantiative vs. Sum categorical";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Julian Woo";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES SUM_DOCPATREL * SUM_DOCPATREL_cat /
		NOROW
		NOCOL
		NOPERCENT
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


/*   START OF NODE: One-Way Frequencies4   */
%LET _CLIENTTASKLABEL='One-Way Frequencies4';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 8:30:30 PM
   By task: One-Way Frequencies4

   Input Data: Local:JWOODATA.MEPS_FULLYR_2012_SUBSET
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:JWOODATA.MEPS_FULLYR_2012_SUBSET
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.MARRY12X
	FROM JWOODATA.MEPS_FULLYR_2012_SUBSET as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies for MARRY12X";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Julian Woo";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES MARRY12X /  SCORES=TABLE;
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


/*   START OF NODE: Missing Marry12x   */
%LET _CLIENTTASKLABEL='Missing Marry12x';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_FULLYR_2012_MARRYRE);

PROC SQL;
   CREATE TABLE WORK."MEPS_FULLYR_2012_MARRYRE"n AS 
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
          /* MARRY12X_RECODE */
            (CASE 
               WHEN -7 = t1.MARRY12X THEN .
               WHEN -9 = t1.MARRY12X THEN .
               ELSE t1.MARRY12X
            END) LABEL="Whether married or not recode missing" AS MARRY12X_RECODE
      FROM JWOODATA.MEPS_FULLYR_2012_SUBSET t1;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Marry12X_Recat   */
%LET _CLIENTTASKLABEL='Marry12X_Recat';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
%put ERROR: Unable to get SAS code. Unable to open input data;


GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis2 for Marry12x   */
%LET _CLIENTTASKLABEL='Table Analysis2 for Marry12x';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 8:49:56 PM
   By task: Table Analysis2 for Marry12x

   Input Data: Local:WORK.MEPS_FULLYR_2012_MARRY_CAT_RE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_MARRY_CAT_RE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MARRY12X, T.MARRY12X_rec_cate
	FROM WORK.MEPS_FULLYR_2012_MARRY_CAT_RE(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis fo Marry12X";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) By Julian Woo";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES MARRY12X * MARRY12X_rec_cate /
		NOROW
		NOCOL
		NOPERCENT
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


/*   START OF NODE: Marry12X_Recat1   */
%LET _CLIENTTASKLABEL='Marry12X_Recat1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
%put ERROR: Unable to get SAS code. Unable to open input data;


GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis3   */
%LET _CLIENTTASKLABEL='Table Analysis3';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 8:55:14 PM
   By task: Table Analysis3

   Input Data: Local:WORK.MEPS_FULLYR_2012_MARRY_CAT__0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_MARRY_CAT__0000
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MARRY12X, T.MARRY12X_rec_cate
	FROM WORK.MEPS_FULLYR_2012_MARRY_CAT__0000 as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis for MARRY12X";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Julian Woo";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES MARRY12X * MARRY12X_rec_cate /
		NOROW
		NOCOL
		NOPERCENT
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


/*   START OF NODE: Query Builder   */
%LET _CLIENTTASKLABEL='Query Builder';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR_2012__0000);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR_2012__0000 AS 
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
          t1.EDRECODE, 
          /* EDURECODE_Missing */
            (CASE 
               WHEN -7 = t1.EDRECODE THEN .
               WHEN -8 = t1.EDRECODE THEN .
               WHEN -9 = t1.EDRECODE THEN .
               ELSE t1.EDRECODE
            END) LABEL="EDURECODE recode missing data" AS EDURECODE_Missing
      FROM JWOODATA.MEPS_FULLYR_2012_SUBSET t1;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Education_cate   */
%LET _CLIENTTASKLABEL='Education_cate';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
%put ERROR: Unable to get SAS code. Unable to open input data;


GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis2   */
%LET _CLIENTTASKLABEL='Table Analysis2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 8:45:43 PM
   By task: Table Analysis2

   Input Data: Local:WORK.MEPS_FULLYR_2012_CATEEDU
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_CATEEDU
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.EDURECODE_Miss_cate, T.EDRECODE
	FROM WORK.MEPS_FULLYR_2012_CATEEDU as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis for EDURECODE";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Julian Woo";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES EDRECODE * EDURECODE_Miss_cate /
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

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
