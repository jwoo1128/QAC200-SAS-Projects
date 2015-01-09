/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Friday, January 09, 2015     TIME: 9:47:41 AM
PROJECT: WooJ_SAS_project_010815a
PROJECT PATH: P:\QAC\qac200\students\jwoo\WooJ_SAS_project_010815a.egp
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_010815a.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_010815a.egp';

GOPTIONS ACCESSIBLE;
LIBNAME JWOODATA BASE "P:\QAC\qac200\students\jwoo" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data and Observ.   */
%LET _CLIENTTASKLABEL='Data and Observ.';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_010815a.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_010815a.egp';

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
          t1.CERVREMS
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_010815a.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_010815a.egp';
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


/*   START OF NODE: One-Way Frequencies for 2012 MEPS Full Year Subset   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for 2012 MEPS Full Year Subset';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_010815a.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_010815a.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 08, 2015 at 4:24:19 PM
   By task: One-Way Frequencies for 2012 MEPS Full Year Subset

   Input Data: Local:JWOODATA.MEPS_FULLYR_2012_SUBSET
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:JWOODATA.MEPS_FULLYR_2012_SUBSET
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.AGE12X, T.SEX, T.REGION12, T.RACETHX, T.MARRY12X, T.EDUYRDEG, T.UNEMP12X, T.UNEIMP12, T.ADAPPT42, T.ADCAPE42, T.ADCLIM42, T.ADCMPD42, T.ADCMPM42, T.ADCMPY42, T.ADDAYA42, T.ADDOWN42, T.ADDPRS42, T.ADDRBP42, T.ADEFRT42, T.ADEGMC42
		     , T.ADEXPL42, T.ADEZUN42, T.ADFFRM42, T.ADFHLP42, T.ADGENH42, T.ADHECR42, T.ADHOPE42, T.ADILCR42, T.ADILWW42, T.ADINSA42, T.ADINSB42, T.ADINST42, T.ADINTR42, T.ADL3MO42, T.ADLANG42, T.ADLHLP42, T.ADLIST42, T.ADMALS42
		     , T.ADMWLM42, T.ADNDCR42, T.ADNERV42, T.ADNRGY42, T.ADNSMK42, T.ADOVER42, T.ADPAIN42, T.ADPALS42, T.ADPRTM42, T.ADPRX42, T.ADPWLM42, T.ADRESP42, T.ADREST42, T.ADRISK42, T.ADRTCR42, T.ADRTWW42, T.ADSAD42, T.ADSMOK42, T.ADSOCA42
		     , T.ADSPEC42, T.ADSPRF42, T.ADTLHW42, T.ADUPRO42, T.ADWRTH42, T.PHQ242, T.HPRAP12, T.HPRAU12, T.HPRDE12, T.HPRFE12, T.HPRJA12, T.HPRJL12, T.HPRJU12, T.HPRMA12, T.HPRMY12, T.HPRNO12, T.HPROC12, T.HPRSE12, T.MDDLPR42, T.MDDLRS42
		     , T.MDUNAB42, T.MDUNPR42, T.MDUNRS42, T.MDDLAY42, T.DNUNAB42, T.DNUNPR42, T.DNUNRS42, T.DNDLAY42, T.DNDLPR42, T.DNDLRS42, T.RTHLTH31, T.RTHLTH42, T.RTHLTH53, T.ARTHAGED, T.ARTHDX, T.ARTHTYPE, T.ASTHAGED, T.ASTHDX, T.ADHDADDX
		     , T.ADHDAGED, T.DSDIA53, T.MIAGED, T.MIDX, T.HIBPAGED, T.HIBPDX, T.LEUKAGED, T.LEUKREMS, T.LUNGAGED, T.LUNGREMS, T.LYMPAGED, T.LYMPREMS, T.MELAAGED, T.MELAREMS, T.OTHRAGED, T.OHRTAGED, T.OHRTDX, T.SKDKAGED, T.SKDKREMS
		     , T.SKNMAGED, T.STRKAGED, T.STRKDX, T.THRTAGED, T.THRTREMS, T.THYRAGED, T.THYRREMS, T.AIDHLP31, T.AIDHLP53, T.SSECP12X, T.BUSIMP12, T.BUSNP12X, T.BRAIAGED, T.BRAIREMS, T.BRSTAGED, T.BRSTEX53, T.BRSTREMS, T.CABLADDR, T.CABRAIN
		     , T.CABREAST, T.CACERVIX, T.CACOLON, T.CALEUKEM, T.CALUNG, T.CALYMPH, T.CAMELANO, T.CANCERDX, T.CAOTHER, T.CAPROSTA, T.CARECO42, T.CASHP12X, T.CASKINDK, T.CASKINNM, T.CATHROAT, T.CATHYROD, T.CERVAGED, T.CERVREMS
	FROM JWOODATA.MEPS_FULLYR_2012_SUBSET(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for 2012 MEPS Full Year Subset";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
FOOTNOTE2 "Julian Woo";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES AGE12X / MISSPRINT  SCORES=TABLE;
	TABLES SEX / MISSPRINT  SCORES=TABLE;
	TABLES REGION12 / MISSPRINT  SCORES=TABLE;
	TABLES RACETHX / MISSPRINT  SCORES=TABLE;
	TABLES MARRY12X / MISSPRINT  SCORES=TABLE;
	TABLES EDUYRDEG / MISSPRINT  SCORES=TABLE;
	TABLES UNEMP12X / MISSPRINT  SCORES=TABLE;
	TABLES UNEIMP12 / MISSPRINT  SCORES=TABLE;
	TABLES ADAPPT42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCAPE42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCLIM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPD42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPY42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDAYA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDOWN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDPRS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDRBP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEFRT42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEGMC42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEXPL42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEZUN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADFFRM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADFHLP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADGENH42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHECR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHOPE42 / MISSPRINT  SCORES=TABLE;
	TABLES ADILCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADILWW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINSA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINSB42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINTR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADL3MO42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLANG42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLHLP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLIST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADMALS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADMWLM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNDCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNERV42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNRGY42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNSMK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADOVER42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPAIN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPALS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPRTM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPRX42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPWLM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRESP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADREST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRISK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRTCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRTWW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSAD42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSMOK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSOCA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSPEC42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSPRF42 / MISSPRINT  SCORES=TABLE;
	TABLES ADTLHW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADUPRO42 / MISSPRINT  SCORES=TABLE;
	TABLES ADWRTH42 / MISSPRINT  SCORES=TABLE;
	TABLES PHQ242 / MISSPRINT  SCORES=TABLE;
	TABLES HPRAP12 / MISSPRINT  SCORES=TABLE;
	TABLES HPRAU12 / MISSPRINT  SCORES=TABLE;
	TABLES HPRDE12 / MISSPRINT  SCORES=TABLE;
	TABLES HPRFE12 / MISSPRINT  SCORES=TABLE;
	TABLES HPRJA12 / MISSPRINT  SCORES=TABLE;
	TABLES HPRJL12 / MISSPRINT  SCORES=TABLE;
	TABLES HPRJU12 / MISSPRINT  SCORES=TABLE;
	TABLES HPRMA12 / MISSPRINT  SCORES=TABLE;
	TABLES HPRMY12 / MISSPRINT  SCORES=TABLE;
	TABLES HPRNO12 / MISSPRINT  SCORES=TABLE;
	TABLES HPROC12 / MISSPRINT  SCORES=TABLE;
	TABLES HPRSE12 / MISSPRINT  SCORES=TABLE;
	TABLES MDDLPR42 / MISSPRINT  SCORES=TABLE;
	TABLES MDDLRS42 / MISSPRINT  SCORES=TABLE;
	TABLES MDUNAB42 / MISSPRINT  SCORES=TABLE;
	TABLES MDUNPR42 / MISSPRINT  SCORES=TABLE;
	TABLES MDUNRS42 / MISSPRINT  SCORES=TABLE;
	TABLES MDDLAY42 / MISSPRINT  SCORES=TABLE;
	TABLES DNUNAB42 / MISSPRINT  SCORES=TABLE;
	TABLES DNUNPR42 / MISSPRINT  SCORES=TABLE;
	TABLES DNUNRS42 / MISSPRINT  SCORES=TABLE;
	TABLES DNDLAY42 / MISSPRINT  SCORES=TABLE;
	TABLES DNDLPR42 / MISSPRINT  SCORES=TABLE;
	TABLES DNDLRS42 / MISSPRINT  SCORES=TABLE;
	TABLES RTHLTH31 / MISSPRINT  SCORES=TABLE;
	TABLES RTHLTH42 / MISSPRINT  SCORES=TABLE;
	TABLES RTHLTH53 / MISSPRINT  SCORES=TABLE;
	TABLES ARTHAGED / MISSPRINT  SCORES=TABLE;
	TABLES ARTHDX / MISSPRINT  SCORES=TABLE;
	TABLES ARTHTYPE / MISSPRINT  SCORES=TABLE;
	TABLES ASTHAGED / MISSPRINT  SCORES=TABLE;
	TABLES ASTHDX / MISSPRINT  SCORES=TABLE;
	TABLES ADHDADDX / MISSPRINT  SCORES=TABLE;
	TABLES ADHDAGED / MISSPRINT  SCORES=TABLE;
	TABLES DSDIA53 / MISSPRINT  SCORES=TABLE;
	TABLES MIAGED / MISSPRINT  SCORES=TABLE;
	TABLES MIDX / MISSPRINT  SCORES=TABLE;
	TABLES HIBPAGED / MISSPRINT  SCORES=TABLE;
	TABLES HIBPDX / MISSPRINT  SCORES=TABLE;
	TABLES LEUKAGED / MISSPRINT  SCORES=TABLE;
	TABLES LEUKREMS / MISSPRINT  SCORES=TABLE;
	TABLES LUNGAGED / MISSPRINT  SCORES=TABLE;
	TABLES LUNGREMS / MISSPRINT  SCORES=TABLE;
	TABLES LYMPAGED / MISSPRINT  SCORES=TABLE;
	TABLES LYMPREMS / MISSPRINT  SCORES=TABLE;
	TABLES MELAAGED / MISSPRINT  SCORES=TABLE;
	TABLES MELAREMS / MISSPRINT  SCORES=TABLE;
	TABLES OTHRAGED / MISSPRINT  SCORES=TABLE;
	TABLES OHRTAGED / MISSPRINT  SCORES=TABLE;
	TABLES OHRTDX / MISSPRINT  SCORES=TABLE;
	TABLES SKDKAGED / MISSPRINT  SCORES=TABLE;
	TABLES SKDKREMS / MISSPRINT  SCORES=TABLE;
	TABLES SKNMAGED / MISSPRINT  SCORES=TABLE;
	TABLES STRKAGED / MISSPRINT  SCORES=TABLE;
	TABLES STRKDX / MISSPRINT  SCORES=TABLE;
	TABLES THRTAGED / MISSPRINT  SCORES=TABLE;
	TABLES THRTREMS / MISSPRINT  SCORES=TABLE;
	TABLES THYRAGED / MISSPRINT  SCORES=TABLE;
	TABLES THYRREMS / MISSPRINT  SCORES=TABLE;
	TABLES AIDHLP31 / MISSPRINT  SCORES=TABLE;
	TABLES AIDHLP53 / MISSPRINT  SCORES=TABLE;
	TABLES SSECP12X / MISSPRINT  SCORES=TABLE;
	TABLES BUSIMP12 / MISSPRINT  SCORES=TABLE;
	TABLES BUSNP12X / MISSPRINT  SCORES=TABLE;
	TABLES BRAIAGED / MISSPRINT  SCORES=TABLE;
	TABLES BRAIREMS / MISSPRINT  SCORES=TABLE;
	TABLES BRSTAGED / MISSPRINT  SCORES=TABLE;
	TABLES BRSTEX53 / MISSPRINT  SCORES=TABLE;
	TABLES BRSTREMS / MISSPRINT  SCORES=TABLE;
	TABLES CABLADDR / MISSPRINT  SCORES=TABLE;
	TABLES CABRAIN / MISSPRINT  SCORES=TABLE;
	TABLES CABREAST / MISSPRINT  SCORES=TABLE;
	TABLES CACERVIX / MISSPRINT  SCORES=TABLE;
	TABLES CACOLON / MISSPRINT  SCORES=TABLE;
	TABLES CALEUKEM / MISSPRINT  SCORES=TABLE;
	TABLES CALUNG / MISSPRINT  SCORES=TABLE;
	TABLES CALYMPH / MISSPRINT  SCORES=TABLE;
	TABLES CAMELANO / MISSPRINT  SCORES=TABLE;
	TABLES CANCERDX / MISSPRINT  SCORES=TABLE;
	TABLES CAOTHER / MISSPRINT  SCORES=TABLE;
	TABLES CAPROSTA / MISSPRINT  SCORES=TABLE;
	TABLES CARECO42 / MISSPRINT  SCORES=TABLE;
	TABLES CASHP12X / MISSPRINT  SCORES=TABLE;
	TABLES CASKINDK / MISSPRINT  SCORES=TABLE;
	TABLES CASKINNM / MISSPRINT  SCORES=TABLE;
	TABLES CATHROAT / MISSPRINT  SCORES=TABLE;
	TABLES CATHYROD / MISSPRINT  SCORES=TABLE;
	TABLES CERVAGED / MISSPRINT  SCORES=TABLE;
	TABLES CERVREMS / MISSPRINT  SCORES=TABLE;
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


/*   START OF NODE: Recode Variables   */
%LET _CLIENTTASKLABEL='Recode Variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_010815a.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_010815a.egp';

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


/*   START OF NODE: Table Analysis   */
%LET _CLIENTTASKLABEL='Table Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_010815a.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_010815a.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 9:41:04 AM
   By task: Table Analysis

   Input Data: Local:WORK.MEPS_FULLYR_2012_SUBSET_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SUBSET_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADWRTH42, T.FEEL_WORTHLESS
	FROM WORK.MEPS_FULLYR_2012_SUBSET_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADWRTH42 * FEEL_WORTHLESS /
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


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwoo\WooJ_SAS_project_010815a.egp';
%LET _CLIENTPROJECTNAME='WooJ_SAS_project_010815a.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\Data\MEPS";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 9:46:36 AM
   By task: Data Set Attributes

   Input Data: P:\QAC\qac200\Data\MEPS\meps_fullyr_2012.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsFormeps_fullyr_2012);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=ECLIB000.meps_fullyr_2012 OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTContentsFormeps_fullyr_2012(LABEL="Contents Details for meps_fullyr_2012");
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
			typemem LABEL="Data Set Type" FROM WORK.CONTContentsFormeps_fullyr_2012
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTContentsFormeps_fullyr_2012 OUT=WORK.CONTContentsFormeps_fullyr_2012;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTContentsFormeps_fullyr_2012
		WHERE memname='MEPS_FULLYR_2012';
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

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
