*****************************************************
* STUDENT:		Edward Roske						*
* DATE: 		November 8, 2020					*
* CLASS:		MSDS6372 - Applied Statistics		*
* PROJECT:		2 - Logistic Regression				*
*****************************************************;

* Header Information;
LIBNAME STATS "/home/u43010517/sasuser.v94/STATS/DS6372";
RUN;
TITLE "Logistic Regression";

* Explore data;
PROC FREQ DATA=STATS.BANK;
*		default*subscribed;
	TABLES
		job*subscribed
		marital*subscribed
		education*subscribed
		housing*subscribed
		loan*subscribed
		contact*subscribed
		month*subscribed
		day_of_week*subscribed
		poutcome*subscribed
		/ chisq relrisk;
RUN;QUIT;

PROC MEANS DATA=STATS.BANK N NMISS MEAN MEDIAN MIN MAX STD;
*		  default;
	CLASS subscribed
		  job
		  marital
		  education
		  housing
		  loan
		  contact
		  month
		  day_of_week
		  poutcome
		  ;
*		default*subscribed;
	TYPES subscribed
		job*subscribed
		marital*subscribed
		education*subscribed
		housing*subscribed
		loan*subscribed
		contact*subscribed
		month*subscribed
		day_of_week*subscribed
		poutcome*subscribed
	;
	VAR age duration campaign pdays previous emp_var_rate cons_price_idx
	cons_conf_idx euribor3m nr_employed ;
RUN;

PROC MEANS DATA=STATS.BANK N NMISS MEAN MEDIAN MIN MAX STD;
	CLASS subscribed
		  month
		  ;
	TYPES subscribed
		  month*subscribed
		  ;
	VAR Duration Campaign Emp_var_rate Euribor3m Nr_employed Previous ;
RUN;

/*
PROC MEANS DATA=STATS.BANK N NMISS MEAN MEDIAN MIN MAX STD;
	CLASS subscribed
		  month
		  ;
	TYPES subscribed
		  month*subscribed
		  ;
	VAR nr_employed ;
RUN;
*/

