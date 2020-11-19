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

* Load data;
/*
PROC IMPORT REPLACE DATAFILE="/home/u43010517/sasuser.v94/STATS/DS6372/Dataset/bank_clean_ds_train.csv"
PROC IMPORT REPLACE DATAFILE="/home/u43010517/sasuser.v94/STATS/DS6372/Dataset/bank_clean_train.csv"
*/
PROC IMPORT REPLACE DATAFILE="/home/u43010517/sasuser.v94/STATS/DS6372/Dataset/bank_clean.csv"
	DBMS=CSV
	OUT=STATS.Bank;
	GETNAMES=YES;
RUN;

* If Class instead of subscribed;
DATA STATS.Bank;
   SET STATS.Bank;
   IF subscribed='ye' then subscribed='yes';
RUN;