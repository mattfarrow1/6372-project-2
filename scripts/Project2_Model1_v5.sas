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

* Logistic Regression;
*    - All Continuous Variables;
PROC LOGISTIC data=STATS.bank ;
	TITLE2 "All Continuous Variables";
	MODEL subscribed(event='ye') =  
		age duration campaign pdays previous emp_var_rate cons_price_idx
		cons_conf_idx euribor3m nr_employed
		
		/ SCALE=none aggregate lackfit;		* Can add "influence" but it does too many regression diagnostics;
RUN;

* Logistic Regression;
*    - All Categorical Variables;
PROC LOGISTIC data=STATS.bank ;
	TITLE2 "All Categorical Variables";
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

	MODEL subscribed(event='ye') =  
		job marital education housing loan contact month day_of_week poutcome
		
		/ SCALE=none aggregate lackfit;		* Can add "influence" but it does too many regression diagnostics;
RUN;


* Logistic Regression;
*    - All Variables;
PROC LOGISTIC data=STATS.bank ;
	TITLE2 "All Continuous & Categorical Variables";
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

	MODEL subscribed(event='ye') =  
		job marital education housing loan contact month day_of_week poutcome
		age duration campaign pdays previous emp_var_rate cons_price_idx
		cons_conf_idx euribor3m nr_employed
		
		/ SCALE=none aggregate lackfit;		* Can add "influence" but it does too many regression diagnostics;
RUN;


* Logistic Regression;
*    - Statistically Relevant Variables (based on alpha<0.5 p-values from step above);
PROC LOGISTIC data=STATS.bank ;
	TITLE2 "Statistically Relevant Variables";
	CLASS subscribed
		  job
		  contact
		  month
		  day_of_week
		  poutcome
		  ;

	MODEL subscribed(event='ye') =  
		job contact month day_of_week poutcome
		duration campaign previous emp_var_rate cons_price_idx cons_conf_idx euribor3m
		
		/ SCALE=none aggregate lackfit;		* Can add "influence" but it does too many regression diagnostics;
RUN;


* Logistic Regression with Effect Plots;
*    - Statistically Relevant Variables;
PROC LOGISTIC data=STATS.bank ;
	TITLE2 "Statistically Relevant Variables";
	CLASS subscribed job marital contact month day_of_week poutcome ;
	MODEL subscribed(event='ye') =  
		job marital contact month day_of_week poutcome
		duration previous emp_var_rate cons_price_idx cons_conf_idx euribor3m
		/ SCALE=none;
	EFFECTPLOT slicefit(sliceby=job plotby=duration) / noobs;
	EFFECTPLOT slicefit(sliceby=marital plotby=duration) / noobs;
	EFFECTPLOT slicefit(sliceby=contact plotby=duration) / noobs;
	EFFECTPLOT slicefit(sliceby=month plotby=duration) / noobs;
	EFFECTPLOT slicefit(sliceby=day_of_week plotby=duration) / noobs;
	EFFECTPLOT slicefit(sliceby=poutcome plotby=duration) / noobs;
	EFFECTPLOT slicefit(sliceby=previous plotby=duration) / noobs;
	EFFECTPLOT slicefit(sliceby=cons_price_idx plotby=duration) / noobs;
	EFFECTPLOT slicefit(sliceby=cons_conf_idx plotby=duration) / noobs;
	EFFECTPLOT slicefit(sliceby=euribor3m plotby=duration) / noobs;

	* And a few interesting plots;
	EFFECTPLOT slicefit(sliceby=month plotby=cons_price_idx) / noobs;
	EFFECTPLOT slicefit(sliceby=duration plotby=emp_var_rate) / noobs;
RUN;


* Logistic Regression;
*    - All Variables
*	 - Feature Selection: Forward;
PROC LOGISTIC data=STATS.bank ;
	TITLE2 "Feature Selection: Forward";
	CLASS subscribed
		  job marital education housing loan contact month day_of_week poutcome;
	MODEL subscribed(event='ye') =  
		job marital education housing loan contact month day_of_week poutcome
		age duration campaign pdays previous emp_var_rate cons_price_idx
		cons_conf_idx euribor3m nr_employed
		
		/	SELECTION=FORWARD start=3 
			SCALE=none aggregate lackfit;
RUN;


* Logistic Regression;
*    - All Variables
*	 - Feature Selection: Backward;
PROC LOGISTIC data=STATS.bank ;
	TITLE2 "Feature Selection: Backward";
	CLASS subscribed
		  job marital education housing loan contact month day_of_week poutcome;
	MODEL subscribed(event='ye') =  
		job marital education housing loan contact month day_of_week poutcome
		age duration campaign pdays previous emp_var_rate cons_price_idx
		cons_conf_idx euribor3m nr_employed
		
		/	SELECTION=BACKWARD start=3 
			SCALE=none aggregate lackfit;
RUN;


* Logistic Regression;
*    - All Variables
*	 - Feature Selection: Stepwise;
PROC LOGISTIC data=STATS.bank ;
	TITLE2 "Feature Selection: Stepwise";
	CLASS subscribed
		  job marital education housing loan contact month day_of_week poutcome;
	MODEL subscribed(event='ye') =  
		job marital education housing loan contact month day_of_week poutcome
		age duration campaign pdays previous emp_var_rate cons_price_idx
		cons_conf_idx euribor3m nr_employed
		
		/	SELECTION=STEPWISE start=3 
			SCALE=none aggregate lackfit;
RUN;


* Logistic Regression;
*    - Statistically Relevant Variables (based on results of STEPWISE);
PROC LOGISTIC data=STATS.bank ;
	TITLE2 "Statistically Relevant Variables";
	CLASS subscribed
		  job education contact month day_of_week poutcome ;

	MODEL subscribed(event='ye') =  
		job education contact month day_of_week poutcome
		duration campaign previous emp_var_rate cons_price_idx euribor3m
		
		/ SCALE=none aggregate lackfit;
RUN;


* Logistic Regression;
*    - Statistically Relevant Variables (based on results of STEPWISE)
*	 - ROC Curves;
PROC LOGISTIC data=STATS.bank ;
	TITLE2 "Statistically Relevant Variables";
	CLASS subscribed
		  job education contact month day_of_week poutcome ;

	MODEL subscribed(event='ye') =  
		job education contact month day_of_week poutcome
		duration campaign previous emp_var_rate cons_price_idx euribor3m
		/ SCALE=none aggregate lackfit;
		
	ROC 'Just Duration'
		duration
	;
	ROC 'Just Education'
		education
	;
	ROC 'Random Chance'
	;
	ROCCONTRAST  / estimate e;
RUN;


* Logistic Regression;
*    - Statistically Relevant Variables (based on results of STEPWISE)
*	 - ROC Curve;
PROC LOGISTIC data=STATS.bank plots(only)=roc;
	TITLE2 "Model 1: ROC Curve";
	CLASS subscribed
		  job education contact month day_of_week poutcome ;

	LogisticModel: MODEL subscribed(event='ye') =  
		job education contact month day_of_week poutcome
		duration campaign previous emp_var_rate cons_price_idx euribor3m;
		*/ SCALE=none aggregate lackfit;
		
   OUTPUT out=LogiOut predicted=LogiPred;       /* output predicted value, to be used later if we want to see the predictions */
RUN;



* Logistic Regression with Influence Diagnostics;
*    - Statistically Relevant Variables;
PROC LOGISTIC data=STATS.bank PLOTS(MAXPOINTS=NONE)=INFLUENCE;
	TITLE2 "Statistically Relevant Variables";
	CLASS subscribed
		  job education contact month day_of_week poutcome ;

	LogisticModel: MODEL subscribed(event='ye') =  
		job education contact month day_of_week poutcome
		duration campaign previous emp_var_rate cons_price_idx euribor3m
		/ SCALE=none aggregate lackfit;
RUN;

