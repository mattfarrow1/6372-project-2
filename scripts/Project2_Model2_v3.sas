*****************************************************
* STUDENT:		Edward Roske						*
* DATE: 		November 23, 2020					*
* CLASS:		MSDS6372 - Applied Statistics		*
* PROJECT:		2 - Logistic Regression				*
*****************************************************;

* Header Information;
LIBNAME STATS "/home/u43010517/sasuser.v94/STATS/DS6372";
RUN;
TITLE "Model 2 - More Complicated Model";


* Add transformations;
* - Begin with previously relevant variables;
DATA STATS.BANKnew
		(DROP=age marital housing loan pdays cons_conf_idx nr_employed);
	SET STATS.BANK;

	* Logarithms;
	if duration gt 0 then durationLog=log(duration);
	if campaign gt 0 then campaignLog=log(campaign);
	if previous gt 0 then previousLog=log(previous);
	if emp_var_rate gt 0 then emp_var_rateLog=log(emp_var_rate);
	if cons_price_idx gt 0 then cons_price_idxLog=log(cons_price_idx);
	if euribor3m gt 0 then euribor3mLog=log(euribor3m);

	* Quadratics;
	duration2=duration**2;
	campaign2=campaign**2;
	previous2=previous**2;
	emp_var_rate2=emp_var_rate**2;
	cons_price_idx2=cons_price_idx**2;
	euribor3m2=euribor3m**2;
	
	*** Will add interactions later in model code;
RUN; 


* Logistic Regression;
*    - All Variables including new transformations;
*    - Had to leave out previousLog and emp_var_rateLog because of too many NAs;
PROC LOGISTIC data=STATS.BANKnew ;
	TITLE2 "All Continuous & Categorical Variables";
	CLASS subscribed
		  job education contact month day_of_week poutcome;

	MODEL subscribed(event='ye') =  
		job education contact month day_of_week poutcome
		duration campaign previous emp_var_rate cons_price_idx euribor3m
		duration2 campaign2 previous2 emp_var_rate2 cons_price_idx2 euribor3m2
		durationLog campaignLog cons_price_idxLog euribor3mLog
		
		/ SCALE=none aggregate lackfit;		* Can add "influence" but it does too many regression diagnostics;
RUN;

* All variables are still statistically impactful to the model;
* - Campaign is less relevant but still alpha < 0.05;


* Logistic Regression;
*	 - Add interactions;
*    - All Variables including new transformations;
*    - Had to leave out previousLog and emp_var_rateLog because of too many NAs;
PROC LOGISTIC data=STATS.BANKnew  plots(only)=roc;
	TITLE2 "All Continuous & Categorical Variables with Interactions";
	CLASS subscribed
		  job education contact month day_of_week poutcome
		  ;

	MODEL subscribed(event='ye') =  
		job education contact month day_of_week month day_of_week poutcome

		job*education job*education*month month*day_of_week month*poutcome
		campaign | previous | emp_var_rate | cons_price_idx | euribor3m
		
		duration campaign previous emp_var_rate cons_price_idx euribor3m
		
		duration2 campaign2 previous2 emp_var_rate2 cons_price_idx2 euribor3m2
		durationLog campaignLog cons_price_idxLog euribor3mLog
		
		/ SCALE=none aggregate lackfit;		* Can add "influence" but it does too many regression diagnostics;
		
	ROC 'Random Chance'
	;
	ROCCONTRAST  / estimate e;

RUN;


* Logistic Regression;
*    - All Variables p-value > 0.05 (from last step)
*	 - Feature Selection: Stepwise;
PROC LOGISTIC data=STATS.BANKnew ;
	TITLE2 "Variables: Stepwise Selection";
	CLASS subscribed
		  job education contact month day_of_week poutcome
		  ;

	StepwiseModel: MODEL subscribed(event='ye') =  
		education contact month day_of_week month day_of_week poutcome

		job*education job*education*month month*day_of_week month*poutcome
		previous*cons_price_idx emp_var_rate*euribor3m cons_price_idx*euribor3m
		emp_var_rate*cons_price_idx*euribor3m 
		
		duration previous cons_price_idx euribor3m
		
		duration2 campaign2 previous2 emp_var_rate2 cons_price_idx2 euribor3m2
		durationLog cons_price_idxLog euribor3mLog
		
		/	SELECTION=STEPWISE start=3 
			SCALE=none aggregate lackfit;
		
	ROC 'All variables'
		education contact month day_of_week month day_of_week poutcome
		job*education job*education*month month*day_of_week month*poutcome
		previous*cons_price_idx emp_var_rate*euribor3m cons_price_idx*euribor3m
		emp_var_rate*cons_price_idx*euribor3m 
		duration previous cons_price_idx euribor3m
		duration2 campaign2 previous2 emp_var_rate2 cons_price_idx2 euribor3m2
		durationLog cons_price_idxLog euribor3mLog	
	;
	ROCCONTRAST  / estimate e;

RUN;
* ROC for stepwise variables: 0.9349
*   ROC for stepwise if I leave in job*education*month: 0.9401
* ROC for all statistically relevant variables: 0.9471


* Logistic Regression;
*    - Statistically Relevant Variables (based on results of STEPWISE);
PROC LOGISTIC data=STATS.BANKnew plots(only)=roc;
	TITLE2 "Final Stepwise Model";
	CLASS subscribed
		  job education month poutcome ;

	FullModel: MODEL subscribed(event='ye') =  
		education month poutcome job*education duration duration2 durationLog euribor3mLog
		job*education*month
		
		/ SCALE=none aggregate lackfit;

	ROC 'Stepwise (no job*education*month)'
		education month poutcome job*education duration duration2 durationLog euribor3mLog
		job*education*month
	;
	ROC 'Only job*education*month'
		job*education*month
	;
	ROC 'Random Chance'
	;
	ROCCONTRAST  / estimate e;
RUN;


* Final variables in the more complex model:
		education month poutcome duration
		duration2 durationLog euribor3mLog
		job*education 
		job*education*month