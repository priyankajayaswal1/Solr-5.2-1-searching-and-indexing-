%macro remove_badly_behaving_vars 
           (data_in=,                 /* incoming GP table                                      */
            var_list=_numeric_,       /* list of variables to check                             */
            removal_reason_dataset=   /* new data set:  list of vars plus reason for removal    */
           );

   %** 2015-11-02  Also automatically remove DATE variables, not just DATE-TIME.                 ;
   %*********************************************************************************************;
   %**  Remove Badly Behaving Independent Variables = those meeting any of these conditions:     ;
   %**     - CSS > 1.0e+30                                                                       ;
   %**     - CSS = 0                                                                             ;
   %**     - any datetime variables                                                              ;
   %**     - any missing values                                                                  ;
   %**  DO NOT plan to scale variables that are too large.  That needs to be handled ahead of    ;
   %**  time, before using the model factory macros.  If it were handled here instead, then      ;
   %**  the automatically generated model package would contain an incorrect scoring formula.    ;
   %**  For now, all 4 removal tests are performed.  In the future, we may add parameters        ;
   %**  to control which of the tests are performed, with defaults of Y for all tests.           ;
   %*********************************************************************************************;
   %**  AFTER taking subsets related to a single row of parameter table, CSS will be re-tested   ;
   %**  and additional variables can be removed.                                                 ;
   %*********************************************************************************************;


   %*********************************************************************************************;
   %**  Step 1:  Gather information needed to identify badly behaving variables.                 ;
   %**  Note that PROC HPSUMMARY does not support multiple OUTPUT statements.                    ;
   %*********************************************************************************************;
   proc hpdmdb data=&data_in. varout=summary_stats (keep=name nmiss css);
      var &var_list.;
      &PERFORMANCE_STATEMENT. ;
   run;
   proc contents data=&data_in. (keep=&var_list) noprint out=_contents_ (keep=name format label);
   run;


   %*********************************************************************************************;
   %**  Step 2:  Create an output data set detailing "badly behaving variables":                 ;
   %**           - 1 observation per incoming variable                                           ;
   %**           - Variable name, format, label, CSS, and NMISS                                  ;
   %**           - reason for removing it (blank reason = keep the variable)                     ;
   %*********************************************************************************************;

   data summary_stats;
      set summary_stats;
      Variable_Name = upcase(name);
      drop name;
   run;
   data _contents_;
      set _contents_;
      Variable_Name = upcase(name);
      drop name;
   run;
   proc sort data=summary_stats;
      by Variable_Name;
   run;
   proc sort data=_contents_;
      by Variable_Name;
   run; 
   data &removal_reason_dataset;
      length Variable_Name $ 32;
      merge _contents_ 
            summary_stats;
      by Variable_Name;
      if  upcase(format) =: 'DATETIME' then reason_for_removal='Date-Time variable';
      else if upcase(format) in: ('DATE', 'YYMMDD', 'MMDDYY') then reason_for_removal='Date variable';
      else if NMISS                    then reason_for_removal='Missing values';
      else if CSS = 0                  then reason_for_removal='CSS=0';
      else if CSS > 1.0e+30 then reason_for_removal='CSS high';
   run;

%mend remove_badly_behaving_vars;
