Enter file contents here%macro executeBatchDataMining;

/***************************************************************************************************/
/**  The executeBatchDataMining macro finishes data preparation and does the following:     ********/
/**  1. Across all the target variables used in the expanded parameter table, the macro     ********/
/**     examines the relationship between each independent variable and the dependent       ********/
/**     variable using the remove_badly_behaving_vars macro to find independent variables   ********/
/**     that either have 0 or > 1.0e+30 values for the Corrected Sum of Squares (CSS).      ********/
/**     If any such variables are found, they are removed from the analysis.                ********/
/**     A macro list of acceptable variables is created to use in the next loop for         ********/
/**     variable reduction.                                                                 ********/
/**  2. For each row of the expanded parameter table, a variable reduction process is run   ********/
/**     to eventually produce a final set of 75 variables and potentially significant       ********/
/**     interactions to be input into a variety of models which based on the target         ********/
/**     variable type include:                                                              ********/
/**         Binary Numeric - HPLOGISTIC  HPFOREST  HPNEURAL                                 ********/
/**         Binary Categorical - HPLOGISTIC  HPFOREST  HPNEURAL                             ********/
/**         Ordinal - HPLOGISTIC HPNEURAL                                                   ********/
/**         Interval - HPREG HPFOREST  HPNEURAL                                             ********/
/**         Categorical - HPLOGISTIC(LINK=GLOGIT) HPFOREST HPNEURAL                         ********/
/**     The results of these models are then put into the Model Manager system for future   ********/
/**     access                                                                              ********/
/***************************************************************************************************/

     %**  Capture original options settings, so that debugging can temporarily switch them and then    ;
     %**  return to the original settings.                                                             ;
     %local original_settings;
     %let original_settings = %sysfunc(getoption(mprint))
                              %sysfunc(getoption(mlogic))
                              %sysfunc(getoption(symbolgen));

     %**  Remove large tables remaining from prior runs of the model factory.                          ;
     proc datasets library=gplib;
        delete mod_: sub_: alldri_targ:;
     run;

     %**  Parse the list of the tables that will be processed produce table and schema names           ;
     %let tableCounter = 1;
     %let only&tableCounter = gplib.%scan(&tableList,&tableCounter, " ");
     %let sonly&tableCounter = &sschema..%scan(&tableList,&tableCounter, " ");
     %let table&tableCounter = gplib.mod_%scan(&tableList,&tableCounter," ");
     %let ccc&tableCounter = %scan(&tableList,&tableCounter, " ");
     %let schema&tableCounter = &sschema..mod_%scan(&tableList,&tableCounter," ");
     %let subtable&tableCounter = gplib.sub_%scan(&tableList,&tableCounter, " ");
     %let subschema&tableCounter = &sschema..sub_%scan(&tableList,&tableCounter, " ");
     %**  For each independent table in the list, create a group of macro variables to reference      ;
     %**   each independent table in the list                                                         ;
     %do %while("&&ccc&tableCounter" NE "");
	 %**  STOP the job if one of the independent variable tables does not exist              ;
	 	 %if ^%sysfunc(exist(&&only&tableCounter..)) %then %do;
 	       %put **************************************************************************;
           %put **************************************************************************;
           %put ERROR: &&only&tableCounter.. does not exist;
           %put ERROR- NO further checking will be done ;
           %put WARNING: Please check existence of all GP tables in the Param_TableList variable.;
           %put ERROR: Factory Model Job will now end;
	       %put **************************************************************************;
           %put **************************************************************************;
			%abort cancel;
		 %end;

	 %**  SET up the counter macros for use in the macro do loops              ;
         %let tableCounter = %eval(&tableCounter + 1);
         %let ccc&tableCounter = %scan(&tableList,&tableCounter, " ");
         %let only&tableCounter = gplib.%scan(&tableList,&tableCounter, " ");
         %let sonly&tableCounter = &sschema..%scan(&tableList,&tableCounter, " ");
         %let table&tableCounter = gplib.mod_%scan(&tableList,&tableCounter, " ");
         %let schema&tableCounter = &sschema..mod_%scan(&tableList,&tableCounter, " ");
         %let subtable&tableCounter = gplib.sub_%scan(&tableList,&tableCounter, " ");
         %let subschema&tableCounter = &sschema..sub_%scan(&tableList,&tableCounter, " ");
     %**  End of loop to create a group of macro variables to reference         ;
     %end;
     %let tableCounter =  %eval(&tableCounter - 1);
     %put 1 &only1 &table1 &schema1;
     %put &tableCounter &&only&tableCounter &&table&tableCounter &&schema&tableCounter;


     %**  Loop #1 below requires a list of names of dependent variables from the dependent table.      ;
     %**  This step retrieves a constant list, and so is not part of the loop.                         ;
     %**  Create that list now, in the form needed for SQL:                                            ;
     %**      dep.var1, dep.var2, dep.var3, etc.                                                       ;
     %**  Some coding revisions attempted a slightly different style (no longer being used):           ;
     %**      dep."var1", dep."var2", dep."var3", etc.                                                 ;
     %**  When Greenplum uses a list in this style, capitalization matters.  The added code needed     ;
     %**  to retrieve the exact spelling including capitalization is left in place below.              ;

options MPRINT ;
     proc contents data=gplib.&dep_table. out=_contents_dep_table noprint varnum; 
     run;
     %local dependent_varlist 
            numeric_indeps_kept_comma
            var_name
            hpvar_name
            count
            sup_var_name
            unsup_var_name;

     proc sql noprint;
        create table  _dependent_variables_needed_ as 
               select distinct(upcase(dependent_variable)) as upcase_name
               from   &parameter_file;
        create table  _dependents_proper_spelling_ as
               select spelling.name
               from   _dependent_variables_needed_ needed
               left join _contents_dep_table       spelling
               on needed.upcase_name = upcase(spelling.name);
        select ('dep.' || strip(name)) into : dependent_varlist
               separated by ','
          from _dependents_proper_spelling_;
     quit;
     %local segmentation_varlist;
     %if &numattr > 0 %then %do;
         data _segmentation_variables_needed_;
            length upcase_name $ 32;
            %do i=1 %to &numattr;
                upcase_name = upcase("&&attr&i");
                output;
            %end;
         run;
         proc sql noprint;
            %**  In theory, a segmentation variable for one model could be the dependent variable for  ;
            %**  another model.  Remove any overlap, so the SQL code in Loop #1 below will not attempt ;
            %**  to retrieve the same variable twice.                                                  ;
            create table _segmentation_no_overlap_ as
                   select * from                   _segmentation_variables_needed_ seg
                   where not exists (select * from _dependent_variables_needed_    dep
                                     where seg.upcase_name=dep.upcase_name);
            create table  _segmentation_proper_spelling_ as
                   select spelling.name
                   from   _segmentation_no_overlap_ needed
                   left join _contents_dep_table    spelling
                   on needed.upcase_name = upcase(spelling.name);
            select ('dep.' || strip(name)) into : segmentation_varlist
                   separated by ','
              from _segmentation_proper_spelling_;
         quit;
     %end;
     proc sql noprint;
        select strip(name) into : IdVar
               from _contents_dep_table
               where strip(upcase(name)) = strip(upcase("&IdVar"));
     quit;
options &original_settings;

     %**  Loop #1                                                                                      ;
     %**  For each independent table:                                                                  ;
     %**    - remove badly behaving independent variables                                              ;
     %**    - join with dependent table (needed vars only)                                             ;
     %**    - subset rows to match dependent table                                                     ;
           
     %let iii = 0; /* Counter to replace tabCnt when there are tables with NO valid variables            */      
	 %let tot_tab = &tableCounter; /* Counter to replace tableCounterfor tables with NO valid variables  */
     %do tabCnt = 1 %to &tableCounter;  /* Start of Loop #1 */
		 %let iii = %eval(&iii + 1); /*Increment iii each time tabcnt increments -- may decrement later  */
         %put Loop #1 beginning iteration #&tabCnt;
         %local numeric_indeps_&iii. 
                numeric_indeps_kept_&iii. 
                count&iii.;
         proc contents data=&&only&iii out= perm.contents_&iii noprint varnum; 
         run;
         proc sql noprint;
            select name into :numeric_indeps_&iii. separated by ' ' 
              from perm.contents_&iii where type = 1;
         quit;
         %**  Remove Badly Behaving Independent Variables                                              ;
         %remove_badly_behaving_vars (data_in=&&only&iii,
                                      var_list=&&numeric_indeps_&iii..,
                                      removal_reason_dataset=perm.bad_vars_&iii.
                                     );
         proc print data=perm.bad_vars_&iii.;
            where reason_for_removal > ' ';
            var Variable_Name reason_for_removal label css format nmiss;
            format label $40.;
            title "Badly Behaving Variables being removed from &&only&iii";
         run;

         %**  Create macro variables to hold any variable lists that might be needed.                  ;
         %**  Save lists of independent variables NOW, before combining with dependent table.          ;
         %**  This guarantees that lists of independent variables cannot include additional            ;
         %**  dependent variables or segmentation variables from dependent data.                       ;

         proc sql;
            select variable_name into : numeric_indeps_kept_&iii separated by ' ' 
              from perm.bad_vars_&iii. where reason_for_removal=' ';
            select variable_name into : numeric_indeps_kept_comma   separated by ',' 
              from perm.bad_vars_&iii. where reason_for_removal=' ';
         quit;
         %**  For any GP table with all BAD variables, renumber the table list so that the             ;
         %**  misbehaving table is removed from processing.                                            ;
      %if &&numeric_indeps_kept_&iii = %then %let tot_tab = %eval(&tot_tab - 1);    
      %if &&numeric_indeps_kept_&iii = %then %do i = &iii %to &tot_tab;    
         %let I_1 = %eval(&I + 1);
         %let ccc&I = &&ccc&I_1;
         %let only&I = &&only&I_1;
         %let table&I = &&table&I_1;
         %let subtable&I = &&subtable&I_1;
         %let sonly&I = &&sonly&I_1;
         %let schema&I = &&schema&I_1;
         %let subschema&I = &&subschema&I_1;
      %end; 
         %**  Join Dependent Variable tables with Independent Variable tables                          ;
         %**  Subset the records that match on the ID variable.                                        ;
         %**  Bring in just the needed variables from the dependent table:                             ;
         %**  dependents, ID variable, and segmentation variables.                                     ;

        /* create the combined table only if there are dependent variables */
      %if &&numeric_indeps_kept_&iii ^= %then %do; 
         %drop_table_iff (&&schema&iii)
OPTIONS MPRINT;

         proc sql;
            Connect to Greenplm (server=&server user=&user password=&pwd
                                 database=&database schema="&sschema" );
		 execute (set gp_autostats_mode='NONE') by Greenplm;
 	     execute ( create table &&schema&iii with (appendonly=true, compresstype=QUICKLZ, OIDS=FALSE) as
                      select   dep.&idVar, 
                               &dependent_varlist,
                               %if %length(&segmentation_varlist) %then &segmentation_varlist,;
                               &numeric_indeps_kept_comma
                        from   &sschema..&dep_table. as dep, 
                               &&sonly&iii as indep
                      where dep.&idVar. = indep.&idVar. DISTRIBUTED BY (&idVar.)) by Greenplm;
		 execute (analyze &&schema&iii (&idVar. &attrlistc.)) by Greenplm;
         quit;
OPTIONS &original_settings;
	  %end;
	     /* If a table has NO valid variables, then reduce the counter by one                         */
      %if &&numeric_indeps_kept_&iii = %then %let iii = %eval(&iii - 1);    

         %put Loop #1 finishing iteration #&tabCnt;
     %end; /* end of Loop #1:  processing each independent table */

     %**  Set total tables to be tot_tab.                                                              ;
     %LET tableCounter = &tot_tab;

     %**  Loop #2:                                                                                     ;
     %**  Process each row of parameter table separately  ... runs through the end of this macro.      ;
     %**  This loop does:                                                                              ;
     %**  1. The HPREDUCE variable reduction                                                           ;
     %**  2. Builds the models                                                                         ;
	 %**  3. Passes the results to Model Manager                                                       ;
     %do targCnt = 1 %to &targetVarCounter.;
         data work.parameters_expand1;
            length segmentation_variables_needed
                   where_clause                    $ 1000;
            set perm.parameters_expand (firstobs=&targCnt. obs=&targCnt.);
            %**  For each segmentation variable, determine the subsetting required                      ;
            %if &numattr > 0 %then %do;
                array ar_attr{&numattr.} &attrlist.;
                do i = 1 to &numattr.;
            %**  For a segmentation variable that has the key word, 'COMBINED', ignore that segmentation variable ;
            %**  If segmentation is required, create the subsetting where clause which may go across multiple segmentation variables ;
                   if upcase(ar_attr{i}) ne 'COMBINED' then do;
                      segmentation_variables_needed = 
                      strip(segmentation_variables_needed) || ' ' || vname(ar_attr{i});
                      if where_clause = ' ' then where_clause =
                      'where ' || vname(ar_attr{i}) || "='" || trim(ar_attr{i}) || "'";
                      else where_clause = trim(where_clause) ||
                       ' and ' || vname(ar_attr{i}) || "='" || trim(ar_attr{i}) || "'";
                   end;
                end;
            %end;
            %**  Values retrieved from one row of expanded parameter table:                            ;
            %**    - Final HPREDUCE HP_Reduce_Stopping_Rule, and threshold                             ;
            %**      Acceptable values for HP_Reduce_Stopping_Rule:                                    ;
            %**      VarExp, MaxEffects, MINVARIANCEINCREMENT                                          ;
            %**    - WHERE clause for subsetting                                                       ;
            %**    - Target (dependent) variable                                                       ;
            %**    - "Repeat" (Y/N:  Can data subset from previous row be re-used for this row?)       ;
            %**    - Sample percent for modeling                                                       ;
            %**    - Event parameter                                                                   ;
            %**    - Parameters for each statistical model:                                            ;
            %**        > HPLOGISTIC 1                                                                  ;
            %**        > HPLOGISTIC 2                                                                  ;
            %**        > HPNEURAL 1                                                                    ;
            %**        > HPNEURAL 2                                                                    ;
            %**        > HPFOREST 1                                                                    ;
            %**        > HPFOREST 2                                                                    ;
            %**        > HPREG 1                                                                       ;
            %**        > HPREG 2                                                                       ;
            call symputx('dependent_variable', dependent_variable,'G');
            call symputx('HP_Reduce_Stopping_Rule', HP_Reduce_Stopping_Rule,'G');
            call symputx('HPReduce_Stopping_Rule_Threshold', HPReduce_Stopping_Rule_Threshold,'G');
            call symputx('segmentation_variables_needed',segmentation_variables_needed,'G');
            call symputx('where_clause',where_clause,'G'); /* Where clause from segmentation variables */
            call symputx("targetVar&targCnt",dependent_variable,'G');
            call symputx("repeat",repeat,'G');
            call symputx("Training_samppct",Training_samppct,'G');
            call symputx("Event",Event,'G');
            call symputx("HPLogistic_method_1",HPLogistic_method_1,'G'); /* Parameters for HPLOGISTIC 1 */
            call symputx("HPLogistic_method_2",HPLogistic_method_2,'G'); /* Parameters for HPLOGISTIC 2 */
            call symputx("HPNeural_method_1",HPNeural_method_1,'G'); /* Parameters for HPNEURAL 1*/
            call symputx("HPNeural_method_2",HPNeural_method_2,'G'); /* Parameters for HPNEURAL 2*/
            call symputx("HPForest_method_1",HPForest_method_1,'G'); /* Parameters for HPFOREST 1*/
            call symputx("HPForest_method_2",HPForest_method_2,'G'); /* Parameters for HPFOREST 1*/
            call symputx("HPREG_method_1",HPREG_method_1,'G'); /* Parameters for HPREG 1*/
            call symputx("HPREG_method_2",HPREG_method_2,'G'); /* Parameters for HPREG 1*/
            drop i;
         run;	

         %put WHERE clause is:  &where_clause.;

         %**  Parsing for HPNEURAL parameters                                                           ;
         %do i=1 %to 2;
             %let architecture_&i.= %scan(%scan(&&HPNeural_method_&i.,1,'|'),2,'=');
             %let hidden_&i.= %scan(%scan(&&HPNeural_method_&i.,2,'|'),2,'=');
             %let numtries_&i.= %scan(%scan(&&HPNeural_method_&i.,3,'|'),2,'=');
             %let maxiter_&i.= %scan(%scan(&&HPNeural_method_&i.,4,'|'),2,'=');
             %let weight_&i.= %scan(%scan(&&HPNeural_method_&i.,5,'|'),2,'=');

             %put arch: &&architecture_&i.;
             %put hidden: &&hidden_&i. ;
             %put numtries: &&numtries_&i.;
             %put maxiter: &&maxiter_&i.;
             %put weight: &&weight_&i. ;
         %end;

         %**  Delete existing SAS datasets that need to be used in this Model Factory run               ;
         %delete_sasdata_iff (perm.vcp_unsup_targ&targCnt)
         %delete_sasdata_iff (perm.vcp_sup_targ&targCnt.)
         ;
         %**  Classify target variable as binary, continuous, categorical, or ordinal                  ;
         proc sql;
            SELECT COUNT(DISTINCT &&targetVar&targCnt) into :targ_freq FROM gplib.&dep_table.;
            SELECT type into :targ_type FROM _contents_dep_table where upcase(name) = upcase("&&targetVar&targCnt");
         quit;

         OPTIONS NOMLOGIC NOSYMBOLGEN;

         %**  For the Target variable, Determine its frequency, type, and class for use in modeling      ;
         %put Frequency of Target variable &&targetVar&targCnt is &targ_freq.;
         %put Type of Target variable &&targetVar&targCnt is &targ_type.;
         %if (&targ_freq. = 2 and &targ_type. = 1) %then %do;
             %let targ_class  = binary numeric;
             %let model_selection = _VMISC_;
         %end;
         %else %if (&targ_freq. = 2 and &targ_type. = 2) %then %do;
             %let targ_class  = binary categorical;
             %let model_selection = _VMISC_;
         %end;
         %else %if (&targ_freq. > 2 and &targ_freq. < 10 and  &targ_type. = 1) %then %DO;
             %let targ_class  = ordinal;
             %let model_selection = _VMISC_;
         %end;
         %else %if (&targ_freq. >=  10 and  &targ_type. = 1) %then %DO;
             %let targ_class  =interval;
             %let model_selection = _VASE_;
         %end;
         %else %if (&targ_freq. > 2 and &targ_type. =2) %then %DO;
             %let targ_class  = categorical;
             %let model_selection = _VMISC_;
         %end;
         %put Class of Target variable &&targetVar&targCnt is &targ_class.;

         %**  For each target variable, loop through the tables of independent variables.              ;
         %**  Process each table, reducing each set of independent variables.                          ;
         %**  Both supervised and unsupervised variable reduction will be used                         ;
         %**  BEGIN:  PROCESS EACH INDEPENDENT TABLE                                                   ;
         %do tabCnt = 1 %to &tableCounter;
             %**  Create a new subset of independents using a two-step process, IF the where-clause    ;
             %**  has changed so that the selected records will change:                                ;
             %**     - select just the records required by the new WHERE clause                        ;
             %**     - test the CSS for independents in that subset, removing any that have bad        ;
             %**       CSS values within the subset.                                                   ;
             %**  "Remove" in this sense does not change the data.  Rather, it changes a macro         ;
             %**  variable holding a list of independents that will be processed further.              ;
             %if &repeat. = N %then %do; 
                 %drop_table_iff(&&subtable&tabCnt)
                 %drop_table_iff(&&subschema&tabCnt)
                 proc sql;
                    Connect to Greenplm (server=&server user=&user password=&pwd
                                         database=&database schema="&sschema" );
		            execute (set gp_autostats_mode='NONE') by Greenplm;
                    execute ( create table &&subschema&tabCnt with (appendonly=true, compresstype=QUICKLZ, OIDS=FALSE) as
                              select a.* from &&schema&tabCnt as a
                    &where_clause. DISTRIBUTED BY (&idVar.))by greenplm;
		 execute (analyze &&schema&tabCnt (&idVar.)) by Greenplm;
                 quit;
                 %**  Test CSS for this subset.                                                        ;
                 %**  TEMPORARY PITFALL:  There is only one LIBNAME statement, which is used as part   ;
                 %**  of the SUBTABLE macro variables.  However, the code above writes to the user     ;
                 %**  schema, which is used as part of the SUBSCHEMA macro variables.  And both        ;
                 %**  locations refer to the same schema.  So it is correct to switch from referring   ;
                 %**  to SUBSCHEMA to SUBTABLE as the HPDMDB input data.                               ;
                 proc hpdmdb data=&&subtable&tabCnt varout=summary_stats (keep=name css);
                    var &&numeric_indeps_kept_&tabCnt.;
                    &PERFORMANCE_STATEMENT. ;
                 run;
                 %local numeric_indeps_final_list_&tabCnt.;
                 proc sql noprint;
                    select strip(name) into : numeric_indeps_final_list_&tabCnt. separated by ' '
                           from summary_stats where (0 < css < 1.0e+30);
                 quit;
             %end; 
             %**  Supervised variable reduction                                                        ;
             %**   Investigating the relationship of each independent variable with the target         ;
             %**   variable Using correlation between the two                                          ;
             %**  The top 1550 variables from all the Supervised reduction will be used                ;
             title "Analysis row &tabCnt of &tableCounter, dependent is &&targetVar&targCnt";
             title3 "Now processing &&subtable&tabCnt ...";

             proc hpreduce data=&&subtable&tabCnt outcp=perm.cp_sup_table&tabCnt._targ&targCnt;
                &PERFORMANCE_STATEMENT. ;
                reduce supervised  &&targetVar&targCnt = &&numeric_indeps_final_list_&tabCnt.. / 
                                   varexp = &varExpRedVal;
                ods table selectionsummary=perm.sel_sup_table&tabCnt._targ&targCnt; 
             run;
             data vcp_sup_table&tabCnt._targ&targCnt;
                length table $32;
                set perm.cp_sup_table&tabCnt._targ&targCnt;
                where _type_ = 'CORR' and upcase(_var_) ^= upcase("&&targetVar&targCnt");
                keep table_num table _var_ v1 vv1;
                vv1 = abs(v1);
                table = "&&table&tabCnt";
                table_num = &tabCnt;
             run;
             %**  Sort the results by descending correlation for each independent variable with the target variable ;
             proc sort data=vcp_sup_table&tabCnt._targ&targCnt;
                by descending vv1;
             run;
    	      proc sql;
                create table vcp_sel_sup_table&tabCnt._targ&targCnt as select 'S' as Super, a.*,b.* 
                            from perm.sel_sup_table&tabCnt._targ&targCnt a 
                            left join vcp_sup_table&tabCnt._targ&targCnt b
                            on variable = _var_ order by VarExp;
             quit;
options &original_settings;

             %**  The reduce_summary macro produces a report of the variable reduction and creates needed macro variables ;
             %reduce_summary(perm.sel_sup_table&tabCnt._targ&targCnt)
             %let n_reduce_vars_tab&tabCnt._targ&targCnt = &n_reduce_vars;
             %let reduce_vars_tab&tabCnt._targ&targCnt = &reduce_vars;
             %let reduce_vars_comma_tab&tabCnt._targ&targCnt = &reduce_vars_comma;
             %let n_reduce_class_tab&tabCnt._targ&targCnt = &n_reduce_class;
             %let reduce_class_tab&tabCnt._targ&targCnt = &reduce_class;
             %let reduce_class_comma_tab&tabCnt._targ&targCnt = &reduce_class_comma;
             %put NOTE: REDUCE VARS&tabcnt.: perm.sel_sup_table&tabCnt._targ&targCnt PRODUCES
                        &&n_reduce_vars_tab&tabCnt._targ&targCnt :
                        &&reduce_vars_tab&tabCnt._targ&targCnt &&reduce_vars_comma_tab&tabCnt._targ&targCnt ;
             %put NOTE: REDUCE perm.sel_sup_table&tabCnt._targ&targCnt PRODUCES
                        &&n_reduce_class_tab&tabCnt._targ&targCnt :
                        &&reduce_class_tab&tabCnt._targ&targCnt &&reduce_class_comma_tab&tabCnt._targ&targCnt;

             %**  Unsupervised variable reduction                                                       ;
             %**   Investigating the relationship of each independent variable with respect to the      ;
             %**   Total Variance Using correlation                                                     ;
             %**  The variables from the Unsupervised reduction will be used for interactions only      ;
options MPRINT;

             proc hpreduce data=&&subtable&tabCnt outcp=perm.cp_unsup_table&tabCnt._targ&targCnt;
                &PERFORMANCE_STATEMENT. ;
                reduce unsupervised &&numeric_indeps_final_list_&tabCnt.. / 
                                    varexp = &unSupVarExpRedVal;* maxeffects=300;
                ods table selectionsummary=perm.sel_unsup_table&tabCnt._targ&targCnt; 
             run;
             data perm.sel_unsup_table&tabCnt._targ&targCnt;
                length table $32;
                set perm.sel_unsup_table&tabCnt._targ&targCnt;
                alag = lag(varexp); drop alag;
                if varexp > 1 or varexp = . then varexp = alag;
                Ivarexp = varexp - lag(varexp);
                if _n_ = 1 then Ivarexp = varexp;
                table = "&&table&tabCnt";
                table_num = &tabCnt;
             run;
             %**  Identify all independent variables in unsupervised not in supervised                 ;
             %**  The only independent variables from unsupervised that are of interest are those      ;
			 %**   from the not in the supervised list                 ;
             proc sql;
                create table sel_unsup_notsup&tabCnt._targ&targCnt as select 'U' as Super, a.* 
                       from perm.sel_unsup_table&tabCnt._targ&targCnt a 
                       where variable not in (select variable from vcp_sel_sup_table&tabCnt._targ&targCnt)
                       order by varexp;
             quit;
             %**  END:  PROCESS EACH INDEPENDENT TABLE                                                 ;
options &original_settings;

         %end; /* End of processing each independent table */
         %**  Put all tables of unsupervised together                                                  ;
         data perm.vcp_unsup_targ&targCnt;
            LENGTH variable $ 32;
            rename variable = _var_;
            set  %do tabCnt = 1 %to &tableCounter;
                     sel_unsup_notsup&tabCnt._targ&targCnt
                 %end;
                 ;
         run;
         %**  Put all tables of supervised together                                                    ;
         data perm.vcp_sup_targ&targCnt;
            LENGTH _var_ $ 32;
            set  %do tabCnt = 1 %to &tableCounter;
                     vcp_sup_table&tabCnt._targ&targCnt
                 %end;
                 ;
         run;
         %**  Sort the combined unsupervised results by variable name and then descending variance explained   ;
         %**    Removing the duplicate names                                                                   ;
         proc sort data=perm.vcp_unsup_targ&targCnt;
            by _var_ descending Ivarexp; /* variance explained */
         run;
		 data perm.vcp_unsup_targ&targCnt; set perm.vcp_unsup_targ&targCnt;
            by _var_; /* variable */
			if first._var_;
		 run;
         %**  Sort the resulting combined unsupervised results by descending variance explained                ;
         proc sort data=perm.vcp_unsup_targ&targCnt;
            by descending Ivarexp; /* variance explained */
         run;
         %**  Sort the combined supervised results by variable name and then descending correlation            ;
         %**    Removing the duplicate names                                                                   ;
         proc sort data=perm.vcp_sup_targ&targCnt;
            by _var_ descending vv1; /* correlation */
         run;
		 data perm.vcp_sup_targ&targCnt; set perm.vcp_sup_targ&targCnt;
            by _var_; /* variable */
			if first._var_;
		 run;
         %**  Sort the combined supervised results by descending correlation                                   ;
         proc sort data=perm.vcp_sup_targ&targCnt;
            by descending vv1; /* correlation */
         run;
         %**  Supervised:  Take top 1,550 variables                                                    ;
         data perm.svcp_sup_targ&targCnt;
            set perm.vcp_sup_targ&targCnt (obs=1550);
         run;
         %**  Supervised:  Take top 20 variables                                                       ;
         data perm.svcp_sup_targ_20_&targCnt; 
            set perm.vcp_sup_targ&targCnt (obs=20);
         run;
         %**  Unsupervised:  Take top 20 variables                                                     ;
         data perm.svcp_unsup_targ&targCnt; 
            set perm.vcp_unsup_targ&targCnt (obs=20);   
         run;
         %**  Top 1,550 supervised, plus top 20 unsupervised                                           ;
         data perm.svcp_unsup_sup_targ&targCnt;
            set perm.svcp_sup_targ&targCnt perm.svcp_unsup_targ&targCnt;
            keep tab_var _var_ table table_num v1 vv1 varexp ivarexp;
			tab_var = 'a' !! strip(table_num) !! '.' !! _var_;
         run;
         proc sort data=perm.svcp_unsup_sup_targ&targCnt nodupkey;
            by _var_; /* alphabetic */
         run;
         proc sort data=perm.svcp_unsup_sup_targ&targCnt;
            by descending vv1; /* correlation */
         run;

         %**  Create list of variables for SQL and HPREDUCE                                            ;
         proc sql;
            select tab_var into :tab_var_name separated by ',' from perm.svcp_unsup_sup_targ&targCnt;
            select _var_ into :var_name separated by ',' from perm.svcp_unsup_sup_targ&targCnt;
            select _var_ into :hpvar_name separated by ' ' from perm.svcp_unsup_sup_targ&targCnt;
            select count(*) into :count from perm.svcp_unsup_sup_targ&targCnt;
         %**  Interaction terms:  unsupervised * supervised, and unsupervised * unsupervised           ;
            select _var_ into :sup_var_name separated by ' ' from perm.svcp_sup_targ_20_&targCnt;
            select _var_ into :unsup_var_name separated by ' ' from perm.svcp_unsup_targ&targCnt;
         quit;
         %put tab_var_name: &tab_var_name.;
         %put var_name: &var_name.;
         %put hpvar_name: &hpvar_name.;
         %put count: &count.;
         %put sup_var_name: &sup_var_name.;
         %put unsup_var_name: &unsup_var_name.;

         %**  Create pairs of variables that will form interaction terms.                                       ;
         %**  To guard against duplicates, NAME1 will be alphabetically closer to "A" than NAME2                ;
         %**  and NAME2 will be alphabetically closer to "Z" than NAME1.                                        ;
         %**  The variable names for NAME1 and NAME2 will be derived from the names of the top 20               ;
         %**   supervised and the top 20 unsupervised variables                                                 ;
         data pairs (keep=name1 name2);
            length name1 name2 $ 32;
            %** Take pairs involving top 20 UNsupervised vs. top 20 supervised.                                 ;
            length unsup_varname sup_varname $ 32;
            do i = 1 to 20;
               unsup_varname = upcase(scan("&unsup_var_name.",i));
               if (unsup_varname > ' ') then do j = 1 to 20;
                  sup_varname = upcase(scan("&sup_var_name.",j));
                  if sup_varname < unsup_varname then do;
                     name1 = sup_varname;
                     name2 = unsup_varname;
                     output;
                  end;
                  else if unsup_varname < sup_varname then do;
                     name1 = unsup_varname;
                     name2 = sup_varname;
                     output;
                  end;
                  %** Automatically skips cases where sup_varname = unsup_varname.                        ;
               end;
            end;
            %** Take pairs involving top 20 UNsupervised vs. top 20 UNsupervised.                         ;
            length unsup_varname1 unsup_varname2 $ 32;
            do i = 1 to 19;
               unsup_varname1 = upcase(scan("&unsup_var_name.",i));
               if (unsup_varname1 > ' ') then do j = i+1 to 20;
                  unsup_varname2 = upcase(scan("&unsup_var_name.",j));
                  if unsup_varname2 > ' ' then do;
                     if unsup_varname1 < unsup_varname2 then do;
                        name1 = unsup_varname1;
                        name2 = unsup_varname2;
                        output;
                     end;
                     else do;
                        name1 = unsup_varname2;
                        name2 = unsup_varname1;
                        output;
                     end;
                  end;
               end;
            end;
         run;
         proc sort data=pairs NODUPKEY;
            by name1 name2;
         run;
         %local interactions1 interactions2;
         %let interactions1=;
         %let interactions2=;
         %**  Put the interaction terms into macro variables called interactions1 and interactions2      ;
         data _null_;
            length string1 string2 $ 30000;
            retain string1 string2;
            set pairs end=done;
            if length(string1) < 29030 then
            string1 = strip(string1) || ' ' || strip(name1) || '*' || strip(name2);
            else
            string2 = strip(string2) || ' ' || strip(name1) || '*' || strip(name2);
            if done;
            call symputx('interactions1', string1);
            call symputx('interactions2', string2);
         run;
         %put INTERACTIONS SET #1:  &interactions1;
         %put INTERACTIONS SET #2:  &interactions2;
         %let lt1tableCounter = %eval(&tableCounter - 1);
         %**  Create greenplum table of supervised and unsupervised variables needed for analysis         ;
         %**  If there are more than 20 tables of independent variables, then two steps will be required  ;
         %**    to combine the 1550 supervised and 20 unsupervised variables together                     ;
         options dsnferr;

         %**  Remove previously defined greenplum tables of supervised and unsupervised variables needed for analysis ;
         %drop_table_iff (gplib.ALLdri_targ&targCnt._new)
         %drop_table_iff (&sschema..ALLdri_targ&targCnt._new);

         %**    Less than or equal to 20 tables to join together                                          ;
	%if (&tableCounter <= 20) %then %do; 
         proc sql;
            connect to Greenplm (server=&server user=&user password=&pwd
                                 database=&database schema="&sschema" );
            execute (set gp_autostats_mode='NONE') by Greenplm;
            execute ( create table &sschema..ALLdri_targ&targCnt._new 
                      with (appendonly=true, compresstype=QUICKLZ, OIDS=FALSE) as
                      select a1.&idVar., a1.&&targetVar&targCnt, &tab_var_name. 
                        from
                             %do tabCnt = 1 %to &lt1tableCounter;
                                 &&subschema&tabCnt. a&tabCnt.,
                             %end;
                                 &&subschema&tableCounter. a&tableCounter. 
                        where
                              %do tabCnt = 2 %to &lt1tableCounter;
                                  a1.&idVar. = a&tabCnt..&idVar. and
                              %end;
                                  a1.&idVar. = a&tableCounter..&idVar. DISTRIBUTED BY (&idVar.)) by greenplm
                       ;
		 execute (analyze &sschema..ALLdri_targ&targCnt._new (&idVar.)) by Greenplm;
         quit;
	%end;
         %**    Greater than 20 tables to join together                                                    ;
	%else %do;
         %**  Create 10 empty macro variables to hold the 10 lists of variables in the first join          ;
           %do i = 1 %to 10;
	           %let gvar&i = ;
	           %let tab_gvar&i = ;
	       %end;
         %**  Prepare for first joining by creating the list of variables to be put in each GP table            ;
		 proc sql;
		    %let num_group = %eval((&tableCounter-1)/20 + 1);
		     %put &num_group;
		     %do i = 1 %to &num_group.;
		      %let strt = %eval((&i-1)*20+1);
		      %let stop = %eval(&i*20);
		         select _var_ into :gvar&i separated by ',' from perm.svcp_unsup_sup_targ&targCnt 
                        where table_num>=&strt. and table_num<=&stop.;
			  %put &i &strt &stop &&gvar&i;
		         select tab_var into :tab_gvar&i separated by ',' from perm.svcp_unsup_sup_targ&targCnt 
                        where table_num>=&strt. and table_num<=&stop.;
			  %put &i &strt &stop &&tab_gvar&i;
			 %end;
		 quit;
         %**  Perform joining of tables in groups of 20 or less up to 200 total tables                          ;
	       %do i = 1 %to &num_group.;
		            %let strt = %eval((&i-1)*20+1);
		            %let strt1 = %eval((&i-1)*20+2);
		            %let stop = %eval(&i*20-1);
		            %let last = %eval(&i*20);
			        %if (&i. = &num_group.) %then %do;
		               %let stop = %eval(&tableCounter-1);
		               %let last = &tableCounter;
			        %end;
             proc sql;
                connect to Greenplm (server=&server user=&user password=&pwd
                                     database=&database schema="&sschema" );
                execute (set gp_autostats_mode='NONE') by Greenplm;
                execute ( create table &sschema..ALLdri_targ&targCnt._new&i. 
                      with (appendonly=true, compresstype=QUICKLZ, OIDS=FALSE) as
				  %if &&gvar&i. = %then %do;
                         select a&strt..&idVar., a&strt..&&targetVar&targCnt
                  %end;
				  %else %do;
                         select a&strt..&idVar., a&strt..&&targetVar&targCnt, &&tab_gvar&i. 
                  %end;
		                from
		                  %do tabCnt = &strt. %to &stop.;
		                      &&subschema&tabCnt. a&tabCnt.,
		                  %end;
		                      &&subschema&last. a&last.
		                where
		                   %do tabCnt = &strt1. %to &stop.;
		                      a&strt..&idVar. = a&tabCnt..&idVar. and
		                   %end;
		                      a&strt..&idVar. = a&last..&idVar. DISTRIBUTED BY (&idVar.)) by greenplm
		              ;
          		execute (analyze &sschema..ALLdri_targ&targCnt._new&i. (&idVar.)) by Greenplm;
		     quit;
	       %end;
         %**  Perform joining of the up to 20 subtables into the final GP table for analysis                    ;
	       %let num_group1 = %eval(&num_group.-1);
		   proc sql;
                connect to Greenplm (server=&server user=&user password=&pwd
                                     database=&database schema="&sschema" );
                execute (set gp_autostats_mode='NONE') by Greenplm;
                execute ( create table &sschema..ALLdri_targ&targCnt._new 
                      with (appendonly=true, compresstype=QUICKLZ, OIDS=FALSE) as
                        select a1.&idVar., a1.&&targetVar&targCnt, &var_name. 
		              from
		                %do i = 1 %to &num_group1.;
                            &sschema..ALLdri_targ&targCnt._new&i. a&i.,
		                %end;
                            &sschema..ALLdri_targ&targCnt._new&num_group. a&num_group. 
		              where
		                 %do i = 2 %to &num_group1.;
		                    a1.&idVar. = a&i..&idVar. and
		                 %end;
		                    a1.&idVar. = a&num_group..&idVar. DISTRIBUTED BY (&idVar.)) by greenplm
		            ;
		     execute (analyze &sschema..ALLdri_targ&targCnt._new (&idVar.)) by Greenplm;
           quit;
         %**  Delete temporary GP tables used above                                                             ;
		     %do i = 1 %to &num_group.;
				 %drop_table_iff (gplib.ALLdri_targ&targCnt._new&i.)
		         %drop_table_iff (&sschema..ALLdri_targ&targCnt._new&i.);
			 %end;
	%end;

         %**  After each table of independent variables has been examined and the final 1550 supervised         ;
         %**   and top interactions from all tables have been found,                                            ;
         %**   A final HPREDUCE is run to determine the top variables and interactions that will be passed to   ;
         %**   the modeling steps.A final HPREDUCE is run to determine the top variables and interactions that will be passed to   ;

         %**  Perform supervised variable reduction (like varselect).                                           ;
         %**  This step uses the top 1550 main efffects from all the tables, plus                               ;
         %**  the interactions from the top 20 from each of the supervised and unsupervised steps.              ;
         %**  The interactions are the unsupervised by unsupervised, and unsupervised by supervised.            ;
         proc hpreduce data=gplib.ALLdri_targ&targCnt._new outcp=perm.cp_sup_targ&targCnt;
            &PERFORMANCE_STATEMENT. ;
            *class &&targetVar&targCnt / missing; 
            reduce supervised  &&targetVar&targCnt = &hpvar_name &interactions1 &interactions2 /
                   %if %upcase(&HP_Reduce_Stopping_Rule.) = MINVARIANCEINCREMENT %then %do;
                       &HP_Reduce_Stopping_Rule. = &HPReduce_Stopping_Rule_Threshold.;
                   %end;
                   %else %do;
                       &HP_Reduce_Stopping_Rule. = &HPReduce_Stopping_Rule_Threshold.
                       MINVARIANCEINCREMENT = .0001; * hard coded;
                   %end;
                   ods table selectionsummary=perm.sel_sup_targ&targCnt;
         run;
         %**  Include a maximum of 75 effects.                                                                 ;
         data perm.max_sel_sup_targ&targCnt; 
            set perm.sel_sup_targ&targCnt(obs=75); 
         run;
         %**  Separate interaction effects, drop * between variables                                           ;
         data perm.vsel_sup_targ&targCnt._only; 
            set perm.max_sel_sup_targ&targCnt;
            length variable_a $32;
            variable_a = 'wwwww';
            rename variable_a=variable; 
            drop variable;
            if scan(variable,2,'*') = ' ' then do; 
               variable_a=variable; 
               output; 
            end;
            else do;
               variable_a=scan(variable,1,'*'); output;
               variable_a=scan(variable,2,'*'); output;
            end;
         run;

         %**  Create macro variable of top variables from hpreduce - NO interactions terms, but using all       ;
         %**   variables including those from the interactions terms                                            ;
         proc sort data= perm.vsel_sup_targ&targCnt._only nodupkey;
            by variable;
         run;
         proc sql;
            select variable into :pred_var_name_only separated by ' ' 
              from perm.vsel_sup_targ&targCnt._only
              ;
         quit;

         %**  Create macro variable of top variables from hpreduce                                            ;
         proc sql;
            select variable into :pred_var_name separated by ' ' from perm.max_sel_sup_targ&targCnt;
         quit;
         %put pred_var_name: &pred_var_name.;
         %put pred_var_name_only: &pred_var_name_only.;

           %**  Creating partition using HPSAMPLE based on Training_samppct given in Parameter Table            ;
         %drop_table_iff (gplib.Part_ALLdri_targ&targCnt.)
         proc hpsample data=gplib.ALLdri_targ&targCnt._new (keep = &idVar &&targetVar&targCnt &pred_var_name_only)
                       out=gplib.Part_ALLdri_targ&targCnt.
                       partition
                       samppct=&Training_samppct 
                       seed=76576587;
            &PERFORMANCE_STATEMENT. ;
            class  &idVar 
                   %if %upcase(%scan(&targ_class.,1)) = BINARY %then %do;
                       &&targetVar&targCnt
                   %end;
                   ;
            %if %upcase(%scan(&targ_class.,1)) = BINARY %then %do;
                target &&targetVar&targCnt;
            %end;
            var  %if %upcase(%scan(&targ_class.,1)) ^= BINARY %then %do;
                     &&targetVar&targCnt
                 %end;
                 &pred_var_name_only.;
         run;

         %**  Models created are based on target variable type.                                               ;
         %**    Binary Numeric - HPLOGISTIC  HPFOREST  HPNEURAL                                               ;
         %**    Binary Categorical - HPLOGISTIC  HPFOREST  HPNEURAL                                           ;
         %**    Ordinal - HPLOGISTIC HPNEURAL                                                                 ; 
         %**    Interval - HPREG HPFOREST  HPNEURAL                                                           ;   
         %**    Categorical - HPLOGISTIC(LINK=GLOGIT) HPFOREST HPNEURAL                                       ; 	

         %**  Depending on the class of the target variable, create needed macro variables to use in the      ;
         %**   modeling algorithms                                                                            ;
         %if &targ_class =binary numeric %then %do;
             %let DO_HP_LOGISTIC = Y;
             %let DO_RESPONSE_OPTION= (DESC);
             %let DO_LINK= ;
             %let DO_ASSO= /ASSOCIATION;
             %let DO_HP_NEURAL = Y;
             %let DO_TARG_LEVEL= NOM;
             %let DO_HP_FOREST = Y;
             %let DO_HP_REG = N;
             %let DO_LEVEL= BINARY;
             %let c_stat=Y;
             %let DO_TYPE=CLASSIFICATION;
         %end;

         %else %if &targ_class =binary categorical %then %do;
             %let DO_HP_LOGISTIC = Y;
             %let DO_RESPONSE_OPTION= (DESC);
             %let DO_LINK= ;
             %let DO_ASSO= /ASSOCIATION;
             %let DO_HP_NEURAL = Y;
             %let DO_TARG_LEVEL= NOM;
             %let DO_HP_FOREST = Y;
             %let DO_HP_REG = N;
             %let DO_LEVEL= BINARY;
             %let c_stat=Y;
             %let DO_TYPE=CLASSIFICATION;
         %end;

         %else %if &targ_class =ordinal %then %do;
             %let DO_HP_LOGISTIC = Y;
             %let DO_RESPONSE_OPTION= ;
             %let DO_LINK= ;
             %let DO_ASSO= ;
             %let DO_HP_NEURAL = Y;
             %let DO_TARG_LEVEL= NOM;
             %let DO_HP_FOREST = N;
             %let DO_HP_REG = N;
             %let DO_LEVEL= ORDINAL;
             %let EVENT =;
             %let c_stat=N;
             %let DO_TYPE=CLASSIFICATION;
         %end;

         %else %if &targ_class =interval %then %do;
             %let DO_HP_LOGISTIC = N;
             %let DO_RESPONSE_OPTION= ;
             %let DO_LINK= ;
             %let DO_ASSO= ;
             %let DO_HP_NEURAL = Y;
             %let DO_TARG_LEVEL= INT;
             %let DO_HP_FOREST = Y;
             %let DO_HP_REG = Y;
             %let DO_LEVEL= INTERVAL;
             %let EVENT =;
             %let c_stat=N;
             %let DO_TYPE=PREDICTION;
         %end;

         %else %if &targ_class =categorical %then %do;
             %let DO_HP_LOGISTIC = Y;
             %let DO_RESPONSE_OPTION= ;
             %let DO_LINK= /LINK=GLOGIT;
             %let DO_ASSO= ; 
             %let DO_HP_NEURAL = Y;
             %let DO_TARG_LEVEL= NOM;
             %let DO_HP_FOREST = Y;
             %let DO_HP_REG = N;
             %let DO_LEVEL= NOMINAL;
             %let EVENT =;
             %let c_stat=N;
             %let DO_TYPE=CLASSIFICATION;
         %end;

         %**  There are 4 Model Types depending on the class of the target variable                        ;
         %**   #1:  PROC HPLOGISTIC                                                                        ;
         %**   #2:  PROC HP_NEURAL                                                                         ;
         %**   #3:  PROC HP_FOREST                                                                         ;
         %**   #4:  PROC HP_REG                                                                            ; 

         %**  Model Type #1:  PROC HPLOGISTIC                                                              ;
         %**  _PARTIND_ =1 corresponds to TRAINING, _PARTIND_=0 corresponds to VALIDATION                  ;
         %**  Estimate logistic regression on the TRAINING sample                                          ;
         %if &DO_HP_LOGISTIC.= Y %then %do;
             %local k;
             %let HPLogistic_used_1 = N;
             %let HPLogistic_used_2 = N;
             %do k= 1 %to 2; * Start of HPLOGISTIC iterations;
                 %** Check whether HPLOGISTIC_method parameter is blank                                    ;
                 %if "&&HPLogistic_method_&k." ^= "" %then %do;
                     %let HPLogistic_used_&k. = Y;

                     title "PROC HPLOGISTIC for &&targetVar&targCnt. (&targ_class. dependent variable) using Selection method= &&HPLogistic_method_&k.";
                     proc hplogistic data=gplib.Part_ALLdri_targ&targCnt. (where = (_PARTIND_ = 1)) 
                                                                              noclprint noitprint nostderr ;
                        &PERFORMANCE_STATEMENT. ;
                        id &idVar;
                        class &idVar &&targetVar&targCnt.;
                        selection method=&&HPLogistic_method_&k.;
                        model &&targetVar&targCnt. &DO_RESPONSE_OPTION. = &pred_var_name. &do_link. &DO_ASSO. ;
                        ods output parameterestimates=perm.est_HPLog_&k._targ&targCnt.
                                   modelinfo=info_HPLog_&k._targ&targCnt. 
                                   %if "&DO_ASSO." ^= "" %then %do;
                                       association=Asso&k._targ&targCnt.
                                   %end; 
                        ;
                        output pred=pred_HPLog_&k._targ&targCnt.;
                        code file = "&rootPath/HL_&k._targ&targCnt..sas";
                     run;
                     title;

                     %** DS TRANS code can run on GP                                                       ;
                     filename _outds2 "DS2_HL_&k._targ&targCnt..sas";
                     proc dstrans ds_to_ds2
                        in = "&rootPath/HL_&k._targ&targCnt..sas"
                        out = _outds2
                        outdir = "&rootPath/"
                        aster
                        nocomp;
                     run;
                     quit;
                     filename _outds2;

                     %drop_table_iff (gplib.HL_&k._scores_targ&targCnt.)

                     %** Score Training and Validation samples using hplogistic                            ;
                     proc hpds2 data=gplib.Part_ALLdri_targ&targCnt.
                             out=gplib.HL_&k._scores_targ&targCnt.;
                             %include "&rootPath/DS2_HL_&k._targ&targCnt..sas";
                             &PERFORMANCE_STATEMENT. ;
                          run;

                     %** Produce plots and tables of scored data                                           ;
                     %delete_sasdata_iff (perm.bins_HLog_&k._targ&targCnt.)
                     %delete_sasdata_iff (perm.binstat_HLog_&k._targ&targCnt.)
                     %delete_sasdata_iff (perm.expand_HLog_&k._targ&targCnt.)
                     %aamodel
                     %aa_model_eval (DATA = gplib.HL_&k._scores_targ&targCnt.,
                                     TARGET = &&targetVar&targCnt.,        
                                     VAR =  p_&&targetVar&targCnt..&Event., 
                                     LEVEL =  &DO_LEVEL., 
                                     EVENT= &Event.,
                                     OUT = perm.bins_HLog_&k._targ&targCnt.,
                                     BINSTATS=perm.binstat_HLog_&k._targ&targCnt.,
                                     BINS = 20,
                                     HPDS2 = Y,
                                     EXPAND= perm.expand_HLog_&k._targ&targCnt. ,
                                     partitionvar=_partind_)
                     %em_new_report_JW (bins=perm.bins_HLog_&k._targ&targCnt., 
                                        binstats=perm.binstat_HLog_&k._targ&targCnt., 
                                        expand=perm.expand_HLog_&k._targ&targCnt.)
                 %end;
             %end; 
         %end; 


         %**  Model Type #2:  HP NEURAL                                                                    ;
         %if &DO_HP_NEURAL.=Y %then %do;
             %let HPNeural_used_1 = N;
             %let HPNeural_used_2 = N;
             %do k= 1 %to 2;
                 %** Check whether HPNEURAL method parameter is blank                                      ;
                 %if &&HPNeural_method_&k.^= %then %do; 
                     %let HPNeural_used_&k = Y;
                     
                     title "PROC HPNEURAL for &&targetVar&targCnt. (&targ_class. dependent variable)";
                     proc hpneural data=gplib.Part_ALLdri_targ&targCnt. distr=ALL;
                        architecture &&architecture_&k. ;
                        id &idVar;
                        input &pred_var_name_only. / level=int;
                        target &&targetVar&targCnt /level=&DO_TARG_LEVEL.;
                        hidden &&hidden_&k.;
                        code file="&rootPath/HN_&k._targ&targCnt..sas";
                        partition rolevar=_Partind_(train=1);
                        train numtries=&&numtries_&k. maxiter=&&maxiter_&k. 
                              outmodel=HPNEURAL_&k._targ&targCnt._outmodel ;
                       *weight =&weight.; * Not used since data provided did not have a weight variable;
                       &PERFORMANCE_STATEMENT. ;
                     run;
                     title;

                     %**  DS TRANS code so it can be run on GP                                             ;
                     filename _outds2 "DS2_HN_&k._targ&targCnt..sas";
                     proc dstrans ds_to_ds2
                        in = "&rootPath/HN_&k._targ&targCnt..sas"
                        out = _outds2
                        outdir = "&rootPath/"
                        aster
                        nocomp;
                     run;
                     quit;
                      filename _outds2;

                      %drop_table_iff (gplib.HN_&k._scores_targ&targCnt)

                      %**  Score Training and Validation samples logistic regression                        ;
                      proc hpds2 data=gplib.Part_ALLdri_targ&targCnt.
                         out=gplib.HN_&k._scores_targ&targCnt;
                         %include "&rootPath/DS2_HN_&k._targ&targCnt..sas";
                         &PERFORMANCE_STATEMENT. ;
                      run;

                      %**  Produce plots and tables of scored data                                          ;
                      %delete_sasdata_iff (perm.bins_HNN_&k._targ&targCnt.)
                      %delete_sasdata_iff (perm.binstat_HNN_&k._targ&targCnt.)
                      %delete_sasdata_iff (perm.expand_HNN_&k._targ&targCnt.)
                      %aamodel
                      %aa_model_eval (DATA = gplib.HN_&k._scores_targ&targCnt.,
                                      TARGET = &&targetVar&targCnt.,      
                                      VAR =  p_&&targetVar&targCnt..&Event., 
                                      LEVEL =  &DO_LEVEL, 
                                      EVENT=&Event.,
                                      OUT = perm.bins_HNN_&k._targ&targCnt.,
                                      BINSTATS=perm.binstat_HNN_&k._targ&targCnt.,
                                      BINS = 20,
                                      HPDS2 = Y,
                                      EXPAND= perm.expand_HNN_&k._targ&targCnt.,
                                      partitionvar=_partind_)
                      %em_new_report_JW (bins=perm.bins_HNN_&k._targ&targCnt., 
                                         binstats=perm.binstat_HNN_&k._targ&targCnt., 
                                         expand=perm.expand_HNN_&k._targ&targCnt.)
                 %end;
             %end;
         %end;


         %**  Model Type #3:  PROC HPFOREST                                                                ;
         %if &DO_HP_FOREST.=Y %then %do; * Start of process for #3 PROC HPFOREST;
             %let HPForest_used_1 = N;
             %let HPForest_used_2 = N;
             %do k= 1 %to 2;
                 %** Check whether HPFOREST method parameter is blank                                     ;
                 %if &&HPForest_method_&k.^= %then %do;
                     %let HPForest_used_&k = Y;
                     title "PROC HPFOREST for &&targetVar&targCnt. (&targ_class. dependent variable)";
                     proc hpforest data=gplib.Part_ALLdri_targ&targCnt. (where = (_PARTIND_ = 1)) 
                                                                                &&HPForest_method_&k.;
                        input &pred_var_name_only. / level = interval;
                        target &&targetVar&targCnt / level = &DO_LEVEL.;
                        ID &idVar;
                        ods output Baseline= HPFOREST_&k._baseline_targ&targCnt
                                   FitStatistics=HPFOREST_&k._iteration_targ&targCnt
                                   VariableImportance=perm.HPFOREST_&k._varimport_targ&targCnt;
                        save file="&rootPath/HF_&k._OUTMDLFILE_targ&targCnt..bin";
                        &PERFORMANCE_STATEMENT. ;
                     run;
                     title;

                      %** Score the data using HP4SCORE                                                    ;
                     %drop_table_iff (gplib.HF_&k._scores_targ&targCnt)
                     proc hp4score data=gplib.Part_ALLdri_targ&targCnt.;
                        id &idVar &&targetVar&targCnt _partind_;
                        score file="&rootPath/HF_&k._OUTMDLFILE_targ&targCnt..bin"
                        out=gplib.HF_&k._scores_targ&targCnt;
                        &PERFORMANCE_STATEMENT. ;
                     run;

                      %**  Produce plots and tables of scored data                                         ;
                     %delete_sasdata_iff (perm.bins_HF_&k._targ&targCnt.)
                     %delete_sasdata_iff (perm.binstat_HF_&k._targ&targCnt.)
                     %delete_sasdata_iff (perm.expand_HF_&k._targ&targCnt.)
                     %aamodel
                     %aa_model_eval (DATA = gplib.HF_&k._scores_targ&targCnt.,
                                     TARGET = &&targetVar&targCnt.,
                                     VAR =  p_&&targetVar&targCnt..&Event., 
                                     LEVEL = &DO_LEVEL., 
                                     EVENT=&Event.,
                                     OUT = perm.bins_HF_&k._targ&targCnt.,
                                     BINSTATS=perm.binstat_HF_&k._targ&targCnt.,
                                     BINS = 20,
                                     HPDS2 = Y,
                                     EXPAND= perm.expand_HF_&k._targ&targCnt.,
                                     partitionvar=_partind_)
                     %em_new_report_JW (bins=perm.bins_HF_&k._targ&targCnt., 
                                        binstats=perm.binstat_HF_&k._targ&targCnt., 
                                        expand=perm.expand_HF_&k._targ&targCnt.)
                 %end;
             %end;
         %end;


         %**  Model Type #4:  PROC HPREG                                                                   ;
         %if &DO_HP_REG.=Y %then %do;
             %let HPReg_used_1 = N; * Check for parameter 1;
             %let HPReg_used_2 = N; * Check for parameter 2;
             %do k= 1 %to 2; * Start of HPREG iterations;
                 %if &&HPReg_method_&k.^= %then %do; * Check for parameters 1&2;
                     %let HPReg_used_&k = Y;
                     proc hpreg data=gplib.Part_ALLdri_targ&targCnt.  noclprint; 
                        &PERFORMANCE_STATEMENT. ;
                        class &idVar;
                        id &idVar;
                        model &&targetVar&targCnt= &pred_var_name. ;
                        selection method=&&HPReg_method_&k. ;
                        partition rolevar=_Partind_(train='1');
                        ods output parameterestimates=perm.est_HPREG_&k._&targCnt 
                                   modelinfo=info_HPREG_&k._&targCnt;
                        output  pred=hpreg_&k._targ&targCnt._pred;
                        code file = "&rootPath/HR_&k._targ&targCnt..sas";
                     run;

                     %**  DS TRANS code so it can be run on GP                                             ;
                     filename _outds2 "DS2_HR_&k._targ&targCnt..sas";
                     proc dstrans ds_to_ds2
                        in = "&rootPath/HR_&k._targ&targCnt..sas"
                        out = _outds2
                        outdir = "&rootPath/"
                        aster
                        nocomp;
                     run;
                     quit;
                     filename _outds2;
                     %drop_table_iff (gplib.HR_&k._scores_targ&targCnt.)

                     %**  Score Training and Validation samples                                            ;
                     proc hpds2 data=Part_ALLdri_targ&targCnt.
                        out=gplib.HR_&k._scores_targ&targCnt;
                        %include "&rootPath/DS2_HR_&k._targ&targCnt..sas";
                        &PERFORMANCE_STATEMENT. ;
                     run;

                     %**  Produce plots and tables of scored data                                          ;
                     %delete_sasdata_iff (perm.bins_HREG_&k._targ&targCnt.)
                     %delete_sasdata_iff (perm.binstat_HREG_&k._targ&targCnt.)
                     %delete_sasdata_iff (perm.expand_HREG_&k._targ&targCnt.)
                     %aamodel
                     %aa_model_eval (DATA = gplib.HR_&k._scores_targ&targCnt,
                                     TARGET = &&targetVar&targCnt.,
                                     VAR =  p_&&targetVar&targCnt..&Event.,
                                     LEVEL =  &DO_LEVEL., 
                                     OUT = perm.bins_HREG_&k._targ&targCnt.,
                                     BINSTATS=perm.binstat_HREG_&k._targ&targCnt.,
                                     BINS = 20,
                                     HPDS2 = Y,
                                     EXPAND= perm.expand_HREG_&k._targ&targCnt.,
                                     partitionvar=_partind_)
                     %em_new_report_JW (bins=perm.bins_HREG_&k._targ&targCnt., 
                                        binstats=perm.binstat_HREG_&k._targ&targCnt., 
                                        expand=perm.expand_HREG_&k._targ&targCnt.)
                 %end;
             %end;
         %end;

         %**  Model Comparison                                                                            ;
         %**  Fnd which model fits the data best                                                          ;
         %**  Fit statistics table                                                                        ;
         %**  Put the model desc based on the algorithm selected for target variable                      ;

         options nodsnferr;

         %**  C-Stats calculation (for Binary target only)                                                ;
         %do k=1 %to 2;

             %let c_stats_hlog_&k._targ&targCnt. = .;
             %let c_stats_hnn_&k._targ&targCnt. = .;
             %let c_stats_hf_&k._targ&targCnt. = .;
             %let c_stats_hreg_&k._targ&targCnt. = .;

             %if &c_stat.=Y %then %do;
%macro SKIP;
		proc sql;
		select (sum(cVal))/count into: c_stats_hlog_&k._targ&targCnt.
			from (select case when t1.LL>t2.LL then 1
							  when t1.LL=t2.LL then 0.5
							  				   else 0
					 		end as cVal, count(*) as count
				  from 
				  	( select p_&&targetVar&targCnt..&Event. as LL
						from gplib.HL_&k._scores_targ&targCnt.
						 where &&targetVar&targCnt. =1) as t1,
					( select p_&&targetVar&targCnt..&Event. as LL
						from gplib.HL_&k._scores_targ&targCnt.
						 where &&targetVar&targCnt. =0) as t2
				  );
		select (sum(cVal))/count into: c_stats_hnn_&k._targ&targCnt.
			from (select case when t1.LL>t2.LL then 1
							  when t1.LL=t2.LL then 0.5
							  				   else 0
					 		end as cVal, count(*) as count
				  from 
				  	( select p_&&targetVar&targCnt..&Event. as LL
						from gplib.HN_&k._scores_targ&targCnt.
						 where &&targetVar&targCnt. =1) as t1,
					( select p_&&targetVar&targCnt..&Event. as LL
						from gplib.HN_&k._scores_targ&targCnt.
						 where &&targetVar&targCnt. =0) as t2
				  );
		select (sum(cVal))/count into: c_stats_hf_&k._targ&targCnt.
			from (select case when t1.LL>t2.LL then 1
							  when t1.LL=t2.LL then 0.5
							  				   else 0
					 		end as cVal, count(*) as count
				  from 
				  	( select p_&&targetVar&targCnt..&Event. as LL
						from gplib.HF_&k._scores_targ&targCnt.
						 where &&targetVar&targCnt. =1) as t1,
					( select p_&&targetVar&targCnt..&Event. as LL
						from gplib.HF_&k._scores_targ&targCnt.
						 where &&targetVar&targCnt. =0) as t2
				  );
		quit;
%mend SKIP;
%macro REPLACE_SKIP;
    %** This is the replacement code that needs to be tested, to speed up cstat calculations.   **;
    %** When we have the bandwidth to test this, remove the macro definition and execute the    **;
    %** code. Once it is working, remove commented out references to C_STAT in the subsequent   **;
    %** DATA step.                                                                              **;

    %local table_name predictor event cstat_target;

    proc sql;

       Connect to Greenplm as dbcon (server=&server user=&user password=&pwd
       database=&database schema="&sschema");

       %let table_name=gplib.HL_&k._scores_targ&targCnt.;
       %let predictor=p_&&targetVar&targCnt..&Event.;
       %let event=&&targetVar&targCnt.;
       %let cstat_target = c_stats_hlog_&k._targ&targCnt.;

       %cstat_total

       %let table_name=gplib.HN_&k._scores_targ&targCnt.;
       %let cstat_target = c_stats_hnn_&k._targ&targCnt.;

       %cstat_total

       %let table_name=gplib.HF_&k._scores_targ&targCnt.;
       %let cstat_target = c_stats_hf_&k._targ&targCnt.;
       %cstat_total
    quit;

%mend REPLACE_SKIP;

             %end;
         %end;

         data fitstatistics_targ&targCnt.;
            length model_desc $30;
            label model_desc='Model Description';
            set  %do k=1 %to 2;
                     perm.binstat_HLog_&k._targ&targCnt. (in=l&k.)
                     perm.binstat_HNN_&k._targ&targCnt. (in=n&k.)
                     perm.binstat_HF_&k._targ&targCnt. (in=f&k.)
                     perm.binstat_HREG_&k._targ&targCnt. (in=r&k.)
                 %end;
                 ;
            if l1 then do; 
               model_desc      = "HL_m1";
               *c_stat = &&c_stats_hlog_1_targ&targCnt..; 
            end;
            else if l2 then do; 
               model_desc = "HL_m2";
               *c_stat = &&c_stats_hlog_2_targ&targCnt..; 
            end;
            else if n1 then do;
               model_desc = "HN_m1 "; 
               *c_stat = &&c_stats_hnn_1_targ&targCnt..; 
            end;
            else if n2 then do; 
               model_desc = "HN_m2 "; 
               *c_stat = &&c_stats_hnn_2_targ&targCnt..; 
            end;
            else if f1 then do; 
               model_desc = "HF_m1 "; 
               *c_stat = &&c_stats_hf_1_targ&targCnt..; 
            end; 
            else if f2 then do; 
               model_desc = "HF_m2 ";
               *c_stat = &&c_stats_hf_2_targ&targCnt..; 
            end;
            else if r1 then do; 
               model_desc = "HR_m1 ";
               *c_stat = &&c_stats_hreg_1_targ&targCnt..; 
            end;
            else if r2 then do; 
               model_desc = "HR_m2 ";
               *c_stat = &&c_stats_hreg_2_targ&targCnt..; 
            end;
         run;
         proc sort data=fitstatistics_targ&targCnt. ;
            by &model_selection;
         run;
         %delete_sasdata_iff (perm.fitstatistics_targ&targCnt.)

         data perm.fitstatistics_targ&targCnt.;
            set fitstatistics_targ&targCnt.;
            rank=_N_;
         run;
         title "Model Comparison: Fit Statistics for &&targetVar&targCnt.._targ&targCnt.";
         proc print data=perm.fitstatistics_targ&targCnt. noobs label;
         run; 
         title;

         %**  Find the top three models which will be passed to Model Manager                              ;
         %**  Creating macros for top three models for Model Package                                       ;
         %local rrank1 rrank2 rrank3 ;
         data _null_; 
            set perm.fitstatistics_targ&targCnt. (obs=3);
            call symputx('rrank' || put(rank,1.), model_desc);
         run;
         %put rrank1: &rrank1.   rrank2: &rrank2.   rrank3: &rrank3.;

         options dsnferr;

         %local ProjectName;

         %local original_settings;
         %let original_settings = %sysfunc(getoption(mprint)) %sysfunc(getoption(mlogic)) %sysfunc(getoption(symbolgen));
         options notes mprint mlogic symbolgen;
         %CreateSubFolders (ProjectName=targ&targCnt.,
                            Type=&DO_TYPE.)

         %local j model_type model_pos;
         options notes mprint mlogic symbolgen;
         %**  Create top 3 model packages.  Key step:  retrieve the type of model.                         ;
         %do j=1 %to 3;
             %let model_type = %scan(&&rrank&j.,1,'_');
             %let model_pos  = %scan(&&rrank&j.,1,'_m', b);
             %if "%scan(&&rrank&j.,1,'_')" ^= "HF" %then %do;
                 %ModelPackage (modelnm=targ&targCnt._&&rrank&j. ,
                                mrpath=/Shared Data/Model Manager,
                                reg= N,
                                spkfolder=&mm_fldr. ,
                                spk= Y,
                                modeldata=gplib.Part_ALLdri_targ&targCnt.,
                                Mdl_trgt=&&targetVar&targCnt..,
                                MMlevel=&DO_LEVEL. ,
                                miningfnctn=&DO_TYPE. ,
                                miningalg=&model_type. ,
                                scorecodefile=&rootPath./&model_type._&model_pos._targ&targCnt..sas,
                                scoredata=gplib.&model_type._&model_pos._scores_targ&targCnt. ,
                                ProjectName=targ&targCnt.
                               )
             %end;
         %end;

         options notes mprint mlogic symbolgen;
         %**  Again, type of model plays a key role in determining the model package.                      ;
         %do j=1 %to 3;
             %let model_type = %scan(&&rrank&j.,1,'_');
             %let model_pos  = %scan(&&rrank&j.,1,'_m', b);
             %if "&DO_TYPE." = "PREDICTION" %then %do;
                 %let Model_Template=HPForestFidelityPrediction; *Do NOT Modify name or remove, It Refers to Model Template in MM ;
             %end;
             %if "&DO_TYPE." = "CLASSIFICATION" %then %do;
                 %let Model_Template=HPForestFidelityClassification;*Do NOT Modify name or remove, It Refers to Model Template in MM ;
             %end;
             %if "%scan(&&rrank&j.,1,'_')" = "HF" %then %do;
                 %HPFOREST_ModelPackage (ProgCode="&rootPath/HF_&model_pos._OUTMDLFILE_targ&targCnt..bin",
                                         ScrCode="&scoreloc/score.sas",
                                         inputdata=gplib.Part_ALLdri_targ&targCnt. ,
                                         modelnm=targ&targCnt._&&rrank&j.. ,
                                         target=&&targetVar&targCnt.. ,
                                         scoredata=gplib.&model_type._&model_pos._scores_targ&targCnt.,
                                         FolderName=&FolderName.,
                                         ProjectName=targ&targCnt. ,
                                         Model_Template= &Model_Template.)
             %end;
         %end;
         options &original_settings;
     %end;

%mend executeBatchDataMining;
