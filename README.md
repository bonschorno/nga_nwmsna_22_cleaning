# nga_nwmsna_22_cleaning


Scripts 01-04 should be self-explanatory. `01_get_data.R` retreives the raw data from Kobo's API. It also creates `final_deletionfile.csv` as well as `pre_clean_data.csv` which serves as the starting point for the data cleaning process. More precisely, respondents younger than 18 or older than 70, those who did not consent and respondents from outside Nigeria were automatically filtered out. 

The script `create_final_logfile.R` creates the final log file, structured according to the HQ guidelines.

The `fn_*` scripts contain helper functions for the data cleaning process.

The `merge_*` scripts constitute the complete cleaning process. I have to make a little tangent here. Since I joined in the middle of the data collection, some log files already existed. However, some issues were present which made me to split the folder structure into old (e.g. old_logicalchecks) and new (e.g. new_otherchecks) folders. Essentially, since I started, the old files remained untouched and new processes were established for the new files. This means that the data cleaning scripts follow this logic, which means that you find two files for the "other"-answers as well as the logical checks. The numbers as prefixes indicate the chronological order of the replication process. That is, `101_merge_old_otherchecks.R` is the first file, `104_merge_new_logicalchecks.R` the last. 
