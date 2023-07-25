Started 25 July 2023
By Lizzie so far

This folder and process is in development, but here's what we have so far for FILES:

- tablemerge.rnw (and tex and pdf) -- Sweave file for some issues, no one really needs this and Lizzie should perhaps delete it someday soon.

- tablemergeclean_basics.R -- code mostly by Lizzie that cleans up growth and GSL metrics mostly then writes out grephontablesemiclean.csv

- tablemergeclean_methodexoendo.R -- code mostly by Alana that reads in  grephontablesemiclean.csv cleans up exo and endo stuff and works on a new method column and writes out grephontable.csv


FAQ:

Question: Help! I want to help clean up some columns, what should I do? 
Answer: It depends ...
	- If the columns are being cleaned already in a script you should reach out the main script author about how to do it.
	- If the columns have not been cleaned, write a new script that reads in output/grephontable.csv and clean from there. Upload the code or send to Lizzie so she can add it to the cleaning pipeline eventually.


Question: Help! I am trying to analyze the table, how do I start?
Answer: Read in output/grephontable.csv and pay attention to columns at the END -- these have generally been cleaned already. 
