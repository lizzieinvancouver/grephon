Started 25 July 2023
By Lizzie so far

<><><><><><><><><><><>
This folder contains all the cleaning code, some analyses (whathappened.R) and plots (plotspecies.R). 

For CLEANING! README please: *Everything now is sourced in cleantableall.R*
-> This means that if you want to edit any file sourced (in source folder), then you need to run the preceeding code in cleantableall.R 
-> For example, if you want to run and edit and play around with cleanageclass.R, I would run through Step 3 (line 42), then open and work in cleanageclass.R.


<><><><><><><><><><><>
More info on some files here:

- tablemerge.rnw (and tex and pdf) -- Sweave file for some issues, no one really needs this and Lizzie should perhaps delete it someday soon.

- whathappened.R -- current code with analyses for paper

FAQ:

Question: Help! I want to help clean up some columns, what should I do? 
Answer: It depends ...
	- If the columns are being cleaned already in a script you should reach out the main script author about how to do it.
	- If the columns have not been cleaned, write a new script that reads in output/grephontable.csv and clean from there. Upload the code or send to Lizzie so she can add it to the cleaning pipeline eventually.


Question: Help! I am trying to analyze the table, how do I start?
Answer: Read in output/grephontable.csv and pay attention to columns at the END -- these have generally been cleaned already. 


growthxelevationetc/ -- this is the folder for two growth x elevation analyses we did:
1) A lit review (so there's a mix of data, analyses and methods in here -- oy!) which also included elevation. This is currently in the paper. Plus more notes on this in https://github.com/lizzieinvancouver/grephon/issues/24
2) Mount Rainier 
See _READMEgrowthxelevationetc.txt for more info. 
