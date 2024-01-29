Started 29 January 2024
Dealing with Sweave and Overleaf

See ALSO: gitgit/gitoverleaf/_READMEsweaveoverleaf.txt

Overleaf has ended up feeling like a pain because:

(1) It does not like much folder structure (and wow, this is their solution instead of just fixing it: https://www.overleaf.com/learn/latex/Questions/My_main_file_is_in_a_subfolder%2C_but_this_is_not_allowed_on_Overleaf._But_I_don%27t_want_to_change_my_folder_structure_and_%5Cinput_paths!) 

(2) It cannot accept Sweave (it will take knitR but feels not worth the effort) so here is what I currently do:

People can edit:
	workingdraftOverleaf.tex and 
	grephonbibOverleaf.bib 

I then diff:
	workingdraft.tex
	workingdraftOverleaf.tex
And put in edits semi-manually (I have not done the bib yet). 

I then COPY workingdraft.tex and RENAME it workingdraftOverleaf.tex and we repeat this process. Not pretty. 

Oh, and when you copy it update the following:
	Delete the Sweave usepackage line in preamble.
	Comment out the external file
	Replace bib lines:
\bibliography{grephonbibOverleaf.bib}
\bibliographystyle{agsm}
