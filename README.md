# Prediction-Integration-Forward Inferences

Studying message-level processing during reading

A pipeline for taking raw .asc files, tidying and extracting reading time data.

## Exploring Files

Follow the instructions in the first part of `filenames_script.R` to generate `filenames.csv`, which is a list of .asc files, with information on the original filepath for each of these files. This can be found in the Terminal_and_R_scripts file. 

The R script in the second part of `filenames_script.R` re-writes `filenames.csv` with multiple columns.

## Expand Start Coordinates

FixAlign appears to struggle with fixations that lie to the left of the first character of each line.

The shell script `expand_aois.sh` conducts a find and replace function on all .asc files which expands the left x axis for the first character in each line, to accomodate these fixations. Processed files have the prefix `w_`.

1. Make sure all .asc files are in one folder (`RAW_asc`) with the `expand_aois_2.sh` script
2. Create a new folder inside this folder called `wider_aois`
3. Open terminal and navigate to the folder that contains the .asc files using `cd` (change directory) followed by a space and then the folder path
4. Make the script executable by typing `chmod +x expand_aois_2.sh`
5. Type `bash expand_aois_2.sh` to run the script

## Detecting a script error and making .lst files

For all experimental trials, the 2nd line should be the same across conditions. 
However, for item 16, in the initial version of the script, the 2nd line differed between conditions. 
This was subsequently corrected.

The initial versions were as follows:
- `E1I16D0`: He had recently been considering giving up alcohol for health reasons.
- `E2I16D0`: He always enjoyed visiting rural Dorset at this time of year.

When corrected, both `E1I16D0` and `E2I16D0` had the 2nd line used originally for `E1I16D0` (He had recently been considering giving up alcohol for health reasons.)

The fourth character of these sentences differs.
- 'h' in `E1I16D0`
- 'a' in `E2I16D0`
These characters are both character 74 of the whole vignette. 
Part of the following code counts the number of instances where 'a' appears in this exact position.
There are no instances where this character appears in this position in any other trials.

For the purpose of clarity and work flow I have created a new sub folder within the FixAlign folder labeled 'Detecting_script_error_and_lst'. Move your 'wider_aois' data into this folder.

To differentiate between trials run using corrected and uncorrected versions of the script:
1. Make sure all .asc files are in one folder
2. Open terminal and navigate to the folder that contains the .asc files using `cd` (change directory) 
3. Type `grep -c "CHAR 74 1 a 248 365 264 427" *.asc > detect_script_error.csv`
4. Run `detect_error_script.R` to write two separate .csv files 
5. In terminal, type `cat filenames_corr.csv > filenames_corr.lst` and then `cat filenames_error.csv > filenames_error.lst` to generate the two .lst files if you have two lists.
Alternatively, if you've had to as I have batch out the lst files. You simply modify the text to read `cat FA_batch1_corr.csv > FA_batch1_corr.csv` then `cat FA_batch2_corr.csv > FA_batch2_corr.csv` then `cat FA_batch3_corr.csv > FA_batch3_corr.csv` then `cat FA_batch4_error.csv > FA_batch4_error.csv` then `cat FA_batch4_error.csv > FA_batch4_error.csv`

These files will write to wherever your R project file is stored in my case Duncans_Grant. I have copy and pasted these into Detecting_script_error_and_lst. 

## Dealing with a split participant file

An error occured whilst during `DBP35`. The experiment was aborted and then resumed using a new file `DBP35T`. 
So, both of these files contain data from the same participant.

## Fix_Align
FixAlign.R (v 0.92) – R code for the correction of vertical locations and the removal of outliers and ambiguous fixations in reading experiments. Cohen (2013), Behavior Research Methods.

Andrew Cohen's programme consists of a function which aligns fixations using regression lines calculated for each line of text.

The .asc files are taken from the `wider_aois` folder. The processed files are written to `FixAlign` with the suffix `_fa`.

The `Parameters_info` document contains an explanation of the relevant parameters.

You may get some data loss as the fixalign script seems to encounter some unknown problems on the same participant numbers, in our case 5 participants (24, 27, 35, 52, 84).

With the files you have created you can complete eyedry analysis on them once you've completed the remaining steps below in data extraction. For clarity of workflow I have moved the fixalign tidied asc files into a folder named FA_tidy.

## Data Extraction

The relevant files here are in the `FA_tidy` folder. The `Instruction_Files` folder contains explanations of how `Robodoc`, `question_acc` and `make_cnt` work. These are all Python programmes. The `Parameters_info` document in this folder contains an explanation of the relevant parameters in the `parameters.txt` file.

Each programme can be run with the `reticulate_data_extraction.R` script, which uses the `reticulate` package in R.

### Robodoc

`Robodoc.py` is for deleting blinks and creating DA1 files.

### Question Accuracy

`question_acc.py` creates files for assessing the accuracy of participants' responses to questions. Two files are created:

- `QuestSum.txt` provides the total number and percentage correct for each participant
- `subj_quest.txt` provides the breakdown for each question for each participant

It is important to note that the script is set up to give you accuracy information on experimental items only. As we are not interested in the filler items.

### Making a .cnt file

A .cnt file specifies analysis regions with character positions. 
It uses .del files: script files with region delimiters, which includes characters that split up regions.
Becuase of the script error discussed above, two .del files are needed (characters positions are different for line 2 in item 16).

With this script, there is a UnicodeDecode error on the 'é' character  in item 14. For the purposes of creating the .cnt file, this can be replaced with a regular 'e'.

In order to run this portion of the script you must have created a .del file deliminating your regions of interest. To do this use text edit to create a file from your original Questions_01_10_18.script that includes ^ before ech line break as a deliminater for each region. 
e.g. Aidan had been studying abroad in a French-speaking part of Canada. ^\nHe had recently started introductory French lessons. ^\nThe teacher was unfriendly and his classes were very difficult. ^\nAidan's brother was visiting and asked, "How is learning the language going?" ^\nAidan replied, "It's not going as well as I'd hoped." ^\nThey went ice skating to give Aiden a break from learning French. ^\n

## Eyedry set up, data extraction, and R set up.
It is important to note that up to this point the scripting has been written for MAC users. Therefore, some features may need slight modifications to run on a windows PC. Importantly, eyedry must be used with Windows or some sort of interface as it is not compatible with MAC.

In the interest of work flow I have created a folder within the FixAlign folder called FA_eyedry.

In this file you must have all the eyedry program files. This can be found of the UMASS "EyeTrack" website. These can be found and at https://blogs.umass.edu/eyelab/software/

In addition, The UMASS eye-tracking handbook (A Guide to Reading Experiments Using the UMass Eyetracking Lab Software Suite) can be found here...
https://people.umass.edu/eyelab/eyelab%20manual.pdf

You will also need to either copy or move the following files into the FA_eyedry folder...
- Any .CNT files
- Any .lst files
- All .da1 files
- Your original questions script Questions_01_10_18.script

### Recreating correctly named .lst files
You will need to recreate your .lst files in terminal using the new .asc files created in FixAlign. These now have the suffix _fa your list files do not. Move or copy the detect_error_script.R to the FA_tidy folder. Re-run the "#### Detecting a script error and making .lst files". Make sure to change the pathway of your .script to the FA_tidy file directory. This is where your .asc files with the suffix _fa are located, provided you have organized your data in the same way. Like before these will write to the folder where your R project is stored. In our case Duncans_Grant. You will need to move them back into FA_tidy.


### Helpful Hints: 
When using eyedry for the purpose of this study...
- You can ignore the section on making count file using "EDPREP 60" and making the list files as we have done this using psychopy scripts.
- All file names should be short or face the wrath of crashing.

####4.5.1 Initial steps in eyetracking handbook
1. The new version of eyedry does not ask if you want a hard copy so ignore this in the handbook.
2 and 3. We are not interested in the trace or string files so simply call them trace and string.
4. You should make your maximum fixations something high as we do not want to loose any fixations. I always put 300 as participants rarely if ever exceed this number of fixations.
5. Type the name of the file containing control information? The first time you run the analysis you will not have one yet and therefore select enter on your keyboard. If you have created one because you have already run your first analysis (e.g. first pass fication times) you can skip the remaining steps in the next section and type the file path (e.g. FA_cont.ctl).

####4.5.2 Creating the control file steps in eyetracking handbook (responses for our study)
(a) Debug level... 0
(b) This is asking if the question is the same. For our study the answer is no because our question is labeled 0 and sentence 1.
(c) No.
(d) The smallest numbered experimental item is 1. 
- The largest is 32. 
- For the maximum number of regions look in your .cnt file and + 2. This is because eyedry is buggy and crashes otherwise when dealing with this type of data. In our case type: 8.
- The smallest condition is 1.
- The largest 2. 
- For the subconditions take your number of conditions + 1 even though you don't have subconditions because 0 makes eyedry crash.
* Note: The fields for FixAlign are different to eyedoctor tidied data.
- Field condition number: 2
- Field item number: 3
- Field number of fixations: 6
- Field data start in: 7
(e) We would control for this at this stage and truncate fixation greater than 1200 milliseconds.
(f) We would control for this at this stage and discard fixations shorter than 80 milliseconds. This is because below this the participant is unlikely to have comprehended any text. 
(g) Screen Width is 160 
(h) Make your maximum number of lines something high so that the analysis will encompass all the lines of the experiment. I usually put 30.
(i) Yes we would like to permit regions to wrap around as we use \n to delineate multiple lines.
(j) Yes our studies do have blank lines.
(k) No we want to analyze all of our trials. 
(l) No we want to analyze all trials.
(m) This is important if you want to extract data more than once. We do as we will want to analyse first pass, total time, regression path, regressions in, regression out from both our corr and error files. I have named ours FA_cont.ctl.

6. Here you want to type in the data file that contains your list information. For our study we have called it FA_filenmes_corr.lst for the corrected file list and FA_filenames_error.lst for the error file list. You can only load one at a time but can use the same control file (FA_cont.ctl) for each.
7. We have no exception files.
8. enter the .cnt file appropriate to the .da1 files you are investigating here (corr_ROIs2.cnt for the corrected lists and error_ROIs2.cnt for the error lists)

####4.6 Which Analysis?
For our study you will be selecting in any order 
2 = first pass time
- string
- throw away zero fixations: yes
- cumulative 
- raw
- conditionalize on regression in: no 
- conditionalize on fixation in critical region: no 
- Upper summed cutoff should be something high like 20000 so we don't loose any data
- conditionalize on fixation before each region: no
- file of item X subject combinations: This is the analysis file you want for R. You should try and stick with a naming system like FP_FA_corr standing for first pass, fixalign, corrected list. So FP_FA_corr and then FP_FA_error for the error list.
- all trials
- wide format
- we are not interested in the next 4 file types. If you wish to analyse the data in SPSS as separate subject by subject and item by items file than name these.Other wise just hit enter for the next 4 lines.
- Long and short times: Although we do not care about this information selecting yes will let us know if the system is going to write the data to excel or crash.
- Type out of data: Although we do not care about this information selecting yes will let us re-select a new analysis without having to go through loads of excess questions.

3 = total time
- string
- throw away zero fixations: yes
- cumulative 
- raw
- conditionalize on regression in: no 
- conditionalize on fixation in critical region: no 
- Upper summed cutoff should be something high like 40000 so we don't loose any data. This needs to be higher for TT then FP or RP as it encompasses re-reading.
- conditionalize on fixation before each region: no 
- file of item X subject combinations: This is the analysis file you want for R. You should try and stick with a naming system like TT_FA_corr standing for first pass, fixalign, corrected list. So TT_FA_corr and then TT_FA_error for the error list.
- all trials
- wide format
- we are not interested in the next 4 file types. If you wish to analyse the data in SPSS as separate subject by subject and item by items file than name these.Other wise just hit enter for the next 4 lines.
- Long and short times: Although we do not care about this information selecting yes will let us know if the system is going to write the data to excel or crash.
- Type out of data: Although we do not care about this information selecting yes will let us re-select a new analysis without having to go through loads of excess questions.

13 = time from first entering a region to first going past it (aka regression path)
- string
- throw away zero fixations: yes
- cumulative 
- raw
- 1 = all from first entering a region to first going past it. This measures regression path including re-reading.
- conditionalize on regression in: no 
- conditionalize on fixation in critical region: no 
- Upper summed cutoff should be something high like 20000 so we don't loose any data
- conditionalize on fixation before each region: no 
- file of item X subject combinations: This is the analysis file you want for R. You should try and stick with a naming system like RP_FA_corr standing for first pass, fixalign, corrected list. So RP_FA_corr and then RP_FA_error for the error list.
- all trials
- wide format
- we are not interested in the next 4 file types. If you wish to analyse the data in SPSS as separate subject by subject and item by items file than name these.Other wise just hit enter for the next 4 lines.
- Long and short times: Although we do not care about this information selecting yes will let us know if the system is going to write the data to excel or crash.
- Type out of data: Although we do not care about this information selecting yes will let us re-select a new analysis without having to go through loads of excess questions.

4 = first pass regressions out (FPRO)
- string
- throw away zero fixations will not be asked on these measures as they mean something here.
- file of item X subject combinations: This is the analysis file you want for R. You should try and stick with a naming system like FPRO_FA_corr standing for first pass, fixalign, corrected list. So FPRO_FA_corr and then FPRO_FA_error for the error list.
- all trials
- wide format
- we are not interested in the next 4 file types. If you wish to analyse the data in SPSS as separate subject by subject and item by items file than name these.Other wise just hit enter for the next 4 lines.
- Long and short times: Although we do not care about this information selecting yes will let us know if the system is going to write the data to excel or crash.
- Type out of data: Although we do not care about this information selecting yes will let us re-select a new analysis without having to go through loads of excess questions.

5 = regressions in
- string
- throw away zero fixations will not be asked on these measures as they mean something here.
- file of item X subject combinations: This is the analysis file you want for R. You should try and stick with a naming system like RI_FA_corr standing for first pass, fixalign, corrected list. So RI_FA_corr and then RI_FA_error for the error list.
- all trials
- wide format
- we are not interested in the next 4 file types. If you wish to analyse the data in SPSS as separate subject by subject and item by items file than name these.Other wise just hit enter for the next 4 lines.
- Long and short times: Although we do not care about this information selecting yes will let us know if the system is going to write the data to excel or crash.
- Type out of data: Although we do not care about this information selecting yes will let us re-select a new analysis without having to go through loads of excess questions.

### Excel manipulation
Once you have created your excel files following the eyetracking handbook you must do some tidying in excel. Remove any extra conditions and regions. (hint: In the subconditons parameter add the number of conditions you have + 1 when creating your .ctl file). Select sort file by condiiton and remove all the conditions labelled 0. For me I delete region 8. Region 8 will be empty. Region 7 will have data in it this will be left in but we are unsure what it realtes to as we do not have a region 7. This is likely due to a problem with FixAlign and while it may reflect data of our participants it is impossible to know from what region. Therefore it must not be analysed until we can asertain where it belongs.I tend to save this as a workbook and a .csv. You will need the .csv to import the dataset into R.

Repeat this process with any and all analysis you wish to undertake (e.g. first pass, total time, regression path, FPRO, and regressions in) as well as with both data sets (e.g. corr and error .lst).






