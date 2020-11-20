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

### FixAlign Extraction complete time to move onto Eyedry set up and data extraction.

In the interest of work flow I have created a folder within the FixAlign folder called FA_eyedry.

In this file you must have all the eyedry program files. This can be found of the UMASS "EyeTrack" website. These can be found and at https://blogs.umass.edu/eyelab/software/

In addition, The UMASS eye-tracking handbook (A Guide to Reading Experiments Using the UMass Eyetracking Lab Software Suite) can be found here...
https://people.umass.edu/eyelab/eyelab%20manual.pdf

You will also need to either copy or move the following files into the FA_eyedry folder...
- Any .CNT files
- Any .lst files
- All .da1 files
- Your original questions script Questions_01_10_18.script




