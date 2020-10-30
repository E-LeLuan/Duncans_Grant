# Prediction-Integration

Studying message-level prediction and integration during reading

A pipeline for taking raw .asc files, tidying and extracting reading time data.

## Exploring Files

Follow the instructions in the first part of `filenames_script.R` to generate `filenames.csv`, which is a list of .asc files, with information on the original filepath for each of these files. 

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

To differentiate between trials run using corrected and uncorrected versions of the script:
1. Make sure all .asc files are in one folder
2. Open terminal and navigate to the folder that contains the .asc files using `cd` (change directory)
3. Type `grep -c "CHAR 74 1 a 248 365 264 427" *.asc > detect_script_error.csv`
4. Run `detect_error_script.R` to write two separate .csv files 
5. In terminal, type `cat filenames_corr.csv > filenames_corr.lst` and then `cat filenames_error.csv > filenames_error.lst` to generate the two .lst files.

## Dealing with a split participant file

An error occured whilst during `DBP35`. The experiment was aborted and then resumed using a new file `DBP35T`. 
So, both of these files contain data from the same participant.

## Fix_Align

Andrew Cohen's programme consists of a function which aligns fixations using regression lines calculated for each line of text.

The .asc files are taken from the `wider_aois` folder. The processed files are written to `FixAlign` with the suffix `_fa`.

The `Parameters_info` document contains an explanation of the relevant parameters.

## Data Extraction

The relevant files here are in the `FixAlign` folder. The `Instruction_Files` folder contains explanations of how `Robodoc`, `question_acc` and `make_cnt` work. These are all Python programmes. The `Parameters_info` document in this folder contains an explanation of the relevant parameters in the `parameters.txt` file.

Each programme can be run with the `reticulate_data_extraction.R` script, which uses the `reticulate` package in R.

### Robodoc

`Robodoc.py` is for deleting blinks and creating DA1 files.

### Question Accuracy

`question_acc.py` creates files for assessing the accuracy of participants' responses to questions. Two files are created:

- `QuestSum.txt` provides the total number and percentage correct for each participant
- `subj_quest.txt` provides the breakdown for each question for each participant

### Making a .cnt file

A .cnt file specifies analysis regions with character positions. 
It uses .del files: script files with region delimiters, which includes characters that split up regions.
Becuase of the script error dicussed above, two .del files are needed (characters positions are different for line 2 in item 16).

With this script, there is a UnicodeDecode error on the 'é' character  in item 14. For the purposes of creating the .cnt file, this can be replaced with a regular 'e'.

