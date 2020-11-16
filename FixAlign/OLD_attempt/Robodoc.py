# Title Robodoc.py
# March 22 2015
# This is a python script that parses an .asc file into fixations.
# It replaces eyedoctor.
# Written by Adrian Staub, with mods by Jesse Harris and Chuck Clifton.
# Permits use of proportional fonts.
# Also deals with display changes (but only for 1-line displays for now)

# Example command line usage (From Jesse Harris change):
# > python3 Robodoc.py parameter.txt --verbose
# > python3 Robodoc.py parameter.txt --v
# Uses parameter.txt as parameter file; prints out information about excluded files

# > python3 Robodoc.py 
# Usage as before: specify parameter file as input when requested; no information about excluded files printed

# History of changes
# 6/3/2015 Modified to add display change check
# 8/23/2015 Modified to deal with trailing spaces
# 10/19/2015 Adrian Modified to allow fillers to be run
# 10/19/2015 also modified now so that if there is no fixation after the blink region, 
			# it sets the blink_check_end time to the end of the trial
# 11/9/2015 Adrian modified to deal with trailing spaces
# 11/23/15 Modified to make the default blink behavior based on first pass, not go-past.
# 12/9/2015 Jesse Harris additions
	# Added optional arguments to program to increase functionality:
	# Can now specify parameters file after program on command line. Also, an optional --verbose [--v] flag was added, which prints
	# which conditions were eliminated from blinks by the pogram for each participant.
	# Modified to include automatic exclusion, and summary of exclusions.
	# Modified to include verbose exclusion output, creation of files_processed.lst if file_list is empty and exclude.lst files if desired.
	# Creates new subfolders for blink, DC, and da1 files to keep directory neat.
	# Writes numbers of trials excluded, by conditions, for blinks/track losses in subs-exclude.csv
	# writes total numbers of trials excluded (blinks,losses, display change problems) in subs.sum
# 1/1/2016 Display change problems (e.g. DC after skipping region) fixed; saved as RoboDocDCZ.py
# 1/11/2016: Jesse Harris's changes integrated into RoboDocDCZ.py as RoboDocDCZ_JH.py
# 1/12/2016: Bug fixes for IDLE and verbose flag.
# 1/25/2016: Bug fixes for DC and non-verbose mode.
# 1/25/2016: Created functionality for excluding subjects on the basis of DC errors.
# 2/2016: bug fixes,to permit blink-critical regions to be at end of line and to exclude blink trials only if blink_exclude is set
# 4/18/16:  AS bug fix to deal with an occasional trial with the wrong condition number sneaking into the tempfile.
# 7/21/18:  BWD modified so that it does not include practice trials that overlap in condition number with targets.

#import modules
import sys
import math
import os
import glob
import csv
import argparse

	##################################################
	# FUNCTION TO CHECK DC VALUES
	##################################################

# Check the values recorded at ESACC (if DC during saccade) or at COMPLETED (if DC after ESACC)
#def CHECK_DC_VALUES(display_change_time,end_of_saccade,start_of_saccade,saccade_X_pos):
def CHECK_DC_VALUES():
	bad_display_change = "OK"
	trial_exclude = 0
	report_value = []
# zap if change too late (< min change time)
	if end_of_saccade - int(display_change_time) < int(min_change_time):
		bad_display_change = "LATE"		# change time too late
# zap if too early (a drift over boundary during fixation - next saccade started after DC time
	elif start_of_saccade > display_change_time:
		bad_display_change = "EARLY"		# change time way too early - drift
# zap if X value of ESACC is to the left of the X boundary - DC happened during saccade but there was a hook
	elif saccade_X_pos < float(X_bound_value):	# end of saccade to left of boundary
		bad_display_change = "HOOK"		# hook
	report_value.append(bad_display_change)
	if bad_display_change != "OK":
		trial_exclude = 2
	report_value.append(trial_exclude)
# output type of DC problem, change time, start and end saccade time, display change X position, end-next-saccade X position
#				output_string_DC = ["C",cond_str,"I",item_str,"DC ERROR",str(bad_display_change),"DC TIME",str(int(display_change_time) - int(trial_start_time)),"SACCADE START", str(int(fields[2]) - int(trial_start_time)), "END",str(int(fields[3]) -int(trial_start_time)),"DC POS",str(X_bound_value),"X POS",str(fields[7])]
	X_output_string_DC = "C {0:3d} I {1:3d} DC ERROR {2:6} DC-TIME {3:6d} SACCADE START {4:6d} END {5:6d} DC-POS {6:4d} X-POS {7:4.0f}".format(int(cond_num),int(item_num),bad_display_change,int(display_change_time) - int(trial_start_time), int(start_of_saccade) - int(trial_start_time), int(end_of_saccade) -int(trial_start_time),int(X_bound_value),float(saccade_X_pos))
	report_value.append(X_output_string_DC)
	return(report_value)


	#################################################
	# READ IN PARAMETERS
	#################################################

# ----------------------------------
# To allow including parameter filename and verbose flags when calling program on command lin.
parser = argparse.ArgumentParser()
parser.add_argument('filename', nargs = "?", help="Optional argument to provide the parameter file.")
parser.add_argument('--verbose', '-v', action="store_true", help="Optional argument to print information about excluded files.")
args = parser.parse_args()
if args.filename:
	filename = args.filename
else:	
	filename = input("What is the name of your parameter file?")
# ----------------------------------

try:
	parameters = open(filename,'r')
	#this reads the file as text
	whole_file = parameters.read()
	#this executes the whole thing as code
	exec(whole_file)
	
except:
	print ("Your parameter file could not be found.  Or, there is an error in the file.")
	sys.exit(0)
	
if blink_reg_exclude == "y":
	try:
		region_file = open(region_file, 'r')
	except:
		print ("Your region file could not be found.")
		sys.exit(0)


# ------------------------------
# Reads in auto_exclude argument in parameter file, looks for thesholds.
# Will write to exclude.lst if specified.
if auto_exclude == 1 or auto_exclude_DC == 1:
	e = open('exclude.lst', 'w')
	kp = open('keep.lst', 'w')
	try:
		exclude_threshold
	except: 
		print ("No exclusion threshold set. Add a numerical threshold to the parameters file, or set auto_exclude to 0.")
		sys.exit(0)
	try:
		abs_exclude_threshold
	except: 
		print ("No absolute exclusion threshold set. Add a numerical threshold to the parameters file, or set auto_exclude to 0.")
		sys.exit(0)
# ------------------------------


# ------------------------------
# Reads in auto_exclude argument in parameter file, looks for thesholds.
# Will write to exclude.lst if specified.
#if auto_exclude_DC == 1:
#	if not DC == 1:
#		print("WARNING: Clash in parameters file. Not identified as a display change experiment, but set to exclude display change errors. Thresholds will be ignored.\n\n")
#	try:
#		exclude_threshold
#	except: 
#		print ("No exclusion threshold set for display change. Add a numerical threshold to the parameters file, or set auto_exclude to 0.")
#		sys.exit(0)
#	try:
#		abs_exclude_threshold
#	except: 
#		print ("No absolute exclusion threshold set. Add a numerical threshold to the parameters file, or set auto_exclude to 0.")
#		sys.exit(0)
# ------------------------------	






	#################################################
	# PRE-READ SCRIPT FILE TO MAKE A DICT OF DISPLAY CHANGE POSITIONS
	# JUST IN CASE DC IS SET TO 1; SKIP IT IF DC = 0
	#################################################


if DC == 1:
	in_fix = 0			# set up toggle for display changes - permit change to take place in fixation
	try:
		script_file = open(script_file_name,'r')
	except:
		print("File %s could not be found." %script_filename)
		sys.exit(0)
	got_condition = 0
	sequence = 0
	script_search_strings = ['trial E','region']
	for line in script_file:
		if script_search_strings[0] in line:
			fields = line.split()#break line into list
			trialid = fields[1]
			first_split = trialid.split('I')#split into the condition, and then item and dependent
			condition = first_split[0]
			cond_num = condition[1:] #strip off the letter from the beginning of condition
			second_split = first_split[1].split('D')#split into item and dependent
			item_num = second_split[0]
			if int(cond_num) >= lowest_cond and int(cond_num) <= highest_cond:
				got_condition = 1
			else:
				got_condition = 0
		elif script_search_strings[1] in line and got_condition == 1:
			fields = line.split()
			xpos = fields[3]
			ypos_end = fields[4]		# bottom of Y 
			xpostemp = fields[5]
			xpostemp_split = xpostemp.split(')')
			xpos_end = xpostemp_split[0]
			ypostemp = fields[2]
			ypostemp_split = ypostemp.split('(')
			ypos = ypostemp_split[1]
			if sequence == 0:
				dcregions = {(item_num,cond_num):[ypos,xpos,ypos_end,xpos_end]}
			else:
				dcregions[(item_num,cond_num)] = [ypos,xpos,ypos_end,xpos_end]
			sequence = sequence + 1
#for k, v in sorted(dcregions.items()):
#	print("Dictionary",k,v)
# END OF SETTING UP DC DICTIONARY
		
#and make a subject summary file
sumsub = open('sum.sub', 'w')
	

	#################################################
	# THIS IS THE MAIN BIG LOOP THROUGH ALL THE ASC FILES
	#################################################


# ------------------------
# Create file list on the fly for empty file_list lists
if not file_list:
	file_list = glob.glob('*.asc')


# Write files in file_list to lst file
f = open('files_processed.lst', 'w')
for filename in file_list:
	#strip off extension
	output = filename[:-4]
	#add .da1 extension to filename
	output_da1 = output + ".da1"

	#Write the name of the new file 
	f.write(str(output_da1)+'\n')

# Create set of filenames to exclude based on blink exclusion criteria
# Ignore if auto_exclude = 0
exclude_list = set()

# Create set of filenames to exclude based on DC exclusion criteria
# Ignore if auto_excludeDC = 0

exclude_list_DC = set()

# All filenames to exclude
exclude_list_total = set()

# Initialize empty dictionary to track trials excluded by blinks	
submaster = {}

# Initialize empty dictionary to track trials excluded by DC errors
submaster_DC = {}

# Initialize empty dictionary to track trials excluded by any errors
submaster_total = {}


# ------------------------
for filename in file_list:
	print(filename)
	counter = 0			# count for sequence in experiment
	try:
		input_asc = open(filename, 'r')
	except:
		print("File %s could not be found." %filename)
		sys.exit(0)

# set up to keep record of actual position in sequence
	trial_sequence = []
	#strip off extension
	output = filename[:-4]
	#add .da1 extension to filename
	output_da1 = output + ".da1"

#open the new file
	os.makedirs('da1_files', exist_ok=True) 
	output_da1 = open('da1_files/'+output_da1, 'w')
	
	#do the same things to make a blink file
	output_bli = output + ".bli"
	os.makedirs('blink_files', exist_ok=True)	
	output_bli = open('blink_files/'+output_bli, 'w')

# and if display change, a display change file
	if DC == 1:
		output_DC = output + ".dc"
		os.makedirs('DC_files', exist_ok=True)	
		output_DC = open('DC_files/'+output_DC, 'w')

	
#define var to hold trial exclusions on blinks
	num_exclusions = 0
	
#define var to hold trial exclusions on DC errors

# --------------------------------
# Initalize cond_count dictionary for keeping track of exclusions
	cond_count = {}
	cond_count_DC ={}
	cond_count_total ={}	
# Create dictionary from conditions listed in parameters file
	for i in range(lowest_cond, highest_cond+1):
		# Key: cond_num
		# Value: Number of exclusions
		cond_count.update({i: int(0)})
		cond_count_DC.update({i: int(0)})
		cond_count_total.update({i: int(0)})
# --------------------------------


	#################################################
	# MAKE A FILE THAT CONTAINS ONLY THE RELEFVANT LINES FROM THE ASC FILE:
	# TRIALID, EFIX, BLINK, DISPLAYON/SYNCTIME, TRIAL_RESULT, REGIONS,END SACCADE LINES
	# MODIFIED AUG 2015 TO CONTAIN ONLY E TRIALS - NOT P ETC.
	#################################################

	copy_it = 0
	search_strings = ['TRIALID', 'SYNCTIME', 'EFIX','EBLINK', 'TRIAL_RESULT','REGION','COMPLETED','ESACC']
# SEARCH STRINGS: 0 trialid, 1 trial start, 2 = efix, 3= eblink, 4 trial end, 5 character line, 6 display change, 7 end of saccade
	tempfile = open('tempfile','w+')
	for line in input_asc:
		for entry in search_strings:
			if entry in line:
				fields = line.split()#break line into list
				if search_strings[0] in fields:		# fields 'msg', time, 'TRIALID', ExxIxxDxx; GOT A NEW TRIAL HERE
					copy_it = 0
					counter = counter + 1
# now getting the trialid info
					trialid = fields[3]
					first_split = trialid.split('I')#split into the condition, and then item and dependent
					condition = first_split[0]
					cond_num = condition[1:] #strip off the letter from the beginning of condition
					second_split = first_split[1].split('D')#split into item and dependent
					item_num = second_split[0]
					print(second_split[1])
					#print(trialid, "before")
					if int(cond_num) >= lowest_cond and int(cond_num) <= highest_cond and condition[0] != 'P' and condition[0] == 'E' and int(second_split[1]) == 0: 
						#print(trialid, "after")
#This is taken out now, to allow fillers to be run
#and condition[0] == "E":
						copy_it = 1
						trial_sequence = trial_sequence + [counter]
# GOT A GOOD TRIAL - COPY LINES UNTIL END OF TRIAL
				if copy_it  == 1:
					tempfile.write(line)
				if search_strings[4] in fields:
					copy_it = 0							
# END OF TRIAL
	tempfile.close()
		
	#################################################
	# NOW GO THROUGH TEMPFILE - THE TRIALS TO BE DOCTORED
	#################################################

	output_string_trial = []	# sequence, condition, item
	trial_seq = 0
# fields is the array of values on a given line of a trial
	tempfile = open('tempfile', 'r')
	for line in tempfile:
		row = line
		fields = row.split()			#break line into list
										#this is for the trialid lines
		if search_strings[0] in fields:
# new trial - fields = 'MSG', time, 'TRIALID', ExxIxxDxx
# a bunch of variables that need to be initialized when a trialid line is encountered
			trial_exclude = 0
			blink_region_char_start = 0
			blink_region_char_end = 0
			blink_y_start = 0
			blink_y_end = 0
			x_coord_char = 0
			x_coord_prev = 0
			trial_start_time = 0
			display_change_time = 0
			bad_display_change = "NO DISPLAY CHANGE"
			last_bad_display_change = "NO DISPLAY CHANGE"
			DC_DONE = 0
			shoulda_got_DC = 0
			output_string_fix = []		# list of x, y, start,end values
			output_string_blink = ["#BLINKS: "]
			output_string_DC = " "		# treat it as a string - for formatting purposes
			blink_end_times = []
			numfix = 0
			current_dur = 0
			previous_dur = 0
			repeat = 0
			crossed_into_crit = 0
			crossed_outof_crit = 0
			blink_check_start = 0
			blink_check_end = 0
# stuff for character positions
			temp_xpos = 0
			temp_xpos_R = 0
			temp_ypos = 0
			XPOS = []
			XPOS_R = []					# right edge of char
			YPOS = []
			X_bound_value = 0
#now getting the trialid info
			trialid = fields[3]
			trialid_time = int(fields[1])
			first_split = trialid.split('I')#split into the condition, and then item and dependent
			condition = first_split[0]
			cond_num = condition[1:] #strip off the letter from the beginning of condition
			second_split = first_split[1].split('D')#split into item and dependent
			item_num = second_split[0]
			output_string_trial = [str(trial_sequence[trial_seq]),str(cond_num), str(item_num)]
			output_string_trial_labeled = ['SEQ:',str(trial_sequence[trial_seq]),'C:',str(cond_num),'I:', str(item_num)]
#get character position of critical region for blink exclusion
			if blink_reg_exclude == 'y' and int(cond_num) >= lowest_cond and int(cond_num) <= highest_cond:
				for line in region_file:
					row2 = line
					fields2 = row2.split()#break line into list
# fields 2: item, cond, start, end in characters
					if fields2[0] == str(item_num) and fields2[1] == str(cond_num):
						blink_region_char_start = int(fields2[blink_region + 2])
						blink_region_char_end = int(fields2[blink_region + 3])
						blink_y_start = 0
						blink_y_end = 0
						if multi_line == "y":
							while blink_region_char_start >= 160:
								blink_region_char_start = blink_region_char_start - 160
								blink_y_start = blink_y_start+1
							while blink_region_char_end >= 160:
								blink_region_char_end = blink_region_char_end - 160
								blink_y_end = blink_y_end+1
#								print("COND item x y start x y end",cond_num,item_num,blink_region_char_start, blink_y_start,blink_region_char_end,blink_y_end)
#have to put region file pointer back at top
				region_file.seek(0,0)


	#################################################
	# GET THE CHARACTER POSITIONS, REGION CHAR
	#################################################

		elif search_strings[5] in fields:
			temp_xpos = int(fields[-4])
			temp_xpos_R = int(fields[-2])
			if multi_line == "n":
				temp_ypos = 0
			else:
				top_left_y = int(first_line_y - (line_sep_y/2))
				temp_ypos = int((int(fields[-3]) - top_left_y)/line_sep_y)
# XPOS, YPOS are lists of raw pixel positions of the top left of characters
			if len(XPOS) == 0 or temp_xpos > XPOS[-1] or multi_line == "y":				
				XPOS = XPOS + [temp_xpos]
				XPOS_R = XPOS_R + [temp_xpos_R]
				YPOS = YPOS + [temp_ypos]


	#################################################
	# CHECK FOR DISPLAY ON TIME - NOW SYNCTIME
	#################################################

		elif search_strings[1] in fields:
			trial_start_time = int(fields[1])

	#################################################
	# CHECK FOR DISPLAY CHANGE PROBLEM
	#################################################

# modified things to keep track of each esacc data, set a flag (in_fix = 1) when a fixation is in progress
# and if is is when a DC happens, use the last ESACC data rather than looking for the next esacc
#  

		elif (trial_start_time > 0) and (DC == 1) and (DC_DONE == 0) and (search_strings[7] in fields):		# ESACC, toggle in_fix, save values from ESACC
			in_fix = 1
# set up for later addition of Y value
			cond_str = str(cond_num)
			item_str = str(item_num)
			X_bound_key = (item_str,cond_str)
			if X_bound_key in dcregions:					# don't worry about multiline display changes yet
				X_Y_bound_value = dcregions[X_bound_key]
				X_bound_value = X_Y_bound_value[1]
				Y_bound_value = X_Y_bound_value[0]
				X_end_value = X_Y_bound_value[3]
				Y_end_value = X_Y_bound_value[2]
			end_of_saccade = int(fields[3])
			start_of_saccade = int(fields[2])
			saccade_X_pos = float(fields[7])
			int_saccade_X_pos = int(saccade_X_pos)
			if ((int_saccade_X_pos > int(X_end_value)) and (shoulda_got_DC == 1)):
				bad_display_change = "Missed Display Change_1"
				trial_exclude =  2
				output_string_DC = "C {0:3d} I {1:3d} MISSED DISPLAY CHANGE".format(int(cond_num),int(item_num))
				last_bad_display_change = bad_display_change
				DC_DONE = 1
			elif ((int_saccade_X_pos > int(X_bound_value)) and (shoulda_got_DC == 1)):
				bad_display_change = "Missed Display Change_2"
				trial_exclude =  2
				output_string_DC = "C {0:3d} I {1:3d} MISSED DISPLAY CHANGE".format(int(cond_num),int(item_num))
				last_bad_display_change = bad_display_change
				DC_DONE = 1
			elif display_change_time > 0:						# got display change during saccade
				DC_data = CHECK_DC_VALUES()
				bad_display_change = DC_data[0]
				trial_exclude = DC_data[1]
				output_string_DC = DC_data[2]
				last_bad_display_change = bad_display_change
				display_change_time = 0
				bad_display_change = 0
				DC_DONE = 1
			elif int_saccade_X_pos > int(X_bound_value):
				shoulda_got_DC = 1
			# Update counter with bad DC, print error if verbose mode
			if trial_exclude == 2:
				#print(item_num)
				#print(cond_num)
				cond_count_DC[int(cond_num)] += 1
				cond_count_total[int(cond_num)] += 1
				if args.verbose:
					print("\t Display change problem on cond: " + cond_num + ", item: " + item_num)	


		elif (trial_start_time > 0) and (DC == 1) and (DC_DONE == 0) and (search_strings[6] in fields):		# found DC COMPLETED
			display_change_time = int(fields[1])
			if (in_fix) == 1:									# DC happened after saccade ended
				DC_data = CHECK_DC_VALUES()
				bad_display_change = DC_data[0]
				trial_exclude = DC_data[1]
				output_string_DC = DC_data[2]
				last_bad_display_change = bad_display_change
				display_change_time = 0
				bad_display_change = 0
				DC_DONE = 1
			# Update counter with bad DC, print error if verbose mode
			if trial_exclude == 2:
				cond_count_DC[int(cond_num)] += 1
				cond_count_total[int(cond_num)] += 1
				if args.verbose:
					print("\t Display change problem on cond: " + cond_num + ", item: " + item_num)	
				


	
	#################################################
	# END OF CHECK FOR DISPLAY CHANGE PROBLEM
	#################################################

	#################################################
	# Got a fixation: check the EFIX line
	#################################################

#need to ignore the fixation if the trial start time has not been set yet
		elif (trial_start_time > 0) and (search_strings[2] in fields) and (trial_exclude == 0):
# fields now contains 'EFIX',''R', start time, endtime, duration, x pixel, y pixel, pupil
#this updates the value of previous_dur
#to be the duration from the previous trial
			previous_dur = current_dur
#and this updates the value of x_coord_prev to be the value of x_coord_char from the previous trial
			x_coord_prev = x_coord_char
			start_time_raw = fields[2]
			end_time_raw = fields[3]
#adjust to start time of trial
			start_time_adj = int(start_time_raw) - int(trial_start_time)
			if start_time_adj < 0:
				start_time_adj = 0
			end_time_adj = int(end_time_raw) - int(trial_start_time)
			current_dur = end_time_adj - start_time_adj
			y_coord_pix = float(fields[6])
			if DC == 1:						# toggle, now not in a fixation
				in_fix = 0

	#################################################
	# SET Y TO ZERO OR ADJUST TO WHICH LINE YOU'RE ON
	#################################################

			if(multi_line == "n"):
				y_coord_char = 0
			else:
				y_coord_char = int((int(y_coord_pix) - first_line_y)/line_sep_y)
# now get X position
			x_coord_pix = float(fields[5])
			xi = 0
			yi = 0
# test for outside range Y fixations
			if((y_coord_char > int(YPOS[-1])) | (y_coord_char < 0)):
				y_coord_char = -1
				xi = -1
			else:		# SCAN THROUGH LIST OF Y POSITIONS UNTIL FIND ONE THAT IS AS BIG AS THE FIXATION VALUE - IN LINES
				while(y_coord_char > YPOS[yi]):
					yi = yi + 1
# test for outside range X fixations on this line
				yyi = yi
				while((yyi < len(YPOS)-1) & (YPOS[yyi] == YPOS[yi])):
					yyi = yyi+1
				if yyi < len(YPOS)-1:		# go back to get X position of previous line
					yyi = yyi - 1
#this is now changed to deal with the problem of that extra space
#that sometimes is in the script after the return
#if(x_coord_pix < float(XPOS[0]) or x_coord_pix > float(XPOS_R[yyi])):
				if(x_coord_pix < float(min(XPOS)) or x_coord_pix > float(max(XPOS_R))):
					xi = -1						# outside range
				elif(yi >= 0):
					#while(x_coord_pix > float(XPOS_R[yi])):
					#ABS testing a change 3.30.18
					while(x_coord_pix > float(XPOS_R[xi])):
						xi = xi + 1			# xi starts at 0, is position in array relative to start of current line
						yi = yi + 1 		# yi is the position in the array of characters               
			x_coord_char = xi
			numfix += 1
			temp_eye_data = [str(x_coord_char), str(y_coord_char), str(start_time_adj), str(end_time_adj)]
			output_string_fix = output_string_fix + temp_eye_data

	#################################################
	#this checks if the fixation you just added is within one character of previous fixation
	#doesn't do it if this is the first fix
	#################################################

			if abs(x_coord_char - x_coord_prev) <= 1 and numfix > 1 and x_coord_char != -1 and y_coord_char != -1:
#check if current duration is less than criterion and remove last fixation if so
#and add time to previous fix
				if current_dur < short_crit:
					del output_string_fix[-4:]
					newdur = int(output_string_fix[-1]) + current_dur
					output_string_fix[-1] = str(newdur)
					numfix = numfix - 1
				elif previous_dur < short_crit: #this deletes the previous fixation
#and adds time to the current one
					del output_string_fix[-8:-4]
					newdur = int(output_string_fix[-2]) - previous_dur
					output_string_fix[-2] = str(newdur)
					numfix = numfix - 1
					
	#################################################
	#this checks if you've crossed into the critical region yet
	#if you have, gets the end time of the penultimate fixation
	#and updates value of crossing in variable
	#nothing here needs to be changed for multi-line tracking
	#################################################

			if blink_reg_exclude == 'y' and int(cond_num) >= lowest_cond and int(cond_num) <= highest_cond:
				if int(output_string_fix[-4]) > blink_region_char_start and ((multi_line == "n") | (int(output_string_fix[-3]) == blink_y_start)):
					if crossed_into_crit == 0:
						if numfix <= 1:
							blink_check_start = 0
							crossed_into_crit = 1
						else:
							blink_check_start = int(output_string_fix[-5])
							crossed_into_crit = 1
#this checks if you've violated the saccade dur limit
#between this fixation and the previous one
#and if so, chucks the trial
							if int(output_string_fix[-2]) - int(output_string_fix[-5]) > saccade_dur_crit:
								if trial_exclude < 1:
									trial_exclude = 1
									if args.verbose:
										print("\t Violated the saccade duration limit on cond: " + cond_num + ", item: " + item_num)
									cond_count[int(cond_num)] += 1
									cond_count_total[int(cond_num)] += 1
						
	#################################################
	#this checks if you've crossed out of the region
	#if you have, gets the start time of the current fix
	#and updates value of crossing out variable
	#################################################

#this sets the blink end time if the fixation is after the critical region
			if blink_reg_exclude == 'y' and int(cond_num) >= lowest_cond and int(cond_num) <= highest_cond:
				if int(output_string_fix[-4]) > blink_region_char_end and int(output_string_fix[-3]) == blink_y_end:
					if crossed_outof_crit == 0:
						if numfix <= 1:
							blink_check_end = int(output_string_fix[-2])
							crossed_outof_crit = 1
						else:
							blink_check_end = int(output_string_fix[-2])
#and checks if you've violated the saccade dur limit
#and chucks the trial, if so

							if int(output_string_fix[-2]) - int(output_string_fix[-5]) > saccade_dur_crit:
								if trial_exclude < 1:
									trial_exclude = 1
									if args.verbose:
										print("\t Violated saccade limit on cond: " + cond_num + ", item: " + item_num)
									cond_count[int(cond_num)] += 1
									cond_count_total[int(cond_num)] += 1
							crossed_outof_crit = 1
#							print("item, blink_check_end, fix-2, fix-5, exclude",item_num,blink_check_end,int(output_string_fix[-2]),int(output_string_fix[-5]),trial_exclude)
							
#and this sets the blink end time if the fixation is *before* the critical region, and you've
#already been in the critical region
#does this only if blink_gopast == 0

			if blink_gopast == 0:
				if blink_reg_exclude == 'y' and int(cond_num) >= lowest_cond and int(cond_num) <= highest_cond:
					if int(output_string_fix[-4]) < blink_region_char_start and int(output_string_fix[-3]) == blink_y_end and crossed_into_crit == 1:
						if crossed_outof_crit == 0:
							if numfix <= 1:
								blink_check_end = int(output_string_fix[-2])
								crossed_outof_crit = 1
							else:
								blink_check_end = int(output_string_fix[-2])
#and checks if you've violated the saccade dur limit
#and chucks the trial, if so
								if int(output_string_fix[-2]) - int(output_string_fix[-5]) > saccade_dur_crit:
									if trial_exclude < 1:
										trial_exclude = 1
										if args.verbose:
											print("\t Blink in critical window on cond: " + cond_num + ", item: " + item_num) # comment out after testing
								# Update count_cond dictionary with exclusion
										cond_count[int(cond_num)] += 1
										cond_count_total[int(cond_num)] += 1
								crossed_outof_crit = 1
	
			
	#################################################
	#this is for blink lines:
	#################################################

		elif (trial_start_time > 0) and (search_strings[3] in fields) and (trial_exclude == 0):	# EBLINK
			b_start_time_raw = fields[2]
			b_end_time_raw = fields[3]
#adjust to start time of trial
			b_start_time_adj = int(b_start_time_raw) - int(trial_start_time)
			b_end_time_adj = int(b_end_time_raw) - int(trial_start_time)
#this checks if blink duration results in exclusion of trial
			if blink_reg_exclude == 'y' and  (b_end_time_adj - b_start_time_adj >= blink_dur_crit): # JH Feb 17
				if trial_exclude < 1:
					trial_exclude = 1
					if args.verbose:
						print("\t Blink duration resulted in exclusion on cond: " + cond_num + ", item: " + item_num)	#JH
					# Update count_cond dictionary with exclusion
					cond_count[int(cond_num)] += 1
					cond_count_total[int(cond_num)] += 1
#this saves the blink end time in a string
#for later checking against critical times
			blink_end_times.append(str(b_end_time_adj))
			temp_blink_data = [str(b_start_time_adj), str(b_end_time_adj)]			
			output_string_blink = output_string_blink + temp_blink_data

#this checks if the number of blinks is too many
			if blink_reg_exclude == 'y' and len(output_string_blink)/2 > blink_num_crit: # JH Feb 17
				if trial_exclude < 1:
					trial_exclude = 1
					# For verbose flag
					if args.verbose:
						print("\t Too many blinks on cond: " + cond_num + ", item: " + item_num)	
					# Update count_cond dictionary with exclusion
					cond_count[int(cond_num)] += 1					
					cond_count_total[int(cond_num)] += 1

	
	#################################################
	# this is for trial_result lines
	# END OF TRIAL HAS BEEN REACHED
	#################################################

		elif search_strings[4] in fields:		# RESULT
			trial_seq += 1
			trial_dur = int(fields[1]) - trial_start_time
#this sets blink_check_end, in case it never got set previously
			if blink_check_end == 0 and blink_check_start > 0:		#CC Feb 18 2016
				blink_check_end = trial_dur
			exit_code = fields[3]
			middle_part = [str(trial_dur), str(exit_code), str(numfix)]
			middle_part_labeled = ['DUR:',str(trial_dur), 'EXIT:',str(exit_code), 'NFIX', str(numfix), 'DC:',str(last_bad_display_change)]
			last_bad_display_change = 0
			output_string_da1 = output_string_trial + middle_part + output_string_fix
			output_string_bli = output_string_trial_labeled + middle_part_labeled + output_string_blink
# this checks if any blink was in the critical time window
			for time in blink_end_times:
				if blink_reg_exclude == 'y' and int(time) > blink_check_start and int(time) < blink_check_end: # JH Feb 17
					if trial_exclude < 1:		# allows for trial_exclude == 2, DC problem?
						trial_exclude = 1
						# For verbose flag
						if args.verbose:
							print("\t Blink in critical window on cond: " + cond_num + ", item: " + item_num)
						# Update count_cond dictionary with exclusion
						cond_count[int(cond_num)] += 1
						cond_count_total[int(cond_num)] += 1

	#################################################
	#this is to output excluded subjects to the  exclude.lst file 
	#################################################			
			
			if auto_exclude == 1 and auto_exclude_DC == 0:
# Test to see if any value in cond_count dictionary is above threshold
				for k,v in cond_count.items():		# Search through key, value pairs
					if int(v) >= exclude_threshold:	# Test for values at or above threshold
						exclude_list.add(filename[:-4]+".da1")	
# Test to see if sum of all values in cond_count dictionary are above absolute threshold
				for k,v in cond_count.items():		# Search through key, value pairs
					if sum(cond_count.values()) >= abs_exclude_threshold:	# Test for values at or above threshold
						exclude_list.add(filename[:-4]+".da1")			
# Add to submaster			
			submaster[filename] = cond_count

	
# Same as above, but for DC experiments
			
			if auto_exclude_DC == 1 and auto_exclude == 0:
# Test to see if any value in cond_count dictionary is above threshold
				for k,v in cond_count_DC.items():		# Search through key, value pairs
					if int(v) >= exclude_threshold:	# Test for values at or above threshold
						exclude_list_DC.add(filename[:-4]+".da1")	
# Test to see if sum of all values in cond_count dictionary are above absolute threshold
				for k,v in cond_count_DC.items():		# Search through key, value pairs
					if sum(cond_count_DC.values()) >= abs_exclude_threshold:	# Test for values at or above threshold
						exclude_list_DC.add(filename[:-4]+".da1")			
# Add to submaster_DC			
			submaster_DC[filename] = cond_count_DC


			if auto_exclude == 1 and auto_exclude_DC == 1:
	# Test to see if any value in cond_count dictionary is above threshold
				for k,v in cond_count_total.items():		# Search through key, value pairs
					if int(v) >= exclude_threshold:	# Test for values at or above threshold
						exclude_list_total.add(filename[:-4]+".da1")	
	# Test to see if sum of all values in cond_count dictionary are above absolute threshold
				for k,v in cond_count_total.items():		# Search through key, value pairs
					if sum(cond_count_total.values()) >= abs_exclude_threshold:	# Test for values at or above threshold
						exclude_list_total.add(filename[:-4]+".da1")			
	# Add to submaster_DC			
			submaster_total[filename] = cond_count_total
			

	#################################################
	#this is to output the line to the .da1 file and blink file
	#if the condition is included, and the trial is not excluded
	#################################################

			if int(cond_num) >= lowest_cond and int(cond_num) <= highest_cond and trial_exclude == 0:
				joined_da1 = ' '.join(output_string_da1)
				output_da1.write(joined_da1)
				output_da1.write("\n")
			
#this is to output the line to the blink file
				output_bli.write("accept: ")
				joined_blink = ' '.join(output_string_bli)
				output_bli.write(joined_blink)
				output_bli.write("\n")
				
			if int(cond_num) >= lowest_cond and int(cond_num) <= highest_cond and trial_exclude == 1:
				num_exclusions += 1
				output_bli.write("rej BL: ")
				joined_blink = ' '.join(output_string_bli)
				output_bli.write(joined_blink)
				output_bli.write("\n")

			if int(cond_num) >= lowest_cond and int(cond_num) <= highest_cond and trial_exclude == 2:
				num_exclusions += 1
				output_bli.write("rej DC: ")
				joined_blink = ' '.join(output_string_bli)
				output_bli.write(joined_blink)
				output_bli.write("\n")
				if DC == 1 and trial_exclude == 2:
					output_DC.write(output_string_DC)
					output_DC.write("\n")

	tempfile.close()		

	#################################################
	# END OF LOOP THROUGH TEMPFILE - END OF THE SUBJECT
	#################################################


	#this is to write out some stuff for each subject to a summary file
	sumsub.write('%r %d \n' %(filename, num_exclusions))
	
	output_da1.close()
	output_bli.close()
	if DC:
		output_DC.close()

	# Print total number of excluded trials while program executes
	if args.verbose:
		if blink_reg_exclude == 'y': # JH Feb 17
			print('\t -------------------------------------------')
			print('\t Total number of excluded trials (BLINK): ' + str(sum(cond_count.values())))
		if DC == 1:
			print('\t Total number of excluded trials (DISPLAY CHANGE): ' + str(sum(cond_count_DC.values())))


	#################################################
	# END OF LOOP THROUGH THE ASC FILES
	#################################################


# ---------------------------------
# Prints how many subjects were added to exclude.lst and write filenames to list.

if auto_exclude == 1 and auto_exclude_DC == 0 & args.verbose:
	print("\n")
	print("========================================")
	print("\n")
#	print("Number of subjects excluded: " + str(len(exclude_list.union(exclude_list_DC))))
	print("Number of subjects excluded: " + str(len(exclude_list)))
	
if auto_exclude == 0 and auto_exclude_DC == 1 & args.verbose:
	print("\n")
	print("========================================")
	print("\n")
#	print("Number of subjects excluded: " + str(len(exclude_list.union(exclude_list_DC))))
	print("Number of subjects excluded: " + str(len(exclude_list_DC)))
	
if auto_exclude == 1 and auto_exclude_DC == 1 & args.verbose:
	print("\n")
	print("========================================")
	print("\n")
	print("Number of subjects excluded: " + str(len(exclude_list_total)))


# Create list of da1 files
file_list_da1 = set()
for filename in file_list:
	file_list_da1.add(filename[:-4]+".da1")		

# Write exclude_list to lst file for EyeDry

# Auto exclude for blinks only
if blink_reg_exclude == 'y' and auto_exclude == 1 and auto_exclude_DC == 0: #JH Feb 17
	# Write list of files to keep
	keep = file_list_da1 - exclude_list 
	for i in sorted(keep):
		kp.write(i)
		kp.write("\n")
	kp.close
	
	# Write list of file to exclude
	for i in sorted(exclude_list):
		e.write(i)
		e.write("\n")
	e.close()
	
# Auto exclude on DC only	
if auto_exclude == 0 and auto_exclude_DC == 1:
	# Write list of files to keep
	keep = file_list_da1 - exclude_list_DC # check on this!
	for i in sorted(keep):
		kp.write(i)
		kp.write("\n")
	kp.close

	# Write list of file to exclude
	for i in sorted(exclude_list_DC):
		e.write(i)
		e.write("\n")
	e.close()
	
# Auto exclude on blinks and DC 	
if blink_reg_exclude == 'y' and auto_exclude == 1 and auto_exclude_DC == 1: #JH Feb 17
	# Write list of files to keep
	keep = file_list_da1 - exclude_list_total
	for i in sorted(keep):
		kp.write(i)
		kp.write("\n")
	kp.close

	# Write list of file to exclude
	for i in sorted(exclude_list_total):
		e.write(i)
		e.write("\n")
	e.close()

# ----------------------------------------------
# Summary of trials removed by condition printed in table in csv file.
if  blink_reg_exclude == 'y': # JH Feb 17

	# Open csv file
	with open('Blinks-summary.csv', 'w', newline='') as f:
		w = csv.writer(f)
		# Create headers
		headers = list(range(lowest_cond, highest_cond+1))
		headers.insert(0, 'Filename')
		headers.insert(1, 'Excluded?')
		# Write headers
		w.writerow(headers)
		# Iterate over dictionary
		for k, v in sorted(submaster.items()):
			# Initialize list for writing row
			row = []
			# Some muckery
			j = k[:-4]+".da1"
			row.append(j)
			# Notes if file has been automatically excluded
			if j in exclude_list:
				row.append('Auto excluded')
			else:
				row.append('')
			# Assigns number of excluded trials by condition
			for cond in range(lowest_cond, highest_cond+1):
				row.append(v[cond])
			w.writerow(row)


if DC == 1:
	with open('Display-change-errors-summary.csv', 'w', newline='') as f:
		w = csv.writer(f)
		# Create headers
		headers = list(range(lowest_cond, highest_cond+1))
		headers.insert(0, 'Filename')
		headers.insert(1, 'Excluded?')
		# Write headers
		w.writerow(headers)
		# Iterate over dictionary
		for k, v in sorted(submaster_DC.items()):
			# Initialize list for writing row
			row = []
			# row.append(k)
			# Some muckery
			j = k[:-4]+".da1"
			row.append(j)
			# Notes if file has been automatically excluded
			if j in exclude_list_DC:
				row.append('Auto excluded')
			else:
				row.append('')
			# Assigns number of excluded trials by condition
			for cond in range(lowest_cond, highest_cond+1):
				row.append(v[cond])
			w.writerow(row)


sumsub.close()

