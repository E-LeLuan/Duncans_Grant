#takes in .asc files, spits out all question data

import sys
import glob
import csv
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('filename', nargs = "?", help="Optional argument to provide the parameter file.")
parser.add_argument('--verbose', '-v', action="store_true", help="Optional argument to print information about question accuracy.")
args = parser.parse_args()
if args.filename:
	filename = args.filename
else:	
	filename = input("What is the name of your parameter file?")
	
try:
	parameters = open(filename,'r')
	#this reads the file as text
	whole_file = parameters.read()
	#this executes the whole thing as code
	exec(whole_file)
	
except:
	print("Your parameter file could not be found. Or, there is an error in the file.")
	sys.exit(0)
	
# Allows empty file_list variable in parameters file. If not specified, uses all asc files in directory.
if not file_list:
	file_list = glob.glob('*.asc')

#and make a subject summary file
subj_sum = []
subj_quest_file = open('subj_quest.txt', 'w')
summary_file = open('QuestSum.txt','w')
summary_file.write('Subj Nques Cans Pcorr\n')
for file in file_list:

	try:
		filename = open(file, 'r')
	except:
		print("File %s could not be found." %file)
		sys.exit(0)

	if args.verbose:
		print(file)
		
	search_strings = ['TRIALID', 'QUESTION_ANSWER', 'TRIAL_RESULT']
	temp_quest_file = open('temp_quest_file','w+')
	for line in filename:
		for entry in search_strings:
			if entry in line:
				temp_quest_file.write(line)
				
	temp_quest_file.seek(0,0)
	qcount = 0		# count of questions
	acount = 0		# count of accurate answers
	for line in temp_quest_file:
		if search_strings[0] in line:
			correct = 'none'
		
			fields = line.split()
			start_time = int(fields[1])
			trialid = fields[3]
			first_split = trialid.split('I')#split into the condition, and then item and dependent
			condition = first_split[0]
			cond_num = condition[1:] #strip off the letter from the beginning of condition
			second_split = first_split[1].split('D')#split into item and dependent
			item_num = second_split[0]
		
		elif search_strings[1] in line:
			fields = line.split()
			correct = fields[3]
			
		else:
			fields = line.split()
			end_time = int(fields[1])
			answer = fields[3]
			
			#write out line, if the item had a question
			if correct != 'none':
				#print("foo")
				#print(file, cond_num, item_num, correct, answer, str(end_time - start_time))
				output_line = [file[:-4], cond_num, item_num, correct, answer, str(end_time - start_time)]
				subj_quest_file.write(' '.join(output_line))
				subj_quest_file.write('\n')
				qcount = qcount + 1
				if correct == answer:
					acount = acount + 1

	temp_quest_file.close()
	
	if args.verbose:
		print(file,qcount,acount,float(acount/qcount))
		
	subj_sum = [file, str(qcount), str(acount),str(float(acount/qcount))]
	subj_sum_join = ' '.join(subj_sum)
	summary_file.write(subj_sum_join)
	summary_file.write('\n')

subj_quest_file.close()
summary_file.close()
