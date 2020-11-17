#this is a script to make a count (.cnt) file from a delimited eyetrack script file (.dry)
#for multi-line text, starts the second line on character 160, third on 120, and so on.

import sys

input_file = input("What is the name of your delimited script file?")
	
output_file = input_file[:-4] + ".cnt"
output_file = open(output_file, 'w')

try:
	dry_file = open(input_file,'r')
	
except:
	print ("Your delimited script file could not be found.")
	sys.exit(0)

delim_char = input("What character do you use as your region delimiter?")

lowest_cond = int(input("What is the lowest condition number to be analyzed?"))
highest_cond = int(input("What is the highest condition number to be analyzed?"))

#have to include the space after 'trial', otherwise you get the trialtype line
search_strings = ['trial ','inline']

tempfile = open('tempfile','w+')

for line in dry_file:
	for x in search_strings:
		if x in line:
			tempfile.write(line)
			print(line)
			
tempfile.seek(0,0)

cond_include = 0
	
for line in tempfile:
	if search_strings[0] in line:
		fields = line.split()
		trialid_split = fields[1].split('I')
		condition = trialid_split[0]
		cond_num = int(condition[1:])
		second_split = trialid_split[1].split('D')
		item_num = int(second_split[0])
		if cond_num >= lowest_cond and cond_num <= highest_cond:
			cond_include = 1
			#print(line)
			
	else:
		if cond_include == 1:
			#print(cond_num)
			fields = line.split("|")
			lines = fields[1].split("\\n")
			#this adds a fictitious last line after the last \n
			#so that needs to be taken off
			lines = lines[:-1]
			num_lines = len(lines)
			reg_counter_final = []
			for n in range(len(lines)):
				region_sum = 0
				region_counter = []
				regions = lines[n].split(delim_char)
				#again, seems to add a fictitious last region
				regions = regions[:-1]
				for r in range(len(regions)):
					cur_reg_length = len(regions[r])
					region_sum = region_sum + cur_reg_length
					region_counter.append(str(region_sum + (160*n)))
				reg_counter_final.extend(region_counter)
		
			output_line = [str(item_num), str(cond_num), str(len(reg_counter_final) + 1), '0']
			output_line = output_line + reg_counter_final
			output_string = ' '.join(output_line)
			output_file.write(output_string)
			output_file.write("\n")
			cond_include = 0
					
output_file.close()
			
		
	
	
