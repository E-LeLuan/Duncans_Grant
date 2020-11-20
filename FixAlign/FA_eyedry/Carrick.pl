use Fcntl;

print "\t\t\tLINK6DROP program (6/23/05)\n\nThis program converts Eyelink ASC output files from the UMASS eyelink programs\n into a format that can be read by Henderson's picdry2 program\n";
print "NOTE This program uses the UMASS style of condition and item numbers E##I##D##\n";
print "NOTE: This program only works for one eye at a time, and it won't handle dependent trials right.\n\n";

 print "Please provide output file extension (e.g., da1): ";
 $ext = <STDIN>; 
chomp ($ext);   


######################################################
#####       Start Parameter setup                #####
######################################################
##### This bit of code attempts to read in a     #####
##### bunch of parameters that I don't want to   #####
##### type in everytime that I ran the program   #####
#####       					                 #####
#####  Things included:			                 #####
#####	Eye being tracked (if more than 1)		 #####
#####   number of conditions	                 #####
#####   range of condition numbers               #####
#####   Correct response for each condition      #####
#####	Stimuli Resolution (X and Y)			 #####
#####   I guess there is a maximum of 400		 ##### 
#####		conditions    					     #####
######################################################
for ($q = 0; $q < 500; $q++)
	{
	$paramarray[$q] = 0;
	}


#####  Firstly, I check to see if there is a parameter file already made ##### 
print "Have you created a parameter file? (y or n) ";
$tempcr = <STDIN>; 
chomp ($tempcr);
	if ( ($tempcr eq "y")||($tempcr eq "Y"))
	{
	#####  If there is one, this reads in the name of the file and the data from the file #####
		print "Please provide the parameter file name: ";
 		$paraname = <STDIN>; 
		chomp ($paraname);   
		open (PARAMETERFILE, $paraname) or die "Can't open datafile: $!\n";
		$s = 0;
		while ($condline = <PARAMETERFILE>) 
		{
			chomp $condline;
			$paramarray[$s] = $condline;
			print "$paramarray[$s]\n";
			$s = $s +1; 

    	}

	close PARAMETERFILE;
	}
	else 
	{
	##### If there is no parameter file, This will create the file #####
		print "Create new parameter file.\n";
		print "Please provide a parameter file name: ";
		$newparaname = <STDIN>; 
		chomp ($newparaname);   
		sysopen(NEWPARAMETERFILE, $newparaname, O_WRONLY|O_CREAT) or die "Can't open output file: $!\n";	
	
	##########################################################################
	##### The Picdry program only takes the data from one eye, so this   #####
	##### question asks which eye is to be analyzed.				     #####
	##### If there is only one eye recorded (which seems to be standard) #####
	##### the eye in the file is analyzed. If both eyes are there, this  #####
	##### will tell the program to pick either the R or L eye. 			 #####
	##########################################################################
	
	print "\nDoes the ASC file contain data from both eyes? y or n (Default = no) ";
	$eyenum = <STDIN>; 
	chomp ($eyenum);

	if ( $eyenum ne "y")
	{
		$paramarray[0] = "n";
		$paramarray[1] = "nada";
		# $eyenum = "n";
		# $whichI = "nada";
	}
	else
	{
		print "\nWhich eye do you want to analyze? r or l (default = r) ";
		$whicheye = <STDIN>; 
		chomp ($whicheye);
		# $eyenum = "y";
		$paramarray[0] = "y";
		if (($whicheye eq "l")||($whicheye eq "L"))
		{
		 # $whichI = "L";
		 $paramarray[1] = "L";
		}
		else 
		{
		   #   $whichI = "R";
		$paramarray[1] = "R";
		}
	}

    ########################################################################
	##### This section takes in the stimuli resolution because Eyelink #####
	##### records everything in the windows resolution of the display. #####
	##### This means that while the stimuli might be 800x600, Eyelink  #####
	##### might record its gaze coordinates (which are the data used)  #####
	##### in 1280x1024 or 1600x1200 pixels. In order to put the 	   #####
	##### fixations into the stimuli coordinates (which is how Picdry  #####
	##### operates) a correction/conversion is needed. Also, if people #####
	##### change the base resolution of the screen (like what		   #####
	##### happened to me), the current program will make the  		   #####
	##### appropriate correction back to the pixel coordinates of the  #####
	##### stimuli. 													   #####
	##### I still don't understand the logic of the Eyelink system 	   #####
	##### in this, but what can I do other than fix it. Alas.   	   #####
	########################################################################
	print "\nStimuli Screen Resolution X dimension (in pixels; e.g., 800) :";
		$screenx = <STDIN>; 
		chomp ($screenx);
		
		$paramarray[2] = $screenx;  ###### Size of X dimension of stimuli in pixels ##### 


	print "\nStimuli Screen Resolution Y dimension (in pixels; e.g., 600) :";
		$screeny = <STDIN>; 
		chomp ($screeny);
		
		$paramarray[3] = $screeny;  ###### Size of Y dimension of stimuli in pixels ##### 
		
		############################################################################################
		##### Picdry can use a accuracy variable which we don't get out of the Eyelink program #####
		##### Here is a way to get that data into the output. This is not elegant, but it does #####
		##### work. But it only works if the correct response is always the same for a         #####
		##### condition number. Currently, it matches the button pressed with the condition-   #####
		##### correct response to see if the response was accurate. If you can't think far 	   #####
		##### enough ahead to use different conditions for different responses then you won't  #####
		##### get what you want. Therefore, THINK AHEAD WHEN MAKING THE STIM FILES!!!!!!!!!!   #####
		#####																				   #####
		##### Buttons: Blue = 1, Red = 2, Green = 3, Yellow = 4, Purple = 5					   #####
		############################################################################################		
		print "\nCorrect responses\n These will need to be consecutive condition numbers, \nbut can start at any number you want. \n\n";
		print "\n What is the low condition number? ";
		$condnumlow = <STDIN>; 
		chomp ($condnumlow);

		print "\nWhat is the high condition number? ";
		$condnumhigh = <STDIN>; 
		chomp ($condnumhigh);
	
		$high = $condnumhigh +1;
		
		print "\n";
		
		for ($w = $condnumlow; $w < $high; $w++)
		{	# This loop takes in the correct responses for each condition in the experiment
		print "Correct response for Condition $w? ";
		$condacctemp = <STDIN>; 
		chomp ($condacctemp);
		$paramarray[$w+3] = $condacctemp;
		}
	
	for ($p = 0; $p < ($condnumhigh+4); $p++)
	{ # Here we write out the parameter file
	print NEWPARAMETERFILE "$paramarray[$p]\n";
	}


	close NEWPARAMETERFILE;
	}
	
	##############################################
	#####	End of Parameter file writer	######
	##############################################


$eyenum = $paramarray[0];
$whichI = $paramarray[1];
$resx = $paramarray[2];#screen X coordinates 
$resy = $paramarray[3];#screen Y coordinates
#print "\n$eyenum    $whichI";

######################################################
#####      End of parameter reading              #####
######################################################


 print "Please provide the data list file: ";
 $lstname = <STDIN>; 
chomp ($lstname);   

open (LISTFILE, $lstname) or die "Can't open listfile: $!\n";
#gets the list file in from the user which contains the names of the files that will be run





while ($filename = <LISTFILE>){



	# print "Please provide the data file: ";
 #	$name = <STDIN>; 
	chomp ($filename);   

	open (INDATA, $filename) or die "Can't open datafile: $!\n";
	#gets the data file in ASCII format from the user


	substr($filename, -3, 3) = $ext;
	#in a rather ugly way, I change the file extension for output
 
	unlink($filename); # this erases output prior to adding input to prevent garbage from being left around


	sysopen(FH, $filename, O_WRONLY|O_CREAT) or die "Can't open output file: $!\n";

	# sysopen(FH, $name, O_WRONLY|O_EXCL|O_CREAT) or die "Can't open output file: $!\n";
	# this one will not run if the file exists : O_EXCL means fail if exists
	print "Output file name:\t$filename\n";
	######################################################
	#####        End of File Setup                   #####
	######################################################



	######################################################
	#####        Begin file reading                  #####
	######################################################


	$trialnum = 0;
	$displayon = 0;
	$fixcount = 0;
#		print "Trial\tCond\tItem\n";
  	while ($line = <INDATA>) {

      @dataline = split(" ", $line); # this splits the line up using space as the delimiter

	$numel = @dataline; # gets the total number of elements on the line -- I don't know if this is needed or not

	
	if ($dataline[0] eq "MSG")
	{
	   if ($dataline[2] eq "TRIALID") # this finds the beginning of a trial
	   {
		$conditem = $dataline[3]; # this is the condition/item identifier
		
		#print "$conditem\n";
		
		@conditionitem = split(/I/,$conditem);
		
		@itemcatch = split(/D/,$conditionitem[1]);
		
		$item = $itemcatch[0];
		$cond = $conditionitem[0];
		substr($cond, 0, 1) = " ";       # delete first character
		
#		print "$trialnum\t$cond\t$item\n";
		
		#print "$conditionitem[0], $conditionitem[1]"; #debugging stuff
		
		#print FH "$conditem\t";
		
		$trialokflag = 0;
	
	   }
	   
	   if (($dataline[2] eq "TRIAL") && ($dataline[3] eq "OK"))
	   {
	   $trialokflag = 1;
	   }		

  	   if (($dataline[2] eq "DISPLAY") && ($dataline[3] eq "ON")) 
	  {
	  $trialstarttime = $dataline[1];
	  $displayon = 1;
	  @fixlister = ("");
	  $fixcount = 0;
	  # I added this in case there were some fixations were recorded before the display came on
	  # the flag that is set is checked before the EFIX is read in

		#print "\ndisplayon\n"; #debugging statement
	  }

  	   if ($dataline[2] eq "GAZE_COORDS") 
	  {
	  	$screenresX = $resx/($dataline[5]+1);
	  	$screenresY = $resy/($dataline[6]+1);
	  	
	  # This if statement constantly checks the coordinates that the eyelink is recording in and compares it to the 
	  # stimuli dimensions. What I get out of this is a multiplier that can be used to convert Eyelink coordinates into
	  # pixel coordinates of the actual stimuli.
	  # By checking every trial, we insure that nothing funky is happening
	  #
	  }

	if (($dataline[2] eq "ENDBUTTON")&& ($displayon == 1))
	{
		$trialendtime = $dataline[1];
		$rt = $trialendtime - $trialstarttime;
		$resp = $dataline[3];
		
		if ($resp == $paramarray[$cond + 3]) {
		$respacc=1;
		}
		else{
		 $respacc = 0;
		}
	}



	}
	
		

	if (($dataline[0] eq "EFIX") && ($displayon == 1))
	{
	
	if (($eyenum eq "n") || ($dataline[1] eq $whichI))
		{ # allows only those fixations that come from only one eyes
		
	# Here are all of the fixation information bits:

	  $fixstart = $dataline[2];
	  $fixend = $dataline[3];
	  $fixduration = $dataline[4];
	  $fix_xpos = sprintf("%.0f", ($dataline[5] * $screenresX));# This is a correction for the screen resolution 
	  $fix_ypos = sprintf("%.0f", ($dataline[6] * $screenresY)); # This is a correction for the screen resolution

  
		$fixcount = $fixcount +1;	
	#print "$dataline[2]\t$dataline[3]\t$dataline[4]\t$dataline[5]\t$dataline[6]\t";
	#print FH "$fixstart\t$fixend\t$fixduration\t$fix_xpos\t$fix_ypos\t";
	push (@fixlister, $fix_xpos, $fix_ypos, $fixstart, $fixend ); 
	# this line puts in X Y Start Stop into the array: This is the way that Gary wants it
		}
	}
	
	
	if ($trialokflag == 1)
	{
	#print "\n";
	#$respacc = 0; # a filler for the accuracy of the response
	print FH "$trialnum\t $cond\t $item\t $rt  \t $resp\t $fixlister[3]\t $respacc\t $fixcount\t"; #fixlister[3] is the start time
	$a = @fixlister;
	#$a=$a+1;
	if (@fixlister[a-1] > $trialendtime){
	@fixlister[a-1] = $trialendtime;} # fixes a problem with the fixation time
	
	for ($i = 1; $i < $a; $i++)
		{
		print FH "$fixlister[$i]\t";
		}
	
	
	# print FH "@fixlister";
		print FH "\n"; 
	$displayon = 0;
	$trialnum=$trialnum+1;
	$trialokflag = 0;
	}
	
	    
  }

print ">>>>>finished file: $filename<<<<<\n";
close INFILE;
close FH;
}# this is the end of the while loop that runs the list file stuff
print ">>>>>finished all files<<<<<\n";
<>