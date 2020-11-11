#This is a Perl script that changes multiple choice answers in brackets in Linger data into numbers that can
#be analysed in R.
#e.g., {3 - so so} --> 3

#Meg Grant, 2008


#first we need to input the data file

# print("Enter the name of your sentence file: ");
$ARGV=shift;
$inputfile = $ARGV;

print ("The file you input is: $inputfile\n");

#next make the output file

#print("What do you want your output file to be called?");
$ARGV=shift;
$outputfile = $ARGV;

print ("The output file is: $outputfile\n");

open(inputfile, $inputfile) or die("can't open file/n");
open(outputfile, ">$outputfile") or die("can't write to file/n");

while ($line = <inputfile>) {

$line =~ s/{Terrible}/1/;
$line =~ s/{pretty_bad}/2/;
$line =~ s/{so_so}/3/;
$line =~ s/{not_so_bad}/4/;
$line =~ s/{Perfect}/5/;

print(outputfile "$line");
}
close(inputfile);
close(outputfile);

print("done!")