
print("Enter the name of your sentence file: ");
$inputfile = <STDIN>;

print ("The file you input is: $inputfile");

#next make the output file

print("What do you want your output file to be called?");
$outputfile = <STDIN>;

open(inputfile, $inputfile) or die("can't open file/n");
open(outputfile, ">$outputfile") or die("can't write to file/n");

while ($line = <inputfile>) {

$line =~ s/\{.+?\}//g;



print(outputfile "$line");

}

close(inputfile);
close(outputfile);

print("done!")