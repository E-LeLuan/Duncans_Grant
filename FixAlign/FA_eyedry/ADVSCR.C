#include "stdlib.h"
#include "stdio.h"
#include "string.h"
#include "dos.h"
#include "ctype.h"
#include "mem.h"
#include "alloc.h"
#include "io.h"
#include "stddef.h"
#define CPMEOF 0x1a
#define ERR -1

main()
{
char bigbuf[35][400];
char fname[20];
char temp[10];
char terminator;
int printx;
int charac;
int i,j;
FILE *isens, *ptrs, *osens;
printf("\nWhat is name of input sentence file? ");
while((isens = fopen(gets(fname),"r")) == 0)
	printf("\nNope, try again: ");
printf("\nWhat is name of file of random order? ");
while((ptrs = fopen(gets(fname),"r")) == 0)
	printf("\nNope, try again: ");
printf("\nWhat is name of output sentence file? ");
if((osens = fopen(gets(fname),"w")) == 0)
	{
	printf("\nNo can do!");
	exit(0);
	}
printf("\nWhat is the terminator character for each element in file? ");
gets(fname);
terminator = fname[0];
if(terminator == '\0')
	terminator = 0x0A;	/* LF */

printf("\nDo you want console output? y or n: ");
gets(temp);
printx = tolower(temp[0]);

/*for(i=0;fgets(bigbuf[i++],200,isens) != 0;)
	printf("\nSentence %d in, %s",i,bigbuf[i-1]); */

for(i=0,j=0; (charac = fgetc(isens)) != ERR && charac != CPMEOF;)
	{
	if(charac != 0x0d)
		{
		if(charac == 0x0a)
			charac = '\n';
		bigbuf[i][j++] = charac;
		if(charac == terminator)
			{
			bigbuf[i++][j] = '\0';
			j = 0;		/* column number */
			printf("\n%d",i);
			if(printx == 'y')
				printf(" Sentence IN, %s",bigbuf[i-1]);
			}
		}
	}
printf("\n%d sentences read in; last one was %s.",i,bigbuf[i-1]);
printf("\nType any character to continue.");
getchar();
j = 1;
while(fscanf(ptrs,"%d",&i) != ERR)
	{
	fputs(bigbuf[i-1],osens);
	printf("\n%d (%d)",j++,i);
	if(printx == 'y')
		printf("\n sentence %d OUT, %s",i,bigbuf[i-1]);
	}
fclose(osens);
fclose(isens);
fclose(ptrs);
}
