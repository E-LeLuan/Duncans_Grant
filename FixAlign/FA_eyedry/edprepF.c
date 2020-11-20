/* version to build a dry file for fillers */

/* EDPREP */
/* EYEDRY PREP FOR SR EYELINK 2 EYETRACK */
/* REVISED AND TIDIED UP 2/10/2005 */
/* REVISED TO WORK WITH THE E...D0 CONDITION IDENTIFICATION OF EYETRACK0.6.0 
renamed EDPREP60 - to be renamed as EDPREP sometime
AUGUST 1 2005 */

/* MODIFIED TO IGNORE NEWLINES THAT FOLLOW NEWLINES - TO BE COMPATIBLE WITH EYEDOCTOR */

/* note, not the same as the previous edprep (saved as edprep.col);
it didn't work right. This is a revision of etedprep.c, with some
features of edprep.c added */

/* TEMPLATE OF PROGRAM TO OUTPUT ONE FILE OF
POSITIONS OF START OF EACH WORD FOR EACH CONDITION-ITEM
COMBINATION. PRODUCES A SINGLE NUMBER 0-159, 160-319, ETC.,
FOR START OF EACH WORD, BY COMBINING X AND Y VALUE.

TO USE IT:
1. copy your script as a del file (sticking to the 8.3 convention)
2. edit the headers off the del file, so that it starts with the first trial.
3. Put your delimiter (e.g. ^) at the START of each analysis region apart
from the first region. Generally, put it before the space the precedes
the first word in the region. Put a ^ at the end of the text - eyedry
will create a fake region 20 characters long afte the end of the text.
Leave the \n characters in the file, i.e., don't change them to
newlines or CR/LF. If you do have newlines, replace them with \n
4. Don't bother deleting any extra information on the trial lines
of the script file, as long as the trial name is a single word (without
spaces). The program skips program name and any extra stuff, including
the "inline" notation. 
5. run edprep60 (it should be on the path; you don't have to add any code).
You can give it the names of your .dry (.del) and .cnt files as command line
arguments.
6. tell edprep the min and max of the conditions you want to analyze, and
give it the name of your .del file and your output .cnt file
7. look at it in VEDIT. 

This results in a cnt file that might just let you analyze your eyelink da1
files in standard eyedry. The one trick is that it assumes you have up to 160
characters on a display line. If you have more than that, you'll be messed up!
Otherwise, type in 160 when eyedry asks you how many characters you have on
a line.

*/


/* 6/18/89 */
/* 11/15/89 Error with repeating condition numbers corrected */
/* 11/25/89 updated to allocate memory dynamically */
/* 9/19/2004 modified for EyeTrack (Stracuzzi) */
/* 2/10/2005 revised to skip over unneeded material on a trial line*/
/* 12/2009 revised not to skip leading spaces */
/* 1/2013 revised to ignore multiple newlines (eyedoctor compatability - from edprepnl.c */

/* THE FOLLOWING DEFINES CAN BE TAILORED TO YOUR EXPERIMENT.
SET max_chars TO THE NUMBER OF CHARACTERS IN ONE ITEM
BEFORE YOU DO ANY SELECTION, AND max_regions TO THE MAXIMUM NUMBER OF ANALYSIS
REGIONS YOU HAVE. IF THE NUMBERS GET TOO BIG, YOU MAY NOT BE ABLE TO FIT
THE WHOLE EXPERIMENT INTO MEMORY. */

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <dos.h>
#include <ctype.h>
#include <stdarg.h>
#include <sys\stat.h>
#include <fcntl.h>
#include <mem.h>
#include <alloc.h>
#include <io.h>
#include <stddef.h>
#include <conio.h>
int input(char *output);

#define LF 0x0a
#define CR 0x0d
#define max_chars 2500		/* longest item, before any cleaning up */
#define max_regions 50		/* max number of anaysis regions */
FILE *countout,*printer,*infile;

void main(int argc, char* argv[])		/* OBLIGATORY STUFF */
{
char temp[100];
char c;
int CCOND,CTRIAL,CITEM;
char *tempitem;
char *ttl,*tsptr;
int xyv;
int basepos;
char *xyp;
unsigned int xyc,xval;
int i,j,k,ic;
int linelength;
int bSCON,eSCON;
int tCCOND;
char delim;
char string[80];
char tempstring[80];
int hc;
int maxcond,mincond,adjmaxcond,minsen,maxsen;
int *mat_tCCOND,*mat_CITEM,*mat_xyc;
int *mat_pos;
char *mat_sen;
char *FindString(char *dv,char *dc);
int PartStringCompare(char *tempstring,char *compstring);
char *FindInlineString(char *tempitem,int item,int cond);
int gotNL;

printf("\n\nNOTE: FILLER ITEMS ONLY!\n\nTHIS VERSION IS SET FOR A MAXIMUM OF %d CHARACTERS\nAND %d ANALYSIS REGIONS IN A DISPLAY;\nREVISE THE #DEFINES TO CHANGE THEM IF YOU RUN OUT\nOF MEMORY ALLOCATION SPACE!\n\nNOTE: IGNORES MULTIPLE NEWLINES, LIKE EYEDOCTOR DOES\n\n",max_chars,max_regions);mincond = input("What is the smallest experimental condition number? ");
maxcond = input("What is the largest experimental condition number? ");
minsen = input("What is the smallest experimental sentence number?");
maxsen = input("What is the largest experimental sentence number? ");
adjmaxcond = maxcond-mincond+1;
printf("\nWhat is delimiter character? (space includes end of line): ");
gets(string);
delim=string[0];
linelength = 160;		/* an arbitrary large number...a bad kludge*/

if(argc < 2)
	{
	printf("Type name of delimited input file: ");
	do
		{
		infile = fopen(gets(temp),"r");
		if(infile == NULL)
			printf("\nNOPE, try again: ");
		}
	while(infile == NULL);
	}
else
	{
	if((infile = fopen(argv[1],"r")) == NULL)
		{
		printf("\nOops. No such input file as %s.",argv[1]);
		exit(1);
		}
	}


if(argc < 3)
	{
	printf("Type name of position count output file: ");
	do
		{
		countout = fopen(gets(temp),"w");
		if(countout == NULL)
			printf("\nNOPE, try again: ");
		}
	while(countout == NULL);
	}
else
	{
	if((countout = fopen(argv[2],"w")) == NULL)
		{
		printf("\nOops. No such input file.");
		exit(1);
		}
	}

/* note, the max_regions stuff is not used. Strip it out sometime. */

if((tempitem = calloc(max_chars,1)) == NULL)
	{
	printf("\nCan't calloc memory for tempitem");
	exit(1);
	}
if((mat_tCCOND = (int *) calloc(adjmaxcond,sizeof(int))) == NULL)
	{
	printf("\nCan't calloc memory for mat_tCCOND");
	exit(1);
	}
if((mat_CITEM = (int *)calloc(adjmaxcond,sizeof(int))) == NULL)
	{
	printf("\nCan't calloc memory for mat_CITEM");
	exit(1);
	}
if((mat_xyc = (int *)calloc(adjmaxcond,sizeof(int))) == NULL)
	{
	printf("\nCan't calloc memory for mat_xyc");
	exit(1);
	}
if((mat_pos = (int *)calloc(adjmaxcond,max_regions * sizeof(int))) == NULL)
	{
	printf("\nCan't calloc memory for mat_pos");
	exit(1);
	}
if((mat_sen = calloc(adjmaxcond,max_chars)) == NULL)
	{
	printf("\nCan't calloc memory for mat_sen");
	exit(1);
	}
printf("\nSuccessfully calloc'd space.");

CTRIAL = 0;
while(fgets(tempitem,max_chars,infile) != NULL)
	{
	if(strlen(tempitem) > 2 && PartStringCompare(tempitem,"trial") && (*(tempitem+6) == 'F'))	/* only FILLER trial */
		{
		ttl = tempitem+7;		/* assume cond number starts right after F */
		tsptr = tempstring;
		while(isdigit(*ttl))
			*(tsptr++) = *(ttl++);
		*tsptr = '\0';
		CCOND = atoi(tempstring);
		if((c = toupper(*(ttl++))) != 'I')
			{
			printf("\nOOPS: Found %c when looking for I on cond %d, %s; check file! ",c,CCOND,tempitem);
			exit(1);
			}	
		tsptr = tempstring;
		while(isdigit(*ttl))
				*(tsptr++) = *(ttl++);
		*tsptr = '\0';
		CITEM = atoi(tempstring);
/* insist on D0 */
		if((c = tolower(*(ttl++))) != 'd')
			{
			printf("\nOOPS: Found %c when looking for D on cond %d item %d, %s; check file! ",c,CCOND,CITEM,tempitem);
			exit(1);
			}
/*printf("\nCCOND %d CITEM %d c %c; going to FindInlineString",CCOND,CITEM,c);*/
		if(*(ttl) == '0' && CCOND >= mincond && CCOND <= maxcond && CITEM >= minsen && CITEM <= maxsen)		/* an experimental nondependent item */
			{
			if((ttl = FindInlineString(tempitem,CITEM,CCOND)) == 0)
				{
				printf("\nOOPS: Reached end of string while looking for inline");
				exit(0);
				}
			basepos = 0;
			*(mat_pos+(CCOND-mincond)) = 0;		/* assume start in first column */
			gotNL = 0;
			for(xyp=ttl,xyc=1,xval=0;*xyp != '\0';xyp++,xval++)
				{
				while(xval==0 && (*xyp == '\n' || *xyp == LF || *xyp == CR))
					xyp++;			/* skip leading new lines  BUT NOT SPACES ANY MORE */
				if(*xyp == delim || (delim == ' ' && (*xyp == '\\' && *(xyp+1) == 'n')))
					{
					*(mat_pos+(CCOND-mincond)+(xyc++)) = xval;
					if(delim != ' ')
						xval--;			/* don't count non-space delimiter in */
										/* character count */
					}
				if(*xyp == '\\' && *(xyp+1) == 'n')
					{
					xyp++;
					xval--;
					if(gotNL == 0)
						{
						basepos += linelength;		/* next line */
						xval=basepos-1;		/* 1 before start of line; will increase */
						}
					gotNL = 1;				/* set up to ignore following newlines */
					}
				else
					{
					gotNL = 0;
					}
				if(*xyp == CR)
					xval--;			/* throw away CRs  - won't be any!*/
				}
			fprintf(countout,"%5u%5u%5u",CITEM,CCOND,xyc);
			for(k=0;k<xyc;k++)
				fprintf(countout,"%5d",*(mat_pos+(CCOND-mincond)+k));
			fprintf(countout,"\n");
			*(mat_CITEM + (CCOND-mincond)) = CITEM;
			*(mat_tCCOND + (CCOND-mincond)) = CCOND;
			*(mat_xyc + (CCOND-mincond)) = xyc;
			}
		CTRIAL++;				/* GO ON TO NEXT ITEM */
		printf("\nTrial %d",CTRIAL);
/*if(getch() == 'q')
	exit(1);*/
		}				/*	end if strlen(tl) > 2) */
							/* now write it all in order */

/*	for(i=mincond;i<=maxcond;i++)
		{
		for(j=0;j<=maxcond-mincond;j++)
			{
			if(*(mat_tCCOND +j) == i)	
				{
				fprintf(countout,"%5u%5u%5u",*(mat_CITEM+j),*(mat_tCCOND+j),*(mat_xyc+j));
				for(k=0;k<*(mat_xyc+j);k++)
					fprintf(countout,"%5d",*(mat_pos+(j*max_regions)+(k)));
				fprintf(countout,"\n");
				}
			}
		} */
	}						/* end while fgets() */
fclose(countout);
free(mat_sen);
free(mat_pos);
free(mat_xyc);
free(mat_CITEM);
free(mat_tCCOND);
}


int input(char *output)
{
char buffer[100];
printf("%s",output);
return(atoi(gets(buffer)));
}

/************************************************/

char *FindString(char *dv,char *dc)
{
char *tdc;

/*printf("\nEntering findstring with %s and %s",dv,dc);*/

while(*dv != '\0')
	{
	tdc = dc;
	while(tolower(*dv) != *tdc)			/* find start of second string */
		{
		if(*dv == '\0')
			{
			printf("\nReached end of input string while searching for %s",dc);
			return(0);
			}
		else
			dv++;
		}
	for(;tolower(*dv) == tolower(*tdc) || *tdc == '\0';dv++,tdc++)		/* is it the right string? */
		{
		if(*tdc == '\0')
			return(++dv);				/* identical */
		}
	}												/* nope, found a difference before the end */
return(0);					/* different */
}
		
/************************************************/


/************************************************/

char *FindInlineString(char *tempitem,int item,int cond)
{
char *ttl;
int i;
ttl = tempitem;
/*printf("\nEntering findInlinestring with %s item %d cond %d",tempitem,item,cond);*/
while(fgets(tempitem,max_chars,infile) != NULL)
	{
/*printf("\nNEXT LINE %s",tempitem);*/
	if(PartStringCompare(tempitem,"end"))
		{
		printf("\nFound end before inline for item %d cond %d; check input. ",item,cond);
		exit(1);
		}
	if(PartStringCompare(tempitem,"inline"))
		{
		for(i = 0;*(ttl+i) != '|' && *(ttl+i) != '\0';i++)
			;
		if(*(ttl+i) == '\0')
			{
			printf("\nReached end of line looking for pipe after inline, item %d cond %d; check input. ",item,cond);
			exit(1);
			}
		else
			return(ttl+i+1);
		}
	}
return(0);
}
		
/************************************************/

int PartStringCompare(char *tempstring,char *compstring)
{
char *ttl;
int i,j;
i = j = 0;
ttl = tempstring;
while(isspace(*(ttl+i)))
	i++;
while(*(compstring+j) != '\0')
	{
	if(*(ttl+i) == *(compstring+j))
		{
		i++;
		j++;
		}
	else
		return(0);		/* mismatch */
	}
return(1);		/* match */
}


