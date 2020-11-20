/* EYEWASH PREPARATION PROGRAM -- EWPREP -- USE TO CONVERT DAVID'S
EYELINK SCRIPTS TO .SEN AND .POS FILES FOR EYEWASH */

/*copyright 1990, 2003 Charles Clifton. Users of this program are permitted
to modify and distribute it at no charge but may not sell it or
include any of its code in a commercial program.*/


/* ASSUMES THAT THE USER HAS STRIPPED THE HEADER AND THE DEFINES
OUT OF THE SCRIPT FILE, SO THAT THE SCRIPT FILE STARTS WITH THE FIRST TRIAL.
ALSO ASSUMES THAT THE TRIAL TYPE THAT PRECEDES TRIALID HAS NO SPACES */

/* ALSO ASSUMES THAT THE SCRIPT FILE NAME FOLLOWS DOS CONVENTIONS I.E.
NORE MORE THAN 8 CHARACTERS IN NAME, NO MORE THAN 3 CHARACTERS IN
EXTENSION - SO RENAME YOUR FILES "....SCR" */

/* note: assumes that TRIALID is ALWAYS of the form CxxIyy..., where
xx is the condition number and yy is the item numeber!!! 
(needed to allow use of eyewash, eyedry; other nonnumeric characters 
can follow yy if you want */

/* note: assumes that data input will come from asc2dat, which
ssumes that each item number is unique. If there are multiple
occurrences of a particular item number, the program only writes out the
last occurrence. The data are written out in order of item number. 
asc2dat uses character positions written to the data file in David's program.
Permits up to 4000 characters on a trial. Outputs the data in terms of
character number on a line (0 to 80, or whatever) and line number (0 to 24).
If a fixation falls outside the list of characters in the data file for
the current trial, it is assigned x = 99, y = 99. Note, this output is 
functionally identical to the pcexpt output so it can be used with 
eyewash and eyedry */

/* NOTE WELL: Eyedry and eyewash are limited to 64 seconds of eye records
on a trial. If your trial goes longer than 64 seconds, it won't work, and
I WON'T fix it. -- Chuck */

char VERSION[] = "3/8/2004";

/* INCLUDES AND DEFINES */

#include "stdio.h"
#include "math.h"
#include "stdlib.h"
#include "string.h"
#include "dos.h"
#include "ctype.h"
/*#include <stat.h>*/		/* uncomment for TC 1.5 */
#include <fcntl.h>
#include "mem.h"
#include "alloc.h"
#include "io.h"
#include "stddef.h"
#include "conio.h"
#define MAXTRIALS 256
#define MAXLINE 4500
#define MAXFIX 2000
#define MAXWORDS 1500

/* DECLARATIONS OF GLOBALS */

int mincond, maxcond;
FILE *infile,*senout,*countout;
int fcond,lcond,fitem,litem;
char tl[MAXLINE];
#define LF 0x0a
#define CR 0x0d
xpos[MAXWORDS];
ypos[MAXWORDS];

/* DECLARACTIONS OF FUNCTIONS */

void main(void);
void get_files(void);
int input(char *output);
int PartStringCompare(char *dc,char *dv);
int GetNextNumber(char* dc,int *i);
long GetNextLongNumber(char* dc,int *i);
float GetNextFloat(char* dc,int *i);
void WriteData(int ntrials);
char *FindString(char *dv,char *dc);


/* MAIN PROGRAM */


void main(void)
{
char tempstring[80];
char *ttl,*tsptr;
char *ttlc,*ttlt,*ttlp;
int xval,yval,xyc;
int i;
char c;
int cbal,maxcbal;
int trial, cond, item;
struct date today;
printf("\n\nVersion of %s\n\n",VERSION);
get_files();																/* open input and output files */
mincond = input("\nWhat is the smallest condition number to eyewash?\r\n ");
maxcond = input("\nWhat is the largest condition number to eyewash?\r\n ");
maxcbal = input("\nHow many counterbalancing conditions in the experiment?\r\n ");
cbal = input("\nWhat counterbalancing condition is this for?\r\n ");
while(fgets(tl,MAXLINE,infile) != NULL)
	{
/*printf("\nNext line = %s,length = %d",tl,strlen(tl));*/
	if(strlen(tl) > 2)
		{
		ttl = tl;
		tsptr = tempstring;
/*printf("%s\n",tl);*/
		while(!isspace(*(ttl++)))		/* skip over to space before C */
			;
		while(tolower(*(ttl++)) != 'c')
			;
		while(isdigit(*ttl))
			*(tsptr++) = *(ttl++);
		*tsptr = '\0';
		cond = atoi(tempstring);
		if((c = tolower(*(ttl++))) != 'i')
			{
			printf("\nOOPS: Found %c when looking for i (item number); check file! ",c);
			exit(1);
			}	
		tsptr = tempstring;
		while(isdigit(*ttl))
			*(tsptr++) = *(ttl++);
		*tsptr = '\0';
		item = atoi(tempstring);
		if(cond <= maxcond && cond >= mincond && cond%maxcbal == (item+cbal-1)%maxcbal)
			{
			if((ttl = FindString(ttl,"inline")) == 0)
				{
				printf("\nOOPS: Reached end of string while looking for inline");
				exit(0);
				}
/* compress string to change \n to '\n' */
			for(ttlc=ttl,ttlt=ttl;*ttlt != '\0';ttlc++,ttlt++)
				{
				if(*ttlt == '\\' && *(ttlt+1) == 'n')
					{
					*ttlc = '\n';
					ttlt++;
					}
				else
					(*ttlc = *ttlt);
				}
			*ttlc = '\0';
/* set 78 character limit */
			for(i=0,ttlc=ttl,ttlt=ttl;*ttlt != '\0';ttlc++,ttlt++,i++)
				{
				if(i >= 78)
					{
					ttlp = ttlt;							/* ttlt points to the end of the current line */
					while(*(ttlp) != '\n')		/* seach for the newline */
						{
						ttlp++;
						}
					while(*ttlp != '\0')			
						{
						*ttlc = *ttlp;					/* put newline into ttlc = ttlt */
						ttlc++;									/* and pop the array to the end */
						ttlp++;
						}
					*ttlc = '\0';
					ttlc = ttlt;							/* reset the pointer to the newline */
					i = 0;
					}
				else if(*ttlt == '\n')
					i = 0;										/* fine, short line, just copy it */
				else
					*ttlc = *ttlt;						/* just copy it */
				}


			fprintf(senout,"%4d%4d %s|\n",cond,item,ttl);
			xpos[0]=-1;
			ypos[0]=0;
			for(xyc=1,xval=0,yval=0;*ttl != '\0';ttl++,xval++)
				{
				if(*ttl == ' ')	/* could put a specified delimiter here */
					{
					xpos[xyc] = xval;
					ypos[xyc++] = yval;
					}
				else if(*ttl == '\n' || *ttl == LF)
					{
					xpos[xyc]=xval;
					ypos[xyc++]=yval++;
					xval=-1;
					}
				else if(*ttl == CR)
					xval--;			/* throw away CRs */
				}
			fprintf(countout,"%5d%5d%5d",cond,item,xyc);
			for(i=0;i<xyc;i++)
				fprintf(countout,"%5d%5d",xpos[i],ypos[i]);
			fprintf(countout,"\n");
			}
		}
	}
fclose(senout);
fclose(countout);
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


int PartStringCompare(char *dv,char *dc)
{
for(;*dc == *dv || *dv == '\0';dc++,dv++)
	{
	if(*dv == '\0')
		return 1;				/* identical */
	}
	return 0;					/* different */
}

/************************************************/

int GetNextNumber(char* dc,int *i)
{
char num[20];
int j = 0;
while(!isdigit(*dc))
	{
	dc++;
	(*i)++;
	}
while(isdigit(*dc))
	{
	num[j++] = *(dc++);
	(*i)++;
	}
while(isspace(*dc))
	{
	dc++;
	(*i)++;
	}
num[j] = '\0';
return(atoi(num));
}


/************************************************/

long GetNextLongNumber(char* dc,int *i)
{
char num[20];
int j = 0;
while(!isdigit(*dc))
	{
	dc++;
	(*i)++;
	}
while(isdigit(*dc))
	{
	num[j++] = *(dc++);
	(*i)++;
	}
while(isspace(*dc))
	{
	dc++;
	(*i)++;
	}
num[j] = '\0';
return(atol(num));
}

/************************************************/

float GetNextFloat(char* dc,int *i)
{
char num[30];
int j = 0;
while(!isdigit(*dc))
	{
	dc++;
	(*i)++;
	}
while(isdigit(*dc)|| *dc == '.')
	{
	num[j++] = *(dc++);
	(*i)++;
	}
while(isspace(*dc))
	{
	dc++;
	(*i)++;
	}
num[j] = '\0';
return(atof(num));
}


/************************************************/

/* FUNCTION TO GET NAMES OF FILES AND OPEN THEM UP */

void get_files(void)
{
char filename[80],filenameo[80],tempstr[20];
int inter,i;
printf("Input data file name? ");
while((infile = fopen(gets(filename),"r"))==NULL)
	{
	printf("\nNot a good name, try again: ");
	}
printf("Sentence output data file name? ");
while(!strcmp(gets(filenameo),filename))
	printf("\nDon't use same name for input and output! Try again.\n");
if(fopen(filenameo,"r") != NULL)
	{
	printf("\n %s exists; you can\n  Quit and choose a new name\n  Erase the old file and write a new one\nTYPE q or e:  ",filenameo);
	gets(tempstr);
	if(tolower(tempstr[0]) == 'e')
		{
		if((senout=fopen(filenameo,"w")) == NULL)	
			{
			printf("\nCan't create %s\n",filenameo);
	    	exit(1);
			}
		}
	else
		{
		exit(1);
		printf("\nExiting program; please restart.\n");
		}
	}
else
	{
	if((senout=fopen(filenameo,"w")) == NULL)	
		{
		printf("\nCan't create %s\n",filenameo);
	   	exit(1);
		}
	}

printf("Position output data file name? ");
while(!strcmp(gets(filenameo),filename))
	printf("\nDon't use same name for input and output! Try again.\n");
if(fopen(filenameo,"r") != NULL)
	{
	printf("\n %s exists; you can\n  Quit and choose a new name\n  Erase the old file and write a new one\nTYPE q or e:  ",filenameo);
	gets(tempstr);
	if(tolower(tempstr[0]) == 'e')
		{
		if((countout=fopen(filenameo,"w")) == NULL)	
			{
			printf("\nCan't create %s\n",filenameo);
	    	exit(1);
			}
		}
	else
		{
		exit(1);
		printf("\nExiting program; please restart.\n");
		}
	}
else
	{
	if((countout=fopen(filenameo,"w")) == NULL)	
		{
		printf("\nCan't create %s\n",filenameo);
	   	exit(1);
		}
	}
}


/***********************************************************************/


int input(char *output)
{
char buffer[MAXLINE];
printf("%s",output);
buffer[0]='\0';
return(atoi(gets(buffer)));
}


