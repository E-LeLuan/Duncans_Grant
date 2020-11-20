/* TrueTime - correct .da1 files from Eyetrack/Eyedoctor to reference times */
/* to synctime not gaze on time */

/* either open a DOS window, cd to where your .asc and .da1 files are, 
copy TRUETIME.EXE into this directory or put it on the path, type TRUETIME,
and answer questions about input file names 
or
open Explorer or My Computer, go to the directory where your .asc and da1
files are, copy TRUETIME.EXE into this directory, doubleclick it, and answer
the questions.
Running in DOS will give you error messages that disappear in Windows
I may do a proper Windows version sometime. Dunno. */

/* this version gets the min and max conditions and items from the .da1 file.


/*copyright 1990, 2003, 2008 Charles Clifton. Users of this program are permitted
to modify and distribute it at no charge but may not sell it or
include any of its code in a commercial program.*/

char VERSION[] = "1/16/2010";

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
#define MAXLINE 512


/* DECLARATIONS OF GLOBALS */

FILE *ascii_file,*da1_file,*da2_file;


#define LF 0x0a
#define CR 0x0d

/* DECLARACTIONS OF FUNCTIONS */

void main(void);
void get_range(int *mincond,int *maxcond,int *minitem,int *maxitem,char *da1_name);
int input(char *output);
int GetNextTrial(FILE* infile);
long GetTrialTime(FILE* infile);
int PartStringCompare(char *dc,char *dv, int* i);
int RemainingStringCompare(char *dc,char *dv,int* i);
int StringCompare(char *dc,char *dv, int* i);
int GetNextNumber(char* dc,int *i);
long GetNextLongNumber(char* dc,int *i);
long LongNumber(char* dc,int *i);
float GetNextFloat(char* dc,int *i);

long temptime;
char tl[1000];
char tl2[1000];
long *gaze_on;
long *synctime;
int cond,item;
int seq;
int x,y,st,et;
int tt,nr,temp1,temp2,nf;
int DONE;
int correction;

/* MAIN PROGRAM */


void main(void)
{
int ti,nsub;
char buff[80];
char tempstr[80];
char file[80];
char ascii_name[80],da1_name[80],da2_name[80];
int i,j,k,OK;
struct date today;
int numitems = 0;
int maxitem = 0;
int maxcond = 0;
int minitem = 0;
int mincond = 0;


printf("\n\nVersion of %s\n\n",VERSION);
printf("\nThis corrects .da1 files to time from synctime, outputs .da2 version");

printf("\nWhat is the name of your .asc file? ");
scanf("%s",ascii_name);
printf("\nWhat is name of your .da1 file? ");
scanf("%s",da1_name);
get_range(&mincond,&maxcond,&minitem,&maxitem,da1_name);
numitems = maxitem-minitem+1;

if ((gaze_on = (long *)calloc((maxcond-mincond + 1)*(maxitem-minitem+1),sizeof(long)))== NULL)
	{
	printf("\nERROR: can't alloc space for gaze_on");
	exit(2);
	}
if ((synctime = (long *)calloc((maxcond-mincond + 1)*(maxitem-minitem+1),sizeof(long)))== NULL)
	{
	printf("\nERROR: can't alloc space for synctime");
	exit(2);
	}

if((ascii_file = fopen(ascii_name,"r")) == NULL)
	{
	printf("\nCan't open ascii file, %s",ascii_name);
	exit(2);
	}
if((da1_file = fopen(da1_name,"r")) == NULL)
	{
	printf("\nCan't open da1 file, %s",da1_name);
	exit(2);
	}
for(i=0;da1_name[i]!='.';i++)
	da2_name[i]=da1_name[i];
da2_name[i++] = '.';
da2_name[i++] = 'd';
da2_name[i++] = 'a';
da2_name[i++] = '2';
da2_name[i++] = '\0';
if((da2_file = fopen(da2_name,"w")) == NULL)
	{
	printf("\nCan't open da2 file, %s",da1_name);
	exit(2);
	}


/* fill the matrices of timestamps from the ascii file */

while(fgets(tl,1000,ascii_file) != NULL)
	{
	OK = 0;
	if(tl[0] == 'M' && tl[1] == 'S' && tl[2] == 'G')
		{
		j = 0;
		OK = 0;
		i = 2;
		temptime = GetNextLongNumber(&(tl[i]),&i);
		if(RemainingStringCompare("TRIALID",&tl[i],&i))
			{
			i++;
			if(tl[i++] != 'E')
				;						/* P or F or something trial, skip it */
			else
				{
				while(isdigit(tl[i]))
					tempstr[j++] = tl[i++];
				tempstr[j] = '\0';
				cond = atoi(tempstr);
				if(tl[i++] != 'I')
					{
					printf("\nOOPS didn't find I in %s i = %d",tl,i);
					exit(2);
					}
				j = 0;
				while(isdigit(tl[i]))
					tempstr[j++] = tl[i++];
				tempstr[j] = '\0';
				item = atoi(tempstr);
				if(RemainingStringCompare("D0",&tl[i],&i))
					OK = 1;
				else
					OK = 0;
				}
/*printf("\ncond %d item %d OK %d",cond,item,OK);*/
			if(cond >= mincond && cond <= maxcond && item >= minitem && item <= maxitem && OK == 1)
				{						/* we're on a trial we want */
				DONE = 0;
				while(fgets(tl,1000,ascii_file) != NULL && DONE == 0)
					{
					i = 0;
					if(tl[0] == 'M' && tl[1] == 'S' && tl[2] == 'G')
						{
						i = 2;
						temptime = GetNextLongNumber(&(tl[i]),&i);
						if(RemainingStringCompare("GAZE TARGET ON",&(tl[i]),&i))
							{
							*(gaze_on + numitems * (cond-mincond) + (item-minitem)) = temptime;
							}
						else
							{
							i = 2;
							temptime = GetNextLongNumber(&(tl[i]),&i);
							if(RemainingStringCompare("SYNCTIME",&(tl[i]),&i))
								{
							*(synctime + (numitems * (cond-mincond)) + (item-minitem)) = temptime;
								DONE = 1;
								}
							}				/* end else not GAZE TARGET ON */
						}					/* end if MSG */
					}						/* end found line in ascii file */
				}							/* end got OK cond and item */
			}								/* end found TRIALID loop */
		}									/* end outer MSG loop */
	}										/* end while read ascii loop */

/* check it out */

for(i=mincond;i<=maxcond;i++)
	{
	printf("\nE %d\n",i);
	for(j = minitem;j<=maxitem;j++)
		{
		printf(" I %d",j);
		printf(" %ld",*(gaze_on + numitems * (i-mincond) + (j-minitem)));
		printf(" %ld\n",*(synctime + numitems * (i-mincond) + (j-minitem)));
		}
	printf("\n");
	}

/* go through the .da1 file, write a .da2 file */

while(fgets(tl,1000,da1_file) != NULL)
	{
	i = 0;
	seq = GetNextNumber(&(tl[i]),&i);
	cond = GetNextNumber(&(tl[i]),&i);
	item = GetNextNumber(&(tl[i]),&i);
	if(cond >= mincond && cond <= maxcond && item >= minitem && item <= maxitem)
		{
		correction = (int)(*(synctime + numitems * (cond-mincond) + (item-minitem)) - *(gaze_on + numitems * (cond-mincond) + (item-minitem)));
		tt = GetNextNumber(&(tl[i]),&i);
		nr = GetNextNumber(&(tl[i]),&i);
		temp1 = GetNextNumber(&(tl[i]),&i);
		temp2 = GetNextNumber(&(tl[i]),&i);
		nf = GetNextNumber(&(tl[i]),&i);
		fprintf(da2_file,"%d %d %d %d %d %d %d %d",seq,cond,item,tt-correction,nr,temp1,temp2,nf);
		for(j=0;j<nf;j++)
			{
			x = GetNextNumber(&(tl[i]),&i);
			y = GetNextNumber(&(tl[i]),&i);
			st = GetNextNumber(&(tl[i]),&i);
			et = GetNextNumber(&(tl[i]),&i);
			st = st - correction;
			et = et - correction;
			fprintf(da2_file," %d %d %d %d",x,y,st,et);
			}
		fprintf(da2_file,"\n");
		}
	else
		printf("\n\nWATCH IT - FOUND COND %d ITEM %d, not in ascii file",cond,item);
	}
fclose(ascii_file);
fclose(da1_file);
fclose(da2_file);
}


/************************************************/
int StringCompare(char *dc,char *dv, int* i)		/* same to end of calling string */
{
for(;*dc == *dv || *dc == '\0';dc++,dv++,(*i)++)
	{
	if(*dc == '\0')
		return(1);				/* identical */
	}
	return(0);					/* different */
}


/************************************************/

int PartStringCompare(char *dc,char *dv,int* i)		/* same up to space character */
{
while(isspace(*dv))		/* skip leading blanks */
	{
	dv++;
	(*i)++;
	}
for(;*dc == *dv || *dc == '\0';dc++,dv++,(*i)++)
	{
/*printf("\nGot char %c",*dc);*/
	if(*dc == '\0' || isspace(*dc))
		return(1);				/* identical */
	}
	return(0);					/* different */
}

/************************************************/


int RemainingStringCompare(char *dc,char *dv,int* i)		/* same up to end */
{
while(isspace(*dv))		/* skip leading blanks */
	{
	dv++;
	(*i)++;
	}
for(;*dc == *dv || *dc == '\0';dc++,dv++,(*i)++)
	{
/*printf("\nGot char %c",*dc);*/
	if(*dc == '\0')
		return(1);				/* identical */
	}
	return(0);					/* different */
}

/************************************************/


int GetNextNumber(char* dc,int *i)
{
char num[20];
int j = 0;
while(!isdigit(*dc))
	{
	if(*dc == '.'&& (*(dc-1) == ' ' || *(dc-1) == '\t') && (*(dc + 1) == ' ' || *(dc+1) == '\t'))				/* missing data ! */
		return(-9999);
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
	if(*dc == '.'&& *(dc-1) == ' ' && *(dc + 1) == ' ')				/* missing data ! */
		return(-999999);
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


/***********************************************************************/


int input(char *output)
{
char buffer[MAXLINE];
int input;
printf("%s",output);
/*buffer[0]='\0';*/
scanf("%d",&input);
gets(buffer);		/* mop up */
return(input);
/*return(atoi(gets(buffer)));*/
}


/*************************************************************/

void get_range(int *mincond, int *maxcond,int *minitem,int *maxitem,char *name)
{
int i, seq, cond, item;
if((da1_file = fopen(name,"r")) == NULL)
	{
	printf("\nCan't open da1 file, %s",name);
	exit(2);
	}
while(fgets(tl,1000,da1_file) != NULL)
	{
	int i = 0;
	int seq;
	int cond;
	int item;
	GetNextNumber(&(tl[i]),&i);
	cond = GetNextNumber(&(tl[i]),&i);
	item = GetNextNumber(&(tl[i]),&i);
	if(cond > *maxcond)
		*maxcond = cond;
	if(item > *maxitem)
		*maxitem = item;
	if(*mincond == 0)
		*mincond = cond;
	else
		if(cond < *mincond)
			*mincond = cond;
	if(*minitem == 0)
		*minitem = item;
	else
		if(item < *minitem)
			*minitem = item;
	}
fclose(da1_file);
}
