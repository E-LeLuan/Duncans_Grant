/* EYELINK PROGRAM -- ASC2DAT -- USE TO CONVERT EYELINK ASCII DATA */
/* TO PCEXPT .DAT FORMAT */

/* RIGHT EYE VERSION */

/*copyright 1990, 2003 Charles Clifton. Users of this program are permitted
to modify and distribute it at no charge but may not sell it or
include any of its code in a commercial program.*/

/* "standard" version, taken from asc2datk.c 6/15/04 with
fixed font support to be added 
and response recording to be added */

/* stuff for Courier fixed pitch font */

/* requires user to type in Y offset, height of line, and character
width in pixels */


/* horizontal defaults, initially set to 97 pixel
character height and 18 pixel character width.
Assumes left margin set at 65 pixels.
Thus, character 1 on a row goes from 65 to 83, character
2 from 84 to 102, etc. (assigning the pixel after a character to the
next character */ 


/* vertical defaults; assumes top margin = 65, actual character height of ???
pixels, ??? pixels above and below -for 97 pixel line spacing*/


/* OLD PREVIOUS OUT OF DATE example, indicating where lines are divided for analysis:

start line 1 24 pixels down (= top margin - (line spacing-char height)/2))
initial line characters top 60 (= top margin)
initial line characters bottom 93 (= top margin + char height)
end line 1 129 pixels down (= top margin + char height + (line spacing-char height)/2)
start line 2 130 pixels down
second line characters top 165
second line characters bottom 198
end line 2 234 pixels down (= start line 3)
start line 3 235 pixels down
third line characters top 270
third line characters bottom 303
end line 3 339 (= start line 4)
fourth line characters top 375
third line characters bottom 408
etc.

*/

/* end of stuff for courier fixed pitch font */


/* revised for Kathryn 12/03; works even though TRIALID of the form trialcxiy */
/* revised 4/8/04 to write .tlb (blink) trials */

/* note: assumes that TRIALID is ALWAYS of the form CxxIyy..., where
xx is the condition number and yy is the item numeber!!! 
(needed to allow use of eyewash, eyedry; other nonnumeric characters 
can follow yy if you want */

/* note: assumes that each item number is unique. If there are multiple
occurrences of a particular item number, the program only writes out the
last occurrence. The data are written out in order of item number. */

/* uses character positions written to the data file in David's program.
Permits up to 1000 characters on a trial. Outputs the data in terms of
character number on a line (0 to 80, or whatever) and line number (0 to 24).
If a fixation falls outside the list of characters in the data file for
the current trial, it is assigned x = 99, y = 99. Note, this output is 
functionally identical to the pcexpt output so it can be used with 
eyewash and eyedry */

/* NOTE WELL: Eyedry and eyewash are limited to 64 seconds of eye records
on a trial. If your trial goes longer than 64 seconds, it won't work, and
I WON'T fix it. -- Chuck */

char VERSION[] = "6/15/2004 revised RIGHT EYE; makes .tlb";

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
#define MAXTRIALS 120
#define MAXLINE 200
#define MAXFIX 2000
#define MAXCHAR 4500

/* DECLARATIONS OF GLOBALS */

int CTRIAL;
int dummy[1];
struct CLOC
	{
	int lx;
	int ty;
	int rx;
	int by;
	int charpos;
	int line;
	}cloc[MAXCHAR];

int FixedPitch;			/* 1 = courier, 0 lookup table */
int maxitem, maxcond;
int minitem,mincond;
FILE *infile,*interfile,*outfile,*blinkfile;
int fcond,lcond,fitem,litem;
int char_width,char_height;	/* pixels */
int horiz_offset, vert_offset;	/* also pixels */
int line_spacing;
struct dtrial
	{
	int ord;
	int cond;
	int item;
	unsigned rt;
	int resp;
	unsigned rt2;
	int resp2;
	int obs;
	int extra1;
	int extra2;
	int* x;
	int* y;
	unsigned* s;
	unsigned* e;
	int nblinks;
	unsigned* startblink;
	unsigned* endblink;
	}	dtrials[MAXTRIALS];

#define LF 0x0a
#define CR 0x0d

/* DECLARACTIONS OF FUNCTIONS */

void main(void);
void get_files(void);
int CountTrials(FILE* infile);
int input(char *output);
int GetNextTrial(FILE* infile,int *ntrials);
long GetTrialTime(FILE* infile);
int GetFixations(FILE* infile,long start_time,int charcount);
int PartStringCompare(char *dc,char *dv);
int GetNextNumber(char* dc,int *i);
int GetCond(char* dc,int *i);
int GetItem(char* dc,int *i);
long GetNextLongNumber(char* dc,int *i);
float GetNextFloat(char* dc,int *i);
void WriteData(int ntrials);
void get_sizes(void);		/* fixed pitch */


/* MAIN PROGRAM */


void main(void)
{
char tempstr[80];
int i,inter,ntrials,strial,number_fixations;
int trial, cond, item;
int file_position;
int charcount;
long starttime;
struct date today;
printf("\n\nVersion of %s\n\n",VERSION);
FixedPitch = input("\nType 1 for fixed pitch (courier) font, 0 otherwise (look up position): ");
if (FixedPitch != 1)
	FixedPitch = 0;
if(FixedPitch == 1)
	get_sizes();
mincond = input("\nWhat is the smallest condition number to analyze?  ");
maxcond = input("What is the largest condition number to analyze?  ");
minitem = input("What is the smallest item number to analyze?  ");
maxitem = input("What is the largest item number to analyze?  ");
get_files();																/* open input and output files */
ntrials = CountTrials(infile);							/* count lines in input file */
printf("\ncounting trials.....");
printf("\n%d TRIALS - press key to continue\n",ntrials);
getch();
rewind(infile);


for(CTRIAL = 0;CTRIAL < ntrials;CTRIAL++)
	{
	while((charcount = GetNextTrial(infile,&ntrials)) == 0)
		;
	if(dtrials[CTRIAL].cond <= maxcond && dtrials[CTRIAL].item <= maxitem && dtrials[CTRIAL].cond >= mincond && dtrials[CTRIAL].item >= minitem && charcount != -1)
		{
		printf("\nAnalyzing trial %d cond %d item %d",CTRIAL+1,dtrials[CTRIAL].cond,dtrials[CTRIAL].item);
		starttime = GetTrialTime(infile);
		if(starttime != -1)
			{
			i = GetFixations(infile,starttime,charcount);
			printf("  %d fixations",i);
/*		if(getch() == 'q')
			exit(1);*/
			}
		else
			{
			printf("...aborted early");
			}
		}
	else
		printf("\nSkipping trial %d cond %d item %d charcount %d",CTRIAL+1,dtrials[CTRIAL].cond,dtrials[CTRIAL].item,charcount);
	}
WriteData(ntrials);
}
/************************************************/

int CountTrials(FILE *infile)
{
char tl[160];
char c;
int i;
int trials = 0;
while(fgets(tl,160,infile) != NULL)
	{
	i = 0;
/*	if(PartStringCompare("START",&(tl[i])))*/
	if(tl[0] == 'M' && tl[1] == 'S' && tl[2] == 'G')
		{
		i = 2;
		GetNextLongNumber(&(tl[i]),&i);
		if(PartStringCompare("TRIALID",&(tl[i])))
			trials++;
		}
	}
return(trials);
}

/************************************************/


int GetNextTrial(FILE *infile,int *ntrials)
{
char tl[160];
char c;
char* endsignal;
int i,j;
int line,lasty;
int index;
while((endsignal = fgets(tl,160,infile)) != NULL)
	{
	if(tl[0] == 'M' && tl[1] == 'S' && tl[2] == 'G')
		{
		i = 2;
		GetNextLongNumber(&(tl[i]),&i);
		if(PartStringCompare("TRIAL REPEATED",&(tl[i])))
			{
			CTRIAL--;
			(*ntrials)--;
			printf("\nTRIAL %d REPEATED",CTRIAL+1);
/*if (getch() == 'q')
	exit(1);*/
			return(0);
			}
		else if(PartStringCompare("TRIALID",&(tl[i])))
			{
			dtrials[CTRIAL].ord = CTRIAL+1;
			dtrials[CTRIAL].cond = GetCond(&(tl[i]),&i);
			dtrials[CTRIAL].item = GetItem(&(tl[i]),&i);
			dtrials[CTRIAL].rt = 0;
			dtrials[CTRIAL].resp = 0;
			dtrials[CTRIAL].rt2 = 0;
			dtrials[CTRIAL].resp2 = 0;
			dtrials[CTRIAL].obs = 0;
			dtrials[CTRIAL].extra1 = 0;
			dtrials[CTRIAL].extra2 = 0;
			break;
			}
		}
	}
if(endsignal == NULL)
	{
	printf("\nReached End of File without reading trial %d; aborting. ",CTRIAL+1);
	exit(1);
	}
lasty = 0;
line = -1;		/* kludge to make first line 0 */
j = 0;
while((endsignal = fgets(tl,160,infile)) != NULL)
	{
	if(tl[0] == 'M' && tl[1] == 'S' && tl[2] == 'G')
		{
		i = 2;
		GetNextLongNumber(&(tl[i]),&i);
		if(PartStringCompare("REGION CHAR",&(tl[i])))
			{
			index = GetNextNumber(&(tl[i]),&i);
			if(index > MAXCHAR)
				{
				printf("\nOops - over MAXCHAR characters");
				exit(1);
				}
			GetNextNumber(&(tl[i]),&i);		/* throw away '1' */
			i+=1;			/* skip Tim's damn numbers */
			cloc[index].lx = GetNextNumber(&(tl[i]),&i);
			cloc[index].ty = GetNextNumber(&(tl[i]),&i);
			cloc[index].rx = GetNextNumber(&(tl[i]),&i);
			cloc[index].by = GetNextNumber(&(tl[i]),&i);
			if(cloc[index].ty > lasty)
				{
				j = 0;
				lasty = cloc[index].ty;
				line++;
/*printf("\nLINECHANGE q to exit: char %d x %d %d y %d %d pos %d line %d (line %d)",index,cloc[index].lx,cloc[index].rx,cloc[index].ty,cloc[index].by,cloc[index].charpos,cloc[index].line,line);
if(getch() == 'q')
	exit(1);*/
				}
			if(line == 0)
				cloc[index].ty = 0;			/* kludge, count all fixations above line 1 as on line 1 */
			cloc[index].charpos = j++;
			cloc[index].line = line;
/*if(index%50 == 0)
{
printf("\nq to exit: char %d x %d %d y %d %d pos %d line %d (line %d)",index,cloc[index].lx,cloc[index].rx,cloc[index].ty,cloc[index].by,cloc[index].charpos,cloc[index].line,line);
if(getch() == 'q')
	exit(1);
}*/
			}
		else if(PartStringCompare("RECCFG",&(tl[i])))
			{
			break;
			}
		else if(PartStringCompare("TRIAL ERROR",&(tl[i])))
			{
			printf("\nFound TRIAL ERROR on trial %d",CTRIAL+1);
			return(-1);
			}
		}
	}
if(endsignal == NULL)
	{
	printf("\nReached End of File without reading trial %d; aborting. ",CTRIAL+1);
	exit(1);
	}
else
	{
	printf("\nFound %d characters on trial %d cond %d item %d",index+1,CTRIAL+1,dtrials[CTRIAL].cond,dtrials[CTRIAL].item);
	return(index+1);
	}
}


/************************************************/


long GetTrialTime(FILE* infile)
{
char tl[160];
char c;
char* endsignal;
int i;
long ltime,dtime;
unsigned *s, *e;	/* pointers to stored times and positions */
int *x, *y;
/*unsigned stime[MAXFIX],etime[MAXFIX];
int ax[MAXFIX],ay[MAXFIX];*/
while((endsignal = fgets(tl,160,infile)) != NULL)
	{
	if(tl[0] == 'M' && tl[1] == 'S' && tl[2] == 'G')
		{
		i = 2;
		ltime = GetNextLongNumber(&(tl[i]),&i);
/*printf("\nInGetTrialTime,%s at %ld",&(tl[i]),ltime);
getch();*/
		if(PartStringCompare("DISPLAY ON",&(tl[i])))
			{
/*			printf("\nGot DISPLAY ON at %ld",ltime);
			getch();*/
			break;
			}
		else if(PartStringCompare("TRIAL ERROR",&(tl[i])))
			{
			printf("\nTRIAL ERROR before DISPLAY ON");
			ltime = -1;
			if((dtrials[CTRIAL].s = (s = ((unsigned *)(calloc(1,sizeof(unsigned)))))) == NULL)
				{
				printf("\nOut of allocation space!");
				exit(1);
				}
			if((dtrials[CTRIAL].e = (e = ((unsigned *)(calloc(1,sizeof(unsigned)))))) == NULL)
				{
				printf("\nOut of allocation space!");
				exit(1);
				}
			if((dtrials[CTRIAL].x = (x = ((int *)(calloc(1,sizeof(int)))))) == NULL)
				{
				printf("\nOut of allocation space!");
				exit(1);
				}
			if((dtrials[CTRIAL].y = (y = ((int *)(calloc(1,sizeof(int)))))) == NULL)
				{
				printf("\nOut of allocation space!");
				exit(1);
				}
			*(x) = 0;
			*(y) = 0;
			*(s) = 0;
			*(e) = 0;
			return(-1);
			}
		else if(PartStringCompare("TRIAL ABORTED",&(tl[i])))
			{
			printf("\nTRIAL ABORTED before DISPLAY ON");
			if((dtrials[CTRIAL].s = (s = ((unsigned *)(calloc(1,sizeof(unsigned)))))) == NULL)
				{
				printf("\nOut of allocation space!");
				exit(1);
				}
			if((dtrials[CTRIAL].e = (e = ((unsigned *)(calloc(1,sizeof(unsigned)))))) == NULL)
				{
				printf("\nOut of allocation space!");
				exit(1);
				}
			if((dtrials[CTRIAL].x = (x = ((int *)(calloc(1,sizeof(int)))))) == NULL)
				{
				printf("\nOut of allocation space!");
				exit(1);
				}
			if((dtrials[CTRIAL].y = (y = ((int *)(calloc(1,sizeof(int)))))) == NULL)
				{
				printf("\nOut of allocation space!");
				exit(1);
				}
			*(x) = 0;
			*(y) = 0;
			*(s) = 0;
			*(e) = 0;
			printf("\nIn GETTRIALTIME, TRIAL %d cond %d item %d aborted",CTRIAL+1,dtrials[CTRIAL].cond,dtrials[CTRIAL].item);
/*			getch();*/
			return(-1);
			}
		}
	}
if(endsignal == NULL)
	{
	printf("\nReached End of File without reading start time, trial %d; aborting. ",CTRIAL);
	exit(1);
	}
else
	return(ltime);
}


/************************************************/

int GetFixations(FILE* infile,long start_time, int charcount)
{
char tl[160];
char c;
char* endsignal;
int i,j,k;
int ix,iy;
long longvalue;
unsigned startblink[50],endblink[50];
float fx, fy;
int nfix = 0;
unsigned *s, *e;						/* pointers to stored times and positions */
int *x, *y;
unsigned stime[MAXFIX],etime[MAXFIX];
int ax[MAXFIX],ay[MAXFIX];
int nblinks = 0;
while((endsignal = fgets(tl,160,infile)) != NULL)
	{
	if(tl[0] == 'E' && tl[1] == 'B' && tl[2] == 'L' && tl[3] == 'I' && tl[4] == 'N' && tl[5] == 'K' && tl[7] == 'R' && nblinks < 50)
		{
		i = 7;
		startblink[nblinks] = (unsigned)(GetNextLongNumber(&tl[i],&i)-start_time);
		endblink[nblinks] = (unsigned)(GetNextLongNumber(&tl[i],&i)-start_time);
		nblinks++;
printf("\nGot blink starttime %ld start %u end %u number %d\n   trial %d cond %d item %d",start_time,startblink[nblinks-1],endblink[nblinks-1],nblinks,CTRIAL+1,dtrials[CTRIAL].cond,dtrials[CTRIAL].item);
/*getch();*/
		}
	else if(tl[0] == 'E' && tl[1] == 'F' && tl[2] == 'I' && tl[3] == 'X' && tl[5] == 'R')
		{
		i = 5;
		stime[nfix] = (unsigned)(GetNextLongNumber(&(tl[i]),&i)-start_time);
		etime[nfix] = (unsigned)(GetNextLongNumber(&(tl[i]),&i)-start_time);
		GetNextNumber(&(tl[i]),&i);
		fx = GetNextFloat(&(tl[i]),&i);
		fy = GetNextFloat(&(tl[i]),&i);
		if(FixedPitch == 1)
			{
			ax[nfix] = (int)((fx-horiz_offset)/char_width);
			ay[nfix] = (int)((fy-vert_offset)/line_spacing);			/* convert pixels to chars */
			if(ay[nfix] < 0)
				ay[nfix] = 0;
			nfix++;
			}
		else		/* look up table for char position */
			{
			ix = (int)fx;
			iy = (int)fy;
/*printf("\nix %d iy %d",ix,iy);*/
			for(j=0;j<charcount;j++)
				{
				if(ix >= cloc[j].lx && ix < cloc[j].rx && iy >= cloc[j].ty && iy < cloc[j].by)
					{
					ax[nfix] = cloc[j].charpos;
					ay[nfix] = cloc[j].line;
					nfix++;
/*printf("\nfixation %d x %d y %d ax %d ay %d char %d",nfix,ix,iy,ax[nfix-1],ay[nfix-1],j+1);
if(getch() == 'q')
	exit(1);*/
					break;
					}
				}
			if(j == charcount)
				{
				printf("\nCouldn't find match for fixation %d",nfix+1);
				ax[nfix] = 99;
				ay[nfix++] = 99;
				}
			}
		}
	else if(tl[0] == 'M' && tl[1] == 'S' && tl[2] == 'G')		/* end of trial??? */
		{
		i = 2;
		longvalue = GetNextLongNumber(&tl[i],&i);
		if(PartStringCompare("TRIAL_RESULT",&(tl[i])))						/* got the end */
			{
			dtrials[CTRIAL].resp = GetNextNumber(&(tl[i]),&i);
			dtrials[CTRIAL].rt = (unsigned)(longvalue - start_time);
			if(nfix > 0)
				nfix--;		/* wee kludge for Slattery, to eliminate possible end-fix between ENDBUTTON and TRIAL_RESULT */
			dtrials[CTRIAL].obs = nfix;
			if(nfix > 0)
			{
			if((dtrials[CTRIAL].s = (s = ((unsigned *)(calloc(nfix,sizeof(unsigned)))))) == NULL)
				{
				printf("\nOut of allocation space!");
				exit(1);
				}
			if((dtrials[CTRIAL].e = (e = ((unsigned *)(calloc(nfix,sizeof(unsigned)))))) == NULL)
				{
				printf("\nOut of allocation space!");
				exit(1);
				}
			if((dtrials[CTRIAL].x = (x = ((int *)(calloc(nfix,sizeof(int)))))) == NULL)
				{
				printf("\nOut of allocation space!");
				exit(1);
				}
			if((dtrials[CTRIAL].y = (y = ((int *)(calloc(nfix,sizeof(int)))))) == NULL)
				{
				printf("\nOut of allocation space!");
				exit(1);
				}
			}
			for(k = 0; k < nfix; k++)
				{
				*(x++) = ax[k];
				*(y++) = ay[k];
				*(s++) = stime[k];
				*(e++) = etime[k];
				}
/*printf("\nTRIAL_RESULT TRIAL %d nfix %d",CTRIAL+1,nfix);
if(tolower(getch()) == 'q')
	exit(1);*/
			if(nblinks > 0)
				{
				dtrials[CTRIAL].nblinks = nblinks;
				if((dtrials[CTRIAL].startblink = (unsigned *)(calloc(nblinks,sizeof(unsigned)))) == NULL)
					{
					printf("\nOut of allocation space, startblinks!");
					exit(1);
					}
				if((dtrials[CTRIAL].endblink = (unsigned *)(calloc(nblinks,sizeof(unsigned)))) == NULL)
					{
					printf("\nOut of allocation space, endblinks!");
					exit(1);
					}
				for(i=0;i<nblinks;i++)
					{
					*((dtrials[CTRIAL].startblink)+i) = startblink[i];
					*((dtrials[CTRIAL].endblink)+i) = endblink[i];
printf("\ndtrials startblink %u endblink %u i %d",*((dtrials[CTRIAL].startblink)+i),*((dtrials[CTRIAL].endblink)+i),i);
					}
				}
			else
				dtrials[CTRIAL].nblinks = 0;
			return nfix;
			}
		else if(PartStringCompare("TRIAL ABORTED",&(tl[i])))
			{
			if((dtrials[CTRIAL].s = (s = ((unsigned *)(calloc(1,sizeof(unsigned)))))) == NULL)
				{
				printf("\nOut of allocation space!");
				exit(1);
				}
			if((dtrials[CTRIAL].e = (e = ((unsigned *)(calloc(1,sizeof(unsigned)))))) == NULL)
				{
				printf("\nOut of allocation space!");
				exit(1);
				}
			if((dtrials[CTRIAL].x = (x = ((int *)(calloc(1,sizeof(int)))))) == NULL)
				{
				printf("\nOut of allocation space!");
				exit(1);
				}
			if((dtrials[CTRIAL].y = (y = ((int *)(calloc(1,sizeof(int)))))) == NULL)
				{
				printf("\nOut of allocation space!");
				exit(1);
				}
			*(x) = 0;
			*(y) = 0;
			*(s) = 0;
			*(e) = 0;
			printf("\nTRIAL %d cond %d item %d aborted",CTRIAL+1,dtrials[CTRIAL].cond,dtrials[CTRIAL].item);
/*			if(tolower(getch()) == 'q')
				exit(1);*/
			if(nfix > 0)	
				nfix--;
			return(nfix);
			}
		else if(PartStringCompare("TRIAL ERROR",&(tl[i])))
			{
			dtrials[CTRIAL].obs=0;
			if((dtrials[CTRIAL].s = (s = ((unsigned *)(calloc(1,sizeof(unsigned)))))) == NULL)
				{
				printf("\nOut of allocation space!");
				exit(1);
				}
			if((dtrials[CTRIAL].e = (e = ((unsigned *)(calloc(1,sizeof(unsigned)))))) == NULL)
				{
				printf("\nOut of allocation space!");
				exit(1);
				}
			if((dtrials[CTRIAL].x = (x = ((int *)(calloc(1,sizeof(int)))))) == NULL)
				{
				printf("\nOut of allocation space!");
				exit(1);
				}
			if((dtrials[CTRIAL].y = (y = ((int *)(calloc(1,sizeof(int)))))) == NULL)
				{
				printf("\nOut of allocation space!");
				exit(1);
				}
			*(x) = 0;
			*(y) = 0;
			*(s) = 0;
			*(e) = 0;
/*			printf("\nTRIAL %d cond %d item %d TRIAL ERROR",CTRIAL+1,dtrials[CTRIAL].cond,dtrials[CTRIAL].item);
			getch();*/
			if(nfix > 0)
				nfix--;
			return(nfix);
			}
		}
	if(nfix > MAXFIX)
		{
		printf("\nDAMMIT TIM WHY DID YOU HAVE THAT MANY FIXATIONS!");
		exit(1);
		}
	}
if(endsignal == NULL)
	{
	printf("\nReached End of File prematurely, trial %d; aborting. ",CTRIAL);
	exit(1);
	}
}


/************************************************/
int PartStringCompare(char *dc,char *dv)
{
for(;*dc == *dv || *dc == '\0';dc++,dv++)
	{
	if(*dc == '\0')
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

void WriteData(int ntrials)
{
int map[MAXTRIALS];
int i,j,iobs;
int* xptr;
int* yptr;
unsigned* sptr;
unsigned* eptr;
for(i=0;i<MAXTRIALS;i++)
	map[i] = -999;
for(i=0;i<ntrials;i++)			/* sort trials */
	{
	if(dtrials[i].item <= maxitem & dtrials[i].cond <= maxcond)
		{
		printf("\nPreparing to write legal item, trial %d cond %d item %d",i,dtrials[i].cond,dtrials[i].item);
		map[dtrials[i].item] = i;
		}
	}
for(i=0;i<MAXTRIALS;i++)
	{
	if(map[i] == -999)
		for(j=i+1;j<MAXTRIALS;j++)
			map[j-1] = map[j];		/* make compact, put all items together */
	}
for(i=0;i<MAXTRIALS;i++)
	{
	if(map[i] != -999)
	{
	CTRIAL = map[i];
	printf("\nwriting trial %d map[i] = %d cond %d item %d",i+1,map[i],dtrials[CTRIAL].cond,dtrials[CTRIAL].item);
	fprintf(outfile,"%d %d %d %u %d %u %d ",dtrials[CTRIAL].ord,dtrials[CTRIAL].cond,dtrials[CTRIAL].item,dtrials[CTRIAL].rt,dtrials[CTRIAL].resp,dtrials[CTRIAL].rt2,dtrials[CTRIAL].resp2);
	fprintf(outfile,"0 0    ");										/* extra 1 and 2 */
	fprintf(outfile,"%d ",dtrials[CTRIAL].obs);
	if(dtrials[CTRIAL].obs != 0)							/* eyetracking data */
		{
		xptr = dtrials[CTRIAL].x;
		yptr = dtrials[CTRIAL].y;
		sptr = dtrials[CTRIAL].s;
		eptr = dtrials[CTRIAL].e;
		*(sptr) = 0;									/* initial fix time = 0 = display on */
		for(iobs=0;iobs<dtrials[CTRIAL].obs;iobs++)
			fprintf(outfile,"%d %d %u %u ",(*(xptr+iobs)),(*(yptr+iobs)),*(sptr+iobs),*(eptr+iobs));
		free(dtrials[CTRIAL].x);
		free(dtrials[CTRIAL].y);
		free(dtrials[CTRIAL].s);
		free(dtrials[CTRIAL].e);
		}
	fprintf(outfile,"\n");
	fprintf(blinkfile,"%d %d %d",dtrials[CTRIAL].ord,dtrials[CTRIAL].cond,dtrials[CTRIAL].item);
	fprintf(blinkfile,"     %d",dtrials[CTRIAL].nblinks);
	for(j = 0;j<dtrials[CTRIAL].nblinks;j++)
		fprintf(blinkfile," %u %u",*((dtrials[CTRIAL].startblink)+j),*((dtrials[CTRIAL].endblink)+j));
	fprintf(blinkfile,"\n");
	}
/*else
	printf("\nNothing to write, trial %d",i);*/
	}
fclose(infile);
fclose(outfile);
fclose(blinkfile);
}


/************************************************/

/* FUNCTION TO GET NAMES OF FILES AND OPEN THEM UP */

void get_files(void)
{
char filename[80],filenameo[80],tempstr[20],filenameb[80];
int inter,i;
printf("Input data file name? ");
while((infile = fopen(gets(filename),"r"))==NULL)
	{
	printf("\nNot a good name, try again: ");
	}
printf("Output data file name? ");
while(!strcmp(gets(filenameo),filename))
	printf("\nDon't use same name for input and output! Try again.\n");
if(fopen(filenameo,"r") != NULL)
	{
	printf("\n %s exists; you can\n  Quit and choose a new name\n  Erase the old file and write a new one\nTYPE q or e:  ",filenameo);
	gets(tempstr);
	if(tolower(tempstr[0]) == 'e')
		{
		if((outfile=fopen(filenameo,"w")) == NULL)	
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
	if((outfile=fopen(filenameo,"w")) == NULL)	
		{
		printf("\nCan't create %s\n",filenameo);
	   	exit(1);
		}
	}
for(i=0;filenameo[i] != '.';i++)
	filenameb[i]=filenameo[i];
filenameb[i++]='.';
filenameb[i++]='t';
filenameb[i++]='l';
filenameb[i++]='b';
filenameb[i]='\0';
if((blinkfile=fopen(filenameb,"w")) == NULL)
	{
	printf("\nCan't create %s\n",filenameb);
   	exit(1);
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


int GetCond(char* dc,int *i)
{
char num[20];
char lastchar;
int j = 0;
while(!isdigit(*dc))
	{
	lastchar = *dc;
	dc++;
	(*i)++;
	}
while(isdigit(*dc))
	{
	if(tolower(lastchar) != 'c')
		{
		printf("\nERROR on trial %d; did not find C\n",CTRIAL+1);
		exit(1);
		}
	num[j++] = *(dc++);
	(*i)++;
	}
num[j] = '\0';
return(atoi(num));
}

int GetItem(char* dc,int *i)
{
char num[20];
char lastchar;
int j = 0;
while(!isdigit(*dc))
	{
	lastchar = *dc;
	dc++;
	(*i)++;
	}
while(isdigit(*dc))
	{
	if(tolower(lastchar) != 'i')
		{
		printf("\nERROR on trial %d; did not find i\n",CTRIAL+1);
		exit(1);
		}
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


/*************************************************************/
/* note, specialized for Kathryn Marzalek */

void get_sizes()
{
int tempvalue;
printf("\nTo accept defaults, just press ENTER or 0 followed by ENTER");
if ((tempvalue = input("\nHow wide is each character (pixels)? ")) == 0)
	char_width = 18;			/* 18 pixels */
else
	char_width = tempvalue;
/*if ((tempvalue = input("How high is each line (pixels)? ")) == 0)
	char_height = 33;	
else
	char_width = tempvalue;*/
if ((tempvalue = input("What is the horizontal offset (pixels?) ")) == 0)
	horiz_offset = 65;			/* 80 pixels */
else
	horiz_offset = tempvalue;
if ((tempvalue = input("What is the vertical offset (pixels?) ")) == 0)
	vert_offset = 65;			/* 60 pixels */
else
	vert_offset = tempvalue;
if ((tempvalue = input("What is the vertical line spacing (pixels?) ")) == 0)
	line_spacing = 97;			/* 105 pixels */
else 
	line_spacing = tempvalue;
printf("\nSELECTED VALUES IN PIXELS:\ncharacter width %d horizondal offset %d\nvertical line width %d vertical offset %d\n",char_width,horiz_offset,line_spacing,vert_offset);
}
