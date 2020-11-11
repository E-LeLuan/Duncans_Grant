/* EYELINK PROGRAM -- ASC2DAT -- USE TO TAKE EYELINK ASCII DATA */
/* TO ANALYZE FOR SIMPLE VISUAL WORLD EXPERIMENT */
/* INITIALLY WITH 1-20 VERTICAL SLICES AS REGIONS */

/* specialized for initial Steven Frisson headmount sub-inter study
to correct for condition numbers:
	1 = ambig, subsective on left
	2 = ambig, subsecetive on right
	3 = unambig, left
	4 = unamb, right
and to score things so that 
	REGION 1 = subsective or correct
	REGION 2 = middle
	REGION 3 = intersective or error

*/

/* converts one asc version of edf file to dat file */
/* with format, each line, cond item #samples region-of-each-sample */

/* Assumes ascii file of offsets, in ms, of critical region from */
/* start of wave file. File contains Cond Item Offset (ms) */

/* RIGHT EYE VERSION */


/* note: assumes that TRIALID is ALWAYS of the form CxxIyy..., where
xx is the condition number and yy is the item numeber!!! 
(needed to allow use of eyewash, eyedry; other nonnumeric characters 
can follow yy if you want */

/* WELL IT SHOULD DO THAT BUT IN THIS VERSION THE FORMAT IS 
t-01-01PAGE 1 or like that */

/* note, top margin set in do_text_trials() or w32_text_trials() */

/*copyright 1990, 2003 Charles Clifton. Users of this program are permitted
to modify and distribute it at no charge but may not sell it or
include any of its code in a commercial program.*/

char VERSION[] = "12/23/2003";

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
#define MAXTRIALS 512
#define MAXLINE 2000
#define MAXFIX 2000

/* DECLARATIONS OF GLOBALS */

int CTRIAL;
int dummy[1];

int maxitem, maxcond, minitem,mincond;
FILE *infile,*interfile,*outfile, *offsetfile;
FILE *parm;
int fcond,lcond,fitem,litem;
int line_spacing,space_between;
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
	}	dtrials[MAXTRIALS];

#define LF 0x0a
#define CR 0x0d

/* DECLARACTIONS OF FUNCTIONS */

void main(void);
void GetFiles(char *offsetname);
int CountTrials(FILE* infile);
int input(char *output);
int FindDisplayCoordinates(FILE *infile,int* tlx,int* tly, int* brx, int* bry);
void GetNextTrial(FILE* infile);
long GetTrialTime(FILE* infile);
/*int GetFixations(FILE* infile,long start_time);*/
int GetRegions(FILE *infile, int cond,int item,long starttime,int offset,int baseline,int* rightbound,int nsample,int nregions,int subjid);
int GetOffset(FILE* infile,int cond,int item);
int PartStringCompare(char *dc,char *dv, int* i);
int StringCompare(char *dc,char *dv, int* i);
int GetNextNumber(char* dc,int *i);
int GetCond(char* dc,int *i);
int GetItem(char* dc,int *i);
long GetNextLongNumber(char* dc,int *i);
float GetNextFloat(char* dc,int *i);
void WriteData(int ntrials);
void get_sizes(void);


#define samplerate 4

/* MAIN PROGRAM */


void main(void)
{
int ti;
char buff[80];
char tempstr[80];
char offsetname[80];
int offset;
int rightbound[20];
int nregions;
int nsample;
int tlx,tly,brx,bry;						/* display coordinates, top left x etc */
int i,inter,ntrials,strial,number_fixations;
int trial, cond, item;
int file_position;
int baseline;				/* ms before start of offset region to record */
int subjid;
long starttime;
struct date today;
printf("\n\nVersion of %s\n\n",VERSION);

if((parm = fopen("paramet.lst","r")) == NULL)
	{
	printf("\nWhat is name of file of time offsets (in ms)? ");
	scanf("%s",offsetname);
	mincond = input("What is the smallest condition number to analyze? ");
	maxcond = input("What is the largest condition number to analyze? ");
	minitem = input("What is the smallest item number to analyze? ");
	maxitem = input("What is the largest item number to analyze? ");
	nregions = input("How many analysis regions (vertical stripes)? ");
	rightbound[0] = 0;			/* first region starts at far left */
	for (i=1;i<nregions;i++)
		rightbound[i] = input("Right boundary of region: ");
	rightbound[i] = 1600;			/* far right region ends at 1600 pixels */
	baseline = input("How many ms baseline (before offset) do you want? ");
	nsample = (input("How many ms of data do you want to analyze? "))/samplerate;
	}
else			/* previous parameters exist */
	{
	fscanf(parm,"%s%d%d%d%d%d%d%d",offsetname,&mincond,&maxcond,&minitem,&maxitem,&baseline,&nsample,&nregions);
	printf("\nPress ENTER to accept defaults");
	printf("\nFile of time offsets? (default %s): ",offsetname);
	gets(buff);
	if(sscanf(buff,"%s",tempstr) != EOF)
		strcpy(offsetname,tempstr);
	printf("Smallest condition number? (default %d): ",mincond);
	gets(buff);
	if(sscanf(buff,"%s",tempstr) != EOF)
		mincond = atoi(tempstr);
	printf("Largest condition number? (default %d): ",maxcond);
	gets(buff);
	if(sscanf(buff,"%s",tempstr) != EOF)
		maxcond = atoi(tempstr);
	printf("Smallest item number? (default %d): ",minitem);
	gets(buff);
	if(sscanf(buff,"%s",tempstr) != EOF)
		minitem = atoi(tempstr);
	printf("Largest item number? (default %d): ",maxitem);
	gets(buff);
	if(sscanf(buff,"%s",tempstr) != EOF)
		maxitem = atoi(tempstr);
	printf("Ms baseline before offset? (default %d): ",baseline);
	gets(buff);
	if(sscanf(buff,"%s",tempstr) != EOF)
		baseline = atoi(tempstr);
	printf("Ms of data after offset? (default %d): ",nsample*samplerate);
	gets(buff);
	if(sscanf(buff,"%s",tempstr) != EOF)
		nsample = (atoi(tempstr))/samplerate;
	printf("Number of analysis regions? (default %d): ",nregions);
	gets(buff);
	if(sscanf(buff,"%s",tempstr) != EOF)
		nregions = atoi(tempstr);
	rightbound[0] = 0;			/* first region starts at far left */
	for (i=1;i<nregions;i++)
		{
		fscanf(parm,"%d",&rightbound[i]);
		printf("Right boundary of region %d (default %d)",i,rightbound[i]);
		gets(buff);
		if(sscanf(buff,"%s",tempstr) != EOF)
			rightbound[i] = atoi(tempstr);
		}
	rightbound[i] = 1600;			/* far right region ends at 1600 pixels */
	fclose(parm);
	}
if((parm = fopen("paramet.lst","w")) == NULL)
	{
	printf("\n Ooops - can't open paramet.lst to write");
	exit(1);
	}
else
	{
	fprintf(parm,"%s %d %d %d %d %d %d %d ",offsetname,mincond,maxcond,minitem,maxitem,baseline,nsample,nregions);
	for(i=1;i<nregions;i++)
		fprintf(parm,"%d ",rightbound[i]);
	fclose(parm);
	}
nsample += baseline/samplerate;
GetFiles(offsetname);																/* open input and output files */
subjid = input("\nWhat number do you want to assign this subject? ");
ntrials = CountTrials(infile);							/* count lines in input file */
printf("\n%d TRIALS\n",ntrials);
rewind(infile);


for(CTRIAL = 0;CTRIAL < ntrials;CTRIAL++)
	{
	GetNextTrial(infile);	
	if(dtrials[CTRIAL].cond >= mincond && dtrials[CTRIAL].cond <= maxcond && dtrials[CTRIAL].item >= minitem && dtrials[CTRIAL].item <= maxitem)
		{
/* kludge for Steven headmount subsective/intersective */
if(dtrials[CTRIAL].cond <= 2)
	{
	if((ti = dtrials[CTRIAL].item) == 1 || ti == 8 || ti == 10 || ti == 12 || ti == 16)
		dtrials[CTRIAL].cond = 2;			/* intersective on left, ambig, cond 2 */
	else
		dtrials[CTRIAL].cond = 1;			/* subsective on left, ambig, cond 1 */
	}
		printf("\nAnalyzing trial %d cond %d item %d",CTRIAL+1,dtrials[CTRIAL].cond,dtrials[CTRIAL].item);
		starttime = GetTrialTime(infile);
		offset = GetOffset(offsetfile,dtrials[CTRIAL].cond,dtrials[CTRIAL].item);
printf("  offset %d trial %d",offset,CTRIAL+1);
/*getch();*/
		GetRegions(infile,dtrials[CTRIAL].cond,dtrials[CTRIAL].item,starttime,offset,baseline,rightbound,nsample,nregions,subjid);
		}
	else
		printf("\nSkipping trial %d cond %d item %d",CTRIAL+1,dtrials[CTRIAL].cond,dtrials[CTRIAL].item);
/*	getch();*/
	}
fclose(infile);
fclose(outfile);
fclose(offsetfile);
}
/************************************************/
int GetOffset(FILE *infile, int cond, int item)
{
int thiscond,thisitem,thisoffset;
while(fscanf(infile,"%d%d%d",&thiscond,&thisitem,&thisoffset) != EOF)
	{
	if(thiscond == cond)
		if(thisitem == item)
			{
/*			printf("\nthiscond %d thisitem %d thisoffset %d",thiscond,thisitem,thisoffset);*/
			return(thisoffset);
			}
	}
printf("\nCan't find cond %d item %d in infile %d",cond,item,infile);
exit(1);
}



/************************************************/

int GetRegions(FILE *infile, int cond,int item,long starttime,int offset,int baseline,int* rightbound,int nsample,int nregions,int subjid)
{
char tl[160];
int* region;
long time;
int xpos;
char* endsignal;
int i,j,k;
int sample=0;
region = (int*)(calloc(nsample,sizeof(int)));
while((endsignal = fgets(tl,160,infile)) != NULL && sample < nsample)
	{
	if(tl[0] == 'M' && tl[1] == 'S' && tl[2] == 'G')
		{
		i = 2;
		GetNextLongNumber(&(tl[i]),&i);
		if(StringCompare("TRIAL OK",&(tl[i]),&i))
			{
			printf("\ncond %d item %d ended at %d samples",cond,item,sample);
			break;
			}
		}
	else if(!isdigit(tl[0]))
		;
	else
		{
		i = 0;
		if((time = GetNextLongNumber(&tl[i],&i)) < starttime+(long)(offset-baseline))
			;				/* skip early samples */
		else
			{
			if((xpos = GetNextNumber(&tl[i],&i)) == -9999)		/* bad sample */
				{
				if(sample > 0)
					region[sample] = region[sample-1];			/* default to previous region */
				else
					region[sample] = 0;
				sample++;
				}
			else
				{
				if(sample > nsample || xpos <= 0)
					{
					printf("\nBig sample %d or bad xpos %d at cond %d item %d time %ld",sample,xpos,cond,item,time);
					if(getch() == 'q')
						exit(1);
					}
				else
					{
					region[sample] = 0;				/* default */
					for(j=0;j<nregions;j++)
						{
/*if(xpos <= 0)
printf("\nScanning for xpos %d region %d between %d and %d, cond %d item %d",xpos,j,rightbound[j],rightbound[j+1],cond,item);*/
/*printf("\nj %d xpos %d rightbound[j] %d rightbound[j+1] %d sample %d",j,xpos,rightbound[j],rightbound[j+1],sample);*/

						if(xpos > rightbound[j] && xpos <= rightbound[j+1])
							{
							region[sample++] = j+1;			/* count region from 1 */
							break;
							}
						}
					}
				if(cond == 2 || cond == 4)		/* remap so reg 1 = subsec/correct */
					{
					if(region[sample-1] == 3)
						region[sample-1]= 1;
					else if(region[sample-1] == 1)
						region[sample-1] = 3;
					}
				}
			}
		}
	}
fprintf(outfile,"%d %d %d %d %d",subjid,cond,item,baseline/samplerate,sample+1);	/* sample includes baseline observations; both in observations not ms */
if(endsignal != NULL)
	for(i=0;i<sample;i++)
		fprintf(outfile," %d",region[i]);
fprintf(outfile,"\n");
free(region);
}

int CountTrials(FILE *infile)		/* counts # of trials to analyze */
{
char tl[160];
char tempbuff[80];
char c;
int i;
int trials = 0;
while(fgets(tl,160,infile) != NULL)
	{
	i = 0;
/*	if(PartStringCompare("START ",&(tl[i])),&i)*/
	if(tl[0] == 'M' && tl[1] == 'S' && tl[2] == 'G')
		{
		i = 2;
		GetNextLongNumber(&(tl[i]),&i);
		if(PartStringCompare("TRIALID ",&(tl[i]),&i))
			{
			i++;
/*printf("\ngot trialid - tl[i] = %c, tl[i-1] %c tl[i+1] %c, tl[i+3] = %c i = %d",tl[i],tl[i-1],tl[i+1],tl[i+3],i);*/
			if(tl[i] == 't')
				if(isdigit(tl[i+3]))			/* not a filler */
					{
					tempbuff[0] = tl[i+2];
					tempbuff[1] = tl[i+3];
					tempbuff[2] = '\0';
					dtrials[CTRIAL].cond = atoi(tempbuff);
					tempbuff[0] = tl[i+5];
					tempbuff[1] = tl[i+6];
					tempbuff[2] = '\0';
					dtrials[CTRIAL].item = atoi(tempbuff);
					if(dtrials[CTRIAL].cond >= mincond && dtrials[CTRIAL].cond <= maxcond && dtrials[CTRIAL].item >= minitem && dtrials[CTRIAL].item <= maxitem)
						trials++;
					}
			}
		}
	}
return(trials);
}




/************************************************/


/* special version of GetNextTrial for Steven Frisson */

void GetNextTrial(FILE *infile)
{
char tl[160];
char tempbuff[20];
char c;
char* endsignal;
int i;
while((endsignal = fgets(tl,160,infile)) != NULL)
	{
	if(tl[0] == 'M' && tl[1] == 'S' && tl[2] == 'G')
		{
		i = 2;
		GetNextLongNumber(&(tl[i]),&i);
		if(PartStringCompare("TRIALID ",&(tl[i]),&i))
			{
			i++;
/*printf("\ngot trialid - tl[i-1] = %c, tl[i] %c tl[i+1] %c, tl[i+3] = %c i = %d",tl[i-1],tl[i],tl[i+1],tl[i+3],i);*/
			if(tl[i] == 't')
				if(isdigit(tl[i+3]))			/* not a filler */
					{
					tempbuff[0] = tl[i+2];
					tempbuff[1] = tl[i+3];
					tempbuff[2] = '\0';
					dtrials[CTRIAL].cond = atoi(tempbuff);
					tempbuff[0] = tl[i+5];
					tempbuff[1] = tl[i+6];
					tempbuff[2] = '\0';
					dtrials[CTRIAL].item = atoi(tempbuff);
					break;
					}
				else
					{
					dtrials[CTRIAL].cond = dtrials[CTRIAL].item = 0;
					}
			}
		}
	}
if(endsignal == NULL)
	{
	printf("\nReached End of File without reading trial %d, cond %d item %d; aborting. ",CTRIAL,dtrials[CTRIAL].cond,dtrials[CTRIAL].item);
	exit(1);
	}
}



/*
null GetNextTrial(FILE *infile)
{
char tl[160];
char c;
char* endsignal;
int i;
while((endsignal = fgets(tl,160,infile)) != NULL)
	{
	if(tl[0] == 'M' && tl[1] == 'S' && tl[2] == 'G')
		{
		i = 2;
		GetNextLongNumber(&(tl[i]),&i);
		if(PartStringCompare("TRIALID ",&(tl[i]),&i))
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
	printf("\nReached End of File without reading trial %d; aborting. ",CTRIAL);
	exit(1);
	}
}
*/


/************************************************/

int FindDisplayCoordinates(FILE *infile,int* tlx,int* tly, int* brx, int* bry)
{
char tl[160];
char c;
char* endsignal;
int i;
while((endsignal = fgets(tl,160,infile)) != NULL)
	{
	if(tl[0] == 'M' && tl[1] == 'S' && tl[2] == 'G')
		{
		i = 2;
		GetNextNumber(&(tl[i]),&i);
		if(PartStringCompare("DISPLAY_COORDS ",&(tl[i]),&i))
				{
				*tlx = GetNextNumber(&(tl[i]),&i);
				*tly = GetNextNumber(&(tl[i]),&i);
				*brx = GetNextNumber(&(tl[i]),&i);
				*bry = GetNextNumber(&(tl[i]),&i);
				break;
				}
		}
	}
if(endsignal == NULL)
	{
	printf("\nReached End of File without reading DISPLAY_COORDS; aborting. ");
	exit(1);
	}
else
printf("\nDisplay coordinates:top left %d %d bottom right %d %d",*tlx,*tly,*brx,*bry);
	return i;
}


/************************************************/


long GetTrialTime(FILE* infile)
{
char tl[160];
char c;
char* endsignal;
int i;
long ltime,dtime;
while((endsignal = fgets(tl,160,infile)) != NULL)
	{
	if(tl[0] == 'M' && tl[1] == 'S' && tl[2] == 'G')
		{
		i = 2;
		ltime = GetNextLongNumber(&(tl[i]),&i);
		if(StringCompare("SOUND STARTED 0",&(tl[i]),&i))
				break;
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

/*int GetFixations(FILE* infile,long start_time)
{
char tl[160];
char c;
char* endsignal;
int i,j,k, temp;
float fx, fy;
int nfix = 0;
unsigned *s, *e;
int *x, *y;
unsigned stime[MAXFIX],etime[MAXFIX];
int ax[MAXFIX],ay[MAXFIX];
while((endsignal = fgets(tl,160,infile)) != NULL)
	{
	if(tl[0] == 'E' && tl[1] == 'F' && tl[2] == 'I' && tl[3] == 'X' && tl[5] == 'R')
		{
		i = 5;
		stime[nfix] = (unsigned)(GetNextLongNumber(&(tl[i]),&i)-start_time);
		etime[nfix] = (unsigned)(GetNextLongNumber(&(tl[i]),&i)-start_time);
		GetNextNumber(&(tl[i]),&i);
		fx = GetNextFloat(&(tl[i]),&i);
		fy = GetNextFloat(&(tl[i]),&i);
		ax[nfix] = (int)((fx-horiz_offset)/char_width);
		ay[nfix] = (int)((fy-(vert_offset-space_between+1))/line_spacing)+1;	
		if(ay[nfix] < 0)
			ay[nfix] = 0;
		nfix++;
		}
	else if(tl[0] == 'M' && tl[1] == 'S' && tl[2] == 'G')
		{
		dtrials[CTRIAL].obs = nfix;
		i = 2;
		GetNextLongNumber(&(tl[i]),&i);
		if(PartStringCompare("ENDBUTTON ",&(tl[i]),&i))	
			{
			dtrials[CTRIAL].s = (s = ((unsigned *)(calloc(nfix,sizeof(unsigned)))));
			dtrials[CTRIAL].e = (e = ((unsigned *)(calloc(nfix,sizeof(unsigned)))));
			dtrials[CTRIAL].x = (x = ((int *)(calloc(nfix,sizeof(int)))));			
			dtrials[CTRIAL].y = (y = ((int *)(calloc(nfix,sizeof(int)))));			
			for(k = 0; k < nfix; k++)
				{
				*(x++) = ax[k];
				*(y++) = ay[k];
				*(s++) = stime[k];
				*(e++) = etime[k];
				}
			return nfix;
			}
		}			
	}
if(endsignal == NULL)
	{
	printf("\nReached End of File prematurely, trial %d; aborting. ",CTRIAL);
	exit(1);
	}
}
*/

/************************************************/
int StringCompare(char *dc,char *dv, int* i)		/* same to end of calling string */
{
for(;*dc == *dv || *dc == '\0';dc++,dv++,(*i)++)
	{
	if(*dc == '\0')
		return 1;				/* identical */
	}
	return 0;					/* different */
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
		printf("\nFound legal item, trial %d cond %d item %d",i,dtrials[i].cond,dtrials[i].item);
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
/*	if(getch() == 'q')
		exit(1);*/
	if(map[i] != -999)
	{
	CTRIAL = map[i];
	printf("\nwriting trial %d map[i] = %d cond %d item %d",i+1,map[i],dtrials[CTRIAL].cond,dtrials[CTRIAL].item);
	fprintf(outfile,"%d %d %d %d %d %d %d ",dtrials[CTRIAL].ord,dtrials[CTRIAL].cond,dtrials[CTRIAL].item,dtrials[CTRIAL].rt,dtrials[CTRIAL].resp,dtrials[CTRIAL].rt2,dtrials[CTRIAL].resp2);
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
	}
/*else
	printf("\nNothing to write, trial %d",i);*/
	}
fclose(infile);
fclose(outfile);
}


/************************************************/

/* FUNCTION TO GET NAMES OF FILES AND OPEN THEM UP */

void GetFiles(char *offsetname)
{
char filename[80],filenameo[80],tempstr[20];
int inter,i;
if((offsetfile = fopen(offsetname,"r")) == NULL)
	{
	printf("\nCan't open offset file, %s",offsetname);
	exit(2);
	}
printf("\nInput data file name (don't include .asc): ");
gets(filename);
for(i=0;filename[i] != '\0' && filename[i] != '.';i++)
	;
filename[i] = '.';
filename[i+1] = 'a';
filename[i+2] = 's';
filename[i+3] = 'c';
filename[i+4] = '\0';
while((infile = fopen(filename,"r"))==NULL)
	{
	printf("\nNot a good name, try again: ");
	gets(filename);
	for(i=0;filename[i] != '\0' && filename[i] != '.';i++)
		;
	filename[i] = '.';
	filename[i+1] = 'a';	
	filename[i+2] = 's';
	filename[i+3] = 'c';
	filename[i+4] = '\0';
	}
printf("Output data file name (don't include extension; .txt will be added: ");
gets(filename);
for(i=0;filename[i] != '\0' && filename[i] != '.';i++)
	;
filename[i] = '.';
filename[i+1] = 't';
filename[i+2] = 'x';
filename[i+3] = 't';
filename[i+4] = '\0';
if(fopen(filename,"r") != NULL)
	{
	printf("\n %s exists; you can\n  Quit and choose a new name\n  Erase the old file and write a new one\nTYPE q or e:  ",filenameo);
	gets(tempstr);
	if(tolower(tempstr[0]) == 'e')
		{
		if((outfile=fopen(filename,"w")) == NULL)	
			{
			printf("\nCan't create %s\n",filename);
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
	if((outfile=fopen(filename,"w")) == NULL)	
		{
		printf("\nCan't create %s\n",filename);
	   	exit(1);
		}
	}
}


/***********************************************************************/


int input(char *output)
{
char buffer[MAXLINE];
int input;
printf("%s",output);
/*buffer[0]='\0';*/
scanf("%d",&input);
return(input);
/*return(atoi(gets(buffer)));*/
}


/*************************************************************/


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
