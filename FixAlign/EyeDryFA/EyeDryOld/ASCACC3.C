/* EYELINK PROGRAM -- ASCACC -- USE TO CALCULATE ACCURACY OF EYELINK .ASC QUESTION ANSWERS */
/* This version generalized to read correct answer from ascii file */
/* and to compute reaction time (from display onset to button press */

/*copyright 2008 Charles Clifton. Users of this program are permitted
to modify and distribute it at no charge but may not sell it or
include any of its code in a commercial program.*/

char VERSION[] = "1/24/2010 Accuracy counter";

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
#define MAXTRIALS 200
#define MAXLINE 200
#define MAXFIX 2000
#define MAXCHAR 4500
#define MaxNoCond 24
/* DECLARATIONS OF GLOBALS */

int CTRIAL;
int dummy[1];

int FixedPitch;			/* 1 = courier, 0 lookup table */
int maxitem, maxcond;
int minitem,mincond;
FILE *infile,*outfile,*outfile2;
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
int PartStringCompare(char *dc,char *dv);
int GetNextNumber(char* dc,int *i);
int GetCond(char* dc,int *i);
int GetItem(char* dc,int *i);
long GetNextLongNumber(char* dc,int *i);
float GetNextFloat(char* dc,int *i);
int GetAnswer(FILE *infile);
int GetCorrect(FILE *infile);
long GetRespTime(FILE *infile);
long GetSync(FILE *infile);
int answers_correct[MaxNoCond];
int answers_n[MaxNoCond];
int RT;
long RT_Sum;
long RT_Sum_Cond[MaxNoCond];
int correct;
int ncorrect,n;
/* MAIN PROGRAM */


void main(void)
{
char tempstr[80];
int i,answer,target,inter,ntrials,strial,number_fixations;
int lastitem,lastcond;
int trial, cond, item;
int file_position;
long synctime,endtime;
struct date today;
ncorrect = n = 0;
RT_Sum = 0L;
for(i=0;i<MaxNoCond;i++)
	{
	answers_correct[i]=answers_n[i] = 0;
	RT_Sum_Cond[i] = 0L;
	}
printf("\n\nVersion of %s\n\n",VERSION);
minitem = input("What is the smallest question item number to analyze?  ");
maxitem = input("What is the largest question item number to analyze?  ");
mincond = input("\nWhat is the smallest question condition number to analyze?  ");
maxcond = input("What is the largest question condition number to analyze?  ");
get_files();																/* open input and output files */
printf("\ncounting trials.....");
ntrials = CountTrials(infile);							/* count lines in input file */
printf("\n%d TRIALS - press key to continue\n",ntrials);
getch();
rewind(infile);

fprintf(outfile,"Cond	Item	SCond	SItem	Resp	Corr	RT\n");
for(CTRIAL = 0;CTRIAL < ntrials;CTRIAL++)
	{
	GetNextTrial(infile,&ntrials);
	lastitem=dtrials[CTRIAL-1].item;
	item = dtrials[CTRIAL].item;
	lastcond=dtrials[CTRIAL-1].cond;
	cond = dtrials[CTRIAL].cond;

/*printf("\nTo analyze trial %d cond %d item %d lastitem %d",CTRIAL+1,dtrials[CTRIAL].cond,dtrials[CTRIAL].cond,item);*/
	if(cond <= maxcond && item <= maxitem && cond >= mincond && item >= minitem)
		{
		target = GetCorrect(infile);
		synctime = GetSync(infile);
		endtime = GetRespTime(infile);
		answer = GetAnswer(infile);
		if(answer == target)
			correct = 1;
		else
			correct = 0;
		n++;
		answers_n[cond-mincond]++;
		if(correct == 1)
			{
			RT_Sum += endtime-synctime;
			ncorrect++;
			answers_correct[cond-mincond]++;
			RT_Sum_Cond[cond-mincond] += (endtime-synctime);
			}
/*		printf("\nAnalyzing trial %d cond %d item %d %ld %ld %u",CTRIAL+1,dtrials[CTRIAL].cond,item,endtime,synctime,(unsigned)(endtime-synctime));*/
		fprintf(outfile,"%d	%d	%d	%d	%d	%d	%d\n",cond,item,lastcond,lastitem,answer,correct,(unsigned)(endtime-synctime));
		}
	else
		;
/*		printf("\nSkipping trial %d cond %d item %d",CTRIAL+1,dtrials[CTRIAL].cond,dtrials[CTRIAL].item);*/
	}
if(n > 0)
	fprintf(outfile2,"\n\nOverall: N correct %d total N %d percentage correct %d Mean Correct RT %u",ncorrect,n,100*ncorrect/n,(unsigned)(RT_Sum/(long)ncorrect));
else
	printf("\nNot writing overall to outfile;n is zero.");
for(i=mincond;i<=maxcond;i++)
	if(answers_n[i-mincond] > 0)
		fprintf(outfile2,"\nCond %d N correct %d total N %d percentage correct %d Mean Correct RT %ld",i,answers_correct[i-mincond],answers_n[i-mincond],100*(answers_correct[i-mincond])/answers_n[i-mincond],(RT_Sum_Cond[i-mincond]/answers_correct[i-mincond]));
	else
		fprintf(outfile2,"\nCond %d N correct %d total N %d percentage correct 0 RT x",i,answers_correct[i-mincond],answers_n[i-mincond]);

fclose(outfile);
fclose(outfile2);
fclose(infile);
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
			if((dtrials[CTRIAL].cond = GetCond(&(tl[i]),&i)) == 0)
				break;
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


/************************************************/

/* FUNCTION TO GET NAMES OF FILES AND OPEN THEM UP */

void get_files(void)
{
char filename[80],filenameo[80],tempstr[20],filenameo2[80];
int i,j;
printf("Input data file name? ");
while((infile = fopen(gets(filename),"r"))==NULL)
	{
	printf("\nNot a good name, try again: ");
	}
for(i=0;filename[i]!='.';i++)
	{
	filenameo[i]=filename[i];
	filenameo2[i]=filename[i];
	}
filenameo2[i]='.';
filenameo[i++]='.';
filenameo2[i]='s';
filenameo[i++]='a';
filenameo2[i]='u';
filenameo[i++]='c';
filenameo2[i]='m';
filenameo[i++]='c';
filenameo2[i]='\0';
filenameo[i]='\0';

if((outfile=fopen(filenameo,"w")) == NULL)	
	{
	printf("\nIn get_files, can't create %s\n",filenameo);
   	exit(1);
	}
if((outfile2=fopen(filenameo2,"w")) == NULL)	
	{
	printf("\nIn get_files, can't create %s\n",filenameo2);
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
	if(tolower(lastchar) != 'e' && tolower(lastchar) != 'f')			/* look for e100 and e101 trials and f trials for Mara */
		{
		return(0);
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

int GetAnswer(FILE *infile)
{
char tl[160];
char c;
char* endsignal;
int i,j;
int index;
while((endsignal = fgets(tl,160,infile)) != NULL)
	{
	if(tl[0] == 'M' && tl[1] == 'S' && tl[2] == 'G')
		{
		i = 2;
		GetNextLongNumber(&(tl[i]),&i);
		if(PartStringCompare("TRIAL_RESULT",&(tl[i])))
			{
			return(GetNextNumber(&(tl[i]),&i));	
			}
		}
	}
if(endsignal == NULL)
	{
	printf("\nReached End of File without reading trial %d; aborting. ",CTRIAL+1);
	exit(1);
	}

}


int GetCorrect(FILE *infile)
{
char tl[160];
char c;
char* endsignal;
int i,j;
int index;
while((endsignal = fgets(tl,160,infile)) != NULL)
	{
	if(tl[0] == 'M' && tl[1] == 'S' && tl[2] == 'G')
		{
		i = 2;
		GetNextLongNumber(&(tl[i]),&i);
		if(PartStringCompare("QUESTION_ANSWER",&(tl[i])))
			{
			return(GetNextNumber(&(tl[i]),&i));	
			}
		}
	}
if(endsignal == NULL)
	{
	printf("\nReached End of File without reading trial %d; aborting. ",CTRIAL+1);
	exit(1);
	}
}


long GetSync(FILE *infile)
{
char tl[160];
char c;
char* endsignal;
int i,j;
int index;
long temp;
while((endsignal = fgets(tl,160,infile)) != NULL)
	{
	if(tl[0] == 'M' && tl[1] == 'S' && tl[2] == 'G')
		{
		i = 2;
		temp = GetNextLongNumber(&(tl[i]),&i);
		if(PartStringCompare("SYNCTIME",&(tl[i])))
			{
			return(temp);	
			}
		}
	}
if(endsignal == NULL)
	{
	printf("\nReached End of File without reading trial %d; aborting. ",CTRIAL+1);
	exit(1);
	}
}

long GetRespTime(FILE *infile)
{
char tl[160];
char c;
char* endsignal;
int i,j;
int index;
long temp;
while((endsignal = fgets(tl,160,infile)) != NULL)
	{
	if(tl[0] == 'M' && tl[1] == 'S' && tl[2] == 'G')
		{
		i = 2;
		temp = GetNextLongNumber(&(tl[i]),&i);
		if(PartStringCompare("ENDBUTTON",&(tl[i])))
			{
			return(temp);	
			}
		}
	}
if(endsignal == NULL)
	{
	printf("\nReached End of File without reading trial %d; aborting. ",CTRIAL+1);
	exit(1);
	}
}
