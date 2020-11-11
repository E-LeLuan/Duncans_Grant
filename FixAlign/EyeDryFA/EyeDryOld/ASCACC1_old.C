/* EYELINK PROGRAM -- ASCACC -- USE TO CALCULATE ACCURACY OF EYELINK .ASC QUESTION ANSWERS */
/* This version generalized to read correct answer from ascii file */
/* and to compute question-answering reaction time (from display onset to button press */

/* BATCH version - uses list of asc file names */

/* modified to take single file argument */


/* This version assumes that your questions have condition numbers that are distinct from the
experimental sentence item numbers (e.g. 101 for all questions). It produces an output file
for each subject that gives the response, RT, and accuracy for each question, together with
the question condition and item number plus the condition and item numbers for the previous
trial - which is presumably the experimental sentence being questioned. It also produces a
summary file with the mean accuracy for each subject in each EXPERIMENTAL SENTENCE condition */

/*copyright 2008 Charles Clifton. Users of this program are permitted
to modify and distribute it at no charge but may not sell it or
include any of its code in a commercial program.*/

char VERSION[] = "5/18/2010 Batch accuracy counter";

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
#define MAXTRIALS 400
#define MAXLINE 200
#define MaxNoCond 40
/* DECLARATIONS OF GLOBALS */

int CTRIAL;
int dummy[1];

int FixedPitch;			/* 1 = courier, 0 lookup table */
int maxitem, maxcond;
int maxSenitem,maxSencond;
int minitem,mincond;
int minSenitem,minSencond;
FILE *infile,*outfile,*outfile2,*control;
int fcond,lcond,fitem,litem;
int char_width,char_height;	/* pixels */
int horiz_offset, vert_offset;	/* also pixels */
int line_spacing;
struct dtrial
	{
	int ord;
	int cond;
	int item;
	}	dtrials[MAXTRIALS];

#define LF 0x0a
#define CR 0x0d

/* DECLARACTIONS OF FUNCTIONS */

void main(void);
void get_files(char *sfile);
int CountTrials(FILE* infile);
int input(char *output);
int GetNextTrial(FILE* infile,int *ntrials);
int PartStringCompare(char *dc,char *dv);
int GetNextNumber(char* dc,int *i);
int GetCond(char* dc,int *i);
int GetItem(char* dc,int *i);
long GetNextLongNumber(char* dc,int *i);
float GetNextFloat(char* dc,int *i);
int GetAnswer(FILE *infile,int *ntrials, long *endtime);
int GetCorrect(FILE *infile,int *ntrials);
long GetRespTime(FILE *infile,int *ntrials);
long GetSync(FILE *infile,int *ntrials);
int answers_correct[MaxNoCond];
int answers_n[MaxNoCond];
int tot_answers_correct,tot_answers_n;
long RT_Sum_Cond[MaxNoCond];
int correct;
int ncorrect,n;
/* MAIN PROGRAM */


void main(void)
{
char tempstr[80];
char datfilename[80],filenameo2[80];
char sfile[80];
int i,answer,target,inter,ntrials,strial,number_fixations,nsub;
int lastitem,lastcond;
int trial, cond, item;
int ns;
char c;
int file_position;
int single;
long synctime,endtime;
struct date today;
ncorrect = n = 0;
printf("\n\nVersion of %s\nMaximum number of SENTENCE conditions %d\n",VERSION,MaxNoCond);
printf("Do you have a single file to analyze? y or n: ");
if(tolower(getch()) == 'y')
	single = 1;
else
	single = 0;

if(single == 0)
	printf("\nAssuming a file of .asc file names.");
printf("\nProgram outputs accuracy referenced to the previous sentence trial condition,\nand provides correct times from video onset.\n");
minitem = input("What is the smallest *QUESTION ITEM NUMBER* to analyze?  ");
maxitem = input("What is the largest question item number to analyze?  ");
mincond = input("What is the smallest *QUESTION CONDITION NUMBER* to analyze?  ");
maxcond = input("What is the largest question condition number to analyze?  ");
minSencond = input("What is the smallest *EXPERIMENTAL SENTENCE CONDITION NUMBER* to analyze?  ");
maxSencond = input("What is the largest experimental sentence condition number to analyze?  ");
if(single == 0)
	{	
	printf("\nName of file containing data file names? ");
	while((control = fopen(gets(datfilename),"r")) == NULL)
		printf("\nBad file name, try again ");
	ns = 0;
	while((c = fgetc(control)) != EOF)	/* count the subjects */
		{
		printf("%c",c);
		if(c == '\n')
			ns++;
		}
	printf("\n\n%d subjects",ns);
	rewind(control);
	}
else
	{
	printf("\nName of data file to analyze? ");
	while((control = fopen(gets(sfile),"r")) == NULL)
		printf("\nBad file name, try again ");
	ns = 1;
	}
printf("\nName of summary output file? ");
while((outfile2 = fopen(gets(filenameo2),"w")) == NULL)
	printf("\nBad file name, try again ");
fprintf(outfile2,"SubjNo,File,Sencond,Ncorr,Nquest,Percentcorr,MeanRT\n");

for(nsub=0;nsub<ns;nsub++)
	{
	bdos(11,0,0);		/* check for operator interrupt - i.e. control-C*/
	for(i=0;i<MaxNoCond;i++)
		{
		answers_correct[i]=answers_n[i] = 0;
		RT_Sum_Cond[i] = 0L;
		}
	tot_answers_correct = tot_answers_n = 0;
	n = 0;			/* number of questions for this subject */
	if(single == 0)
		fscanf(control,"%s",sfile);
	get_files(sfile);					/* open input and output files */
	printf("\ncounting trials.....");
	ntrials = CountTrials(infile);							/* count lines in input file */
	printf("\n%d TRIALS",ntrials);
	rewind(infile);

	fprintf(outfile,"QCond,QItem,SCond,SItem,Resp,Corr,RT\n");

	for(CTRIAL = 0;CTRIAL < ntrials;CTRIAL++)
		{
		GetNextTrial(infile,&ntrials);			/* in this version, get only EXPERIMENTAL not FILLER, PRACICE trials */
		lastitem=dtrials[CTRIAL-1].item;
		item = dtrials[CTRIAL].item;
		lastcond=dtrials[CTRIAL-1].cond;
		if(lastcond > maxSencond)
			lastcond = maxSencond+1;
		else if(lastcond < minSencond)
			lastcond = minSencond-1;		/* screen out items outside condition range */
		cond = dtrials[CTRIAL].cond;
/*printf("\nOrder %d trial %d cond %d item %d lastitem %d lastcond %d",dtrials[CTRIAL].ord,CTRIAL+1,dtrials[CTRIAL].cond,dtrials[CTRIAL].item,lastitem,lastcond);*/
		if(cond <= maxcond && item <= maxitem && cond >= mincond && item >= minitem)
			{
			target = GetCorrect(infile,&ntrials);
			synctime = GetSync(infile,&ntrials);
			/*endtime = GetRespTime(infile,&ntrials);*/		/* not needed - got in GetAnswer */
			answer = GetAnswer(infile,&ntrials,&endtime);
			if(answer == target)
				{
				tot_answers_correct++;
				correct = 1;
				}
			else
				correct = 0;
			tot_answers_n++;
			n++;
			if(lastcond >= minSencond && lastcond <= maxSencond)
				{
				(answers_n[lastcond-minSencond])++;	/* tabulate by SENTENCE condition,limiting to range*/
				if(correct == 1)
					{
					ncorrect++;
					(answers_correct[lastcond-minSencond])++;
					RT_Sum_Cond[lastcond-minSencond] += (endtime-synctime);
					}
				}
/*		printf("\nAnalyzing trial %d cond %d item %d %ld %ld %u",CTRIAL+1,dtrials[CTRIAL].cond,item,endtime,synctime,(unsigned)(endtime-synctime));*/
		fprintf(outfile,"%d,%d,%d,%d,%d,%d,%d\n",cond,item,lastcond,lastitem,answer,correct,(unsigned)(endtime-synctime));
		}	/* still printing out trials outside condition range */
	else
		;
/*		printf("\nSkipping trial %d cond %d item %d\n",CTRIAL+1,dtrials[CTRIAL].cond,dtrials[CTRIAL].item); */
	}
if(n > 0)
	{
	for(i=0;i<=maxSencond-minSencond;i++)
		{
		if(answers_n[i] > 0)
			{
			if(answers_correct[i] > 0)
				fprintf(outfile2,"%d,%s,%d,%d,%d,%d,%ld\n",nsub+1,sfile,minSencond+i,answers_correct[i],answers_n[i],100*(answers_correct[i])/answers_n[i],(RT_Sum_Cond[i]/(long)(answers_correct[i])));
			else
				fprintf(outfile2,"%d,%s,%d,%d,%d,%d,\n",nsub+1,sfile,minSencond+i,answers_correct[i],answers_n[i],100*(answers_correct[i])/answers_n[i]);
			}
		else
			fprintf(outfile2,"%d,%s,%d,%d,%d,,\n",nsub+1,sfile,minSencond+i,answers_correct[i],answers_n[i]);

		}
	printf("\n\n PERCENTAGE CORRECT FOR THIS SUBJECT: %d\n\n",(tot_answers_correct*100)/tot_answers_n);
	fprintf(outfile,"\nPERCENTAGE CORRECT: %d",(tot_answers_correct*100)/tot_answers_n);
	}
else
	printf("\nNot writing sub %d file %s to summary file;n is zero.\n",nsub+1,sfile);
	

fclose(outfile);
fclose(infile);
}
fclose(control);
fclose(outfile2);
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
			break;
			}
		}
	}
if(endsignal == NULL)
	{
	printf("\nIn GetNextTrial; Reached End of File without reading trial %d; aborting. ",CTRIAL+1);
	exit(1);
	}
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

void get_files(char *sfile)
{
char filename[80],filenameo[80],tempstr[20];
int i,j;
if((infile = fopen(sfile,"r"))==NULL)
	{
	printf("\n%s Not a good name, check data list file: ",sfile);
	exit(1);
	}
strcpy(filename,sfile);
for(i=0;filename[i]!='.';i++)
	{
	filenameo[i]=filename[i];
	}
filenameo[i++]='.';
filenameo[i++]='a';
filenameo[i++]='c';
filenameo[i++]='c';
filenameo[i]='\0';

if((outfile=fopen(filenameo,"w")) == NULL)	
	{
	printf("\nIn get_files, can't create %s\n",filenameo);
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
/*	if(tolower(lastchar) != 'e' && tolower(lastchar) != 'f') */	/* look for e100 and e101 trials and f trials for Mara */
	if(tolower(lastchar) != 'e')				/* this version, only e not f items */
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

int GetAnswer(FILE *infile,int *ntrials,long *endtime)
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
		if(PartStringCompare("TRIAL_RESULT",&(tl[i])))
			{
			*endtime = temp;
			return(GetNextNumber(&(tl[i]),&i));	
			}
		else if(PartStringCompare("TRIALID",&(tl[i])))
			{
			printf("\nOOPS - didn't get answer to question, trial %d,cond %d, item%d",CTRIAL+1,dtrials[CTRIAL].cond,dtrials[CTRIAL].item);
			(*ntrials)--;
			return(0);
			}
		}
	}
if(endsignal == NULL)
	{
	printf("\nIn GetAnswer; Reached End of File without reading trial %d; aborting. ",CTRIAL+1);
	exit(1);
	}

}


int GetCorrect(FILE *infile,int *ntrials)
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
		else if(PartStringCompare("TRIALID",&(tl[i])))
			{
			printf("\nOOPS - didn't get correct response, trial %d,cond %d, item%d",CTRIAL+1,dtrials[CTRIAL].cond,dtrials[CTRIAL].item);
			(*ntrials)--;
			return(0);
			}
		}
	}
if(endsignal == NULL)
	{
	printf("\nIn GetCorrect; Reached End of File without reading trial %d; aborting. ",CTRIAL+1);
	exit(1);
	}
}


long GetSync(FILE *infile,int *ntrials)
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
		else if(PartStringCompare("TRIALID",&(tl[i])))
			{
			printf("\nOOPS - didn't get synctime, trial %d,cond %d, item%d",CTRIAL+1,dtrials[CTRIAL].cond,dtrials[CTRIAL].item);
			(*ntrials)--;
			return(0);
			}
		}
	}
if(endsignal == NULL)
	{
	printf("\nIn GetSync; Reached End of File without reading trial %d; aborting. ",CTRIAL+1);
	exit(1);
	}
}

/* not used - incorporated in getanswer */
long GetRespTime(FILE *infile,int *ntrials)
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
		else if(PartStringCompare("TRIALID",&(tl[i])))
			{
			printf("\nOOPS - didn't get response time, trial %d,cond %d, item%d",CTRIAL+1,dtrials[CTRIAL].cond,dtrials[CTRIAL].item);
			(*ntrials)--;
			return(0);
			}
		}
	}
if(endsignal == NULL)
	{
	printf("\nIn GetRespTime; Reached End of File without reading trial %d; aborting. ",CTRIAL+1);
	exit(1);
	}
}
