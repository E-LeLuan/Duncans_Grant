/* EYELINK PROGRAM -- VWAnal.c, from S4ANAL.c (from ASC2DAT.) -- USE TO TAKE EYELINK ASCII DATA */
/* TO ANALYZE Visual world data lke SEINFELD 4, EyeTrack 0.7.9*/
/* with some #defines to make it more general eventually.... #
/* batch version  - have data.lst with list of .asc file names; 
creates single file for all Ss */

/*Revised to output average percentages by specified time slices */
/* and re-revised from s4anal.c to make these percentages be the percentages
of all legal fixations in the time slice (including off-picture fixations),
not the percengates of the samples in the time slice that were spent in the region */

/*GENERALIZED as VWAnal.c; re-define maxpix and maxitems if you have more than
20 scoring regions (pictures) in a slide or if you have more than 96 items 
and recompile - tcc -ml VWAnal.c */

/* the program assumes you measured your picture regions on a 1280 x 1024 screen */
/* change in the #defines if necessary and recompile */

/* NOTE KLUDGE FOR SEINFELD4 - COLLAPSING CONDITIONS AND SHIFTING REFERENTS...*/

/* RIGHT EYE VERSION */

/*copyright 1990, 2003, 2008 Charles Clifton. Users of this program are permitted
to modify and distribute it at no charge but may not sell it or
include any of its code in a commercial program.*/

char VERSION[] = "7/1/2008";

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

/*Experiment-specific defines and variables */

#define maxpix 20		/* number of picture (scoring) regions in position file */
int nWaveRegions;	/* number of different regions marked in the wave time file */
#define maxitems 96		/* number of items with picture and wave regions */
#define xres 1280	
#define yres 1024		/* x and y resolution used when measuring pictures */

/* DECLARATIONS OF GLOBALS */

int CTRIAL, maxitem, maxcond, minitem,mincond;
FILE *infile,*interfile,*outfile, *offsetfile, *locationfile, *choicefile, *propfile;
FILE *parm;
FILE *datalst;
int fcond,lcond,fitem,litem;
int line_spacing,space_between;
int slice_width;
int npix,nitems;
int response_count, accuracy_count;
struct dtrial
	{
	int ord;
	int cond;
	int item;
	int type;
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

struct square
	{
	int tx;
	int ty;
	int bx;
	int by;
	} squares[maxpix][maxitems];

int tlx,tly,brx,bry;						/* display coordinates, top left x etc */

#define LF 0x0a
#define CR 0x0d

/* DECLARACTIONS OF FUNCTIONS */

void main(void);
int GetFiles(char *offsetname, char *locationname);
int CountTrials(FILE* infile);
int input(char *output);
int FindDisplayCoordinates(FILE *infile);
int GetNextTrial(FILE* infile);
long GetTrialTime(FILE* infile);
/*int GetFixations(FILE* infile,long start_time);*/
int GetRegions(FILE *infile, int cond,int item,long starttime,int offset,int samplerate,int nsample,int subjid);
int GetOffset(FILE* infile,int cond,int item,int offset_point);
int PartStringCompare(char *dc,char *dv, int* i);
int StringCompare(char *dc,char *dv, int* i);
int GetNextNumber(char* dc,int *i);
int GetCond(char* dc,int *i);
int GetItem(char* dc,int *i);
int GetType(char* dc,int *i);
long GetNextLongNumber(char* dc,int *i);
long LongNumber(char* dc,int *i);
float GetNextFloat(char* dc,int *i);
void WriteData(int ntrials);
void get_sizes(void);
int GetLocations(FILE* infile);

/* MAIN PROGRAM */


void main(void)
{
int ti,nsub;
char buff[80];
char tempstr[80];
char file[80];
char offsetname[80],locationname[80];
int offset;
int offset_point;		/* which offset, 1 to 7 for Seinfeld 4, starts the record? */
int nsample;
int i,inter,ntrials,strial,number_fixations;
int trial, cond, item;
int file_position;
int samplerate;
int subjid;
long starttime;
struct date today;
printf("\n\nVersion of %s\n\n",VERSION);
/*printf("\nThis is the general version for visual world experiments using EyeTrack.");*/
printf("\nThis is the kludged version for Seinfeld 4.");
printf("\nIf that's not what you want, type q to quit and find the right program.");
if(getch() == 'q')
	exit(1);
if((parm = fopen("paramet.lst","r")) == NULL)
	{
	printf("\nWhat is name of file of time offsets (in ms)? ");
	scanf("%s",offsetname);
	printf("\nHow many different offsets does this file have for each sentence? ");
	scanf("%d",&nWaveRegions);
	printf("\nWhich offset do you want to use - 1 to %d? ",nWaveRegions);
	scanf("%d",&offset_point);
/*	offset_point = input("Which offset do you want to use - 1 to nWaveRegions? ");*/
	printf("\nWhat is name of file of picture region locations? ");
	scanf("%s",locationname);
	npix = input("How many picture scoring regions do you have in a slide? Max = 20: ");
	nitems = input("How many different slides (items) do you have? Max = 96: ");
	mincond = input("What is the smallest condition number to analyze? ");
	maxcond = input("What is the largest condition number to analyze? ");
	minitem = input("What is the smallest item number to analyze? ");
	maxitem = input("What is the largest item number to analyze? ");
	samplerate = input("How many ms/sample - 1, 2, or 4? ");
	nsample = input("How many ms of data do you want to analyze? ");
	slice_width = input("What should be the width in ms of each data slice? ");
	}
else			/* previous parameters exist */
	{
/*	printf("\nReading parameters in from paramet.lst, file * = %d",parm);*/
	fscanf(parm,"%s%d%d%s%d%d%d%d%d%d%d%d%d",offsetname,&nWaveRegions,&offset_point,locationname,&npix,&nitems,&mincond,&maxcond,&minitem,&maxitem,&samplerate,&nsample,&slice_width);
/*printf("\n%s %d %s %d %d %d %d %d %d %d",offsetname,offset_point,locationname,mincond,maxcond,minitem,maxitem,samplerate,slice_width);*/
	printf("\nPress ENTER to accept defaults");
	printf("\nFile of time offsets? (default %s): ",offsetname);
	gets(buff);
	if(sscanf(buff,"%s",tempstr) != EOF)
		strcpy(offsetname,tempstr);
	printf("\nHow many different offsets does this file have for each sentence? (default %d): ",nWaveRegions);
	gets(buff);
	if(sscanf(buff,"%s",tempstr) != EOF)
		nWaveRegions = atoi(tempstr);
	printf("\nWhich offset point to start (1-%d)? (default %d): ",nWaveRegions,offset_point);
	gets(buff);
	if(sscanf(buff,"%s",tempstr) != EOF)
		offset_point = atoi(tempstr);
	printf("\nFile of picture region locations? (default %s): ",locationname);
	gets(buff);
	if(sscanf(buff,"%s",tempstr) != EOF)
		strcpy(locationname,tempstr);
	printf("Number of picture scoring regions? (default %d): ",npix);
	gets(buff);
	if(sscanf(buff,"%s",tempstr) != EOF)
		npix = atoi(tempstr);
	printf("Number of items? (default %d): ",nitems);
	gets(buff);
	if(sscanf(buff,"%s",tempstr) != EOF)
		nitems = atoi(tempstr);
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
	printf("Sample rate 1, 2, 4 ms per sample (default %d): ",samplerate);
	gets(buff);
	if(sscanf(buff,"%s",tempstr) != EOF)
		samplerate = atoi(tempstr);
	printf("Ms of data after selected start? (default %d): ",nsample);
	gets(buff);
	if(sscanf(buff,"%s",tempstr) != EOF)
		nsample = (atoi(tempstr));
	printf("Width of each data slice, in ms (default %d): ",slice_width);
	gets(buff);
	if(sscanf(buff,"%s",tempstr) != EOF)
		slice_width = atoi(tempstr);
	fclose(parm);
	}
if((parm = fopen("paramet.lst","w")) == NULL)
	{
	printf("\n Ooops - can't open paramet.lst to write");
	exit(1);
	}
else
	{
	fprintf(parm,"%s %d %d %s %d %d %d %d %d %d %d %d %d ",offsetname,nWaveRegions,offset_point,locationname,npix,nitems,mincond,maxcond,minitem,maxitem,samplerate,nsample,slice_width);
	fclose(parm);
	}
nsample = (nsample)/samplerate;
nsub = GetFiles(offsetname,locationname);																/* open input and output files */
GetLocations(locationfile);
for(subjid=0;subjid<nsub;subjid++)
{			/* start of for subject loop */
fscanf(datalst,"%s",file);
if((infile=fopen(file,"r")) == NULL)
	{
	printf("\nCAN'T OPEN FILE %s",file);
	exit(1);
	}
FindDisplayCoordinates(infile);
ntrials = CountTrials(infile);							/* count lines in input file */
/*printf("\n%d TRIALS, coords %d %d %d %d\nPress any key to continue if OK.",ntrials,tlx,tly,brx,bry);
getch();*/
rewind(infile);

response_count = accuracy_count = 0;
for(CTRIAL = 0;CTRIAL < ntrials;CTRIAL++)
	{
	GetNextTrial(infile);	
	if(dtrials[CTRIAL].cond >= mincond && dtrials[CTRIAL].cond <= maxcond && dtrials[CTRIAL].item >= minitem && dtrials[CTRIAL].item <= maxitem && dtrials[CTRIAL].type == 0)
		{
/*		printf("\nAnalyzing trial %d cond %d item %d",CTRIAL+1,dtrials[CTRIAL].cond,dtrials[CTRIAL].item);*/
		starttime = GetTrialTime(infile);
		if(starttime > 0l)
			{
			offset = GetOffset(offsetfile,dtrials[CTRIAL].cond,dtrials[CTRIAL].item,offset_point);
/*printf("\noffset %d cond %d item %d offset_point %d",offset,dtrials[CTRIAL].cond,dtrials[CTRIAL].item,offset_point);
if(getch() == 'q')
	exit(1);*/
			rewind(offsetfile);
			GetRegions(infile,dtrials[CTRIAL].cond,dtrials[CTRIAL].item,starttime,offset,samplerate,nsample,subjid);
			}
		}
	else
		{
/*		printf("\nSkipping trial %d cond %d item %d",CTRIAL+1,dtrials[CTRIAL].cond,dtrials[CTRIAL].item);
		if(getch() == 'q')
			exit(1);*/
		;
		}
	}
if(response_count > 0)
	{
	printf("\nProportion accurate responses: %f\n",(float)(accuracy_count)/(float)(response_count));
	fprintf(choicefile,"Proportion accurate responses, subject %d, = %.2f\n",subjid+1,(float)(accuracy_count)/(float)(response_count));
	}
else
	{
	printf("\nZero trials with responses\n");
	fprintf(choicefile,"Zero trials with responses, subject %d",subjid+1);
	}
fclose(infile);
}		/* end of for subject loop */
fclose(outfile);
fclose(offsetfile);
fclose(propfile);
}


/************************************************/

int GetRegions(FILE *infile, int cond,int item,long starttime,int offset, int samplerate,int nsample,int subjid)
{
char tl[160];
int* region;
int* count;
int prop[maxpix+1];
int nprop;
long time;
int resp,accuracy;
char* endsignal;
int i,j,k,l,ip;
int itemp;
int sample=0;
int gotEndbutton = 0;
int endtrial = 0;
float xpos,ypos;
int xposc,yposc;
long currenttime;
region = (int*)(calloc(nsample,sizeof(int)));
count = (int*)(calloc(nsample,sizeof(int)));
while(((endsignal = fgets(tl,160,infile)) != NULL) && !gotEndbutton && !endtrial)
	{
	if(tl[0] == 'M' && tl[1] == 'S' && tl[2] == 'G')
		{
		i = 2;
/*		if(StringCompare("TRIAL OK",&(tl[i]),&i))*/
/*		if(StringCompare("TRIAL_RESULT",&tl[i],&i))*/
		currenttime = GetNextLongNumber(&(tl[i]),&i);
		if(!gotEndbutton && StringCompare("ENDBUTTON",&tl[i],&i))
			{
			gotEndbutton = 1;
			resp = GetNextNumber(&(tl[i]),&i);
/*printf("\nEndButon: cond %d item %d CTRIAL %d %d samples resp %d",cond,item,CTRIAL,sample,resp);*/
/* KLUDGE FOR SEINFELD 4: conditionalize accuracy on condition numbers:
1 late boundary, high attachment, correct on left; 
2 early boundary, low attachment, correct on right
3 late boundary, high attachment, correct on right; 
4 early boundary, low attachment, correct on left */

			if(cond == 1 || cond == 4)
				{
				if(resp == 2)
					accuracy = 1;		/* correct on left */
				else
					accuracy = 2;		
				}
			else					/* correct on right */
				{
				if(resp  == 2)
					accuracy = 2;
				else
					accuracy = 1;
				}
			fprintf(choicefile,"subject %d cond %d item %d CTRIAL %d resp %d Rtime %d correct  %d\n",subjid+1,cond,item,CTRIAL,resp,((int)(currenttime-starttime))/samplerate,accuracy);
			if(accuracy == 1)
				accuracy_count++;
			response_count++;
			}
		i = 2;
		currenttime = GetNextLongNumber(&(tl[i]),&i);
		if(!gotEndbutton && StringCompare("TRIAL_RESULT",&tl[i],&i))
			{
			fprintf(choicefile,"\nNO RESP Cond %d item %d CTRIAL %d time %ld samples %d",cond,item,CTRIAL,currenttime,sample);
			endtrial = 1;			
			} 
		i = 2;
		currenttime = GetNextLongNumber(&(tl[i]),&i);
		if(!gotEndbutton && StringCompare("TRIAL ERROR",&tl[i],&i))
			{
			fprintf(choicefile,"\nNO DATA Cond %d item %d CTRIAL %d time %ld samples %d",cond,item,CTRIAL,currenttime,sample);
			endtrial = 1;			
			} 
		}
	else if(!isdigit(tl[0]))
		;
	else if(sample < nsample && !gotEndbutton)
		{
		i = 0;
		if((time = GetNextLongNumber(&tl[i],&i)) < starttime+(long)(offset))
			;				/* skip early samples */
		else
			{
			xpos = GetNextFloat(&(tl[i]),&i);
			ypos = GetNextFloat(&(tl[i]),&i);
			xposc = (int)((xpos*((float)(xres)))/(float)(brx-tlx+1));			/* normalize to 1280 x 1024 or xres x yres */
			yposc = (int)((ypos*((float)(yres)))/(float)(bry-tly+1));
			if(xpos == -9999 | ypos == -9999 || (int)xpos <= tlx || (int)ypos <= tly || (int)xpos >= brx || (int)ypos >= bry)
				region[sample] = npix+2;			/* illegal observation */
			else			/* identify box */
				{
				region[sample] = npix+1;				/* default, 1 more than number of pix */
				for(j=0;j<npix;j++)
						{					/* identify which picture is being looked at */
	{
/*	printf("\nxpos %.2f xposc %d ypos %.2f yposc %d",xpos,xposc,ypos,yposc);*/
/*	printf("\npic %d squares tx %d bx %d ty %d by %d",j+1,squares[item-1][j].tx,squares[item-1][j].bx,squares[item-1][j].ty,squares[item-1][j].by);*/
	}
						if(xposc >= squares[item-1][j].tx && xposc <= squares[item-1][j].bx && yposc >= squares[item-1][j].ty && yposc <= squares[item-1][j].by)
							{
							region[sample] = j+1;
							count[sample]++;
/*							break;*/
							j = npix;
							}
						}

/*printf("\nsamp %d time %ld xpos %.2f c %d ypos %.2f c %d reg %d c %d i %d",sample,time,xpos,xposc,ypos,yposc,region[sample],cond,item);
if(getch() == 'q')
	exit(1);*/
				}
		sample++;
			}
		}
	}

/*printf("\nendsignal %s gotEndbutton %d sample %d nsample %d",endsignal,gotEndbutton,sample,nsample);*/
if(cond > 2)
	cond -= 2;			/* kludge for Seinfeld 4, conds 3 and 4 == conds 1 and 2 except for response side */
/* fprintf(outfile,"%d %d %d %d",subjid+1,cond,item,sample+1); */
fprintf(outfile,"%d %d %d %d",(subjid/2)+1,cond,item,sample+1);	/*Special version for Seinfeld 4 - 2 files per subject */
if(endsignal != NULL)
	for(i=0;i<sample;i++)
		fprintf(outfile," %d",region[i]);
fprintf(outfile,"\n");

/* compute and write proportions of fixations */
for(i=0,l=0;i<sample/(slice_width/samplerate);i++)		/* loop thru slices i; l is sample number */
	{
	fprintf(propfile,"S%d,C%d,I%d,%d,%d",(subjid/2)+1,cond,item,i+1,l*samplerate);	/* special for S4 */
	for(ip=0;ip<npix+1;ip++)
		prop[ip] = 0;
	nprop = 0;
	for(k=0;k<slice_width/samplerate;k++,l++)
		{
		for(j=0;j<npix+1;j++)	
			{
			if(region[l] == j+1)			/* got fixation on this picture or outside regions */
				{
				prop[j]++;
				nprop++;
/*printf("\nprop loop i = %d j = %d k = %d l = %d prop = %d nprop %d ",i,j,k,l,prop[j],nprop);*/
				break;
				}
			}
		}
		for(j=0;j<npix+1;j++)	
			{
			if(nprop > 0)
				fprintf(propfile,",%d",(prop[j])*100/nprop);
			else
				fprintf(propfile,",");
			}
		fprintf(propfile,"\n");
	}
free(region);
free(count);
}

/************************************************/

int GetOffset(FILE *infile, int cond, int item,int offset_point)
{
char line[160];
int i,j,nval,ccond,citem;
int off[40];
while(fgets(line,160,infile) != NULL)
	{
	i = 0;
	ccond=GetNextNumber(&(line[i]),&i);
	citem=GetNextNumber(&(line[i]),&i);
	if(ccond == cond)
		if(citem == item)
			{
			for(nval = 0;nval < nWaveRegions;nval++)
				off[nval] = GetNextNumber(&(line[i]),&i);
			return(off[offset_point-1]);
			}
	}
printf("\nCan't find cond %d item %d offset_point %d in infile %d",cond,item,offset_point,infile);
exit(1);
}

/************************************************/


int GetLocations(FILE *infile)	/* specialized for nitem = 16 items, npix pictures each */
{
char line[160];
int nval,citem;
int i;
for(citem=0;citem<nitems;citem++)
	{
	fgets(line,160,infile);
	i = 0;
/*printf("\n%s",line);*/
	for(nval = 0;nval < npix;nval++)
		{
		squares[citem][nval].tx = GetNextNumber(&(line[i]),&i);
		squares[citem][nval].ty = GetNextNumber(&(line[i]),&i);
		squares[citem][nval].bx = GetNextNumber(&(line[i]),&i);
		squares[citem][nval].by = GetNextNumber(&(line[i]),&i);
/*printf("\ncitem %d x %d",citem,squares[citem][nval].tx);
printf(" y %d",squares[citem][nval].ty);
printf(" by %d\n",squares[citem][nval].by);
printf(" bx %d",squares[citem][nval].bx);
if(getch()=='q')
	exit(1);*/
		}
	}
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
	if(tl[0] == 'M' && tl[1] == 'S' && tl[2] == 'G')
		{
		i = 2;
		GetNextLongNumber(&(tl[i]),&i);
		if(PartStringCompare("TRIALID",&(tl[i]),&i))
			trials++;
		}
	}
return(trials);
}

/***********************************************/

/*
int CountTrials(FILE *infile)	
{
char tl[160];
char tempbuff[80];
char c;
int i;
int trials = 0;
while(fgets(tl,160,infile) != NULL)
	{
	i = 0;
	if(tl[0] == 'M' && tl[1] == 'S' && tl[2] == 'G')
		{
		i = 2;
		GetNextLongNumber(&(tl[i]),&i);
		if(PartStringCompare("TRIALID ",&(tl[i]),&i))
			{
			i++;
			if(tl[i] == 't')
				if(isdigit(tl[i+3]))
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
*/

/************************************************/


int GetNextTrial(FILE *infile)
{
char tl[160];
char c;
char* endsignal;
int i,j;
int line,lasty;
while((endsignal = fgets(tl,160,infile)) != NULL)
	{
	if(tl[0] == 'M' && tl[1] == 'S' && tl[2] == 'G')
		{
		i = 2;
		GetNextLongNumber(&(tl[i]),&i);
/*		if(PartStringCompare("TRIAL REPEATED",&(tl[i]),&i))
			{
			CTRIAL--;
			(*ntrials)--;
			printf("\nTRIAL %d REPEATED",CTRIAL+1);
			return(0);
			}*/
		if(PartStringCompare("TRIALID",&(tl[i]),&i))
			{
			dtrials[CTRIAL].ord = CTRIAL+1;
			dtrials[CTRIAL].cond = GetCond(&(tl[i]),&i);
			dtrials[CTRIAL].item = GetItem(&(tl[i]),&i);
			dtrials[CTRIAL].type = GetType(&(tl[i]),&i);
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
else if(PartStringCompare("TRIAL ERROR",&(tl[i]),&i))
	{
	printf("\nFound TRIAL ERROR on trial %d",CTRIAL+1);
	return(-1);
	}
if(endsignal == NULL)
	{
	printf("\nReached End of File without reading trial %d; aborting. ",CTRIAL+1);
	exit(1);
	}
else
	{
	;
/*	printf("\nFound trial %d cond %d item %d type %d",CTRIAL+1,dtrials[CTRIAL].cond,dtrials[CTRIAL].item,dtrials[CTRIAL].type);*/
	;
	}
}



/************************************************/

int FindDisplayCoordinates(FILE *infile)
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
		if(PartStringCompare("DISPLAY COORDS ",&(tl[i]),&i))
				{
				tlx = GetNextNumber(&(tl[i]),&i);
				tly = GetNextNumber(&(tl[i]),&i);
				brx = GetNextNumber(&(tl[i]),&i);
				bry = GetNextNumber(&(tl[i]),&i);
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
	;	/*printf("\nDisplay coordinates:top left %d %d bottom right %d %d",tlx,tly,brx,bry);*/
return(i);
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
		if(StringCompare("AUDIO PLAYBACK ON",&(tl[i]),&i))
				break;
		i = 2;
		GetNextLongNumber(&(tl[i]),&i);
		ltime = 0;
		if(StringCompare("TRIAL ERROR",&(tl[i]),&i))
				break;
		i = 2;
		GetNextLongNumber(&(tl[i]),&i);
		ltime = 0;
		if(StringCompare("TRIAL_RESULT",&(tl[i]),&i))
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
			return(nfix);
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
/*		printf("\nFound legal item, trial %d cond %d item %d",i,dtrials[i].cond,dtrials[i].item);*/
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
fclose(choicefile);
}


/************************************************/

/* FUNCTION TO GET NAMES OF FILES AND OPEN THEM UP */

int GetFiles(char *offsetname, char *locationname)
{
char filename[80],filenameo[80],tempstr[20];
char buff[80];
char c;
int nsub;
int inter,i;
if((offsetfile = fopen(offsetname,"r")) == NULL)
	{
	printf("\nCan't open offset file, %s",offsetname);
	exit(2);
	}
if((locationfile = fopen(locationname,"r")) == NULL)
	{
	printf("\nCan't open offset file, %s",locationname);
	exit(2);
	}
printf("What is the name of the file that lists data files? ");
while((datalst=fopen(gets(filename),"r")) == NULL)
	printf("\nBad file name, try again.\n");
printf("datalst = %s\n",filename);
nsub = 0;
while((c = fgetc(datalst)) != EOF)	/* count the subjects */
	{
/*	printf("%c",c);*/
	if(c == '\n')
		nsub++;
	}
rewind(datalst);
printf("\n%d subjects\n",nsub);


printf("Output data file (without extension): ");
gets(buff);
if(sscanf(buff,"%s",tempstr) != EOF)
	strcpy(filename,tempstr);
for(i=0;filename[i] != '\0' && filename[i] != '.';i++)
	;
filename[i] = '.';
filename[i+1] = 'd';
filename[i+2] = 'a';
filename[i+3] = 't';
filename[i+4] = '\0';
if(fopen(filename,"r") != NULL)
	{
	printf("\n %s exists; you can\n  Quit and choose a new name\n  Erase the old file and write a new one\nTYPE q or e:  ",filename);
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
/* now the response file */
filename[i+1] = 'r';
filename[i+2] = 's';
filename[i+3] = 'p';
if((choicefile = fopen(filename,"w")) == NULL)
		{
		printf("\nCan't create %s\n",filename);
		exit(1);
		}
filename[i+1] = 't';
filename[i+2] = 'x';
filename[i+3] = 't';
if((propfile = fopen(filename,"w")) == NULL)
		{
		printf("\nCan't create %s\n",filename);
		exit(1);
		}
/* write propfile header */
fprintf(propfile,"Subj,Cond,Item,Slice,Time");
for(i=0;i<npix+1;i++)
	fprintf(propfile,",R%d",i+1);
fprintf(propfile,"\n");
return(nsub);
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
	num[j++] = *(dc++);
	(*i)++;
	}
num[j] = '\0';
if(tolower(lastchar) != 'e')			/* looking for e trial, form enninnd0 */
	{
	return(0);
	}
else
	{
	return(atoi(num));
	}
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
		printf("\nERROR on trial %d; did not find i, found %c instead\n",CTRIAL+1, lastchar);
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

int GetType(char* dc,int *i)
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
	if(tolower(lastchar) != 'd')
		{
		printf("\nERROR on trial %d; did not find d\n",CTRIAL+1);
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
