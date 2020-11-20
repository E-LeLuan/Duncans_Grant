/* EYEWASH */
/*copyright 1990, 1999 Charles Clifton. Users of this program are permitted
to modify and distribute it at no charge but may not sell it or
include any of its code in a commercial program.*/

/* modified 6/5/88 to subtract offsets from x and y in final
output file */
/* modified 12/23/88 to make Sara happier */
/* and to make it work with TC 2.0 */
/* modified 4/29/89 to accept long sentences in final fix */
/* and to enable skipping of manual final fix */
/* modified 11/89 to permit analysis of subsets of conditions and items */
/* assumes question trials being with condition numbers >= 100 */
/* and ignores them */
/* modified 6/91 to increase buffer size and screen out CRs */
/* REmodified 9/21/91 to REstore ability to analyze a data file */
/* in multiple sessions */
/* modified to permit skipping extra columns in data file introduced */
/* by VGA experiment running system */
/* modified June 1996 to incorporate .tlb (track loss & blink) files */
/* modified July 1997 to report numbers of rejects in output file */
/* modified Nov 1997 because Rob Mason INSISTS on doing long passages with */
/* leading line feeds and x offsets-- it passes leading line feeds through */
/* to the sentence file, and adds the x offset to the first word after */
/* a new line */
/* modified June 98 to allow longer data lines (4000 characters) and to */
/* trap erroneous trial numbers at the end of a session. */
/* and further modified to correct problem with reading tlb data in get_input */
/* after the last legal trial. */
/* Modified July 1999 to include stuff for boundary/fast prime experiments */
/* (old eyewashp.c code) */
/* modified September 2000 to permit re-eyewashing .da1 files (that don't */
/* have questions in them, while the .tlb files do */

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
#define MAXLINE 4000

char VERSION[] = "June 3 2001, MAXLINE 4000 MAXTRIALS 512";
int CTRIAL;
int scrap_at_boundary,boundary_expt;


FILE *infile,*interfile,*outfile,*senfile,*posfile,*listfile,*tlbfile;
int fcond,lcond,fitem,litem;
char **wdptr;			/* pointer to arrays of words */
int **posptr;			/* pointer to arrays of positions */
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
	int primetime;
	int targettime;
	int *x;
	int *y;
	unsigned *s;
	unsigned *e;
	int *tlbs;
	int *tlbe;
	int tlbnumber;
	}	dtrials[MAXTRIALS];
#define LF 0x0a
#define CR 0x0d
char tempsen[MAXLINE];		/* 4000 chars max */
void main(void);
int get_files(void);
int get_input(int oldnew,int tracknumber);
void get_sentences(int ntrials,int *xoffset,int *yoffset,int inter);
int initial_fix(int ntrials,int scheme);
void final_fix(int ntrials,int scheme,int mt1,int md1,int mt2,int md2,int xoffset,int yoffset);
int input(char *output);
int number_rejected,number_truncated,number_combined,number_eliminated;

void main(void)
{
char tempstr[80];
int xoffset,yoffset;
int i,inter,ntrials,strial;
int scheme,oldnew,tracknumber;
int min_time1, min_dist1,min_time2,min_dist2;
struct date today;
printf("\n\nVersion of %s\n\n",VERSION);
printf("\nDo you have boundary change or fast prime data? y or n: ");
gets(tempstr);
if(tolower(tempstr[0]) == 'y')
	{
	boundary_expt=1;
	printf("\nOK, fastprime, give a thought for the wampus\n");
	}
else
	boundary_expt = 0;
printf("\nDo you want printer (P) or file (F) trace of analysis? ");
gets(tempstr);
if(tolower(tempstr[0]) != 'p')	/* not printer */
	{
	printf("File name for trace of analysis (.tra extension will be added)? ");
	gets(tempstr);
	for(i=0;i<8 && *(tempstr+i) != '.' && *(tempstr+i) != '\0';i++)
		;
	*(tempstr+i) = '.';
	i++;
	*(tempstr+i) = 't';
	i++;
	*(tempstr+i) = 'r';
	i++;
	*(tempstr+i) = 'a';
	i++;
	*(tempstr+i) = '\0';
	if((listfile = fopen(tempstr,"w"))==NULL)
		{
		printf("\nPROBLEMS WITH TRACE FILE %s; writing file temptra.tra",tempstr);
		if ((listfile = fopen("temptra.tra","w"))==NULL)
			{
			printf("\nCan't even open temptra.tra; exiting.");
			exit(1);
			}	
		}
	}
else										/* printer output */
	{
	*listfile = *stdprn;
	}
if(boundary_expt)
	oldnew = 1;
else
	{
	printf("\nIs your data file old (ETCC206 or DA1) or new (ETCCVGA) format?\n(type o if you are re-eyewashing eyewash output files); (o or n): ");
	gets(tempstr);
	if(tolower(tempstr[0] != 'o'))
		oldnew = 1;
	else
		oldnew = 0;		/* old file format */
	}
printf("\nDo your eyetracked items (i.e. nonquestions) have condition numbers");
printf("\nless than 100, or greater than 200? TYPE l(ess) or g(reater): ");
gets(tempstr);
if(tolower(tempstr[0] != 'g'))
	tracknumber = 1;
else
	tracknumber = 0;
getdate(&today);
fprintf(listfile,"\nEYEWASH: DATE %d %d %d VERSION of %s\n",today.da_mon,today.da_day,today.da_year,VERSION);
inter = get_files();
ntrials = get_input(oldnew,tracknumber);
get_sentences(ntrials,&xoffset,&yoffset,inter);
printf("\nThe final fix routine of this version first assimilates very short");
printf("\nfixations to the next (or previous) fixation if the short fixation");
printf("\nis less than TC1 and if the two fixations are within DC1 spaces");
printf("\nof each other. Typically, TC1 = 80, DC1 = 1.");
printf("\n\nThe routine then deletes the remaining short fixations if they are less");
printf("\nthan TC2 msec long and within DC2 spaces of the nearest fixation.");
printf("\nTypically, TC2 = 40 msec, DC2 = 3.");
printf("\n\nTC2 must be less than TC1, and DC2 should be greater than DC1.");
printf("\nThis version also assimilates adjacent fixations on the same point.\n");
if((min_time1 = input("\r\nWhat is TC1 (msec)? (ENTER for default 80) ")) == 0)
	min_time1 = 80;
if((min_dist1 = input("What is DC1 (characters)? (ENTER for default 1) "))== 0)
	min_dist1 = 1;
if((min_time2 = input("\r\nWhat is TC2 (msec)? (ENTER for 40) ")) == 0)
	min_time2 = 40;
if((min_dist2 = input("What is DC2 (characters)? (ENTER for default 3) ")) == 0)
	min_dist2 = 3;
fprintf(listfile,"\nTC1 %d DC1 %d TC2 %d DC2 %d",min_time1,min_dist1,min_time2,min_dist2);
if(boundary_expt)
	{
	printf("\nDiscard trials where fixation occurred immediately after boundary? y or n:");
	scrap_at_boundary = tolower(*(gets(tempstr))) == 'y' ? 1 : 0;
	}
else
	scrap_at_boundary = 0;
if(inter)		/* writing intermediate files */
	{
	scheme = 1;
/* scheme = input("\r\nType 1 if you want to do all initial screening first\n or 2 if you want to go sentence by sentence: "); */
	if(scheme == 1)
		{
		ntrials = initial_fix(ntrials,scheme);
		fprintf(interfile,"\n");
		fclose(interfile);
		final_fix(ntrials,scheme,min_time1,min_dist1,min_time2,min_dist2,xoffset,yoffset);
		}
	else
		{
		for(strial = 0; strial < ntrials; strial++)
			{
			ntrials = initial_fix(strial+1,scheme);
			final_fix(strial+1,scheme,min_time1,min_dist1,min_time2,min_dist2,xoffset,yoffset);
			if(input("\r\nENTER for next trial, any non-zero digit to stop: "))
				break;
			}
		fprintf(interfile,"\n");
		fclose(interfile);
		}
	}
else			/*working from intermediate files */		
	final_fix(ntrials,1,min_time1,min_dist1,min_time2,min_dist2,xoffset,yoffset);	
fprintf(listfile,"\n\n\nNumber of trials rejected %d\nNumber of trials truncated %d\nNumber of fixations eliminated %d\nNumber of fixations combined %d",number_rejected,number_truncated,number_eliminated,number_combined);
printf("\n\n\nNumber of trials rejected %d\nNumber of trials truncated %d\nNumber of fixations eliminated %d\nNumber of fixations combined %d",number_rejected,number_truncated,number_eliminated,number_combined);
printf("\nThese numbers appear at the end of the .tra file.\n");
fprintf(outfile,"\n");
fclose(outfile);
fclose(listfile);
}

/* FUNCTION TO GET NAMES OF FILES AND OPEN THEM UP */

int get_files(void)
{
char filename[80],filenameo[80],tempstr[20];
int inter,i;
printf("Input data file name? ");
while((infile = fopen(gets(filename),"r"))==NULL)
	{
	printf("\nNot a good name, try again: ");
	}
fprintf(listfile,"\r\nInput file name %s",filename);
printf("Output data file name? ");
while(!strcmp(gets(filenameo),filename))
	printf("\nDon't use same name for input and output! Try again.\n");
if(fopen(filenameo,"r") != NULL)
	{
	printf("\n %s exists; you can\n  Quit and choose a new name\n  Erase the old file and write a new one\n  Append the new data to the old file\n    TYPE q, e, or a:  ",filenameo);
	gets(tempstr);
	if(tolower(tempstr[0]) == 'a')
		{
		if((outfile=fopen(filenameo,"a")) == NULL)	
			{
			printf("\nCan't create %s\n",filenameo);
	    	exit(1);
			}
		}
	else if(tolower(tempstr[0]) == 'e')
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
fprintf(listfile,"\nOutput file name %s",filenameo);
for(i=0;filename[i]!='.';i++)
	;
if(filename[++i]!='&')
	{
	inter = 1;	/* writing intermediate files */
	filename[i] = '&';
	if(tolower(tempstr[0]) == 'a')	/* kludge, don't ask if you are already */
												/* adding to an output file */
		{
		if((interfile = fopen(filename,"a"))==NULL)
			{
			printf("\nCan't create or add to file %s\n.",filename);
			exit(1);
			}
		fprintf(listfile,"\nWriting intermediate file %s",filename);
		}
	else if(fopen(filename,"r") != NULL)
		{
		printf("\nIntermediate file %s exists; you can\n  Erase the old intermediate file and write a new one\n  Append the new data to the old intermediate file.\n    TYPE e or a:  ",filename);
		gets(tempstr);
		if(tolower(tempstr[0]) == 'a')
			{
			if((interfile=fopen(filename,"a")) == NULL)	
				{
				printf("\nCan't create %s\n",filename);
	    		exit(1);
				}
			}
		else if(tolower(tempstr[0]) == 'e')
			{
			if((interfile=fopen(filename,"w")) == NULL)	
				{
				printf("\nCan't create %s\n",filename);
		    	exit(1);
				}
			}
		}
	else
		{
		if((interfile=fopen(filename,"w")) == NULL)	
			{
			printf("\nCan't create %s\n",filename);
	   	exit(1);
			}
		}
fprintf(listfile,"\nIntermediate file name %s",filename);
	}
else
	{
	inter = 0;
	printf("\nNot doing initial fix; going directly to final fix.\n");
	}
/* TLB stuff */
filename[i] = 't';
filename[i+1] = 'l';
filename[i+2] = 'b';
if((tlbfile = fopen(filename,"r")) == NULL)
	printf("\nCAN'T FIND .TLB FILE %s\n",filename);
/* end of TLB stuff */
printf("Sentence file name? ");
while((senfile = fopen(gets(filename),"r"))==NULL)
	{
	printf("\nNot a good name, try again: ");
	}
fprintf(listfile,"\nSentence file %s",filename);
printf("Position file name? ");
while((posfile = fopen(gets(filename),"r"))==NULL)
	{
	printf("\nNot a good name, try again: ");
	}
fprintf(listfile,"\nPosition file %s",filename);
fcond = input("Lowest condition number to analyze? ");
lcond = input("Highest condition number to analyze? ");
fitem = input("Lowest item to analyze? ");
litem = input("Highest item to analyze? ");
fprintf(listfile,"\nLow condition %d High condition %d Low item %d High item %d",fcond,lcond,fitem,litem);
return(inter);
}



/************************************************/

/* FUNCTION TO READ ALL DATA FROM INPUT FILE */
/* AND PUT APPROPRIATE TRIALS INTO dtrials[CTRIAL] STRUCTURE ARRAY */

int get_input(int oldnew,int tracknumber)
{
int i;
int temp;
int *xptr, *yptr, *tlbsptr, *tlbeptr;
unsigned *sptr,*eptr;
int tlbseq,tlbcond,tlbitem;
printf("\nReading eyetracking data");
for(CTRIAL=0;fscanf(infile,"%d%d%d%d%d%d%d",&dtrials[CTRIAL].ord,&dtrials[CTRIAL].cond,&dtrials[CTRIAL].item,&dtrials[CTRIAL].rt,&dtrials[CTRIAL].resp,&dtrials[CTRIAL].rt2,&dtrials[CTRIAL].resp2)==7;)
	{
	if(tlbfile != NULL)
		{
		fscanf(tlbfile,"%d%d%d%d",&tlbseq,&tlbcond,&tlbitem,&dtrials[CTRIAL].tlbnumber);
/*printf("\nTLB file, seq %d item %d cond %d (%d tracklosses); ",tlbseq,tlbitem,tlbcond,dtrials[CTRIAL].tlbnumber);*/
		}
	if(oldnew)			/* new format */
		fscanf(infile,"%d%d",&dtrials[CTRIAL].primetime,&dtrials[CTRIAL].targettime);
	if((tracknumber == 1 && dtrials[CTRIAL].cond < 100) || (tracknumber == 0 && dtrials[CTRIAL].cond > 200))		/*not a question */
		{
		if(fgetc(infile) != ' ')		/* no real data this trial */
			dtrials[CTRIAL].obs = 0;
		else
			fscanf(infile,"%d",&dtrials[CTRIAL].obs);
/*printf("\nDATA file, SEQ %d ITEM %d COND %d TOTAL TIME %d (%d observations)\n",(dtrials[CTRIAL].ord)/2+1,dtrials[CTRIAL].item,dtrials[CTRIAL].cond,dtrials[CTRIAL].rt,dtrials[CTRIAL].obs);
getch();*/
		if(dtrials[CTRIAL].cond >= fcond && dtrials[CTRIAL].cond <= lcond && dtrials[CTRIAL].item >= fitem && dtrials[CTRIAL].item <= litem)
			{
			if(tlbfile != NULL)
				{
				while(tlbcond != dtrials[CTRIAL].cond || tlbitem != dtrials[CTRIAL].item)
					{
/*					printf("\nPROBLEM: tlbcond %d or item %d on trial %d mismatches data cond %d or item %d.\nPress any key to continue.",tlbcond,tlbitem,CTRIAL+1,dtrials[CTRIAL].cond,dtrials[CTRIAL].item);*/
					printf("\nSkipping tlbcond %d tlbitem %d",tlbcond,tlbitem);
					for(i=0;i<dtrials[CTRIAL].tlbnumber;i++)
						{
						fscanf(tlbfile,"%d%d",&temp,&temp);
						}
					fscanf(tlbfile,"%d%d%d%d",&tlbseq,&tlbcond,&tlbitem,&dtrials[CTRIAL].tlbnumber);
					}
				if((dtrials[CTRIAL].tlbs  = (tlbsptr = (int *)(calloc((dtrials[CTRIAL].tlbnumber)+1,sizeof(int))))) == NULL)
					{
					printf("\nOOPS; Can't calloc space for tlbsptr; reboot and try again.");
					exit(1);
					}
				if((dtrials[CTRIAL].tlbe  = (tlbeptr = (int *)(calloc((dtrials[CTRIAL].tlbnumber)+1,sizeof(int))))) == NULL)
					{
					printf("\nOOPS; Can't calloc space for tlbeptr; reboot and try again.");
					exit(1);
					}
				for(i=0;i<dtrials[CTRIAL].tlbnumber;i++)
					{
					fscanf(tlbfile,"%d%d",tlbsptr++,tlbeptr++);
					}
/*for(i=0;i<dtrials[CTRIAL].tlbnumber;i++)
	printf("\nCTRIAL %d tlb values: %d %d",CTRIAL,*(dtrials[CTRIAL].tlbs+i),*(dtrials[CTRIAL].tlbe+i));*/
				}
			else
				dtrials[CTRIAL].tlbnumber = 0;
/* end of tlb reading */

			if((dtrials[CTRIAL].x = (xptr = (int *)(calloc((dtrials[CTRIAL].obs)+1,sizeof(int))))) == NULL)
				{
				printf("\nOOPS; Can't calloc space for xptr; reboot and try again.");
				exit(1);
				}
			if((dtrials[CTRIAL].y = (yptr = (int *)(calloc((dtrials[CTRIAL].obs)+1,sizeof(int))))) == NULL)
				{
				printf("\nOOPS; Can't calloc space for yptr; reboot and try again.");
				exit(1);
				}
			if((dtrials[CTRIAL].s = (sptr = (unsigned *)(calloc((dtrials[CTRIAL].obs)+1,sizeof(unsigned))))) == NULL)
				{
				printf("\nOOPS; Can't calloc space for sptr; reboot and try again.");
				exit(1);
				}
			if((dtrials[CTRIAL].e = (eptr = (unsigned *)(calloc((dtrials[CTRIAL].obs)+1,sizeof(unsigned))))) == NULL)
				{
				printf("\nOOPS; Can't calloc space for eptr; reboot and try again.");
				exit(1);
				}
			for(i=0;i<dtrials[CTRIAL].obs;i++)
				{
				fscanf(infile,"%d%d%u%u",xptr++,yptr++,sptr++,eptr++);
				if(*(eptr-1) == 65432)
					*(eptr-1) = *(sptr-1);
				}
			CTRIAL++;
/*	printf("\nGOT A GOOD ITEM, trial %d, item %d, cond %d",CTRIAL,dtrials[CTRIAL-1].item,dtrials[CTRIAL-1].cond);*/
			}
		else
			{
			for(i=0;i<dtrials[CTRIAL].obs;i++)
				fscanf(infile,"%d%d%u%u",&temp,&temp,&temp,&temp);
/*	printf("\nGOT A ---BAD-- ITEM, trial %d, item %d, cond %d",CTRIAL,dtrials[CTRIAL-1].item,dtrials[CTRIAL-1].cond);*/
			if(tlbfile != NULL)
				{
/*printf("\nReading blink times on out of range trial.");*/
				for(i=0;i<dtrials[CTRIAL].tlbnumber;i++)
					fscanf(tlbfile,"%d%d",&temp,&temp);
				}
			}
		}
	else if(tlbfile != NULL)
		{
/*printf("\nReading blink times on non-scored trial.");*/
		for(i=0;i<dtrials[CTRIAL].tlbnumber;i++)
			fscanf(tlbfile,"%d%d",&temp,&temp);
		}

	}
fclose(infile);
printf("\n%d items read in",CTRIAL);
return(CTRIAL);
}




/* FUNCTION TO READ SENTENCES FOR THE APPROPRIATE */
/* COUNTERBALANCING CONDITION INTO MEMORY */
/* TOGETHER WITH INFORMATION ABOUT WORD POSITIONS */

void get_sentences(int ntrials,int *xoffset,int *yoffset,int inter)
{
int ncond,nitem,itrial;
int *posptrptr;
int temp;
char *ts;
int tempconds,tempitems,tempcondp,tempitemp,temppos,i;
ncond=lcond-fcond+1;
nitem=litem-fitem+1;

if(inter)
	{
	*xoffset = input("\r\nWhat is x offset value? ");
	*yoffset = input("\r\nWhat is y offset value? ");
	}
else
	{
	*xoffset = 0;
	*yoffset = 0;
	}
printf("\nReading sentence and position files.\n");
/* allocate space for pointer arrays */

if((wdptr = (char **)(calloc(ncond*nitem,sizeof(char *)))) == NULL)
	{
	printf("\nPROBLEMS WITH ALLOCATING SPACE FOR WDPTR");
	exit(1);
	}
if((posptr = (int **)(calloc(ncond*nitem,sizeof(int *)))) == NULL)
	{
	printf("\nPROBLEMS WITH ALLOCATING SPACE FOR POSPTR");
	exit(1);
	}

/* start to read sentences in */

for(itrial=0;itrial < ntrials;)
	{
	if((i=fscanf(senfile,"%d%d",&tempconds,&tempitems))==NULL || i == EOF)
		{
		printf("\nPROBLEMS READING SENFILE, TRIAL %d",itrial+1);
		exit(1);
		}
/*printf("\ntempconds %d tempitems %d",tempconds,tempitems);*/
	if(tempconds >= fcond && tempconds <= lcond && tempitems >= fitem && tempitems <= litem)
		{
		ts = tempsen;
		while(isspace(temp = fgetc(senfile)) && temp != 0x0a)		/* keep LF */
			;						/* skip leading spaces in sentence */
		*ts++ = temp;
		while((*ts = fgetc(senfile)) != '|' && *ts != EOF)
			if(*ts != 0x0d)		/* filter out CR; shouldn't be there anyway */
				ts++;
		if(*ts == EOF)
			{
			printf("\PROBLEMS READING SENTENCE IN SENFILE, TRIAL %d",itrial+1);
			exit(1);
			}
		*ts = '\0';					/* end the sentence */
		if((wdptr[(tempitems-fitem)*ncond + tempconds-fcond]=calloc(strlen(tempsen)+1,sizeof(char))) == NULL)
			{
			printf("\nPROBLEMS WITH ALLOCATING SPACE FOR WORDS.");
			exit(1);
			}
/* DEBUG */
/*printf("\n&wdptr[(tempitems-fitem)*ncond + tempconds-fcond] = %p",&wdptr[(tempitems-1)*ncond + tempconds]);
printf("\nwdptr[(tempitems-fitem)*ncond + tempconds-fcond] = %p",wdptr[(tempitems-1)*ncond + tempconds]);*/
		strcpy(wdptr[(tempitems-fitem)*ncond + tempconds-fcond],tempsen);
/*printf("\n %d %d\n%s\n%s",tempconds,tempitems,wdptr[(tempitems-fitem)*ncond + tempconds-fcond],tempsen);*/

/* read file of word positions */

		if((i=fscanf(posfile,"%d%d%d",&tempcondp,&tempitemp,&temppos))==NULL || i == EOF)
			{
			printf("\nPROBLEMS READING POSFILE, TRIAL %d",itrial+1);
			exit(1);
			}
/*printf("\n tempcondp %d tempitemp %d temppos %d",tempcondp,tempitemp,temppos);*/
		if(tempconds!=tempcondp || tempitems!=tempitemp)
			{
			printf("\nPROBLEMS: cond or item doesn't match:");
			printf("\ntempconds = %d, tempcondp = %d, tempitems = %d, tempitemp = %d",tempconds,tempcondp,tempitems,tempitemp);
			exit(1);
			}
/*printf("\nPosptr = %lp",posptr);*/
/*printf("\n&Posptr[(tempitems-fitem)*ncond + tempconds-fcond] = %lp",&posptr[(tempitems-fitem)*ncond + tempconds-fcond]);*/
		if((posptr[(tempitems-fitem)*ncond + tempconds-fcond]=(int *)(calloc(2*temppos+1,sizeof(int)))) == NULL)
			{
			printf("\nPROBLEMS WITH ALLOCATING SPACE FOR POSITIONS.");
			exit(1);
			}
		posptrptr = posptr[(tempitems-fitem)*ncond + tempconds-fcond];
/*printf("\nPosptrptr = %lp",posptrptr);
printf("\n&posptrptr[0] = %lp",&posptrptr[0]);*/
		posptrptr[0]=temppos;
		for(i	= 1; i <= temppos*2;)
			{
/*	printf("\nscanning posfile, i = %d",i);*/
			fscanf(posfile,"%d",&posptrptr[i]);
			posptrptr[i++] += *xoffset;
			fscanf(posfile,"%d",&posptrptr[i]);
			posptrptr[i++] += *yoffset;
/*	printf(" x %d y %d",posptrptr[i-2],posptrptr[i-1]);*/
			}
		fgets(tempsen,6000,posfile);			/* attempt to sync */
		itrial++;
/*		printf("\nCOMPLETED SENTENCE %d condition %d item %d",itrial,tempconds,tempitems);*/
		}
	else		/* skip a sentence */
		{
		while((temp = fgetc(senfile)) != '|' && temp != EOF)
			;
		if(temp == EOF)
			{
			printf("\nUnexpected end of file found in sentence, Item %d Cond %d",tempitems,tempconds);
			exit(1);
			}
		fgets(tempsen,6000,posfile);
			;
		}
	}
fclose(posfile);
fclose(senfile);
}

/* FUNCTION TO CORRECT DEVIANT Y VALUES */
/* PRELIMINARY DATA FILTER, OPTIONAL; WILL */
/* WRITE INTERMEDIATE *.&?? FILES */

int initial_fix(int ntrials,int scheme)
{
int iobs,lobs,i,j,x,y,s,e,tobs,which,txy,fwh,lwh;
int *xptr,*yptr,*tlbeptr,*tlbsptr;
int strial;
char temp[80];
int badfix,lastfixbad;
unsigned *sptr,*eptr;
which = 0;		/* initial fake */
if(scheme == 1)		/* initial fix for all */
	strial = 0;
else
	strial = ntrials-1;
number_rejected = number_truncated = number_combined = number_eliminated = 0;
	
for(CTRIAL=strial;CTRIAL<ntrials;CTRIAL++)
	{
	xptr = dtrials[CTRIAL].x;
	yptr = dtrials[CTRIAL].y;
	sptr = dtrials[CTRIAL].s;
	eptr = dtrials[CTRIAL].e;
	tlbsptr = dtrials[CTRIAL].tlbs;
	tlbeptr = dtrials[CTRIAL].tlbe;
	if(dtrials[CTRIAL].cond >= fcond && dtrials[CTRIAL].cond <= lcond && dtrials[CTRIAL].item >= fitem && dtrials[CTRIAL].item <= litem)
		{				/* legal trial; redundant */
		for(iobs=1,lobs=1;iobs < dtrials[CTRIAL].obs;iobs++)
			{
			if(((*(xptr+iobs)<(*(xptr+iobs-1)-20)&&*(yptr+iobs)==*(yptr+iobs-1)) || *(yptr+iobs) != *(yptr+iobs-1) || *(yptr+iobs) < 0 || *(xptr+iobs) <= 0 || which>0) || iobs == 1 || (iobs-lobs >= 10) && iobs < 999)
				{
				if(boundary_expt)
					{
					printf("\nPRIME TIME %d TARGET TIME %d DIFFERENCE %d",dtrials[CTRIAL].primetime,dtrials[CTRIAL].targettime,dtrials[CTRIAL].targettime-dtrials[CTRIAL].primetime);
					if((dtrials[CTRIAL].resp2)%2)			/* cross boundary in fixation */
						printf("\nWARNING: EYE SLUD ACROSS BOUNDARY DURING APPARENT FIXATION\n");
					}
				do
					{
					printf("\n\nCONDITION %d ITEM %d #FIXATIONS %d",dtrials[CTRIAL].cond,dtrials[CTRIAL].item,dtrials[CTRIAL].obs);
					printf("\n0....|....1....|....2....|....3....|....4....|....5....|....6....|....7.2");
					printf("\n%s",wdptr[((dtrials[CTRIAL].item)-fitem)*(lcond-fcond+1) + dtrials[CTRIAL].cond-fcond]);
					for(tobs=iobs-3,lastfixbad=0,lobs = iobs;tobs<=iobs+9;tobs++)
						{
						for(j=0,badfix=0;j<dtrials[CTRIAL].tlbnumber;j++)
							{
							if(tobs >= 0 && tobs < dtrials[CTRIAL].obs && ( (*(eptr+tobs) >= *(tlbsptr+j) && *(eptr+tobs) <= *(tlbeptr+j)) || (*(sptr+tobs) >= *(tlbsptr+j) && *(sptr+tobs) <= *(tlbeptr+j))))
								{
								if(lastfixbad == 0)
									cprintf("\r\n  TLB started at %d, ended at %d",*(tlbsptr+j),*(tlbeptr+j));
								badfix = 1; 
								}
							}
						if(badfix)
							{
							lastfixbad = 1;
							textcolor(BLACK);
							textbackground(WHITE);
							if(tobs >= 0 && tobs < dtrials[CTRIAL].obs)
								if(iobs == tobs)
									cprintf("\r\n*FIX %2d X=%3d Y=%3d START=%6u END=%6u DUR=%6u SACCADE DUR=%4u",tobs+1,*(xptr+tobs),*(yptr+tobs),*(sptr+tobs),*(eptr+tobs),*(eptr+tobs)-*(sptr+tobs),*(sptr+tobs)-*(eptr+tobs-1));
								else
									cprintf("\r\n FIX %2d X=%3d Y=%3d START=%6u END=%6u DUR=%6u SACCADE DUR=%4u",tobs+1,*(xptr+tobs),*(yptr+tobs),*(sptr+tobs),*(eptr+tobs),*(eptr+tobs)-*(sptr+tobs),*(sptr+tobs)-*(eptr+tobs-1));
							}
						else
							{
							lastfixbad = 0;
							normvideo();
							if(tobs >= 0 && tobs < dtrials[CTRIAL].obs)
								if(iobs == tobs)
									cprintf("\r\n*FIX %2d X=%3d Y=%3d START=%6u END=%6u DUR=%6u SACCADE DUR=%4u",tobs+1,*(xptr+tobs),*(yptr+tobs),*(sptr+tobs),*(eptr+tobs),*(eptr+tobs)-*(sptr+tobs),*(sptr+tobs)-*(eptr+tobs-1));
								else
									cprintf("\r\n FIX %2d X=%3d Y=%3d START=%6u END=%6u DUR=%6u SACCADE DUR=%4u",tobs+1,*(xptr+tobs),*(yptr+tobs),*(sptr+tobs),*(eptr+tobs),*(eptr+tobs)-*(sptr+tobs),*(sptr+tobs)-*(eptr+tobs-1));
							}
						}
					normvideo();
					while((which = input("\r\nCHANGE FIX #; ENTER IF NONE, -1 TO CHANGE BLOCK, -2 TO CHANGE X, 999 TO END ITEM ")) < -2)
							printf("\nNot possible, dummy! Try to do it better.\n");
					if(which == -1)			/* change whole block */
						{
						fwh = input("First fixation to change? ");
						lwh = input("Last fixation to change? ");
						txy = input("New Y value for these items? ('999' to cancel) ");
						if(txy != 999)
							{
							for(which = fwh; which <= lwh; which++)
								*(yptr+which-1) = txy;
							}
						}
					else if(which == -2)
						{
						which = input("Change X value for fixation number: ");
						cprintf("New X value (ENTER to keep)? ");
						gets(temp);
						if(strlen(temp) > 0)
							{
							txy = atoi(temp);
							*(xptr+which-1) = txy;
							}
						cprintf("\r\n");
						}
					else if(which != 0 && which != 999)
						{
						cprintf("New Y value (ENTER to keep)? ");
						gets(temp);
						if(strlen(temp) > 0)
							{
							txy = atoi(temp);
							*(yptr+which-1) = txy;
							}
						cprintf("\r\n");
						}
					} while (which >= iobs && which != 999 && which != -1);
				if(which != 0 && which != 999 && which != -1)
					iobs = which;		/* allow change */
				else if (which == 999)		/* terminator */
					{
					iobs = input("Which is the last good fixation? Type 0 to eliminate item. ");
					dtrials[CTRIAL].obs = iobs;
					if(iobs == 0)
						{
/*						dtrials[CTRIAL].cond = 999;*/		/* leave cond number the same for IXS */
						number_rejected++;
						}
					else
						number_truncated++;
					}
				else
					iobs++;
				}
			}
		}
	if(CTRIAL < ntrials-1)
		{
		printf("\nGo to next item? (item %d)? y or n (ENTER for 'y'):  ",CTRIAL+2);
		gets(temp);
		}
	else
		{
		printf("\nFinal item; are you done? y or n: ");
		do
			{
			gets(temp);
			if(tolower(temp[0]) != 'n' && tolower(temp[0]) != 'y')
				printf("\n\bSORRY: please type in either y or n: ");
			}
		while(tolower(temp[0]) != 'n'&& tolower(temp[0]) != 'y');
		}
	if(tolower(temp[0]) == 'n')		/* redo a trial? */
		{
		txy = (input("\r\nGo to item # ? (999 to end session and save intermediate file): ") - 2);	/* increments in loop */
		if(txy < ntrials-1 && txy >= -1)		/* don't get outside range */
			CTRIAL = txy;
		else if(txy == 997)			/* 999 - 2 */
			{
			ntrials = CTRIAL+1;	/* set up to terminate */
			printf("\nTerminating and saving partial intermediate and output files.");
			}
		else
			{
			printf("\n\nWARNING: TRIAL OUTSIDE RANGE, GOING BACK TO TRIAL 1\nPress any key to continue.\n");
			getch();
			CTRIAL = strial-1;		/* default, go back to trial 1 */
			}
		if(CTRIAL < strial-1)
			CTRIAL = strial-1;
		}
	}
for(CTRIAL=strial;CTRIAL<ntrials;CTRIAL++)
	{
	if(CTRIAL != 0)
		fprintf(interfile,"\n");
	xptr = dtrials[CTRIAL].x;
	yptr = dtrials[CTRIAL].y;
	sptr = dtrials[CTRIAL].s;
	eptr = dtrials[CTRIAL].e;

	fprintf(interfile,"%d %d %d %d %d %d %d ",dtrials[CTRIAL].ord,dtrials[CTRIAL].cond,dtrials[CTRIAL].item,dtrials[CTRIAL].rt,dtrials[CTRIAL].resp,dtrials[CTRIAL].rt2,dtrials[CTRIAL].resp2);
	fprintf(interfile,"%d ",dtrials[CTRIAL].obs);
	for(iobs=0;iobs<dtrials[CTRIAL].obs;iobs++)
			fprintf(interfile,"%u %u %u %u ",*(xptr+iobs),*(yptr+iobs),*(sptr+iobs),*(eptr+iobs));

/*	printf("%d %d %d %d %d %d %d ",dtrials[CTRIAL].ord,dtrials[CTRIAL].cond,dtrials[CTRIAL].item,dtrials[CTRIAL].rt,dtrials[CTRIAL].resp,dtrials[CTRIAL].rt2,dtrials[CTRIAL].resp2);
	printf("%d ",dtrials[CTRIAL].obs);
	for(iobs=0;iobs<dtrials[CTRIAL].obs;iobs++)
			printf("%u %u %u %u ",*(xptr+iobs),*(yptr+iobs),*(sptr+iobs),*(eptr+iobs));	*/
	}
return(ntrials);
}

/* FUNCTION FOR DISPLAYING WORDS AND ALL FIXATIONS */
/* AND MAKING FINAL CORRECTIONS ON OUTPUT FILE */

void final_fix(int ntrials,int scheme,int mt1,int md1,int mt2,int md2,int xoffset,int yoffset)
{
char temp[80];
char change[10];
int which,io,iobs;
int ncond,temppos,ipos;
int cx,cy,nx;
int wdcount,ifix,nlet;
int *xptr,*yptr;
unsigned *sptr,*eptr;
char *temps;
int tempcond,tempitem;
int *posptrptr;
int wordstart[400];
char wt;
char *wp, *letter;
int nwords, pad, is,iw;
int strial;
int autocount;
int manual;
int tpos;

wordstart[0]=0;
ncond=lcond-fcond+1;
if(scheme == 1)
	strial = 0;
else
	strial = ntrials-1;
printf("\nDo you want to correct times manually? y or n (ENTER = n): ");
gets(temp);
if(tolower(temp[0]) == 'y')
	{
	printf("\nAre you REALLY sure you want to correct them manually? y or n: ");
	gets(temp);
	if(tolower(temp[0]) == 'y')
		manual = 1;
	else
		manual = 0;
	}	
else
	manual = 0;		/* 0, only automatic changes */
for(CTRIAL=strial;CTRIAL<ntrials;CTRIAL++)
	{
	if(CTRIAL != 0)
		fprintf(outfile,"\n");
	if((tempcond=dtrials[CTRIAL].cond) >= fcond && tempcond <= lcond && (tempitem=dtrials[CTRIAL].item) >= fitem && tempitem <= litem)
		{				/* legal trial */
/*printf("\n&wdptr[(tempitem-fitem)*ncond + tempcond-fcond] = %p",&wdptr[(tempitem-fitem)*ncond + tempcond-fcond]);*/
/*printf("\nwdptr[(tempitem-fitem)*ncond + tempcond-fcond] = %p",wdptr[(tempitem-fitem)*ncond + tempcond-fcond]);*/
/*		printf("\nCOND %d ITEM %d\n%s",dtrials[CTRIAL].cond,dtrials[CTRIAL].item,wdptr[(tempitem-fitem)*ncond + tempcond-fcond]);*/
		temps = wdptr[(tempitem-fitem)*ncond + tempcond-fcond];	/* point to current sen */
		posptrptr = posptr[(tempitem-fitem)*ncond + tempcond-fcond];
		temppos=posptrptr[0];		/* number of fixations in sentence */
		xptr = dtrials[CTRIAL].x;
		yptr = dtrials[CTRIAL].y;
		sptr = dtrials[CTRIAL].s;
		eptr = dtrials[CTRIAL].e;

		wordstart[0]=-1;
		for(is=0,iw=1;(wt = *temps) != '\0'; is++,temps++)
			if(isspace(wt))				/* find starts of words */
				{
				wordstart[iw++] = is;
				}
		temps = wdptr[(tempitem-fitem)*ncond + tempcond-fcond];	/* point to current sen */
		nwords = iw-1;					/* how many words in sentence */

		if(manual)
			{
			printf("\nInitial printer output, before corrections? y or n (y default): ");
			gets(change);
			}
		if(tolower(change[0] != 'n')||!manual)	/* automatically writes if non-manual*/
			{
			fprintf(listfile,"\n\nBEFORE CORRECTIONS:");
			fprintf(listfile,"\n\n trial %d item %d condition %d",CTRIAL+1,tempitem,tempcond);
			if(boundary_expt)
				{
				fprintf(listfile,"\n primetime %d targettime %d difference %d",dtrials[CTRIAL].primetime,dtrials[CTRIAL].targettime,dtrials[CTRIAL].targettime-dtrials[CTRIAL].primetime);
				fprintf(listfile,"\n BOUNDARY CHARACTER POSITION %d",dtrials[CTRIAL].rt2);
				for(tpos=0;tpos<dtrials[CTRIAL].obs;tpos++)
					{
					if(*(sptr + tpos) >= dtrials[CTRIAL].primetime)	/* past point of change */
						{
						fprintf(listfile,"\n PRIMETIME = %4d AFTER FIX %3d X = %3d     START = %5d     END = %5d",dtrials[CTRIAL].primetime,tpos,*(xptr+tpos-1),*(sptr+tpos-1),*(eptr+tpos-1));
						fprintf(listfile,"\n                BEFORE FIX %3d X = %3d     START = %5d     END = %5d",tpos+1,*(xptr+tpos),*(sptr+tpos),*(eptr+tpos));
						tpos = dtrials[CTRIAL].obs;
						}
					}
				for(tpos=0;tpos<dtrials[CTRIAL].obs;tpos++)
					{
					if(*(sptr + tpos) >= dtrials[CTRIAL].targettime)	/* past point of change */
						{
						fprintf(listfile,"\n TARGETTIME = %4d AFTER FIX %3d X = %3d     START = %5d     END = %5d",dtrials[CTRIAL].targettime,tpos,*(xptr+tpos-1),*(sptr+tpos-1),*(eptr+tpos-1));
						fprintf(listfile,"\n                 BEFORE FIX %3d X = %3d     START = %5d     END = %5d",tpos+1,*(xptr+tpos),*(sptr+tpos),*(eptr+tpos));
						tpos = dtrials[CTRIAL].obs;
						}
					}
				if((dtrials[CTRIAL].resp2)%2)
					fprintf(listfile,"\n******EYE CROSSED BOUNDARY DURING FIXATION*******");
				}
			fprintf(listfile,"\n            fix let  dur   fix let  dur   fix let  dur");
			for(wdcount=0; wdcount<nwords;wdcount++)
				{
				fprintf(listfile,"\n");
				letter = temps+wordstart[wdcount];	/* start of word */
				for(pad=0;!isspace(*(++letter));)
					if(pad < 10)
						{
						fputc(*(letter),listfile);
						pad++;
						}
				for(;pad < 11;pad++)
					fputc(' ',listfile);
				cx = posptrptr[wdcount*2+1];
				cy = posptrptr[wdcount*2+2];		/* x and y for this word */
				if(wdcount < temppos-1)				/* (space before word) */
					{
					nx = posptrptr[wdcount*2+3];	/* and for next word */
					if(cy < posptrptr[wdcount*2+4])	/* words ends next line */
						{
						cx = -1 + xoffset;							/* start of word = start of line */
						cy++;								/* next line */
						}
					}
				else
					{
					nx = cx+20;
					}
				for(ifix=0;ifix<dtrials[CTRIAL].obs;ifix++)
					{
					if(*(xptr+ifix) >= cx && *(xptr+ifix) < nx && *(yptr+ifix) == cy)
						{
						nlet = *(xptr+ifix) - cx;
						fprintf(listfile,"%4d%4d%5u  ",ifix+1,nlet,*(eptr+ifix)-*(sptr+ifix));
						}
						}
					}
			}
		if(*(eptr+(dtrials[CTRIAL].obs)-1) == 0)	/* long last fix */
			{
			dtrials[CTRIAL].obs--;		/* scratch last fixation */
			fprintf(listfile,"\nTruncate at original fixation %d",dtrials[CTRIAL].obs);
			}
		for(ifix=0,autocount=0;ifix<dtrials[CTRIAL].obs-1;ifix++)	/* auto screen */
			{
			if(*(eptr+ifix)-*(sptr+ifix) > 9999 || *(eptr+ifix)-*(sptr+ifix) <= 0)		/* long or 0 fixation */
				{
				for(io=ifix;io < dtrials[CTRIAL].obs;io++)
					{
					*(xptr+io) = *(xptr+io+1);
					*(yptr+io) = *(yptr+io+1);
					*(sptr+io) = *(sptr+io+1);
					*(eptr+io) = *(eptr+io+1);
					}
				dtrials[CTRIAL].obs--;
				autocount++;
				fprintf(listfile,"\noriginal long or zero fixation %d deleted",ifix+autocount);
				ifix--;
				number_eliminated++;
				}
			else if(*(xptr+ifix) == *(xptr+ifix+1) && *(yptr+ifix) == *(yptr+ifix+1))
				{				/* two successive fixations on same spot */
				*(sptr+ifix+1) = *(sptr+ifix);	/* assimlate to next */
				for(io=ifix;io < dtrials[CTRIAL].obs;io++)
					{
					*(xptr+io) = *(xptr+io+1);
					*(yptr+io) = *(yptr+io+1);
					*(sptr+io) = *(sptr+io+1);
					*(eptr+io) = *(eptr+io+1);
					}
				dtrials[CTRIAL].obs--;
				autocount++;
				fprintf(listfile,"\nTwo identical fixations: original fixation %d assimilated to next",ifix+autocount);
				ifix--;		/* check orig against next too */
				number_combined++;
				}
			else if(*(eptr+ifix)-*(sptr+ifix) <= mt1)
				{
				if(abs(*(xptr+ifix)-*(xptr+ifix+1)) <= md1 && *(yptr+ifix) == *(yptr+ifix+1))
					{		/* assimilate to next */
					*(sptr+ifix+1) = *(sptr+ifix);
					for(io=ifix;io < dtrials[CTRIAL].obs;io++)
						{
						*(xptr+io) = *(xptr+io+1);
						*(yptr+io) = *(yptr+io+1);
						*(sptr+io) = *(sptr+io+1);
						*(eptr+io) = *(eptr+io+1);
						}
					dtrials[CTRIAL].obs--;
					autocount++;
					fprintf(listfile,"\noriginal fixation %d assimilated to next",ifix+autocount);
					ifix--;
					number_combined++;
					}
				else if(abs(*(xptr+ifix)-*(xptr+ifix-1)) <= md1 && ifix > 0 && *(yptr+ifix)==*(yptr+ifix-1))
					{			/* assimilate to previous */
					*(eptr+ifix-1) = *(eptr+ifix);
					for(io=ifix;io < dtrials[CTRIAL].obs;io++)
						{
						*(xptr+io) = *(xptr+io+1);
						*(yptr+io) = *(yptr+io+1);
						*(sptr+io) = *(sptr+io+1);
						*(eptr+io) = *(eptr+io+1);
						}
					dtrials[CTRIAL].obs--;
					autocount++;
					fprintf(listfile,"\noriginal fixation %d assimilated to previous",ifix+autocount);
					ifix--;
					number_combined++;
					}
				else if(*(eptr+ifix)-*(sptr+ifix) <= mt2)
					{
					if((abs(*(xptr+ifix)-*(xptr+ifix+1))<=md2 && *(yptr+ifix)==*(yptr+ifix+1)) || (abs(*(xptr+ifix)-*(xptr+ifix-1))<=md2 && *(yptr+ifix)==*(yptr+ifix-1)))		
						{
						for(io=ifix;io < dtrials[CTRIAL].obs;io++)
							{
							*(xptr+io) = *(xptr+io+1);
							*(yptr+io) = *(yptr+io+1);
							*(sptr+io) = *(sptr+io+1);
							*(eptr+io) = *(eptr+io+1);
							}
						dtrials[CTRIAL].obs--;
						autocount++;
						fprintf(listfile,"\noriginal fixation %d deleted",ifix+autocount);
						ifix--;
						number_eliminated++;
						}
					}
				}
			}
		if(dtrials[CTRIAL].obs != 0 && (*(eptr+ifix)-*(sptr+ifix) > 9999 || *(eptr+ifix)-*(sptr+ifix) <= 0))
			{
			dtrials[CTRIAL].obs--;		/* throw out weird last trial */
			autocount++;
			fprintf(listfile,"\noriginal fixation %d (last) deleted; too long or 0",ifix+autocount);
			number_eliminated++;
			}
		printf("\n%d replacements made automatically",autocount);
		if(manual)
		for(ipos=0;ipos < nwords; ipos+=16)	/* 16 word blocks */
			{
			do
				{
				printf("\n\n            FIX LET  DUR   FIX LET  DUR   FIX LET  DUR");
				for(wdcount=ipos;wdcount<ipos+20 && wdcount<nwords;wdcount++)
					{											/* show  20 words at once */
					printf("\n");
					letter = temps+wordstart[wdcount];	/* start of word */
					for(pad=0;!isspace(*(++letter));)
						if(pad < 10)
							{
							putchar(*(letter));
							pad++;
							}
					if((*letter) == CR || (*letter) == LF)
						{
						putchar('|');
						pad++;
						}
					for(;pad < 11;pad++)
						putchar(' ');
					cx = posptrptr[wdcount*2+1];
					cy = posptrptr[wdcount*2+2];		/* x and y for this word */
					if(wdcount < temppos-1)				/* (space before word) */
						{
						nx = posptrptr[wdcount*2+3];	/* and for next word */
						if(cy < posptrptr[wdcount*2+4])	/* words ends next line */
							{
							cx = -1 + xoffset;							/* start of word = start of line */
							cy++;								/* next line */
							}
						}
					else
						{
						nx = cx+20;
						}
					for(ifix=0;ifix<dtrials[CTRIAL].obs;ifix++)
						{
/*printf("\ncx = %d cy = %d nx = %d *(xptr+ifix) = %d *(yptr+ifix) = %d ifix = %d\n",cx,cy,nx,*(xptr+ifix),*(yptr+ifix),ifix);*/
						if(*(xptr+ifix) >= cx && *(xptr+ifix) < nx && *(yptr+ifix) == cy)
							{
							nlet = *(xptr+ifix) - cx;
							printf("%4d%4d%5u  ",ifix+1,nlet,*(eptr+ifix)-*(sptr+ifix));
							}
						}
					}
				which = input("\r\nCHANGE WHICH FIX\nENTER IF NONE; -1 FOR PREVIOUS FRAME; 999 TO DISCARD TRIAL): ")-1;
				if(which > 900)
					which = -3;			/* throw trial out */
				if(which >= 0)
					{
					printf("\nHow change?\n (D)elete, assimilate to (P)rev, assimilate to (N)ext, 'ENTER' to cancel? ");
					gets(change);
					switch(tolower(change[0]))
						{
						case 'd':	/* delete fixation */
							for(io=which;io < dtrials[CTRIAL].obs;io++)
								{
								*(xptr+io) = *(xptr+io+1);
								*(yptr+io) = *(yptr+io+1);
								*(sptr+io) = *(sptr+io+1);
								*(eptr+io) = *(eptr+io+1);
								}
/*							posptrptr[0]--;*/
							dtrials[CTRIAL].obs--;
							number_eliminated++;
							break;
						case 'p':	/* asssimilate to previous fixation */
							*(eptr+which-1) = *(eptr+which);
							for(io=which;io < dtrials[CTRIAL].obs;io++)
								{
								*(xptr+io) = *(xptr+io+1);
								*(yptr+io) = *(yptr+io+1);
								*(sptr+io) = *(sptr+io+1);
								*(eptr+io) = *(eptr+io+1);
								}
/*							posptrptr[0]--;*/
							dtrials[CTRIAL].obs--;
							number_combined++;
							break;
						case 'n':	/* assimilate to next fixation */
							*(sptr+which+1) = *(sptr+which);
							for(io=which;io < dtrials[CTRIAL].obs;io++)
								{
								*(xptr+io) = *(xptr+io+1);
								*(yptr+io) = *(yptr+io+1);
								*(sptr+io) = *(sptr+io+1);
								*(eptr+io) = *(eptr+io+1);
								}
/*							posptrptr[0]--;*/
							dtrials[CTRIAL].obs--;
							number_combined++;
							break;
						default:
							break;
						}
					}
				} while (which >= 0);
			if (which == -2)			/* back up */
				ipos -= 32;
			if(ipos < -16)
				ipos = -16;
			if(which == -3)				/* scrap trial */
				{
				fprintf(listfile,"\n***TRIAL REJECTED***\n");
				ipos = nwords;
				dtrials[CTRIAL].cond += 1000;
				number_rejected++;
				}
			}
		}
	if(boundary_expt && scrap_at_boundary && dtrials[CTRIAL].rt2 != 0)
		for(iobs=0;iobs<dtrials[CTRIAL].obs;iobs++)
			{
			if(*(xptr+iobs) == dtrials[CTRIAL].rt2)	/* fix right after bound */
				{
				dtrials[CTRIAL].resp2 += 2;
				fprintf(listfile,"\n*** THROWING OUT TRIAL BECAUSE OF FIXATION %d ***",iobs+1);
				break;
				}
			}
	fprintf(outfile,"%d %d %d %d %d %d %d ",dtrials[CTRIAL].ord,dtrials[CTRIAL].cond,dtrials[CTRIAL].item,dtrials[CTRIAL].rt,dtrials[CTRIAL].resp,dtrials[CTRIAL].rt2,dtrials[CTRIAL].resp2);
	fprintf(outfile,"%d ",dtrials[CTRIAL].obs);
	for(iobs=0;iobs<dtrials[CTRIAL].obs;iobs++)
			fprintf(outfile,"%d %d %u %u ",(*(xptr+iobs))-xoffset,(*(yptr+iobs))-yoffset,*(sptr+iobs),*(eptr+iobs));

if(manual)
	{
	printf("\nFinal printer output? y or n (y default): ");
	gets(change);
	}
if(tolower(change[0] != 'n') || !manual)
	{
	fprintf(listfile,"\n\nAFTER CORRECTIONS:");
	fprintf(listfile,"\n\n TRIAL %d ITEM %d CONDITION %d",CTRIAL+1,tempitem,tempcond);
	fprintf(listfile,"\n            FIX LET  DUR   FIX LET  DUR   FIX LET  DUR");
		for(wdcount=0; wdcount<nwords;wdcount++)
		{
		fprintf(listfile,"\n");
		letter = temps+wordstart[wdcount];	/* start of word */
		for(pad=0;!isspace(*(++letter));)
			if(pad < 10)
				{
				fputc(*(letter),listfile);
				pad++;
				}
				for(;pad < 11;pad++)
					fputc(' ',listfile);
				cx = posptrptr[wdcount*2+1];
				cy = posptrptr[wdcount*2+2];		/* x and y for this word */
				if(wdcount < temppos-1)				/* (space before word) */
					{
					nx = posptrptr[wdcount*2+3];	/* and for next word */
					if(cy < posptrptr[wdcount*2+4])	/* words ends next line */
						{
						cx = -1 + xoffset;							/* start of word = start of line */
						cy++;								/* next line */
						}
					}
				else
					{
					nx = cx+20;
					}
				for(ifix=0;ifix<dtrials[CTRIAL].obs;ifix++)
					{
					if(*(xptr+ifix) >= cx && *(xptr+ifix) < nx && *(yptr+ifix) == cy)
						{
						nlet = *(xptr+ifix) - cx;
						fprintf(listfile,"%4d%4d%5u  ",ifix+1,nlet,*(eptr+ifix)-*(sptr+ifix));
						}
						}
					}
			}
	}
}

/***********************************************************************/


int input(char *output)
{
char buffer[80];
cprintf("%s",output);
buffer[0]='\0';
return(atoi(gets(buffer)));
}




/*********************************************************************************/
