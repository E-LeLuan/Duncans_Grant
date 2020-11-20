 /* EYELINK PROGRAM -- ASC2DAT -- USE TO CONVERT EYELINK ASCII DATA */
/* TO PCEXPT .DAT FORMAT */

/* revised for eyelink 2 - called asc2dat2.c
initial version, specialized for Courier New fixed pitch font
requires user to type in Y offset, height of line, and character
width in pixels */

/* note: assumes that TRIALID is ALWAYS of the form CxxIyy..., where
xx is the condition number and yy is the item numeber!!! 
(needed to allow use of eyewash, eyedry; other nonnumeric characters 
can follow yy if you want */


/* currently provides horizontal defaults, initially set to 30 pixel
character height which gives 18 pixel character width.
Assumes left margin set at screenwidth/20 = 80 pixels of 
horizontal resolution of 1600.
Thus, character 1 on a row goes from 80 to 97, character
2 from 98 to 117, etc. (assigning the pixel after a character to the
next character */ 


/* Currently provides vertical defaults;
assumes top margin at screenheight/20 = 60 for
vertical resolution of 1200, actual character height of 33
pixels, 36 pixels above and below -for 105 pixel line spacing which
and 0 text_padding_size */

/* note, top margin set in do_text_trials() or w32_text_trials() */

/* example, indicating where lines are divided for analysis:

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

/* for characters on a given row, the program assigns fixations
between characters top-18 and characters-bottom + 18 to the
characters on that row */


/*copyright 1990, 2003 Charles Clifton. Users of this program are permitted
to modify and distribute it at no charge but may not sell it or
include any of its code in a commercial program.*/

char VERSION[] = "7/7/2003";

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


FILE *infile,*outfile;
#define LF 0x0a
#define CR 0x0d
void main(void);
void get_files(void);
int input(char *output);

/* DECLARACTIONS OF FUNCTIONS */

int FindDisplayCoordinates(FILE *infile,int* tlx,int* tly, int* brx, int* bry);
void GetNextTrial(FILE* infile);
long GetTrialTime(FILE* infile);
int GetFixations(FILE* infile,long start_time);
int PartStringCompare(char *dc,char *dv);
int GetNextNumber(char* dc,int *i);
int GetCond(char* dc,int *i);
int GetItem(char* dc,int *i);
long GetNextLongNumber(char* dc,int *i);
float GetNextFloat(char* dc,int *i);
void WriteData(int ntrials);
void get_sizes(void);


/* MAIN PROGRAM */


void main(void)
{
char tempstr[400];
int i,sub,cond,item,resp
get_files();																/* open input and output files */
rewind(infile);

/* old stuff starts here */

FindDisplayCoordinates(infile,&tlx,&tly,&brx,&bry);	/* find coordinates */
/* this information is not currently used, but could be, to make
determination of character width etc. automatic */

/*char_width = (brx - tlx)/CharactersAcross;
char_height = (bry - tly)/LinesDown;	*/ 	/* transform to characters */

/* old stuff ends here */

get_sizes();		/* get character size, offsets */

for(CTRIAL = 0;CTRIAL < ntrials;CTRIAL++)
	{
	GetNextTrial(infile);
	starttime = GetTrialTime(infile);
	GetFixations(infile,starttime);
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
	if(PartStringCompare("START",&(tl[i])))
		trials++;
	}
return(trials);
}

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
		if(PartStringCompare("DISPLAY_COORDS",&(tl[i])))
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
	return i;
}


/************************************************/

void GetNextTrial(FILE *infile)
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
		if(PartStringCompare("TRIALID",&(tl[i])))
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
		if(PartStringCompare("DISPLAY ON",&(tl[i])))
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

int GetFixations(FILE* infile,long start_time)
{
char tl[160];
char c;
char* endsignal;
int i,j,k, temp;
float fx, fy;
int nfix = 0;
unsigned *s, *e;						/* pointers to stored times and positions */
int *x, *y;
unsigned stime[MAXFIX],etime[MAXFIX];
int ax[MAXFIX],ay[MAXFIX];
while((endsignal = fgets(tl,160,infile)) != NULL)
	{
	if(tl[0] == 'E' && tl[1] == 'F' && tl[2] == 'I' && tl[3] == 'X')
		{
		i = 3;
		stime[nfix] = (unsigned)(GetNextLongNumber(&(tl[i]),&i)-start_time);
		etime[nfix] = (unsigned)(GetNextLongNumber(&(tl[i]),&i)-start_time);
		GetNextNumber(&(tl[i]),&i);
		fx = GetNextFloat(&(tl[i]),&i);
		fy = GetNextFloat(&(tl[i]),&i);
		ax[nfix] = (int)((fx-horiz_offset)/char_width);
		ay[nfix] = (int)((fy-(vert_offset-space_between+1))/line_spacing);			/* convert pixels to chars */
		if(ay[nfix] < 0)
			ay[nfix] = 0;
		nfix++;
		}
	else if(tl[0] == 'M' && tl[1] == 'S' && tl[2] == 'G')		/* end of trial */
		{
		dtrials[CTRIAL].obs = nfix;
		i = 2;
		GetNextLongNumber(&(tl[i]),&i);
		if(PartStringCompare("ENDBUTTON",&(tl[i])))						/* got the end */
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
	map[dtrials[i].item] = i;
for(i=0;i<MAXTRIALS;i++)
	if(map[i] == -999)
		for(j=i+1;j<MAXTRIALS;j++)
			map[j-1] = map[j];		/* make compact, put all items together */
for(i=0;i<ntrials;i++)
	{
	CTRIAL = map[i];
	fprintf(outfile,"%d %d %d %d %d %d %d ",dtrials[CTRIAL].ord,dtrials[CTRIAL].cond,dtrials[CTRIAL].item,dtrials[CTRIAL].rt,dtrials[CTRIAL].resp,dtrials[CTRIAL].rt2,dtrials[CTRIAL].resp2);
	fprintf(outfile,"%d ",dtrials[CTRIAL].obs);
	if(dtrials[CTRIAL].obs != 0)							/* eyetracking data */
		{
		fprintf(outfile,"0 0      ");										/* extra 1 and 2 */
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
fclose(infile);
fclose(outfile);
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
}


/***********************************************************************/


int input(char *output)
{
char buffer[MAXLINE];
cprintf("%s",output);
buffer[0]='\0';
return(atoi(gets(buffer)));
}


/*************************************************************/

void get_sizes()
{
int tempvalue;
printf("\nTo accept defaults, just press ENTER or 0 followed by ENTER");
if ((tempvalue = input("\nHow wide is each character (pixels)? ")) == 0)
	char_width = 18;			/* 18 pixels */
else
	char_width = tempvalue;
if ((tempvalue = input("\nHow high is each line (pixels)? ")) == 0)
	char_height = 33;			/* 33 pixels */
else
	char_width = tempvalue;
if ((tempvalue = input("\nWhat is the horizontal offset (pixels?) ")) == 0)
	horiz_offset = 80;			/* 80 pixels */
else
	horiz_offset = tempvalue;
if ((tempvalue = input("\nWhat is the vertical offset (pixels?) ")) == 0)
	vert_offset = 60;			/* 60 pixels */
else
	vert_offset = tempvalue;
if ((tempvalue = input("\nWhat is the vertical line spacing (pixels?) ")) == 0)
	line_spacing = 105;			/* 105 pixels */
else 
	line_spacing = tempvalue;
printf("\nSELECTED VALUES:\ncharacter width %d horizontal offset%d\ncharacter height %d vertical offset %d\nvertical line spacing %d\n",char_width,horiz_offset,char_height,vert_offset,line_spacing);
space_between = vert_offset + (line_spacing-char_height)/2; 
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
