char version[] = "Graph preparation program VERSION OF 10/20/04";
/* what it does:
lets you select an item from your eye movement file (the .vec file that
is created by analysis 19 of eyedry - time to enter and leave each word);
lets you open up a file of one utterance;
produces an output file with two columns of start and end fixation times
with word numbers, with . for nonfixation, 
and two columns of start and end pronunciation times with word numbers,
with . for pauses.
Also gives file of eye-voice span defined as:
- for each pronounced word, how long has it been since it was fixated;
- for each pronounced word, at the beginning of pronunciation, what word
was being fixated (get next word if in saccade)
- for each fixated word, what word had begun to be pronounced when fixation
began;

data output format:

.gra file
word fixated		start fix time		word pronounced		start pron time
(. if skipped)	(. if skipped)		(. if pause)			(. if pause)
word fixated		end fix time			word pronounced		end pron time
repeat

e.g.
. . . .
. . 1 904
3 241 1 984
3 797 2 984
4 825 2 1354
4 1181 3 1354
5 1201 3 1684
5 1405 4 1684
6 1429 4 2083
6 1805 5 2083
. . 5 2484
8 1829 6 2484
8 2365 6 2634
9 2385 7 2634
9 2557 7 2724
10 2581 8 2724
10 2913 8 3164
11 2941 9 3164
11 3217 9 3484
12 3241 10 3484
12 3581 10 3823
. . 11 3823
14 3605 11 3954
14 3985 12 3954
. . 12 4143
. . 13 4143
17 4315 13 4373
17 4587 14 4373
. . 14 4594
. . 15 4594
. . . .


.lag file
subj number, item number, condition number
word # 		pron time-fix time		Number of word being				current word # - number
					(starting times)			fixated - current word #		of word being pronounced
					(. if not fixated or	(. if no word fixated or		(. if word not fixated or
					not pronounced)				no pronunciation recorded)	no word being pronounced)

*/

#include "stdio.h"
#include "stdlib.h"
#include "math.h"
#include "alloc.h"
#include "dos.h"
#include "string.h"
#include "ctype.h"
#include "conio.h"

#define EQ ==
#define MAXLINE 250
#define MAXLINEX 4000
#define TRUE 1
#define FALSE 0
#define ERR NULL                        /* from CPM86 */
char pbuff[MAXLINEX],ebuff[MAXLINEX];
FILE *pronfile,*eyefile, *graphfile, *lagfile;
FILE *dbuff;
char file[MAXLINE];
char *tbuff;
char buff[MAXLINE];
int debug;

/* declarations of function prototypes */
void movenextnumber(void);
void movebacknumber(void);
int readnext(int control);
int spacebuff(char *bptr,int val);
int round(int total,int count);
int estart,eend;
char *tgets(char *buff,FILE *stream);
void openfail(char *filestr);
int GetNextNumber(char* dc,int *index);
float GetNextFloat(char *dc, int *index);

void main(int argc, char *argv[]);
/******* START OF MAIN PROGRAM HERE ***********/

void main(int argc,char *argv[])
{
int changes,newvals;
int sample,temp;
int utt,idx;
int pauseflag;
float start,pause,initial;
int x,k,j,i;
int subj;
int item,founditem;
int startfix,startpron;
int word,pline,eline,nwords;
int graph[4][100];
char string[100];
char c;
struct date today;
int StartPrevFix,EndPrevFix;
char *buffptr;

getdate(&today);

if(argc < 2)
	{
	printf("\nWhat subject number is this? ");
	gets(file);
	subj = atoi(file);
	}
else
	subj = atoi(argv[1]);

if(argc < 3)
	{
	printf("\nWhat is the name of your eye fixations file? ");
	while((eyefile=fopen(gets(file),"r")) EQ NULL)
		printf("\nBad file name, try again.\n");
	}
else
	{
	strcpy(file,argv[2]);
	if((eyefile=fopen(file,"r")) EQ NULL)
		{
		printf("\n%s is a bad file name, try again.\n",file);
		exit(1);
		}
	}

if(argc < 4)
	{
	printf("\nWhich item do you want to graph (item number): ");
	gets(file);
	item = atoi(file);
	}
else
	item = atoi(argv[3]);

if(argc < 5)
	{
	printf("\nWhat is the name of your comma-delimited pronunciation file? ");
	while((pronfile=fopen(gets(file),"r")) EQ NULL)
		printf("\nBad file name, try again.\n");
	}
else
	{
	strcpy(file,argv[4]);
	if((pronfile = fopen(file,"r")) == NULL)
		{
		printf("\n%s is a bad file name, try again.\n",file);
		exit(1);
		}
	}		

if(argc < 6)
	{
	printf("\nWhat is the name of your output files \n(no extension; the program will add .gra and .lag)? ");
	gets(file);
	}
else
	strcpy(file,argv[5]);

for(i=0;file[i] != '.' && file[i] != '\0';i++)
	;
file[i++] = '.';
file[i++] = 'g';
file[i++] = 'r';
file[i++] = 'a';
file[i++] = '\0';
if((graphfile=fopen(file,"w")) EQ NULL)
	{
	printf("\nBad file name, try again.\n");
	exit(1);
	}


for(i=0;file[i] != '.' && file[i] != '\0';i++)
	;
file[i++] = '.';
file[i++] = 'l';
file[i++] = 'a';
file[i++] = 'g';
file[i++] = '\0';
if((lagfile=fopen(file,"w")) EQ NULL)
	{
	printf("\nBad file name, try again.\n");
	exit(1);
	}

for(i=0;i<4;i++)
	for(j=0;j<100;j++)
		graph[i][j] = 0;


/* process the eyetracking file */

founditem = 0;
while(founditem != item)
	{
	if(fgets(ebuff,MAXLINEX,eyefile) == NULL)
		{
		printf("\nOH DAMN, couldn't find eyetracking data");
		exit(1);
		}
	i = 0;
	eline = 0;
	GetNextNumber(ebuff,&i);			/* read in, throw away, 4 numbers */
	founditem = GetNextNumber(ebuff,&i);
	GetNextNumber(ebuff,&i);
	nwords = GetNextNumber(ebuff,&i);
	}
for(word=0;word<nwords;word++)
	{
	estart = GetNextNumber(ebuff,&i);		/* end of line */
	eend = GetNextNumber(ebuff,&i);
	if(estart != 0)						/* fixation */
		{
		graph[0][eline] = word+1;
		graph[1][eline] = estart;
		eline++;
		graph[0][eline] = word+1;
		graph[1][eline] = eend;
		eline++;
		}
	else
		{
		graph[0][eline] = 0;
		graph[1][eline] = 0;
		eline++;
		}
	}

/* process the pronunciations file */

pbuff[0] = pbuff[1] = '1';
while(pbuff[0] != ',' && pbuff[1] != ',')
	{
	if(fgets(pbuff,MAXLINEX,pronfile) == NULL)
		{
		printf("\nOH DAMN, ran out of input looking for ,,");
		exit(1);
		}
	}
/*printf("\nINITIAL %s",pbuff);*/
if(fgets(pbuff,MAXLINEX,pronfile) == NULL)
	{
	printf("\nOH DAMN, ran out of input looking for line after ,,");
	exit(1);
	}
/*printf("\nNEXT %s",pbuff);*/
word = 0;
pline = 0;
initial = TRUE;
pauseflag = 0;
while(fgets(pbuff,MAXLINEX,pronfile) != NULL)
	{
/*printf("\nLOOP %s",pbuff);
if(getch() == 'q')
	exit(1);*/
	i = 0;
	utt = GetNextNumber(pbuff,&i);
	idx = GetNextNumber(pbuff,&i);
/*printf("\nutt %d idx %d",utt,idx);*/
	if(*(pbuff+i+1) == ',')
		{								/* found a pronunciation -- i.e. second comma after idx */
		start = GetNextFloat(pbuff,&i);
		if(pauseflag == 1)
			{
			if(word != 0)
				{
				graph[2][pline] = word+1;
				graph[3][pline] = (int)(pause)*1000 - 16;
				pline++;
				}
			graph[2][pline] = 0;
			graph[3][pline] = 0;
			pauseflag = 0;
			pline++;
			word++;
			}
		else if(!initial)
			{
			graph[2][pline] = word+1;
			graph[3][pline] = (int)(start)*1000 - 16;
			word++;
			pline++;
			}

/*printf("\nSTART %f %d i %d incremented word %d",start,(int)(start*1000)+16,i,word);*/
		graph[2][pline] = word+1;
		graph[3][pline] = (int)(start)*1000 - 16;
		pline++;
		initial = 0;
		}
	else
		{								/* found a pause */
		pauseflag = 1;
		pause = GetNextFloat(pbuff,&i);
/*		graph[2][pline] = 0;
			graph[3][pline] = 0;
			pline++; */
		}
/* printf("\nPAUSE %f %d",pause,(int)(pause)*1000+16);
if(getch() == 'q')
	exit(1); */
	}
/* output .gra file */
fprintf(graphfile,"Fixation WordFix Pronunciation WordPron Pronunciation\n");
for(i=0;i<pline || i < eline;i++)
	{
	if(graph[0][i] == 0)
		fprintf(graphfile,". . ");
	else
		fprintf(graphfile,"%d %d ",graph[1][i],graph[0][i]);
	if(graph[2][i] == 0)
		fprintf(graphfile,". .\n");
	else
		fprintf(graphfile,"%d %d\n",graph[3][i],graph[2][i]);	
	}

/* output .lag file */
fprintf(lagfile,"Subj %d item %d\n",subj,item);
for(i=0;i<nwords;i++)
	{
	fprintf(lagfile,"%d ",i+1);
/* scan for voice lag in ms */
	startpron = startfix = 0;
	for(j = 0;j< pline && startpron == 0; j++)
		{
		if(graph[2][j] == i+1)
			startpron = graph[3][j];
		}
	for(k = 0;k< eline && startfix == 0;k++)
		{
		if(graph[0][k] == i+1)
			startfix = graph[1][k];
		}
	if(startpron != 0 && startfix != 0)
		fprintf(lagfile,"%d ",startpron-startfix);
	else
		fprintf(lagfile,". ");
/* scan for voice lag in words */
	startpron = startfix = 0;
	for(j = 0;j< pline && startpron == 0; j++)
		{
		if(graph[2][j] == i+1)
			startpron = graph[3][j];		
		}
	for(k = 0;k< eline && startfix == 0; k++)
		{
		if(graph[1][k] >= startpron)
			startfix = graph[0][k];
		}
	if(startpron != 0 && startfix != 0)
		fprintf(lagfile,"%d ",startfix-(i+1));
	else
		fprintf(lagfile,". ");
/* scan for fixation advance in words */
	startpron = startfix = 0;
	for(j = 0;j< eline && startfix == 0; j++)
		{
		if(graph[0][j] == i+1)
			startfix = graph[1][j];
		}		
	for(k = 0;k< pline && startpron == 0;k++)
		{
		if(graph[3][k] >= startfix)
				startpron = graph[2][k];
		}
	if(startpron != 0 && startfix != 0)
		fprintf(lagfile,"%d \n",i+1-startpron);
	else
		fprintf(lagfile,". \n");
	}


fclose(pronfile);
fclose(eyefile);
fclose(graphfile);
fclose(lagfile);

} /* END OF MAIN */




int GetNextNumber(char* dc,int *index)		/* modify to make like Float */
{
char num[40];
int j = 0;
int flip;
while(!isdigit(*(dc+*index)) && *(dc+*index) != '\0')
	{
	(*index)++;
	}
if(*(dc+(*index)) == '\0')
	return(0);
else if(*(dc+(*index)-1) == '-')
	flip = -1;
else
	flip = 1;
while(isdigit(*(dc+*index)) || *(dc+*index) == '.')
	{
	num[j++] = *(dc+*index);
	(*index)++;
	}
num[j] = '\0';
return(flip*(atoi(num)));
}




/************************************************/

float GetNextFloat(char* dc,int *index)
{
char num[30];
float flip;
int j = 0;
while(!isdigit(*(dc+*index)))
	(*index)++;
if(*(dc+(*index)-1) == '-')
	flip = -1;
else
	flip = 1;
while(isdigit(*(dc+*index))|| *(dc+*index) == '.')
	{
	num[j++] = *(dc+*index);
	(*index)++;
	}
/*while(isspace(*(dc+*index)))
	(*index)++;*/
num[j] = '\0';
/*printf("\n string %s",num);*/
return(flip*(atof(num)));
}








/* reads next ASCII-coded number from buff[] */
/* starts at beginning of buff[] if control = 0 */
int readnext(int control)
{
static int bptr;
int value;
if(control EQ 0)
	bptr = 0;
while (!isdigit(*(buff+bptr)))
	bptr++;
value = atoi(buff+bptr);
while(isdigit(*(buff+bptr++)))
	;
if(debug EQ 1)
	printf("\nIn readnext: value = %d, bptr = %d, control = %d",value,bptr,control);
return(value);
}



/***************************************************************************/
/* get specified datum from buff */
int spacebuff(char *bptr,int val)
{
int i,j;
val--;
j=0;

if(val EQ 0)
	{
	while(isspace(*(bptr+j)))
		j++;
	return(j);
	}

for(i=0;i<val;i++)
	{
	while(isspace(*(bptr+j)))
		j++;    /* move to first/next nonspace */
	while(!isspace(*(bptr+j)))
		j++;    /* then move to next space */
	while(isspace(*(bptr+j)))
		j++;    /* and finally on to next nonspace */
	}
	j--;
return(j);
}/*end spacebuff*/



void oops(char *string)
{
printf("\nOut of allocation space at %s.",string);
exit(1);
}/*end oops*/


void movenextnumber()
{
while(isspace(*tbuff))  /* move on to next number */
	{
	if(*tbuff == '\n')
		return;
	tbuff++;
	}
while(!isspace(*tbuff))
	tbuff++;
while(isspace(*tbuff))
	tbuff++;
} /*end movenextnumber */


void movebacknumber()
{
while(!isspace(*tbuff))
	tbuff--;
while(isspace(*tbuff))
	tbuff--;
while(!isspace(*tbuff))
	tbuff--;
} /*end movebacknumber*/


void openfail(char *filestr)
{
printf("\nCan't open file %s. ",filestr);
exit(1);
}


char *tgets(char *buff,FILE *stream)
	{
	int i;
	fgets(buff,MAXLINE,stream);
	for(i=0;buff[i] != '\n' && i < MAXLINE;i++)
		;
	buff[i] = '\0';
	return(buff);
	}

