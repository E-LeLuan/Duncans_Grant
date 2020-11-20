/* PROGRAM TO CHECK OUTPUT OF MS X MS DATA EYETRACKER */
/* new version, 4/14/01, to use with raw voltages and to
correct for time offset */

/* version to scan for all fixations and write preceding data */

#include "pcexpt.h"

void readline(int filehandle,int* filebuff,int* filelng);
int readitem(int it);

FILE *graph;
int filehandle;
int* filebuff;
int filelng;
int i,ti,cond,item,it,cc,off;
char filename[40],outfilename[40];
int tempbuff[2];
#define SLAG 5
#define M 4
#define S 5
#define adjLF 2				/* threshold value, now in pixels */
#define adjLS 1

void main(int argc, char *argv[])
{
int peak;
int fit,lit;
int gi;
int fix,wantmove,wantfix;
int stf[20],stm[20],sti,gc;
printf("\nUse only with .msd files made using system dated 4/14/01 or later.\n");
if((filebuff = calloc(2*(16000),sizeof(int))) == NULL)
	{
	printf("\nError allocating filebuff.");
	exit(1);
	}
if(argc <= 1)
	{
	printf("\nWhat is name of input file? ");
	scanf("%s",filename);
	}
else
	strcpy(filename,argv[1]);
if((filehandle = open(filename,O_RDONLY | O_BINARY,S_IREAD)) == -1)
	{
	printf("\nCan't open file");
	exit(1);
	}

printf("\nWhat is name of output file: \n");
scanf("%s",outfilename);
	if(strlen(outfilename) != 0)
	if((graph=fopen(outfilename,"at")) == NULL)
		{
		printf("\nOops, can't open file %s",filename);
		exit(1);
		}

printf("\nWhat is the first trial (presentation order) you want to examine?\nType 0 to quit: ");
scanf("%d",&fit);
if(fit == 0)
	exit(1);
printf("\nWhat is the last trial you want to examine? ");
scanf("%d",&lit);
for(it = 0;it<fit;it++)		/* skip leading trials */
	{
	readitem(it);
	printf("- skipping it");
	readline(filehandle,filebuff,&filelng);
	}
for(it=fit;it<lit;it++)
	{
	for(sti=SLAG;sti>=0;sti--)
		stm[sti] = 9999;
	gc = 0;
	readitem(it);
	printf(" - analyzing it");
	readline(filehandle,filebuff,&filelng);
	wantfix = 1;
	wantmove = 0;
	for(i=0,fix = 0;i < filelng*2;i += 2)	
		{
		if(wantfix)			/* looking for a fixation */
			{
			for(sti=SLAG;sti>0;sti--)
				stf[sti]=stf[sti-1];
			stf[0] = *(filebuff+i);
			if(stf[SLAG] != 9999)
				{
				if((stf[0] - stf[SLAG]) > adjLF)
					gc = 0;				/* not stable */
				else
					gc++;
				}
			if(gc > S)
				{
				fix++;
				wantfix = 0;
				wantmove = 0;
				gc = 0;
				}
			}
		else if(wantmove)
			{
			for(sti=M+1;sti>0;sti--)
				stm[sti]=stm[sti-1];
			stm[0] =  *(filebuff+i);
			if(stm[1] != 9999)
				{
				if((stm[0]-stm[1]) > adjLS)
					gc++;				/* possible move, only forward, no abs */
				else
					gc=0;
				}
			if(gc > M)
				{
				wantfix= 1;
				wantmove = 0;
				gc = 0;
				}
			}
		else 				/* write data to file, not first fixation */
			{
			peak = i/2+1-S-SLAG;		/* fixation sample */
fprintf(graph,"\npeak %d i %d",peak,i);
			if(fix > 1)
				{
				for(gi = peak-125;gi<=peak+125;gi++)
					{
					if(gi > 0 && gi < filelng)
						fprintf(graph,"%d,",*(filebuff+((gi-1)*2)));
					else
						fprintf(graph,",");
					}
				fprintf(graph,"\n");
				}
			wantmove = 1;
			}
		}	
	}
	free(filebuff);
	close(filehandle);
	fclose(graph);
}


void readline(int filehandle,int *filebuff,int *filelng)
{
int readok;
if(*filelng > 8000)
	{
	if((readok=read(filehandle,filebuff,32000)) <= 0)
		{
		printf("\nFile read error, reading data; error %d %x",readok,readok);
		exit(1);
		}
	*filelng -= 8000;
	if((readok=read(filehandle,filebuff+16000,(*filelng)*4)) <= 0)
		{
		printf("\nFile read error, reading data; error %d %x",readok,readok);
		exit(1);
		}
	*filelng += 8000;
	}
else if((readok=read(filehandle,filebuff,(*filelng)*4)) <= 0)
	{
	printf("\nFile read error, reading data; error %d %x",readok,readok);
	exit(1);
	}
}

int readitem(int it)
	{
	int readok;
	if((readok = read(filehandle,&cond,2)) <= 0)
		{
		printf("\nFile read error reading cond, or end of file");
		exit(1);
		}
	if(cond == 9999)
		{
		printf("\nRead illegal cond number on trial %d",it);
		exit(1);
		}
	if((readok = read(filehandle,&item,2)) <= 0)
		{
		printf("\nFile read error reading item number, trial %d, or found end of file",it);
		exit(1);
		}
	if((readok=read(filehandle,&filelng,2)) <= 0)
		{
		printf("\nFile read error reading length, trial %d, or end of file",it);
		exit(1);
		}
	if((readok=read(filehandle,&off,2)) <= 0)
		{
		printf("\nFile read error reading time offset, trial %d, or end of file",it);
		exit(1);
		}
	printf("\n%d values to read in for cond %d item %d trial %d",filelng,cond,item,it);
	if(filelng > 16000)
		{
		printf("\nTrial %d too long, eliminate or resize buffer",it);
		exit(1);
		}
	}