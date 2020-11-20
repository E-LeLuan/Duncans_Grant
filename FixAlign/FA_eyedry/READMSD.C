/* PROGRAM TO CHECK OUTPUT OF MS X MS DATA EYETRACKER */
/* new version, 4/14/01, to use with raw voltages and to
correct for time offset 
REVISED 4/18/06 to */

#include "pcexpt.h"

void readline(int filehandle,int* filebuff,int* filelng);

FILE *graph;
int filehandle;
int* filebuff;
int filelng;
int i,ti,cond,item,it,cc,off;
char filename[40],outfilename[40];
int tempbuff[2];
int readok = 1;
int hooksize;

void main(int argc, char *argv[])
{
int peak = 1000;
int gi;
int max,max1,max2,maxi,foundmax;
printf("\nGraphical output version, scan for fixations\nUse only with .msd files made using system dated 4/14/01 or later.\n");
if(argc <= 1)
	{
	printf("\nWhat is name of input file? ");
	scanf("%s",filename);
	}
else
	strcpy(filename,argv[1]);
printf("\nDo you want to write a file for graphic examination? y or n: \n");
if(tolower(getch()) == 'y')
	{
	printf("\nWhat is name of output file: \n");
	scanf("%s",outfilename);
	if(strlen(outfilename) != 0)
		if((graph=fopen(outfilename,"at")) == NULL)
			{
			printf("\nOops, can't open file %s",filename);
			exit(1);
			}
	}
printf("\nThe program scans for hooks. How large does a hook have to be?\n(In pixels; at least 1, typically 2-4): ");
scanf("%d",&hooksize);
while(1)
	{
	startloop:
	printf("\nWhich item do you want to examine? 0 to quit");
	scanf("%d",&it);
	if(it == 0)
		exit(1);
	else
		{
		if((filehandle = open(filename,O_RDONLY | O_BINARY,S_IREAD)) == -1)
			{
			printf("\nCan't open file");
			exit(1);
			}
		printf("...scanning file; please wait\n");
		do
		{
		if((readok = read(filehandle,&cond,2)) <= 0)
			{
			printf("\nFile read error reading cond, or end of file");
			exit(1);
			}
		if(cond == 9999)
			{
			printf("\nReached end of file without reading item number %d; try again.",it);
/*			exit(1);*/
			break;
			}
		if((readok = read(filehandle,&item,2)) <= 0)
			{
			printf("\nFile read error reading item number, or found end of file");
			exit(1);
			}
		if((readok=read(filehandle,&filelng,2)) <= 0)
			{
			printf("\nFile read error reading length, or end of file");
			exit(1);
			}
		if((readok=read(filehandle,&off,2)) <= 0)
			{
			printf("\nFile read error reading time offset, or end of file");
			exit(1);
			}

		if(item == it)
			{
			printf("\n%d values to read in for cond %d item %d offset %d\npress\n   ^C to stop program\n   x to end trial\n   p to back up\n   g to select ms to go go\n   any other key to continue\n",filelng,cond,item,off);
			printf("\nPress any key to continue.");
			getch();
			filebuff = (int *) calloc(2*(filelng+4),sizeof(int));
			readline(filehandle,filebuff,&filelng);
			for(i=0;i < filelng*2;)	
				{
				if(i%48 != 0 || i == 0)
					{
					if(i%48 == 46)
						{
						printf("\n%4d%6d%6d (%4d %4d)\np=previous g=goto ms w=write output s=scan for peak x=stop trial ",i/2+1+off,*(filebuff+i),*(filebuff+i+1),(*(filebuff+i))/8,(*(filebuff+i+1))/14);		
						i+=2;
						}
					else
						{
						printf("\n%4d%6d%6d (%4d %4d)",i/2+1+off,*(filebuff+i),*(filebuff+i+1),(*(filebuff+i))/8,(*(filebuff+i+1))/14);		
						i+=2;
						}
					}
				else
					{
					if((cc = getch()) == 0x03)
						{
						close(filehandle);
						if(strlen(outfilename) > 0)
							fclose(graph);
						exit(1);
						}
					else if(cc == 'x')
						goto startloop;
					else if(cc == 'w')
						{
/*						printf("\nWhat is msec value of center point (peak) of string to write? ");
						scanf("%d",&peak);*/
						for(gi = peak-50;gi<=peak+50;gi++)
							fprintf(graph,"%d,",*(filebuff+((gi-1-off)*2)));
						fprintf(graph,"\n");
						printf("Press key to go ahead.");
						}
					else if(cc == 'p' || cc == 'b' || cc == 0x48)
						{
						if(i > 96)
							i -= 96;
						else
							i = 0;
						printf("\n%4d%6d%6d (%4d %4d)",i/2+1+off,*(filebuff+i),*(filebuff+i+1),(*(filebuff+i))/8,(*(filebuff+i+1))/14);		
						i += 2;	
						}
					else if(cc == 'g')
						{
						printf("\nWhich millisecond in .dat file do you want to go to? ");
						scanf("%d",&ti);
						ti -= off;
						i = ti * 2;
						printf("Press key to go ahead.");
						}
					else if(cc == 's')
						{
						foundmax = 0;
						max1 = *(filebuff+i);
						do
							{
							i+=2;
							if(*(filebuff+i) > max1)
								{
								max1 = *(filebuff+i);
								foundmax = 0;
								maxi = i;
								}
							else if(*(filebuff+i) < max1)
								{
								foundmax = 1;
								}
							}
						while((foundmax == 0) || (*(filebuff+i) > (max1 - hooksize)) || i > maxi+20);
						i = maxi;
						printf("\ni = %d, ms %d; Press key to go ahead.",i,i/2+1+off);
						peak = i/2+1+off;
						for(;i%48!=0;i-=2)
							;
						i+=2;
						}
					else
						{
						printf("\n%4d%6d%6d (%4d %4d)",i/2+1+off,*(filebuff+i),*(filebuff+i+1),(*(filebuff+i))/8,(*(filebuff+i+1))/14);		
						i+=2;
						}
					}	
				}
			}
		else
			{
			filebuff = calloc(2*(filelng+4),sizeof(int));
			readline(filehandle,filebuff,&filelng);
			}
		free(filebuff);
		}	
		while(item != it);
		}
	close(filehandle);
	if(strlen(outfilename) > 0)
		fclose(graph);
	}
}


void readline(int filehandle,int *filebuff,int *filelng)
{
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