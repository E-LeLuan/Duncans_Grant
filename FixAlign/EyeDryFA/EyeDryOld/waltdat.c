/* program to convert old Walter questionnaire data files from one line of 
responses per subject to R format - columns to be labeled:
line language subj cond item resp
where cond 1 = short subject, 2 = long subject, 3= short object, 4 = long object
and resp 1 = N1 attachment, 0 = N2 attachment (unscoreables ignored) */


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


FILE *infile,*outfile,*listfile,*control;
#define LF 0x0a
#define CR 0x0d
void main(void);
int input(char *output);

/* DECLARACTIONS OF FUNCTIONS */

void GetNextTrial(FILE* infile);
int GetNextNumber(char* dc,int *i);
void WriteData(int ntrials);


/* MAIN PROGRAM */

void main(void)
{
char tempstr[400];
char sfile[80];
int i,subj,cond,item,resp,ns,line;
char c;

printf("\nName of file containing data file names? ");
while((control = fopen(gets(tempstr),"r")) == NULL)
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

printf("\nName of summary output file? ");
while((outfile = fopen(gets(tempstr),"w")) == NULL)
	printf("\nBad file name, try again ");
fprintf(outfile,"line,lang,subj,cond,item,N1\n");

for(subj=0;subj<ns;subj++)
{
	line = 0;
	bdos(11,0,0);		/* check for operator interrupt - i.e. control-C*/
	fscanf(control,"%s",sfile);
	if((infile = fopen(sfile,"r"))==NULL)
		{
		printf("\n%s Not a good name, check data list file: ",sfile);
		exit(1);
		}
	fgets(tempstr,256,infile);
	i = 0;
	for(cond =0;cond < 4;cond++)
		{
		for(item = 0; item <32; item++)
			{
			while(!isdigit(tempstr[i]))
				i++;
			resp = tempstr[i++];
			if(resp == '1')		/* to be nice, change E to S for Spanish */
				{
				fprintf(outfile,"%d,E,%d,%d,%d,1\n",line+1,subj+1,cond+1,item+1);
				line++;
				}
			else if(resp == '2')
				{
				fprintf(outfile,"%d,E,%d,%d,%d,0\n",line+1,subj+1,cond+1,item+1);
				line++;
				}
			else
				;
			}			/* end of item loop */
		}			/* end of cond loop */
	fclose(infile);		/* end of subj loop */
}			
fclose(outfile);
}


/***********************************************************************/


int input(char *output)
{
char buffer[MAXLINE];
cprintf("%s",output);
buffer[0]='\0';
return(atoi(gets(buffer)));
}


