#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "ctype.h"
#include "fcntl.h"
#define TRUE 1
#define FALSE 0

main(argc,argv)
int argc;
char *argv[];

{
FILE *datafile;
int i;
char filename[80],temp[80];
int ord,cond,item,rt,resp,rt2,resp2,obs;
int extim1,extim2;
int x,y;
unsigned finishl,xl,yl;
unsigned start,finish;
int si,li,ti;

finish=0;
x=0;
y=0;
if(argc != 2)
	{
	printf("\nData file name to list? ");
	argv[1] = gets(filename);
	}
if(!(datafile = fopen(argv[1],"r")))
	{
	do
		{
		printf("\nNot a good name, try again: ");
		datafile = fopen(gets(filename),"r");
		}
	while (!datafile);
	}

printf("\nWhat is lowest item number to analyze? ");
si = atoi(gets(temp));
printf("\nWhat is highest item number to analyze? ");
li = atoi(gets(temp));
printf("\nAnalyzing items %d to %d",si,li);
while(fscanf(datafile,"%d%d%d%d%d%d%d%d%d",&ord,&cond,&item,&rt,&resp,&rt2,&resp2,&extim1,&extim2))
	{
	ti = (item >= si && item <= li) ? TRUE : FALSE;
	if(item > li)
		{
		fclose(datafile);
		exit(1);
		}
	if(cond < 100)		/*not a question */
		{
		fscanf(datafile,"%d",&obs);
		if(ti)
			{
			printf("\n\nTRIAL %d CONDITION %d ITEM %d TOTAL TIME %d (%d observations)",ord/2+1,cond,item,rt,obs);
			printf("\n     X     Y     START    FINISH  DURATION SACCADE LEN DUR"); 
			}
		for(i=0;i<obs;i++)
			{
			finishl=finish;
			xl=x;
			yl=y;
			fscanf(datafile,"%d%d%u%u",&x,&y,&start,&finish);
			if(yl<y)
				xl=x;			/* 0 if regression */
			if(finish == 65432)
				finish = start;
			if(ti)
				printf("\n%6d%6d%10u%10u%10u%11d%5u",x,y,start,finish,finish-start,x-xl,start-finishl);
			}
		}
	else if(cond > 100 && ti)					/* question */
		{
		printf("\nQUESTION: CONDITION %d ITEM %d RT %d RESPONSE %d",cond,item,rt,resp);
		}
	}
fclose(datafile);
}
