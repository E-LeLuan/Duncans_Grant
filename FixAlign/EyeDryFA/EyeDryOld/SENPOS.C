/* TEMPLATE OF PROGRAM TO OUTPUT FILES OF ACTUAL
SENTENCES SHOWN AND LETTER POSITION COUNTS */
/* assumes that questions start with condition numbers 100 or greater 
and skips them */

#include "pcexpt.h"
#define ca con_adjust
#define cond_adjust con_adjust

main(argc,argv)		/* OBLIGATORY STUFF */
int argc;
char *argv[];

{
int nblocks;
int con_adjust,qconadj;
char temp[20];
FILE *senout,*countout;
int xpos[300],ypos[300],xyv;
char *xyp;
int xyc,xval,yval;
int i;
nblocks = 1;			/* check this !*/
printf("\nType name of sentence output file: ");
do
	{
	senout = fopen(gets(temp),"wb");		/* binary; don't add CR to LF */
	if(senout == NULL)
		printf("\nNOPE, try again: ");
	}
while(senout == NULL);
printf("\nType name of position count output file: ");
do
	{
	countout = fopen(gets(temp),"w");
	if(countout == NULL)
		printf("\nNOPE, try again: ");
	}
while(countout == NULL);
printf("NOTE---\n  SPECIFY THAT YOU WANT NO DATA FILE OUTPUT,\n  AND INDICATE WHAT COUNTERBALANCING (SCON) CONDITION THIS RUN IS FOR.\n");
argc = initialize(argc,argv,VIDEO_CARD);	/* OBLIGATORY STUFF */

while(CTRIAL < NITEMS)
	{				/* START OF YOUR MAIN PROGRAM HERE */
					/* INCLUDE UP TO, BUT NOT INCLUDING, YOUR STATEMENTS */
					/* THAT INCREASE CCOND AND THAT DISPLAY STUFF */

					/* if you have no counterbalancing routine in your
						program, then specify the following:
						
						cond_adjust=0;   */

	if(CCOND < 100)			/* not for questions */
		{
		CCOND += con_adjust;
		fprintf(senout,"%4d%4d %s|\r\n",CCOND,CITEM,trials[0][order[ctrial]].dptr);
		xpos[0]=-1;
		ypos[0]=0;
		for(xyp=trials[0][order[CTRIAL]].dptr,xyc=1,xval=0,yval=0;*xyp != '\0';xyp++,xval++)
			{
			if(*xyp == ' ')	/* could put a specified delimiter here */
				{
				xpos[xyc] = xval;
				ypos[xyc++] = yval;
				}
			else if(*xyp == '\n' || *xyp == LF)
				{
				xpos[xyc]=xval;
				ypos[xyc++]=yval++;
				xval=-1;
				}
			else if(*xyp == CR)
				xval--;			/* throw away CRs */
			}
		fprintf(countout,"%5d%5d%5d",CCOND,CITEM,xyc);
		for(i=0;i<xyc;i++)
			fprintf(countout,"%5d%5d",xpos[i],ypos[i]);
		fprintf(countout,"\n");
		}
	CTRIAL++;
	printf("\nTrial %d",CTRIAL);
	}
fclose(senout);
fclose(countout);
tclear();

}
