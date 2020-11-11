/* BRIEF INSTRUCTIONS FOR USE:

This program is designed to convert .stm (or .sen) files as used on the 
DPI eyetracker so that David Stracuzzi's EYEDOCTOR program can read them.

To use it, first copy nurse.c from c:\data into your directory. Then
insert your "pickone" code from your experiment running program
between the "your code starts here" and the "your code ends here" lines.
Do NOT include any code that displays text on the screen, accepts responses,
or changes CCOND.

Then, working in your directory compile the program with etcc.bat (etcc nurse.c).

Then run the program one time for each counterbalancing condition. Each time you
run it, save the output .sen file with a distinctive name (e.g. mycond1.sen).

If you have a display change, the program will ask you for the character 
you used to delimit the display change material, and delete this material when 
it reads material from your .stm file. (OK FOR DISPLAY CHANGE; NOT YET
COMPLETE FOR FAST PRIMING.)

*/


/*KERNEL FOR PCEXPT PROGRAMS 4/24/99 */
/* modified to just print out sentences into asciii files,
one for each counterbalancing condition 
To be used in connection with David's EYEDOCTOR */

/* COPY AS yourprogramname.C, CHANGE THE PARAMTERS ON THE FIRST
PAGE, ADD YOUR SELECTION AND DISPLAY CODE WHERE INDICATED, AND
DESCRIBE THE CONDITIONS AT THE BOTTOM. */

#include "pcexpt.h"
#define ca con_adjust
#define cond_adjust con_adjust
#define MAXLINEN 2000


void main(int argc,char *argv[])		/* OBLIGATORY STUFF */
{
long far *butptr;
FILE *outfile;
int cond_adjust, testout, nblocks, duration;
long starttime;
char tempstr[MAXLINEN],temp[20];
char filename[80];
char dcchar;
int maxcond;
int i,j,k;
int offset = 0;

/* *** the user may want to change the following parameters*** */
CENTER = TRUE;		/* false if displays not centered */
SPACING = 1;			/* spacing for op_fmt... displays */
row_height = 14;	/* change this -- 14 = singlespace, 20 = 1.5 space, 28 = doublespace */
space_in = 0;			/* x offset; set to 10 for 60 char display */
line_down = 8;		/* y offset; 0 gives 24 singlespaced lines, 4 gives 16 lines, 8 gives 8 ...*/
									/* in general, you get max number of lines - 2*line_down */
									/* remember, fast screen change gives you many fewer lines */
									/* only 7 singlespaced lines maximum on the Gen V */
									/* so you have to set line_down to 1 or 2 */
									/* space_in and line_down affect size of calibration grid */

argc = initialize(argc,argv,VIDEO_CARD);	/* OBLIGATORY STUFF */
	/* VIDEO_CARD 0 = choose default, 1 = herc mono, 2 = color */
getkey();

/* force input order */
CTRIAL = 0;
if(DISKFILEFLAG)
	getnextitem(0);

printf("Output .sen file name? ");
while((outfile = fopen(gets(filename),"w"))==NULL)
	{
	printf("\nNot a good name, try again: ");
	}

printf("What is the maximum condition number you want to include? ");
scanf("%d",&maxcond);

printf("Do you have a display change in the experiment? y or n: ");
scanf("%s",tempstr);
if(tolower(tempstr[0]) == 'y')
	{
	printf("What is the character that delimits your display change (e.g. '/')? ");
	scanf("%s",tempstr);
	dcchar = tempstr[0];
	}
else
	dcchar = '\0';
printf("What was your Y offset (0 for top of screen, etc.)? ");
scanf("%d",offset);

cond_adjust = 0;
fprintf(outfile,"%d\n",offset);
while(CTRIAL < NITEMS)
	{
/**************************** your code starts here *************************/
/* NOTE
			!!!!   DO NOT INCLUDE ANY COMMAND THAT ATTEMPTS TO DISPLAY STUFF
               e.g. eye_disp_sample_store() or question_any_sentence()     !!!!
			!!!!   DO NOT INCLUDE ANY COMMAND THAT INCREMENTS CCOND !!!!        
			!!!!   DO NOT INCLUDE ANY CODE THAT CHANGES CTRIAL e.g. next_trial()!!!!   */






/**************************** your code ends here **************************/

if(dcchar != '\0')
	{
	strcpy(tempstr,CDISP);
	for(i=0;tempstr[i] != '\0';i++)
		{
		if(tempstr[i] == dcchar)
			{
			j = i;
			while(tempstr[j++] != '`')
				;
			k = j-i;
			while(tempstr[i+k] != '\0')
				{
				if(tempstr[i+k] != dcchar)
					{
					tempstr[i] = tempstr[i+k];
					i++;
					}
				else
					k++;
				}
			tempstr[i] = '\0';
			break;
			}
		}
	strcpy(CDISP,tempstr);
	}

/* get rid of that extra CR */
/*printf("%s\n",CDISP);*/
strcpy(tempstr,CDISP);
for(i=0;tempstr[i] != '\0';i++)
	{
/*printf("%2x",tempstr[i]);*/
/*	if(tempstr[i] == 0x0d && tempstr[i-1] == 0x0d)*/
		if(tempstr[i] == 0x0d)
		tempstr[i] = ' ';
	}
strcpy(CDISP,tempstr);
/*printf("\n");
if(getch() == 'q')
	exit(1);*/

	CCOND += cond_adjust;
	if(CCOND <= maxcond)
		fprintf(outfile,"{%d %d}%s\n",CCOND,CITEM,CDISP);
	CTRIAL++;
	}
fclose(outfile);
}

/************************ PUT ANY SUBROUTINES HERE ***************************/

/* replace these with your own functions if you need one */

long specialevent(unsigned dur)
{
return((long)(dur));		/* clear nonsense */
}

long extra_specialevent(unsigned dur,int y,int x,int t_s,int position)
{
return((long)(dur*y*x*t_s*position));
}

/************* DO WRITE A NOTE ABOUT CONDITION NUMBERS HERE ***************/

