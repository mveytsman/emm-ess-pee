#include <msp430.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

#include <io.h>


 
int main(void)
{
    //printf("Hello World\n");
    char* lol = malloc(100);
    lol[2] = 'a';
    return 0;
}
 
/* int  putchar(int c) */
/* { */
/*     //while (!(UTCTL0 & TXEPT)) {}   // wait util tx buffer is free  */
/*     //TXBUF0 = character;            // send character  */
/*     return 1;  */
/* } */
