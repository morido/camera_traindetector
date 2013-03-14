/*

Package: Generic

Purpose: Provide Functions used throughout the entire Server-Software

*/


/*
Headers:
time.h - for time related functions
stdio.h - snprintf() and friends
limits.h - used for platform-dependent prechecks
stdlib.h - for portable EXIT_FAILURE
*/
#include <time.h>
#include <stdio.h>
#include <limits.h>
#include <stdlib.h>


/*
Function: precheck
Purpose: Check if the current plaform is suitable for this program
Parameters: none.
*/
void precheck();


extern void socket_close();

/*
Function: ErrorInternal
Purpose: Prints an Errormessage including timestamp (ISO 8601)
Effects:
unfortunately there is no full-fledged exception-handling in C as is with C++/Ada, so we have to deal with it this way
Note: This function is not supposed to be called directly, but via the define <error>
Parameters:
caller - A string containing the name of the calling function
errormessage - A string containing a description of what has happened
*/
int error(const char *caller, const char *errormessage);
