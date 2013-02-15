#include <sys/types.h> /* POSIX Primitive System Data Types */
#include <stdlib.h> /* for exit() */
#include <sys/socket.h> /* Socket Library */
#include <netinet/in.h> /* IP-Communication */
#include <arpa/inet.h> /* conversion of IP-addresses */
#include <stdio.h> /* I/O comunication */
#include <string.h> /* memset() */
#include <stdbool.h> /* boolean evaluation */
#include <time.h> /* time functions */
#include <errno.h> /* number of last error for error() */

static const int PORTNUMBER=12345;
static const char CLIENTADDRESS[16]="127.0.0.1";
static const int RECEIVE_DATA_LENGTH = 1;
static const int SEND_DATA_LENGTH = 12;

typedef int SOCKET;
SOCKET sockethandler;
struct sockaddr_in clientaddress;
socklen_t clientaddresslength;

/* 
   Prototypes 
*/

void error(const char *errormessage);
/* print errormessage including timestamp (ISO 8601) */

void open_socket();
/* creates a socket for upcoming communication */

void close_socket();
/* closes the socket opened by open_socket() */

char receivefromclient();
/* receive data from the (Ada-)Client */

bool evaluaterequest(const char *request, const char *comparevalue);
/* checks if a proper request has been made by the client */

void sendtoclient(const char *senddata);
/* send data to the client */

char* readimage();
/* reads the whole camera-image and saves it for later use */

char* readimagechunk(const char* image, const int offset, const int length);
/* reads a chunk of the image from the buffer created by readimage() */

int number_of_imagechunks(const char* image);
/* return the number of chunks required to transmit a certain image */

int main();
/* main server part */
