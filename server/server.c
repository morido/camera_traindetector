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

void error(const char *errormessage)
{
  /* unfortunately there is no full-fledged exception-handling in C as is with C++/Ada, so we have to deal with it this way */

  /* print errormessage including timestamp (ISO 8601) */
  time_t now;
  time(&now);
  char timestamp[23];
  char completemessage[80]; /* arbitrary fixed size */
  
  strftime(timestamp, sizeof timestamp, "%F %T.00", localtime(&now)); /* decisecond precision not implemented */
  sprintf(completemessage, "%s %s", timestamp, errormessage);
  perror(completemessage);
  exit(-1);
}

void open_socket()
{
  /* creates a socket for upcoming communication */

  struct sockaddr_in ownaddress;

  sockethandler = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP); /*create an UDP socket*/
  if (sockethandler < 0 )
    {
      error("Sockethandler could not be created.");
    }
  
  memset( &ownaddress, 0, sizeof(ownaddress)); /* Initialize ownaddress with zeroes */
  ownaddress.sin_family = AF_INET;
  ownaddress.sin_addr.s_addr = htonl(inet_network(CLIENTADDRESS)); /* only accept connections from this IP, could be set to INADDR_ANY to accept all IPs */
  ownaddress.sin_port = htons(PORTNUMBER);

  if (bind(sockethandler, (struct sockaddr *) &ownaddress, sizeof(ownaddress)) < 0)
    {
      error("Error during bind process.");
    }
}

void close_socket()
{
  close(sockethandler);
}

char receivefromclient()
{
  char receivedata[RECEIVE_DATA_LENGTH];
  if (recvfrom(sockethandler, receivedata, RECEIVE_DATA_LENGTH, 0, (struct sockaddr *) &clientaddress, &clientaddresslength) < 0)
    {
      error("Data could not be received.");
    }
  return *receivedata;
}

bool evaluaterequest(const char *request, const char *comparevalue)
{
  /* checks if a proper request has been made by the client */
  bool returncode = true;
  if ( *request != *comparevalue )
    {
      fprintf(stderr, "No data was send due to wrong request code.\n");
      fprintf(stderr, "Request code was: %i, Expected: %i\n", *request, *comparevalue); /*FIXME: use atoi?, does not work if we deal with strings */
      /* FIXME: use error() in here somehow */
      returncode = false;
    }
  return returncode;
}

void sendtoclient(const char *senddata)
{
  /* send data to a client */
  if (sendto(sockethandler, senddata, SEND_DATA_LENGTH, 0, (struct sockaddr *) &clientaddress, clientaddresslength) < 0)
    {
      error("Error while sending data.");
    }
}


int main()
{
  open_socket();

  /* begin main server part */
  clientaddresslength = sizeof(struct sockaddr_in);
  while( true )
    {
      /* wait for request of data */
      char requestresult;
      requestresult = receivefromclient();
      if ( evaluaterequest(&requestresult, "1") == false ) { continue; /* start all over and wait for next request */ }

      /* answer the request */
      sendtoclient("Hallo Welt!\n");
    }
  
  close_socket();
  return 0;
}
