#include <sys/types.h> /* POSIX Primitive System Data Types */
#include <stdlib.h> /* for exit() */
#include <sys/socket.h> /* Socket Library */
#include <netinet/in.h> /* IP-Communication */
#include <stdio.h>
#include <string.h> /* for memset() */

static const int PORTNUMBER=12345;
//static const char CLIENTADDRESS=192.168.56.101;
static const int RECEIVE_DATA_LENGTH = 1;
static const int SEND_DATA_LENGTH = 12;

/* FIXME! ungarische Notation verwenden! */

typedef int SOCKET;

void error(const char *errormessage)
{
  /* fatal */
  perror(errormessage);
  exit(1);
}

int main()
{
  SOCKET sockethandler;
  struct sockaddr_in ownaddress, clientaddress;
  socklen_t clientaddresslength;
  char receivedata[RECEIVE_DATA_LENGTH];
  char senddata[SEND_DATA_LENGTH];
  

  sockethandler = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP); /*create an UDP socket*/
  if (sockethandler < 0 )
    {
      error("Sockethandler could not be created.");
    }
  
  memset( &ownaddress, 0, sizeof(ownaddress)); /* Initialize ownaddress with zeroes */
  ownaddress.sin_family = AF_INET;
  //ownaddress.sin_addr.s_addr = htonl(CLIENTADDRESS); /* only accept connections from this IP */
  ownaddress.sin_addr.s_addr = INADDR_ANY;
  ownaddress.sin_port = htons(PORTNUMBER);

  if (bind(sockethandler, (struct sockaddr *) &ownaddress, sizeof(ownaddress)) < 0)
    {
      error("Error during bind process");
    }
  
  /* begin main server part */
  clientaddresslength = sizeof(struct sockaddr_in);
  while(1)
    {
      if (recvfrom(sockethandler, receivedata, RECEIVE_DATA_LENGTH, 0, (struct sockaddr *) &clientaddress, &clientaddresslength) < 0)
	{
	  error("Data could not be received.");
	}
      if ( *receivedata != '1' )
	{
	  fprintf(stderr, "No data was send due to wrong request code.\n");
	  fprintf(stderr, "Request code was: %s, Should be: 1.\n", receivedata);
	  continue;
	}
      /* answer the request */
      //do something with senddata!
      strcpy(senddata, "Hallo Welt!\n");
      if (sendto(sockethandler, senddata, SEND_DATA_LENGTH, 0, (struct sockaddr *) &clientaddress, clientaddresslength) < 0)
	{
	  error("Error while sending data.");
	}
    }
  close(sockethandler);
  return 0;
}

