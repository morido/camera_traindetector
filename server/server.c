#include "server.h"

void error(const char *errormessage)
{
  /* print errormessage including timestamp (ISO 8601) */

  /* unfortunately there is no full-fledged exception-handling in C as is with C++/Ada, so we have to deal with it this way */
  
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
  /* closes the socket opened by open_socket() */

  close(sockethandler);
}

char receivefromclient()
{
  /* receive data from the (Ada-)Client */

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
  /* send data to the client */
  if (sendto(sockethandler, senddata, SEND_DATA_LENGTH, 0, (struct sockaddr *) &clientaddress, clientaddresslength) < 0)
    {
      error("Error while sending data.");
    }
}

char* readimage()
{
  /* reads the whole camera-image and saves it for later use */

  /* FIXME: add appropriate SDK specific code here */

  /* right now we're only reading from a file, everything hardcoded  */

  FILE *rawimagefile;
  int current_character;
  //static char imagedata[size??!]; /* memory will be reused each time we call the function, hence "static" */

  rawimagefile = fopen("testimage.bmp", "r"); /* FIXME, no error handling here, change with SDK! */
  while ( (current_character=getc(rawimagefile)) != EOF) { }

  fclose(rawimagefile);
}

char* readimagechunk(const char* image, const int offset, const int length)
{
  /* reads a chunk of the image from the buffer created by readimage() */
  const int chunksize = 500; /* FIXME: take guaranteed minimum size in IPv4 */
}

int number_of_imagechunks(const char* image)
{
  /* return the number of chunks required to transmit a certain image */
  const int chunksize = 500; /* FIXME: make globally available, see above */
  int result;
  const int imagesize = sizeof(*image);

  if ( imagesize % chunksize == 0 ) { result = (int)(imagesize/chunksize); }
  else { result = (int)(1+imagesize/chunksize); }
  return result;
}


int main()
{
  open_socket();

  /* main server part */
  clientaddresslength = sizeof(struct sockaddr_in);
  while( true )
    {
      /* wait for data request */
      char requestresult;
      requestresult = receivefromclient();
      if ( evaluaterequest(&requestresult, "1") == false ) { continue; /* start all over and wait for next request */ }

      /* answer the request */
      sendtoclient("Hallo Welt!\n");
    }
  
  close_socket();
  return 0;
}
