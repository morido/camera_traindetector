/*

Package: Socket

Purpose: Provide raw UDP-socket commmunication facilities

*/

/*
  Headers:
  sys/types.h - POSIX Primitive System Data Types
  sys/socket.h - Socket Library
  netinet/in.h - IP-Communication
  arpa/inet.h - conversion of IP-addresses
  string.h - memset() and friends
*/

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <string.h>
#include "globalconstants.h"


extern int error(const char *caller, const char *errormessage);


/*
  Constants:
  CLIENTADDRESS - The IP-address of the client to communicate with
  PORTNUMBER - The port used for communication with the client
*/
static const char CLIENTADDRESS[16]="127.0.0.1";
//static const char CLIENTADDRESS[16]="192.168.56.1";
static const int PORTNUMBER=12345;



/*
   Typedefs:
   SOCKET - special type used for the the sockethandler
*/
typedef int SOCKET;


/*
  Structs:
  clientaddress - FIXME A struct contraining the <CLIENTADDRESS> and
  <PORTNUMBER> in a format readable for C FIXME correct?
*/
struct sockaddr_in clientaddress;

/*
  Variables:
  sockethandler - The handler to the socket used for the communication with
  the client, set by <open_socket>
  clientaddresslength - The size of the <clientaddress>-struct
*/
SOCKET sockethandler = -1;
socklen_t clientaddresslength = sizeof(clientaddress);


/*
Function: socket_open
   Purpose:
   Creates a socket for the upcoming communication

   Parameters:
   None.

   Returns:
   Nothing.
*/
void socket_open();

/*
Function: socket_close
   Purpose:
   Close the socket opened previousely by <socket_open>

   Parameters:
   None.

   Returns:
   Nothing.
*/
void socket_close();

/*
Function: socket_receivefromclient
   Purpose:
   Receive data from the client (the controlling Ada-Program)

   Parameters:
   None.

   Returns:
   A pointer to a char-array containing the received data.
*/
char* socket_ReceiveFromClient();

/*
Function: socket_sendtoclient
   Purpose:
   Send data to the client (the controlling Ada-Program)

   Parameters:
   A pointer to the char-array to be transmitted.

   Returns:
   Nothing.
*/
void socket_SendToClient(const char *senddata, const int length);

/*
Function: socket_sendtoclient
   Purpose:
   Try to send errordata to the client (the controlling Ada-Program). If the
   socket is not properly set up this will silently fail.

   Parameters:
   A pointer to the char-array to be transmitted.

   Returns:
   Nothing.
*/
void socket_SendErrorToClient(const char *senddata, const int length);

/*
Section: static
*/

/*
Function: socket_Precheck
   Purpose:
   Check if the socket is set up properly

   Parameters:
   none.

   Returns:
   nothing.
*/
static void socket_Precheck();
