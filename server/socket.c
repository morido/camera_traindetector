#include "socket.h"

void socket_open()
{
  struct sockaddr_in ownaddress;

  /* create an UDP socket */
  sockethandler = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);


  /* Initialize ownaddress with zeroes */
  memset( &ownaddress, 0, sizeof(ownaddress));

  ownaddress.sin_family = AF_INET;
  /* only accept connections from CLIENTADDRESS
     could be set to INADDR_ANY to accept all IPs */
  ownaddress.sin_addr.s_addr = htonl(inet_network(CLIENTADDRESS));
  ownaddress.sin_port = htons(PORTNUMBER);

  if (bind(sockethandler, (struct sockaddr *) &ownaddress, sizeof(ownaddress)) < 0)
    {
      error(__FUNCTION__, "Bind error.");
    }

  {
  /* set send and receive buffers */
  /* FIXME: is this necessary? */
    int tmp = MAXPACKETSIZE;
    if ( setsockopt(sockethandler, SOL_SOCKET, SO_SNDBUF, &tmp, sizeof(tmp)) < 0)
      {
	error(__FUNCTION__, "Error setting sendbuffer.");
      }
    if ( setsockopt(sockethandler, SOL_SOCKET, SO_RCVBUF, &tmp, sizeof(tmp)) < 0)
      {
	error(__FUNCTION__, "Error setting receivebuffer.");
      }
  }
}

void socket_close()
{
  close(sockethandler);

  /* reset condition for socket_Precheck() */
  sockethandler = -1;
}

char* socket_ReceiveFromClient()
{
  static char receivedata[MAXPACKETSIZE+1]; /* +1 for stopbit, i.e. \0 */
  /* receive data from the (Ada-)Client */
  socket_Precheck();

  if (recvfrom(sockethandler, receivedata, MAXPACKETSIZE, 0, (struct sockaddr *) &clientaddress, &clientaddresslength) < 0)
    {
      error(__FUNCTION__ , "Data could not be received.");
    }
  return receivedata;
}


void socket_SendToClient(const char *senddata, const int length)
{
  /* send data to the client */
  socket_Precheck();
  if (sendto(sockethandler, senddata, length, 0, (struct sockaddr *) &clientaddress, clientaddresslength) < 0)
    {
      error(__FUNCTION__ , "Error while sending data.");
    }
}


void socket_SendErrorToClient(const char *senddata, const int length)
{
  /* try to send data regardless of the state of the socket */
  if (sockethandler > 0)
    {
      sendto(sockethandler, senddata, length, 0, (struct sockaddr *) &clientaddress, clientaddresslength);
    }
}

static void socket_Precheck(void)
{
  if (sockethandler < 0)
    {
      /* Socket is not open yet, fix it */
      socket_open();
    }
}
