
/*
Package: Protocol

Purpose:
Implement a protocol to communicate with the client, hence abstract the
functions in <Socket>
*/

/*
defines:
   CHUNKIDWIDTH - width of the the chunkid in bytes
   NULLTERMINATOR - width of the null-terminator byte
   _XOPEN_SOURCE - the timespec of nanosleep is not C99, so this tells the
   compiler we want POSIX defitions in here
*/
#define CHUNKIDWIDTH 4
/* do not set higher than 9, protocol_GetNextChunk() would break otherwise; client will break with any value =! 4 */
#define NULLTERMINATOR 1
#define _XOPEN_SOURCE 500

/*
   includes:
   stdio.h - for snprintf()
   math.h - for ceil(), i.e. to round-up a number
   stdlib.h - for free()
   errno.h - to catch errors of strtol()
   string.h - for memcpy()
   time.h - for nanosleep()
*/
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <time.h>
#include "globalconstants.h"
#include "imageproperties.h"

extern char* socket_ReceiveFromClient(void);
extern void socket_SendToClient(const char *senddata, const int length);
extern struct ImageData camera_ReadImage(struct ImageDimensions Dimensions);
extern int error(const char *caller, const char *errormessage);



/*
constants:
useableMAXPACKETSIZE - useable part <socket.MAXPACKETSIZE> for imagetransfer
*/
static const int useableMAXPACKETSIZE = MAXPACKETSIZE - CHUNKIDWIDTH - NULLTERMINATOR;

/*
variables:
CurrentImage - temporal storage for the image currently in use
Chunkscount - temporal storage for the number of chunks of the current image
LastChunkSize - The size of the very last chunk (which can be smaller than MAXPACKETSIZE)
*/
static unsigned char* CurrentImage = NULL;
static int Chunkscount = -1;
static int LastChunkSize = 0;

/*
   Function: protocol_GetRequest

   Purpose:
   Process requests by the client

   Parameters:
   None.

   Effects:
   Will block until request is made.

   Returns:
   The request as a char-array.
*/
char* protocol_GetRequest(void);

/*
   Function: protocol_PrepareNextImage

   Purpose:
   Prepare the next image for transmission (i.e. get it and crop it)

   Effects:
   Sets <CurrentImage> and <Packetcount> for the upcoming transfer

   Parameters:
   rawrequest - A raw request (string) asking for the next image.

   Returns:
   nothing.
*/
void protocol_PrepareNextImage (char* rawrequest);

/*
   Function: protocol_TransmitChunks

   Purpose:
   Transmit the image-chunks to the client.

   Returns:
   nothing. (since this is an UDP-transfer)
*/
void protocol_TransmitChunks (void);

/*
Section: Static
*/

/*
   Function: protocol_DimensionConversion

   Purpose:
   A helper function to convert the string containing the subimage dimensions into proper integers.

   Parameters:
   rawrequest - The raw array returned by the client containing the dimensions
   offset - First character to read

   Returns:
   an integer with the requested pixel value
*/
static int protocol_DimensionConversion (char* rawrequest, int offset);

/*
   Function: protocol_GetNextChunk

   Purpose:
   Extract the next chunk of imagedata from the image

   Parameters:
   chunk - the id of the chunk to be processed

   Returns:
   The extracted chunk
*/
static char* protocol_GetNextChunk (int chunkid);


/*
   Function: protocol_ChunkidToString

   Purpose:
   Adds appropriate zero-padding to chunk-ids

   Parameters:
   chunkid - the id to be processed

   Returns:
   The chunkid as a string with zero-padding
*/
static char* protocol_ChunkidToString (int chunkid);
