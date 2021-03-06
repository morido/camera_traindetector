#include "protocol.h"

char* protocol_GetRequest()
{
  char* result = socket_ReceiveFromClient();

  if (result[0] != 'I')
    {
      error(__FUNCTION__, "Server request malformed");
    }

  return result;
}

void protocol_PrepareNextImage (char* rawrequest)
{
  struct ImageData imagedata;

  /* read image dimensions */
  struct ImageDimensions Dimensions;
  Dimensions.top_left_x = protocol_DimensionConversion (rawrequest, 2);
  Dimensions.top_left_y = protocol_DimensionConversion (rawrequest, 6);
  Dimensions.bottom_right_x = protocol_DimensionConversion (rawrequest, 10);
  Dimensions.bottom_right_y = protocol_DimensionConversion (rawrequest, 14);

  /* calulate necessary number of chunks */
  imagedata = camera_ReadImage(Dimensions);
  CurrentImage = imagedata.Image;
  if ( useableMAXPACKETSIZE > 0 )
    {
      float chunkscountraw = imagedata.ImageSize / (float)useableMAXPACKETSIZE;
      Chunkscount = (int)ceil(chunkscountraw);
    }
  else
    {
      error(__FUNCTION__, "Implementation Error. Check sourcecode.");
    }

  /* we can only cope with chunks whose countnumber does not exceed four
     digits */
  if (Chunkscount > 9999)
    {
      error(__FUNCTION__, "Image size too large.");
    }

  /* care about the very last chunk, whose size may differ */
  if (Chunkscount > 1)
    {
      LastChunkSize = imagedata.ImageSize % ((Chunkscount-1)*useableMAXPACKETSIZE);
      if ( LastChunkSize == 0 )
	{
	  /* rare case, meaning Chunkscount == chunkscountraw */
	  LastChunkSize = useableMAXPACKETSIZE;
	}
    }
  else
    {
      /* we only have one single chunk */
      LastChunkSize = imagedata.ImageSize;
    }

  {
    /* Send to client */
    char transmitstring[2+CHUNKIDWIDTH+1];
    snprintf(transmitstring, sizeof(transmitstring), "IN%s", protocol_ChunkidToString(Chunkscount));
    socket_SendToClient(transmitstring, sizeof(transmitstring));
  }
}

void protocol_TransmitChunks ()
{
  /* FIXME
     helper function get_next_chunk */

  /* check for preconditions */
  if ((CurrentImage == NULL) || (Chunkscount == -1))
    {
      error(__FUNCTION__, "No access to current image.");
      /* this happens e.g. if the udp-handshake was not complete (only IC instead of IN+IN+IC-triple)*/
    }

  /* reply to request */
  socket_SendToClient("IC", sizeof("IC"));

  {
    int i;
    for (i=0; i<Chunkscount-1; i++)
      {
	socket_SendToClient(protocol_GetNextChunk(i), MAXPACKETSIZE);
	/* FIXME add real congestion-control here */
	{
	  const struct timespec sleeptimer = {
	    .tv_sec = 0,
	    .tv_nsec = 0L
	  };
	  nanosleep(&sleeptimer, (struct timespec *) NULL);
	  /* nanosleep((struct timespec[]){{0, 0}}, NULL); sleep for as short as possible */
	}
      }
    /* very last chunk */
    socket_SendToClient(protocol_GetNextChunk(i), LastChunkSize+CHUNKIDWIDTH);
  }

  /* free the memory for the image */
  free(CurrentImage);
  /* reset the pointer to the CurrentImage, so protocol_PrepareNextImage()
     has to be called again for the next transfer */
  CurrentImage = NULL;
  /* same for Chunkscount */
  Chunkscount = -1;
}

static int protocol_DimensionConversion (char* rawrequest, int offset)
{
  long return_value;
  char *conversionendpointer = NULL;
  char tempsize[5]; /* snprintf writes nullterminators so we need 4+1 in size here! */
  snprintf(tempsize, sizeof(tempsize), "%c%c%c%c", rawrequest[offset], rawrequest[offset+1], rawrequest[offset+2], rawrequest[offset+3]);

  errno = 0;
  return_value = strtol(tempsize, &conversionendpointer, 10);

  /* check for possible errors; there are more (see man 3 strtol) but others
     cannot occur here.
     Note: This is not MISRA-compliant as the use of errno is discuraged there
   */
  if ((errno != 0 && return_value == 0) || (conversionendpointer == tempsize))
    {
      error(__FUNCTION__ , "Conversion failed.");
    }
  return (int)return_value;
}


static char* protocol_GetNextChunk (int chunkid)
{
  static char return_chunk[MAXPACKETSIZE];
  int readuntil = useableMAXPACKETSIZE;

  /* The client starts counting with 1, so chunkid+1 */
  memcpy(return_chunk, protocol_ChunkidToString(chunkid+1), CHUNKIDWIDTH);
  /* FIXME faster alternative that does not \0 the other elements? */

  /* special treatment for the very last chunk */
  if ( chunkid+1 == Chunkscount )
    {
      readuntil = LastChunkSize;
    }

  {
    /* write imagedata */
    int i;
    for (i=0; i<readuntil; i++)
      {
	/* FIXME: this line is wrong. fix it!
	we are reading one byte too far ahead... */
	return_chunk[i+CHUNKIDWIDTH] = CurrentImage[chunkid*useableMAXPACKETSIZE+i];
    }
  /* Null-Terminate our result */
  /* return_chunk[i+1] = '\0'; */
    /* FIXME get the above line back in -- makes sense there */
  }

  return return_chunk;
}

static char* protocol_ChunkidToString (int chunk)
{
  /* the format-string adds appropriate zero-padding */
  char format[5];
  static char chunkid_as_string[5];
  snprintf(format, sizeof(format), "%%0%dd", CHUNKIDWIDTH);
  snprintf(chunkid_as_string, CHUNKIDWIDTH+1, format, chunk);
  return chunkid_as_string;
}
