#include "imagetransfer.h"

void imagetransfer_ServeImageRequest()
{

  /* Step 1:
     get request
  */

  char* request = protocol_GetRequest();

  /* we are only checking for the 2nd character; the 1st is in
     protocol_GetRequest() */
  if (request[1] == 'N')
    {
      protocol_PrepareNextImage(request);
    }
  else if (request[1] == 'C')
    {
      protocol_TransmitChunks();
    }
  else
    {
      error(__FUNCTION__ , "Erroneous request.");
    }
}

