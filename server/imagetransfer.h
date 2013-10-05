/*
   Package: Imagetransfer

   Purpose:
   Handle the entire transfer process to the client, thus abstract <Protocol>
*/

extern char* protocol_GetRequest(void);
extern void protocol_PrepareNextImage(char* rawrequest);
extern void protocol_TransmitChunks (void);
extern int error(const char *caller, const char *errormessage);


/*
   Function: imagetransfer_ServeImageRequests

   Purpose:
   Answer all possible requests by the client

   Parameters:
   None.

   Returns:
   Nothing.
*/
void imagetransfer_ServeImageRequest(void);
