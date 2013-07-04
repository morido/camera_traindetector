/*
Package: Imagetransfer
Purpose: Handle the entire transfer process to the client, thus abstract <Protocol>
*/

extern char* protocol_GetRequest();
extern void protocol_PrepareNextImage();
extern void protocol_TransmitChunks ();
extern void protocol_ReturnMaxImageSize();
extern int error(const char *caller, const char *errormessage);


/*
Function: imagetransfer_ServeImageRequests
Purpose: Answer all possible requests by the client
Parameters: None.
Returns: Nothing.
*/
void imagetransfer_ServeImageRequest();
