#include "generic.h"

void precheck()
{

  /* check if a character is 8 bits wide
     important for socket-communication */
  if (CHAR_BIT != 8)
    {
      error(__FUNCTION__ , "Your platform is not supported.");
    }
}


int error(const char *caller, const char *errormessage)
{
  /* make sure to use the | (pipe) as the escape character */

  time_t now;
  time(&now); /* get current time */
  char timestamp[23];
  char completemessage[76]; /* fixed size to fit in one line */

  strftime(timestamp, sizeof timestamp, "%F %T.00", localtime(&now)); /* decisecond precision not implemented */
  snprintf(completemessage, sizeof(completemessage), "%s %s: %s", timestamp, caller, errormessage); /* errormessages with too many chars will be silently truncated, which is safe here */
  fprintf(stderr, "%s\n", completemessage);

  /*try to send the errormessage to the client via the socket */
  snprintf(completemessage, sizeof(completemessage), "ER%s %s: %s", timestamp, caller, errormessage);
  socket_SendErrorToClient(completemessage, sizeof(completemessage));

  socket_close();
  exit(EXIT_FAILURE);
}
