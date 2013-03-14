#include "server.h"

int main()
{
  while( 1 )
    {
      imagetransfer_ServeImageRequest();
    }

  return EXIT_SUCCESS;
}
