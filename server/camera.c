#include "camera.h"

struct ImageData camera_ReadImage(struct ImageDimensions Dimensions)
{
  // FIXME cropping is missing here

  struct ImageData return_struct;
  return_struct.Image = camera_ReadImageRaw();
  return_struct.ImageSize = SIZERAWIMAGE; /* FIXME replace with some sizeof because of cropping */

  return return_struct;
}

static unsigned char* camera_ReadImageRaw () {
  // FIXME interface to capture.h goes in here

  FILE *file;
  file = fopen("testimage.bmp", "rb");

  if(file == NULL)
    {
      error(__FUNCTION__ , "Could not read the image.");
      //FIXME for final implementation: try again (a couple of times)
    }

  //static unsigned char image_array[SIZERAWIMAGE];
  //allocate memory on the heap
  unsigned char *image_array = (unsigned char *)malloc(SIZERAWIMAGE+1);
  if (!image_array) {
    error(__FUNCTION__ , "No space for image data. Cannot continue.");
  }

  fread(image_array, SIZERAWIMAGE, 1, file); //FIXME use return value?!
  fclose(file);

  return image_array;
}
