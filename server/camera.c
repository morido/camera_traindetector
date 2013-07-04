#include "camera.h"

struct ImageData camera_ReadImage(struct ImageDimensions Dimensions)
{
  /* calulate resulting image size; make sure to add 1 because dimensions are zero-based */
  int imagesize = (Dimensions.bottom_right_x-Dimensions.top_left_x+1) * (Dimensions.bottom_right_y-Dimensions.top_left_y+1);

  struct ImageData return_struct;
  return_struct.Image = camera_ReadImageRaw(Dimensions, imagesize);
  return_struct.ImageSize = imagesize;

  return return_struct;
}

static unsigned char* camera_ReadImageRaw (struct ImageDimensions Dimensions, int resulting_imagesize) {
  // FIXME interface to capture.h goes in here

  FILE *inputfile;
  //file = fopen("testimage.bmp", "rb");
  inputfile = fopen("/home/morido/Dokumente/uni/9_semester/studienarbeit/software/client_traindetection/52.pgm", "rb");
  //inputfile = fopen("/tmp/38.pgm", "rb");


  if(inputfile == NULL)
    {
      error(__FUNCTION__ , "Could not read the image.");
      //FIXME for final implementation: try again (a couple of times)
    }

  //allocate memory on the heap
  unsigned char *image_array = (unsigned char *)malloc(resulting_imagesize);
  if (!image_array) {
    error(__FUNCTION__ , "No space for image data. Cannot continue.");
  }

  char currentchar;
  /* skip the header */
  while(getc(inputfile) != '\n');
  while (getc(inputfile) == '#') /*this is the comments field*/
    {
      while (getc(inputfile) != '\n');
    }
  while(getc(inputfile) != '\n');
  while(getc(inputfile) != '\n');

  /* read in the raw image data with respect to the given Dimensions */
  {
    int row, column;
    int i = 0;
    fseek(inputfile, Dimensions.top_left_y * IMAGEWIDTH, SEEK_CUR);
    for (row = Dimensions.top_left_y; row <= Dimensions.bottom_right_y; row++)
      {
	fseek(inputfile, Dimensions.top_left_x, SEEK_CUR);
	for (column = Dimensions.top_left_x; column <= Dimensions.bottom_right_x; column++)
	  {
	    currentchar = getc(inputfile);
	    image_array[i] = currentchar;
	    i++;
	  }
	fseek(inputfile, IMAGEWIDTH -1 - Dimensions.bottom_right_x, SEEK_CUR);
      }
  }


  fclose(inputfile);

  return image_array;
}
