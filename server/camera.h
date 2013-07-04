/*
Package:
Camera.h

Purpose:
Provide an interface to capture.h to read raw camera data

*/

/*
includes:
stdlib - for dynamic memory allocation, i.e. malloc()
stdio - for file operations FIXME may become obsolete in final version; only for camera_ReadImageRaw()
imageproperties.h - Several structs to hold image-relevant data
*/
#include <stdlib.h>
#include <stdio.h>
#include "imageproperties.h"

extern int error(const char *caller, const char *errormessage);

/*
defines:
IMAGEWIDTH - the width of the image in pixels
*/
#define IMAGEWIDTH 572


/*
   Function:
   camera_ReadImage

   Purpose:
   Read and Crop an image from the camera


   Parameters:
   ImageDimensions - see <ImageDimensions>


   Returns:
   An <ImageData>-struct.


   Exceptions:
   Returns an error if any of the x/y-coordinate values are malformed
*/
struct ImageData camera_ReadImage(struct ImageDimensions Dimensions);

/*
section: Static
*/

/*
   Function:
   camera_ReadImageRaw

   Purpose:
   Read an image from the camera

   Parameters:
   ImageDimensions - see <ImageDimensions>
   resulting_imagesize - the calculated size of the resulting image


   Returns:
   A pointer to the obtained image data
*/
static unsigned char* camera_ReadImageRaw(struct ImageDimensions Dimensions, int resulting_imagesize);
