/*
   Package: Imageproperties

   Purpose:
   Defines various storage-types used for the imagedata.

 */

/*
   structs: Imageproperties


ImageData - A struct containing both the Image itself as well as the size of the
array in which it is saved
ImageDimensions - A struct containing the corner-pixels of the imagearea of interest
 */
struct ImageData {
  unsigned char* Image;
  size_t ImageSize;
};

struct ImageDimensions {
  int top_left_x;
  int top_left_y;
  int bottom_right_x;
  int bottom_right_y;
};
