--------------------------------------------------------------------------------
-- Package: Adaimageprocessor.Protocol
-- Purpose:
-- Implement the primitive UDP-based communication protocol between the server
-- (camera) and the client (this program)
--
-- Effects:
-- Abstracts the underlying <Adaimageprocessor.Protocol.Socket> 
-- package.
--
--------------------------------------------------------------------------------

   
--------------------------------------------------------------------------------
-- Headers: Adaimageprocessor.Protocol
-- Ada.Strings - Functions for String handling
-- Ada.Strings.Fixed - Functions for Strings with fixed length
-- Ada.Streams - Work with arbitrary streamed data
-- Adaimageprocessor.Socket - Handles the raw UDP
-- socket communication
--------------------------------------------------------------------------------
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Streams;
with Adaimageprocessor.Socket;

package Adaimageprocessor.Protocol is
   package SOCKETCOMM renames Adaimageprocessor.Socket;
   package STRINGLIB renames Ada.Strings;
   package STRINGFIXEDLIB renames Ada.Strings.Fixed;
   
   -----------------------------------------------------------------------------
   -- Package: Image_Max_Dimensions
   -- Purpose:
   --   Hide the actual Max_X and Max_Y values from being manipulated directly.
   --
   -- Functions: Image_Max_Dimensions
   --  Image_Max_Dimensions.X - returns the maximum X pixel value
   --  Image_Max_Dimensions.Y - returns the maximum Y pixel value
   --  Is_Set - returns true if the size has been retrieved already; false if
   --  not
   --
   -- Procedure: Set
   --  Purpose: Communicates with the server and sets the Max_X and Max_Y values
   --  Exceptions: COMMUNICATION_ERROR
   -----------------------------------------------------------------------------
   package Image_Max_Dimensions is
      function X return Natural;
      function Y return Natural;
   private
      procedure Set;
   end Image_Max_Dimensions;
   
   -----------------------------------------------------------------------------
   -- Package: Adaimageprocessor.Protocol
   -----------------------------------------------------------------------------
   
   -----------------------------------------------------------------------------
   -- Types: Adaimageprocessor.Protocol
   --  Number_Of_Chunks - an Integer subtype covering all natural numbers up
   --  to 6728, which is the maximum number to be expected for an image
   --  Width_Of_Image - an Integer subtype covering all natural numbers up to
   --  960, which is the maximum width of an image captured by the camera
   --  Height_Of_Image - an Integer subtype covering all natural numbers up to
   --  1280, which is the maximum height of an image captured by the camera
   --  Image_Dimensions - A record specifying the valid X,Y dimensions of a
   --  subimage
   --  Image_Chunk_Data - same as <Adaimageprocessor.Socket.Transmittable_Data_Array>
   --
   --  Image_Chunks - a type defining an array of Chunks with the
   --  dimension of <Number_Of_Chunks> that can hold the actual image chunks
   --  FIXME
   -----------------------------------------------------------------------------
   subtype Number_Of_Chunks is Natural range 0 .. 6728; -- FIXME make server-dependant, relevant for Image_Chunk_Data!
   
   --subtype Width_Of_Image is Natural range 0 .. Image_Max_Dimensions.X;
   --subtype Height_Of_Image is Natural range 0 .. Image_Max_Dimensions.Y;
   subtype Width_Of_Image is Natural range 0 .. 960;
   subtype Height_Of_Image is Natural range 0 .. 1280;
   
   type Image_Dimensions is
      record
	 Top_Left_X : Width_Of_Image;
	 Top_Left_Y : Height_Of_Image;
	 Bottom_Right_X : Width_Of_Image;
	 Bottom_Right_Y : Height_Of_Image;
      end record;
   
   use type Ada.Streams.Stream_Element_Offset;
   subtype Image_Chunk_Data is SOCKETCOMM.Transmittable_Data_Array;
   subtype Image_Chunk_Data_NoNumber is Ada.Streams.Stream_Element_Array(SOCKETCOMM.Transmittable_Data_Array'First+4 .. SOCKETCOMM.Transmittable_Data_Array'Last);
   type Image_Chunks is array ( Number_Of_Chunks range <> ) of Image_Chunk_Data_NoNumber;
   
   -----------------------------------------------------------------------------
   -- Function: Request_Next_Image
   -- Purpose:
   --   Ask the server to internally grab a new image from the camera and
   --   prepare it for transmission (i.e. crop it to the given size)
   --
   -- Effects:
   --   FIXME.
   --
   -- Parameters:
   --  Subimage_Dimensions - a record containing the upper left and lower right
   --  corner of the requested subimage
   --
   -- Returns:
   --  The number of chunks this image will be split into.
   --
   -- Exceptions:
   --  ImageDimensionError - The dimensions of the requested imaged do not make
   --  sense (i.e. the lower right corner is left of/higher up than its upper
   --  left counterpart
   --  CommunicationError - The server did not answer the request correctly
   -----------------------------------------------------------------------------
   function Request_Next_Image ( Subimage_Dimensions : Image_Dimensions )
			       return Number_Of_Chunks;
   
   
   -----------------------------------------------------------------------------
   -- Function: Request_Chunks
   -- Purpose:
   --   Requests all chunks subsequently.
   --
   -- Parameters:
   --   Chunks - How many chunks to receive
   --
   -- Returns:
   --   The image chunks in an unsorted array, due to the underlying
   --   UDP-protocol.
   -----------------------------------------------------------------------------
   function Request_Chunks ( Chunks : in Number_Of_Chunks ) return Image_Chunks;
   
      
private
   -----------------------------------------------------------------------------
   -- Section: Private
   -----------------------------------------------------------------------------
   
   -----------------------------------------------------------------------------
   -- Function: Process_Image_Size
   -- Purpose:
   --   Helper function to pad the specified image size correctly to
   --   four characters
   --
   -- Parameters:
   --   Size - The number to be padded
   --
   -- Returns:
   --   A four character string containing value of <Process_Image_Size.Size>
   --   padded with zeroes from the left.
   --
   -- Exceptions:
   --   Length_Error - The input parameter <Process_Image_Size.Size> has a value
   --   greater than 9999
   -- FIXME
   -----------------------------------------------------------------------------
   function Process_Image_Size ( Size : in Natural ) return String;
   
   -----------------------------------------------------------------------------
   -- Procedure: Camera_Error
   -- Purpose:
   --   Processes error message received from the Camera
   --
   -- Effects:
   --   FIXME
   --
   -- Parameters:
   --   Erroressage - A Stream containing the current received packet.
   --
   -- Returns:
   --   A string with the error message
   --
   -- Exceptions:
   --   None.
   -----------------------------------------------------------------------------
   function Camera_Error (Errormessage: in Image_Chunk_Data) return String;
   
end Adaimageprocessor.Protocol;
