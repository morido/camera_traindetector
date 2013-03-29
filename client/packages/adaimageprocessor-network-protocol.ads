--------------------------------------------------------------------------------
-- Package: Adaimageprocessor.Network.Protocol
-- Purpose:
-- Implement the primitive UDP-based communication protocol between the server
-- (camera) and the client (this program)
--
-- Effects:
-- Abstracts the underlying <Adaimageprocessor.Network.Protocol.Socket>
-- package.
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Headers: Adaimageprocessor.Network.Protocol
-- Ada.Strings.Fixed - Functions for Strings with fixed length
-- Adaimageprocessor.Network.Socket - Handles the raw UDP
-- socket communication
--------------------------------------------------------------------------------
private with Ada.Strings.Fixed;
with Adaimageprocessor.Network.Socket;

package Adaimageprocessor.Network.Protocol is
   package SOCKETCOMM renames Adaimageprocessor.Network.Socket;

   -----------------------------------------------------------------------------
   -- Package: Adaimageprocessor.Network.Protocol
   -----------------------------------------------------------------------------

   -----------------------------------------------------------------------------
   -- Types: Adaimageprocessor.Network.Protocol
   --  Number_Of_Chunks - an Integer subtype covering all natural numbers up
   --  to 6728, which is the maximum number to be expected for an image
   --  Width_Of_Image - an Integer subtype covering all natural numbers up to
   --  960, which is the maximum width of an image captured by the camera
   --  Height_Of_Image - an Integer subtype covering all natural numbers up to
   --  1280, which is the maximum height of an image captured by the camera
   --  Image_Dimensions - A record specifying the valid X,Y dimensions of a
   --  subimage
   --  Image_Chunk_Data - same as <Adaimageprocessor.Network.Socket.Transmittable_Data_Array>
   --
   --  Image_Chunks - a type defining an array of Chunks with the
   --  dimension of <Number_Of_Chunks> that can hold the actual image chunks
   --  FIXME
   -----------------------------------------------------------------------------
   subtype Number_Of_Chunks is Natural range 1 .. 6790; -- FIXME make server-dependant, relevant for Image_Chunk_Data!

   subtype Width_Of_Image is Natural range 0 .. 960;
   subtype Height_Of_Image is Natural range 0 .. 1280;

   type Image_Dimensions is
      record
	 Top_Left_X : Width_Of_Image;
	 Top_Left_Y : Height_Of_Image;
	 Bottom_Right_X : Width_Of_Image;
	 Bottom_Right_Y : Height_Of_Image;
      end record;

   use type STREAMLIB.Stream_Element_Offset;
   subtype Image_Chunk_Data is SOCKETCOMM.Transmittable_Data_Array;
   subtype Image_Chunk_Data_NoNumber is STREAMLIB.Stream_Element_Array(SOCKETCOMM.Transmittable_Data_Array'First+4 .. SOCKETCOMM.Transmittable_Data_Array'Last-1);

   type Image_Chunks_type is array ( Number_Of_Chunks'Range ) of Image_Chunk_Data_NoNumber;
   type Chunk_Data is record
      Image_Chunks : Image_Chunks_type;
      Last_Chunk_Offset : STREAMLIB.Stream_Element_Offset;
   end record;


   -----------------------------------------------------------------------------
   -- Function: Request_Next_Image
   -- Purpose:
   --   Ask the server to internally grab a new image from the camera and
   --   prepare it for transmission (i.e. crop it to the given size)
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
   --  left counterpart)
   --  CommunicationError - The server did not answer the request correctly
   -----------------------------------------------------------------------------
   function Request_Next_Image ( Subimage_Dimensions : in Image_Dimensions )
			       return Number_Of_Chunks;

   -----------------------------------------------------------------------------
   -- Function: Request_Chunks
   --
   -- Purpose:
   --   Abstracts <Adaimageprocessor.Network.Protocol.Request_Chunks_Raw> and adds
   --   exception handling, i.e. handles
   --   <Adaimageprocessor.Network.Socket.CONNECTION_ERROR> appropriately (try transfer
   --   again)
   --
   -- Note:
   --   Implementation similar to <Adaimageprocessor.Network.Socket.Receive_Data>
   --
   -- Parameters:
   --   Chunks - How many chunks to receive
   --
   -- Returns:
   --   The image chunks in an array.
   --
   -- Exceptions:
   --   Propagates <Adaimageprocessor.Network.Socket.CONNECTION_ERROR> if transfer
   --   failed
   -----------------------------------------------------------------------------
   function Request_Chunks ( Chunks : in Number_Of_Chunks ) return Chunk_Data;

private
   -----------------------------------------------------------------------------
   -- Section: Private
   -----------------------------------------------------------------------------

   -----------------------------------------------------------------------------
   -- Constants: Adaimageprocessor.Network.Protocol
   --
   -- MAX_REQUEST_CHUNKS_TRIES - How often should the program try to receive an
   -- entire image before it exits with an error-message
   -----------------------------------------------------------------------------
   MAX_REQUEST_CHUNKS_TRIES : constant Positive := 2;

   -----------------------------------------------------------------------------
   -- Function: Request_Chunks_Raw
   --
   -- Purpose:
   --   Requests all chunks subsequently.
   --
   -- Parameters:
   --   Chunks - How many chunks to receive
   --
   -- Returns:
   --   The image chunks in an array.
   --
   -- Exceptions:
   --   SOCKETCOMM.CONNECTION_ERROR - if no data was received within a given
   --   timeout
   -----------------------------------------------------------------------------
   function Request_Chunks_Raw ( Chunks : in Number_Of_Chunks )
                                return Chunk_Data;


   -----------------------------------------------------------------------------
   -- Function: Process_Image_Size
   --
   -- Purpose:
   --   Helper function to pad the specified image size correctly to
   --   four characters, i.e. do a special integer to string casting
   --
   -- Parameters:
   --   Size - The number to be padded
   --
   -- Returns:
   --   A four character string containing the value of _Size_
   --   padded with zeroes from the left.
   --
   -- Exceptions:
   --   Length_Error - The input parameter _Size_ has a value
   --   greater than 9999
   -----------------------------------------------------------------------------
   function Process_Image_Size ( Size : in Natural ) return String;

   -----------------------------------------------------------------------------
   -- Function: Camera_Error
   --
   -- Purpose:
   --   Processes error messages received from the Camera
   --
   -- Effects:
   --   FIXME
   --
   -- Parameters:
   --   Erroressage - A Stream containing a packet with an error message.
   --
   -- Returns:
   --   A string with the error message
   --
   -- Exceptions:
   --   None.
   -----------------------------------------------------------------------------
   function Camera_Error (Errormessage: in STREAMLIB.Stream_Element_Array) return String;

end Adaimageprocessor.Network.Protocol;
