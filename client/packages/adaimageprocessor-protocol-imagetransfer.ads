--------------------------------------------------------------------------------
-- Package: Adaimageprocessor.Protocol.Imagetransfer
-- Purpose:
--   Abstracts the <Adaimageprocessor.Protocol>-package.
--------------------------------------------------------------------------------
   
--------------------------------------------------------------------------------
-- Headers: Adaimageprocessor.Communication
-- Ada.Streams - Methods to store arbitrary data (i.e. the image)
-- Adaimageprocessor.Communication.Protocol - FIXME
--------------------------------------------------------------------------------
with Ada.Streams;

--- only testing FIXME; for file write
with Ada.Streams.Stream_IO;
-- END test

package Adaimageprocessor.Protocol.Imagetransfer is
   
   -----------------------------------------------------------------------------
   -- Types: Adaimageprocessor.Communication
   --  Image - A datatype which can hold the entire image
   -----------------------------------------------------------------------------
   subtype Image is Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset(Number_Of_Chunks'Last * Image_Chunk_Data'Length));
   
   -----------------------------------------------------------------------------
   -- Group: Client_Contoller
   -- Purpose:
   --  Task to run the actual client
   --
   -- Methods:
   --  Start - Start operation
   --  Stop -  Stop operation
   -----------------------------------------------------------------------------
   task Imagetransfer_Controller;
   
   
   -----------------------------------------------------------------------------
   -- Function: Return_Image
   -- Purpose:
   --   Return an image from the server (camera). The image size may be
   --   specified. The function then only returns a subimage (cropped area of
   --   the entire image).
   --
   -- Effects:
   --   FIXME complete list what it does
   --   *Merely assembles the chunks from
   --    <Adaimageprocessor.Protocol.Requst_Chunks>
   --
   -- Parameters:
   --  Subimage_Dimensions - A record specifying the subimage; see
   --  <Adaimageprocessor.Communication.Protocol.Request_Next_Image>
   --
   -- Exceptions:
   --   FIXME
   -----------------------------------------------------------------------------
   function Return_Image ( Subimage_Dimensions : Image_Dimensions )
			 return Image;
   
   -- FIXME: Missing doc; not for production code anyways
   procedure Write_Image_To_File;
   
private
   -----------------------------------------------------------------------------
   -- Section: Private
   -----------------------------------------------------------------------------
   
   -----------------------------------------------------------------------------
   -- Procedure: Precheck
   -- Purpose:
   --   Check if the program can run on this particular architecture,
   --
   -- Parameters:
   --   none.
   --
   -- Returns:
   --   nothing.
   --
   -- Exceptions:
   --   PLATFORM_ERROR
   -----------------------------------------------------------------------------
   procedure Precheck;
   
   -----------------------------------------------------------------------------
   -- Procedure: Cleanup
   -- Purpose:
   --   Various Cleanups after the program was interrupted
   --
   -- Parameters:
   --   none.
   --
   -- Returns:
   --   nothing.
   --
   -- Exceptions:
   --   QUIT_REQUEST - Always raised. Forces the program to quit immediately.
   -----------------------------------------------------------------------------
   procedure Cleanup;

   
end Adaimageprocessor.Protocol.Imagetransfer;
