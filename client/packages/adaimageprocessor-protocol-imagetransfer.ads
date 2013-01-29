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
   
   procedure Write_Image_To_File;

end Adaimageprocessor.Protocol.Imagetransfer;
