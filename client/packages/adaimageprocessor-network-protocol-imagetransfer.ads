--------------------------------------------------------------------------------
-- Package: Adaimageprocessor.Network.Protocol.Imagetransfer
-- Purpose:
--   Abstracts the <Adaimageprocessor.Network.Protocol>-package.
--------------------------------------------------------------------------------

--- only testing FIXME; for file write
with Ada.Streams.Stream_IO;
with Adaimageprocessor.Image;
-- END test

package Adaimageprocessor.Network.Protocol.Imagetransfer is

   -----------------------------------------------------------------------------
   -- Types: Adaimageprocessor.Communication
   --  Image - A datatype which can hold the entire image
   -----------------------------------------------------------------------------
   subtype Image is STREAMLIB.Stream_Element_Array (1 .. STREAMLIB.Stream_Element_Offset(Number_Of_Chunks'Last * Image_Chunk_Data'Length));

   -----------------------------------------------------------------------------
   -- Group: Client_Controller
   -- Purpose:
   --  Task to run the actual client
   -----------------------------------------------------------------------------
   task Imagetransfer_Controller is
      -- FIXME calculate actual Storage_Size needed
      pragma Storage_Size ( 8192*1024 );
   end Imagetransfer_Controller;

   -----------------------------------------------------------------------------
   -- Function: Return_Image
   -- Purpose:
   --   Return an image from the server (camera).
   --
   -- Effects:
   --   FIXME complete list what it does
   --   * Merely assembles the chunks from
   --    <Adaimageprocessor.Network.Protocol.Request_Chunks>
   --
   -- Parameters:
   --  Subimage_Dimensions - A record specifying the dimensions of the ROI; see
   --  <Adaimageprocessor.Network.Protocol.Request_Next_Image>
   --
   -- Exceptions:
   --   FIXME
   -----------------------------------------------------------------------------
   function Return_Image ( Subimage_Dimensions : in Image_Dimensions )
			 return Image;

   -- FIXME: Missing doc; not for production code anyways
   procedure Write_Image_To_File;

private
   -----------------------------------------------------------------------------
   -- Section: private
   -----------------------------------------------------------------------------

   -----------------------------------------------------------------------------
   -- Constants:
   --
   --  Server_IP - The IP of the server to connect to
   --  Server_Port - The Port of the server to connect to
   -----------------------------------------------------------------------------
   Server_IP : constant String := "127.0.0.1";
   --Server_IP : constant String := "192.168.1.200";
   Server_Port : constant Positive := 12345;

   -----------------------------------------------------------------------------
   -- Procedure: Setup
   -- Purpose:
   --   Check if the program can run on this particular architecture,
   --   initialize socket-connection
   --
   -- Parameters:
   --   none.
   --
   -- Returns:
   --   nothing.
   --
   -- Exceptions:
   --   PLATFORM_ERROR - indicates the program will not run on this platform
   -----------------------------------------------------------------------------
   procedure Setup;

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
   --   KILL - Always raised. Forces the program to quit immediately.
   -----------------------------------------------------------------------------
   procedure Cleanup;


end Adaimageprocessor.Network.Protocol.Imagetransfer;
