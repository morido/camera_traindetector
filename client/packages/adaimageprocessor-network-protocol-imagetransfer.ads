--------------------------------------------------------------------------------
-- Package: Adaimageprocessor.Network.Protocol.Imagetransfer
-- Purpose:
--   Abstracts the <Adaimageprocessor.Network.Protocol>-package.
--------------------------------------------------------------------------------

--- only testing FIXME; for file write
with Ada.Streams.Stream_IO;
-- END test
with Adaimageprocessor.Image;


package Adaimageprocessor.Network.Protocol.Imagetransfer is

   package Imagehandling renames Adaimageprocessor.Image;


   -----------------------------------------------------------------------------
   -- Group: Client_Controller
   -- Purpose:
   --  Task to run the actual client
   -----------------------------------------------------------------------------
   task Imagetransfer_Controller is
      -- FIXME calculate actual Storage_Size needed
      pragma Storage_Size ( 8192*1024 );
   end Imagetransfer_Controller;


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

   -- FIXME Missing documentation

   function Get_Image_From_Remote( Subimage_Dimensions : in Image_Dimensions) return Imagehandling.storage_for_image;

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
