--------------------------------------------------------------------------------
-- Package: Adaimageprocessor.Network.Protocol.Imagetransfer
-- Purpose:
--   Abstracts the <Adaimageprocessor.Network.Protocol>-package.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Headers:
-- Adaimageprocessor.Image - Link to the image processing so data can be sent
-- over
--------------------------------------------------------------------------------
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
   -- Function: Get_Image_From_Remote
   --
   -- Purpose:
   -- Request the image from the remote camera, write it into an appropriate
   -- structure (2d array) and return it.
   --
   -- Parameters:
   -- None.
   --
   -- Returns:
   -- The pixeldata in a 2-dimensional array.
   -----------------------------------------------------------------------------
   function Get_Image_From_Remote return Imagehandling.storage_for_image;

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
