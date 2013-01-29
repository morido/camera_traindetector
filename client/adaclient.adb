-------------------------------------------------------------------------------
-- Package: Adaclient
-- Purpose: 
--   This is the main client file which performs the image-requests from the
--   server, applies the detection algorithm and finally returns the presence
--   and position of a train within the received picture.
--
-- Effects:
--   It is expected to run this as a service on a dedicated machine
--
-- Performance:
--   This program has been optimized for real-time applications.
--   - The image transfer should complete within FIXME ms
--   - The train detection takes FIXME ms
-------------------------------------------------------------------------------

pragma Profile (Ravenscar);

-- headers: Adaclient
-- Ada.Text_IO - Text output
-- Adaimageprocessor.Generic_Subprograms - Subprograms applicable to all packages
-- of this program
-- Adaimageprocessor.Socket - Socket communication

with Ada.Text_IO;
with Adaimageprocessor.Protocol.Imagetransfer;
with Adaimageprocessor.Socket;

-------------------------------------------------------------------------------
-- Procedure: Adaclient
-- Purpose:
--   Main program
--
-- Effects:
--   To be launched automatically once the program starts
--
-- Exceptions:
--   others
-------------------------------------------------------------------------------
procedure Adaclient is   
   package IO renames Ada.Text_IO;
   package SOCKET_COMM renames Adaimageprocessor.Socket;
   package IMAGE_RECV renames Adaimageprocessor.Protocol.Imagetransfer;
   
   -- Constants: Adaclient
   -- Server_IP - The IP-Address of the Server (i.e. the program running on the Camera)
   -- Server_Port - The Port which is used to communicate with the server
   Server_IP : constant String := "127.0.0.1";
   Server_Port : constant Positive := 12345;
   
   COMMUNICATION_ERROR : exception; --FIXME; was soll das hier?
begin
   SOCKET_COMM.Open_Socket(Server_IP, Server_Port);

   IMAGE_RECV.Write_Image_To_File;
   
   SOCKET_COMM.Close_Socket;
   
exception
   -- FIXME: extend possibly with more defined exceptions
   when Error: COMMUNICATION_ERROR =>
      Adaimageprocessor.FatalError(Error);
   when Error: others =>
      Adaimageprocessor.Error(Error);
end Adaclient;


