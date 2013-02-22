with Adaimageprocessor.Socket;

package body Adaimageprocessor.Protocol.Imagetransfer is
   package SOCKET_COMM renames Adaimageprocessor.Socket;
      
   task body Imagetransfer_Controller is
      Server_IP : constant String := "127.0.0.1";
      Server_Port : constant Positive := 12345;
   begin
      Precheck;
      SOCKET_COMM.Open_Socket(Server_IP, Server_Port);
      loop
	 Write_Image_To_File;
	 exit when ShutdownFlag;
      end loop;
   exception
      when Error: END_TASK =>
	 Cleanup;
      when Error: others =>
	 Adaimageprocessor.Error(Error);
   end Imagetransfer_Controller;
   
   function Return_Image ( Subimage_Dimensions : Image_Dimensions )
			 return Image is
      Image1 : Image;
   begin
      return Image1;
      -- essentially do nothing
   end Return_Image;
      
   procedure Write_Image_To_File is
      Dimensions : Image_Dimensions;
      Chunkscount : Number_Of_Chunks;
      
      File : Ada.Streams.Stream_IO.File_Type;
   begin
      -- write all our UDP-packets to a file
      Dimensions.Top_Left_X := 0;
      Dimensions.Top_Left_Y := 0;
      Dimensions.Bottom_Right_X := 960;
      Dimensions.Bottom_Right_Y := 1280;
      
      Chunkscount := Request_Next_Image(Dimensions);
      declare
	 Image_Data : Image_Chunks ( Number_Of_Chunks'First .. Chunkscount );
      begin
	 Image_Data := Request_Chunks(Chunkscount);
	 
	 Ada.Streams.Stream_IO.Open
	   (File => File,
	    Name => "test.out",
	    Mode => Ada.Streams.Stream_IO.Out_File);
	 
	 for IndexA in Image_Data'Range loop
	    -- FIXME: Does the following stop on EOF?
	    Ada.Streams.Stream_IO.Write(File, Image_Data(IndexA));
	 end loop;
      
	 Ada.Streams.Stream_IO.Close (File);
      end;
   end Write_Image_To_File;
   
   -- private
   
         
   procedure Precheck is
      PLATFORM_ERROR: exception;
   begin
      -- check if the size of a Character equals 8-bits
      -- important for socket-communication
      if (System.Storage_Unit /= 8) then
	 raise PLATFORM_ERROR with "Your platform is not supported. Abort.";
      end if;
   end Precheck;
   
   procedure Cleanup is
      KILL : exception;
   begin
      SOCKET_COMM.Close_Socket;
      raise KILL with "Shutdown. Goodbye.";
   exception
      when Error: KILL =>
	 Adaimageprocessor.Error(Error);
   end Cleanup;

   
end Adaimageprocessor.Protocol.Imagetransfer;
