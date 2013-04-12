package body Adaimageprocessor.Network.Protocol.Imagetransfer is

   task body Imagetransfer_Controller is
   begin
      Setup;
      loop
         AllowShutdown; -- has to be called once (initialization phase)
         Write_Image_To_File;
	 exit when InterruptController.Shutdown_Requested;
      end loop;
   exception
      when Error: END_TASK =>
	 Cleanup;
      when Error: others =>
	 Adaimageprocessor.Error(Error);
   end Imagetransfer_Controller;

   function Return_Image ( Subimage_Dimensions : in Image_Dimensions )
                          return Image is
      pragma Warnings(Off); -- FIXME; only for testing to suppress warning about Image1
      Image1 : Image;
   begin
      return Image1;
      -- essentially do nothing
   end Return_Image;

   procedure Write_Image_To_File is
      Dimensions : Image_Dimensions;
      Chunkscount : Number_Of_Chunks;

      File : STREAMLIB.Stream_IO.File_Type;
   begin
      -- write all our UDP-packets to a file
      Dimensions.Top_Left_X := 0;
      Dimensions.Top_Left_Y := 0;
      Dimensions.Bottom_Right_X := 960;
      Dimensions.Bottom_Right_Y := 1280;

      Chunkscount := Request_Next_Image(Dimensions);
      declare
         Image_Data : Chunk_Data := Request_Chunks(Chunks => Chunkscount);
         filename : String := "/tmp/test.out";
      begin

	 STREAMLIB.Stream_IO.Create
	   (File => File,
	    Name => filename,
	    Mode => STREAMLIB.Stream_IO.Out_File);

	 for IndexA in Image_Data.Image_Chunks'First..Image_Data.Image_Chunks'Last-1 loop
	    STREAMLIB.Stream_IO.Write(File, Image_Data.Image_Chunks(IndexA));
	 end loop;
         -- very last chunk
         STREAMLIB.Stream_IO.Write(File, Image_Data.Image_Chunks(Image_Data.Image_Chunks'Last)(Image_Chunk_Data_NoNumber'First..Image_Data.Last_Chunk_Offset));


	 STREAMLIB.Stream_IO.Close (File);
      end;
   end Write_Image_To_File;

   -- private

   procedure Setup is
      PLATFORM_ERROR: exception;
   begin
      -- check if the size of a Character equals 8-bits
      -- important for socket-communication
      if (System.Storage_Unit /= 8) then
	 raise PLATFORM_ERROR with "Your platform is not supported. Abort.";
      end if;

      -- initialize the socket
      SOCKETCOMM.Open_Socket(Server_IP, Server_Port);


   end Setup;

   procedure Cleanup is
      KILL : exception;
   begin
      -- shutdown the socket
      SOCKETCOMM.Close_Socket;


      raise KILL with "Shutdown. Goodbye.";
   exception
      when Error: KILL =>
	 Adaimageprocessor.Error(Error);
   end Cleanup;


end Adaimageprocessor.Network.Protocol.Imagetransfer;
