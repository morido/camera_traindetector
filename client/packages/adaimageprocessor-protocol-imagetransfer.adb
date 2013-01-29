package body Adaimageprocessor.Protocol.Imagetransfer is
   
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
	 Image_Data : Image_Chunks ( Number_Of_Chunks'First .. Chunkscount);
      begin
	 Image_Data := Request_Chunks(Chunkscount);
	 
	 Ada.Streams.Stream_IO.Open
	   (File => File,
	    Name => "test.out",
	    Mode => Ada.Streams.Stream_IO.Out_File);
	 
	 for IndexA in Image_Data'Range loop
	    Ada.Streams.Stream_IO.Write(File, Image_Data(IndexA));
	 end loop;
      
	 Ada.Streams.Stream_IO.Close (File);
      end;
   end Write_Image_To_File;
   
end Adaimageprocessor.Protocol.Imagetransfer;
