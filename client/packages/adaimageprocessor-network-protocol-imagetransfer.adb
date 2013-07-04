package body Adaimageprocessor.Network.Protocol.Imagetransfer is

   task body Imagetransfer_Controller is
      imagedata : Imagehandling.storage_for_image;
   begin
      Setup;
      loop
         AllowShutdown;
         --Write_Image_To_File;
         imagedata := Get_Image_From_Remote;
         Imagehandling.Imagedata.Write(data => imagedata);
      end loop;
   exception
      when Error: END_TASK =>
	 Cleanup;
      when Error: others =>
	 Adaimageprocessor.Error(Error);
   end Imagetransfer_Controller;

   -- private


   function Get_Image_From_Remote return Imagehandling.storage_for_image is
      Chunkscount : Number_Of_Chunks;
   begin
      -- phase 1: set up the transfer
      Chunkscount := Request_Next_Image;

      phase2: declare
         Image_Data_Raw : constant Chunk_Data := Request_Chunks(Chunks => Chunkscount);
         output : Imagehandling.storage_for_image;
         offset_input_stream_number : STREAMLIB.Stream_Element_Offset := -1;
         offset_input_stream_data : STREAMLIB.Stream_Element_Offset range Image_Chunk_Data_NoNumber'Range;
         packet_number : Positive range Image_Data_Raw.Image_Chunks'Range;
         packet_number_zero : constant STREAMLIB.Stream_Element_Offset := STREAMLIB.Stream_Element_Offset(Image_Chunks_type'First);
         packet_length_userdata : constant STREAMLIB.Stream_Element_Offset := Image_Chunk_Data_NoNumber'Length;
         input_stream_data_zero : constant STREAMLIB.Stream_Element_Offset := Image_Chunk_Data_NoNumber'First;
      begin
         for Row in Dimensions_ROI.Top_Left_Y..Dimensions_ROI.Bottom_Right_Y loop
            for Column in Dimensions_ROI.Top_Left_X..Dimensions_ROI.Bottom_Right_X loop
               offset_input_stream_number := offset_input_stream_number + 1;
               packet_number := Positive(packet_number_zero + (offset_input_stream_number / packet_length_userdata));
               offset_input_stream_data := input_stream_data_zero + (offset_input_stream_number mod packet_length_userdata);
               -- Note: We need not to handle the very last packet because through the loop-boundaries we implicitly know
               -- where it ends.
               output(Column)(STREAMLIB.Stream_Element_Offset(Row)) :=  Image_Data_Raw.Image_Chunks(packet_number)(offset_input_stream_data);
            end loop;
         end loop;
         -- Note: the output is constant in size disregarding the requested
         -- Subimage. So all pixels outside of this Subimage are not
         -- initialized!
         -- But since Trackdata and Subimage-Dimensions are not linked anyways,
         -- this cannot really be handled programmatically but should rather be
         -- taken care of by the implementer.
         return output;
      end phase2;

   end Get_image_from_Remote;


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
      raise KILL with "UDP-Communication: Shutdown. Goodbye.";
   exception
      when Error: KILL =>
	 Adaimageprocessor.Error(Error);
   end Cleanup;


end Adaimageprocessor.Network.Protocol.Imagetransfer;
