package body Adaimageprocessor.Network.Protocol is
   COMMUNICATION_ERROR : exception;

   function Request_Next_Image ( Subimage_Dimensions : in Image_Dimensions )
			       return Number_Of_Chunks is
      IMAGE_DIMENSION_ERROR: exception;
   begin
      if Subimage_Dimensions.Top_Left_X >= Subimage_Dimensions.Bottom_Right_X or Subimage_Dimensions.Top_Left_Y >= Subimage_Dimensions.Bottom_Right_Y then
	 raise IMAGE_DIMENSION_ERROR with "given dimensions cannot be processed"; -- can this ever happen in the implementation?
      end if;

      declare
         Request_String : String(1 .. 18);
      begin
         -- unset burst-transfer mode
         SOCKETCOMM.SettingsManager.Burst_Transfer(Activate => False);


         -- "IN" is the identifier for this operation
         Request_String := "IN"
           & Process_Image_Size(Subimage_Dimensions.Top_Left_X)
           & Process_Image_Size(Subimage_Dimensions.Top_Left_Y)
           & Process_Image_Size(Subimage_Dimensions.Bottom_Right_X)
           & Process_Image_Size(Subimage_Dimensions.Bottom_Right_Y);

         -- data transmission
         SOCKETCOMM.Send_String(Request_String);
      end;

      declare
         Return_Array : constant STREAMLIB.Stream_Element_Array := SOCKETCOMM.Receive_Data;
         Return_String : String (1 .. 4);
         Process_Indicator : String(1 .. 2);
      begin
         -- process result
         Process_Indicator := Streamconverter.ToString(Input => Return_Array(1..2));

         if Process_Indicator = "IN" then
            -- OK
            Return_String := Streamconverter.ToString(Input => Return_Array(3..6));
            begin
               -- check if response is malformed (i.e. no valid digits)
               return Number_Of_Chunks'Value(Return_String);
            exception
               when Error : CONSTRAINT_ERROR =>
                  raise COMMUNICATION_ERROR with "Server did not answer image request correctly";
            end;
         elsif Process_Indicator = "ER" then
            -- specific error message from server
            raise COMMUNICATION_ERROR with Camera_Error(Return_Array);
         else
            -- unspecific error message from server
            raise COMMUNICATION_ERROR with "Server did not answer image request correctly";
         end if;
      end;
   end Request_Next_Image;

   function Request_Chunks ( Chunks: in Number_Of_Chunks ) return Chunk_Data is
      subtype Valid_Connection_Tries is Positive range Positive'First .. MAX_REQUEST_CHUNKS_TRIES;
      While_Index : Positive := Valid_Connection_Tries'First;
   begin
      -- set burst mode
      SOCKETCOMM.SettingsManager.Burst_Transfer(Activate => true);

      -- try to receive an entire image, if it fails try MAX_REQUEST_CHUNK_TRIES-times again
      loop
         begin
            return Request_Chunks_Raw(Chunks => Chunks);
         exception
            when SOCKETCOMM.CONNECTION_ERROR =>
               if While_Index < Valid_Connection_Tries'Last then
                  While_Index := While_Index +1;
               else
                  -- propagate the exception, i.e. make the program terminate
                  raise;
               end if;
         end;
      end loop;

   end Request_Chunks;


   -- private

   function Request_Chunks_Raw ( Chunks: in Number_Of_Chunks )
                                return Chunk_Data is
      Return_Record : Chunk_Data;
   begin

      -- FIXME add loop to try twice (i.e. let one transfer fail)

      -- start transfer
      SOCKETCOMM.Send_String("IC");

      -- populate Return_Array
      for Index_A in Number_Of_Chunks'First .. Chunks loop
         declare
            Received_Data : constant STREAMLIB.Stream_Element_Array := SOCKETCOMM.Receive_Data;
            Current_Chunk_Number_As_String : String(1..4);
            Current_Chunk_Number           : Number_Of_Chunks;
         begin

            Current_Chunk_Number_As_String := Streamconverter.ToString(Input => Received_Data(1..4));
            Current_Chunk_Number := Number_Of_Chunks'Value(Current_Chunk_Number_As_String);

            -- automatically sort correctly and assign the correct slice (important for the very last chunk with may be shorter)
            Return_Record.Image_Chunks(Current_Chunk_Number)(Image_Chunk_Data_NoNumber'First..Received_Data'Last) := Received_Data(5..Received_Data'Last);
            --Return_Array(Current_Chunk_Number)(Received_Data'Last+1..Image_Chunk_Data_NoNumber'Last) := (others => Character'Pos(ASCII.SUB));
            Return_Record.Last_Chunk_Offset := Received_Data'Last;
         end;
      end loop;
      return Return_Record;
   end Request_Chunks_Raw;


   function Process_Image_Size ( Size : in Natural ) return String is
      Return_String : String (1 .. 4);
      package STRINGFIXEDLIB renames Ada.Strings.Fixed;
      package STRINGLIB renames Ada.Strings;
   begin
      STRINGFIXEDLIB.Move(Source => Natural'Image(Size)(2 .. Natural'Image(Size)'Last),
			  Target => Return_String,
			  Drop => STRINGLIB.Error,
			  Justify => STRINGLIB.Right,
			  Pad => '0');
      return Return_String;
   end Process_Image_Size;

   function Camera_Error (Errormessage: in STREAMLIB.Stream_Element_Array) return String is
   begin
      -- first two characters are irrelevant as they only contain "ER"
      return Streamconverter.ToString(Errormessage(3..Errormessage'Last));
   end Camera_Error;

end Adaimageprocessor.Network.Protocol;

