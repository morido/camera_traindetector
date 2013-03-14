private with Ada.Streams;
private with Ada.Strings.Unbounded; -- for Camera_Error()

package body Adaimageprocessor.Protocol is
   COMMUNICATION_ERROR : exception;

   function Request_Next_Image ( Subimage_Dimensions : Image_Dimensions )
			       return Number_Of_Chunks is
      IMAGE_DIMENSION_ERROR: exception;
   begin
      if Subimage_Dimensions.Top_Left_X >= Subimage_Dimensions.Bottom_Right_X or Subimage_Dimensions.Top_Left_Y >= Subimage_Dimensions.Bottom_Right_Y then
	 raise IMAGE_DIMENSION_ERROR with "given dimensions cannot be processed"; -- can this ever happen in the implementation?
      end if;

      declare
         Request_String : String(1 .. 18);
      begin
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
         Return_Array : Ada.Streams.Stream_Element_Array := SOCKETCOMM.Receive_Data;
         Return_String : String (1 .. 4);
      begin
         -- process result
         if Character'Val(Return_Array(1)) = 'I' and
           Character'Val(Return_Array(2)) = 'N' then
            -- OK
            Return_String := Character'Val(Return_Array(3))
              & Character'Val(Return_Array(4))
              & Character'Val(Return_Array(5))
              & Character'Val(Return_Array(6));
            begin
               -- check if response is malformed (i.e. no valid digits)
               return Number_Of_Chunks'Value(Return_String);
            exception
               when Error : CONSTRAINT_ERROR =>
                  raise COMMUNICATION_ERROR with "Server did not answer image request correctly";
            end;

         elsif Character'Val(Return_Array(1)) = 'E' and
           Character'Val(Return_Array(2)) = 'R' then
            -- specific error message from server
            raise COMMUNICATION_ERROR with Camera_Error(Return_Array);
         else
            -- unspecific error message from server
            raise COMMUNICATION_ERROR with "Server did not answer image request correctly";
         end if;
      end;
   end Request_Next_Image;

   function Request_Chunks ( Chunks: in Number_Of_Chunks ) return Chunk_Data is
      Return_Record : Chunk_Data;
   begin
      -- start transfer
      SOCKETCOMM.Send_String("IC");

      -- populate Return_Array
      --for Index_A in Number_Of_Chunks'First .. Chunks loop
      for Index_A in Number_Of_Chunks'First .. Chunks loop
         declare
            Received_Data                  : Ada.Streams.Stream_Element_Array := SOCKETCOMM.Receive_Data;
            Current_Chunk_Number_As_String : String(1..4);
            Current_Chunk_Number           : Number_Of_Chunks;
         begin

            Current_Chunk_Number_As_String := Character'Val(Received_Data(1))
              & Character'Val(Received_Data(2))
              & Character'Val(Received_Data(3))
              & Character'Val(Received_Data(4));
            Current_Chunk_Number := Number_Of_Chunks'Value(Current_Chunk_Number_As_String);


            -- automatically sort correctly and assign the correct slice (important for the very last chunk with may be shorter)
            Return_Record.Image_Chunks(Current_Chunk_Number)(Image_Chunk_Data_NoNumber'First..Received_Data'Last) := Received_Data(5..Received_Data'Last);
            --Return_Array(Current_Chunk_Number)(Received_Data'Last+1..Image_Chunk_Data_NoNumber'Last) := (others => Character'Pos(ASCII.SUB));
            Return_Record.Last_Chunk_Offset := Received_Data'Last;
         end;
      end loop;
      return Return_Record;
   end Request_Chunks;


-- private

   function Process_Image_Size ( Size : in Natural ) return String is
      Return_String : String (1 .. 4);
   begin
      STRINGFIXEDLIB.Move(Source => Natural'Image(Size)(2 .. Natural'Image(Size)'Last),
			  Target => Return_String,
			  Drop => STRINGLIB.Error,
			  Justify => STRINGLIB.Right,
			  Pad => '0');
      return Return_String;
   end Process_Image_Size;

   function Camera_Error (Errormessage: in Image_Chunk_Data) return String is
      package S renames Ada.Streams;
      package SU renames Ada.Strings.Unbounded;
      use type S.Stream_Element_Offset; -- for while arithmethics
      subtype Looping_Indexer_Type is S.Stream_Element_Offset range Image_Chunk_Data'First+2 .. Image_Chunk_Data'Last;
      -- positions of Image_Chunk_Data 1 and 2 contain "ER"; not interesting
      Looping_Indexer : Looping_Indexer_Type := Looping_Indexer_Type'First;

      Escape_Character : constant Character := '|';
      Return_String : SU.Unbounded_String := SU.To_Unbounded_String("");
   begin
      while Character'Val(Errormessage(Looping_Indexer)) /= Escape_Character loop
	 SU.Append(Return_String, Character'Val(Errormessage(Looping_Indexer)));
	 Looping_Indexer := Looping_Indexer +1;
      end loop;

      return SU.To_String(Return_String);
   end Camera_Error;

end Adaimageprocessor.Protocol;

