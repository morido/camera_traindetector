package body Adaimageprocessor.Network.Protocol is

   function Request_Next_Image ( ROI_Dimensions : in Image_Dimensions )
			       return Number_Of_Chunks is
      IMAGE_DIMENSION_ERROR: exception;
      Request_String : String(1 .. 18);
   begin
      if ROI_Dimensions.Top_Left_X >= ROI_Dimensions.Bottom_Right_X or ROI_Dimensions.Top_Left_Y >= ROI_Dimensions.Bottom_Right_Y then
	 raise IMAGE_DIMENSION_ERROR with "given dimensions cannot be processed"; -- FIXME can this ever happen in the implementation?
      end if;

      -- unset burst-transfer mode
      SOCKETCOMM.SettingsManager.Burst_Transfer_Off;

      -- construct request_string
      Request_String := OperationIdentifiers.ToString(operation => OperationIdentifiers.Request_Next_Image)
        & Process_Image_Size(ROI_Dimensions.Top_Left_X)
        & Process_Image_Size(ROI_Dimensions.Top_Left_Y)
        & Process_Image_Size(ROI_Dimensions.Bottom_Right_X)
        & Process_Image_Size(ROI_Dimensions.Bottom_Right_Y);

         -- FIXME we need to start here again in case the following send_string does not get through!
         -- FIXME add the same looping-stuff as in raw_receiver below!!! continue here tomorrow

      Main_Block :
      declare
         subtype Valid_Roundtrip_Tries is Positive range Positive'First .. SOCKETCOMM.SettingsManager.Get_Roundtrip_Tries;
         While_Index : Valid_Roundtrip_Tries := Valid_Roundtrip_Tries'First;
      begin
         Main_Loop : loop
            -- data transmission
            SOCKETCOMM.Send_String(Request_String);

            Receive_Block :
            declare
               Return_Array      : constant STREAMLIB.Stream_Element_Array := SOCKETCOMM.Receive_Data;
               Return_String     : String (1 .. 4);
               Process_Indicator : OperationIdentifiers.phase1_operations;
            begin
               -- process result
               Process_Indicator := OperationIdentifiers.ToEnumerationInit(operation => Return_Array(1..2));

               case Process_Indicator is
               when OperationIdentifiers.Request_Next_Image =>
                  -- wonderful, lets go
                  Return_String := Streamconverter.ToString(Input => Return_Array(3..6));
                  begin
                     return Number_Of_Chunks'Value(Return_String); -- very nice, we made it
                  exception
                        -- check if Return_String is malformed (i.e. no valid digits)
                     when Error : CONSTRAINT_ERROR =>
                        raise COMMUNICATION_ERROR with "Server did not answer image request correctly";
                  end;
               when OperationIdentifiers.Error =>
                  raise COMMUNICATION_ERROR with Camera_Error(Return_Array);
               end case;
            exception
               when SOCKETCOMM.CONNECTION_ERROR =>
                  if While_Index < Valid_Roundtrip_Tries'Last then
                     While_Index := While_Index + 1;
                  else
                     -- propagate the exception
                     raise;
                  end if;
            end Receive_Block;
         end loop Main_Loop;
      end Main_Block;
   end Request_Next_Image;


   function Request_Chunks ( Chunks: in Number_Of_Chunks ) return Chunk_Data is
   begin
      -- set burst mode
      SOCKETCOMM.SettingsManager.Burst_Transfer_On(Chunknumber => Chunks);


      -- step 1: tell the server we want the image data

      SetupBlock :
      declare
         subtype Valid_Roundtrip_Tries is Positive range Positive'First .. SOCKETCOMM.SettingsManager.Get_Roundtrip_Tries;
         While_Index : Valid_Roundtrip_Tries := Valid_Roundtrip_Tries'First;
      begin
         Initialize_Loop :
         loop
            begin
               -- start transfer
               SOCKETCOMM.Send_String(OperationIdentifiers.ToString(operation => OperationIdentifiers.Request_Chunks));
               -- server has to reply with same contents
               declare
                  Received_Data : constant STREAMLIB.Stream_Element_Array := SOCKETCOMM.Receive_Data;
                  Process_Indicator : OperationIdentifiers.phase2_operations;
               begin
                  Process_Indicator := OperationIdentifiers.ToEnumerationBulkTransfer(operation => Received_Data(1..2));
                  case Process_Indicator is
                     when OperationIdentifiers.Request_Chunks =>
                        exit Initialize_Loop; -- fine, lets proceed
                     when OperationIdentifiers.Error =>
                        raise COMMUNICATION_ERROR with Camera_Error(Received_Data);
                  end case;
               end;
            exception
               when SOCKETCOMM.CONNECTION_ERROR =>
                  if While_Index < Valid_Roundtrip_Tries'Last then
                     While_Index := While_Index + 1;
                  else
                     -- propagate the exception
                     raise;
                  end if;
            end;
         end loop Initialize_Loop;
      end SetupBlock;

      -- step 2: we're all set! let's go and receive the image

      MainBlock :
      declare
         Return_Record : Chunk_Data;
      begin
-- FIXME is it possible to receive ER-messages here as well? if so we need to handle this
         Chunk_Receive_Loop :
         for Index_A in Number_Of_Chunks'First .. Chunks loop
            declare
               Received_Data                  : constant STREAMLIB.Stream_Element_Array := SOCKETCOMM.Receive_Data;
               Current_Chunk_Number_As_String : String(1..4);
               Current_Chunk_Number           : Number_Of_Chunks;
            begin
               Current_Chunk_Number_As_String := Streamconverter.ToString(Input => Received_Data(1..4));
               Current_Chunk_Number := Number_Of_Chunks'Value(Current_Chunk_Number_As_String);

               -- automatically sort correctly and assign the correct slice (important for the very last chunk which may be shorter)
               Return_Record.Image_Chunks(Current_Chunk_Number)(Image_Chunk_Data_NoNumber'First..Received_Data'Last) := Received_Data(5..Received_Data'Last);
               Return_Record.Last_Chunk_Offset := Received_Data'Last;
            end;
         end loop Chunk_Receive_Loop;
         return Return_Record;
      end MainBlock;

   end Request_Chunks;


   -- private

   package body OperationIdentifiers is

      function ToString (operation : in operations) return String is
      begin
         case operation is
            when Request_Chunks => return OPcode_Chunks;
            when Request_Next_Image => return OPcode_NextImage;
            when Error => return OPcode_Error;
         end case;
      end ToString;


      function ToEnumerationInit(operation: in STREAMLIB.Stream_Element_Array) return phase1_operations is
         StringRepresentation : String(1..2);
      begin
         StringRepresentation := Streamconverter.ToString(Input => operation);
         if StringRepresentation = OPcode_NextImage then
            return OperationIdentifiers.Request_Next_Image;
         elsif StringRepresentation = OPcode_Error then
            return OperationIdentifiers.Error;
         else
            raise COMMUNICATION_ERROR with "Server did not answer image request correctly";
         end if;
      end ToEnumerationInit;

      function ToEnumerationBulkTransfer(operation: in STREAMLIB.Stream_Element_Array) return phase2_operations is
         StringRepresentation : String(1..2);
      begin
         StringRepresentation := Streamconverter.ToString(Input => operation);
         if StringRepresentation = OPcode_Chunks then
            return OperationIdentifiers.Request_Chunks;
         elsif StringRepresentation = OPcode_Error then
            return OperationIdentifiers.Error;
         else
            raise COMMUNICATION_ERROR with "Server did not answer image request correctly";
         end if;
      end ToEnumerationBulkTransfer;

   end OperationIdentifiers;


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
      -- the last one is a null-terminator which we do not need either
      return Streamconverter.ToString(Errormessage(3..Errormessage'Last-1));
   end Camera_Error;

end Adaimageprocessor.Network.Protocol;

