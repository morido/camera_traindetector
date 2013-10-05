package body Adaimageprocessor.Network.Protocol is

   function Request_Next_Image return Number_Of_Chunks is
      IMAGE_DIMENSION_ERROR: exception;
      Request_String : String(1 .. 18);
   begin
      if Dimensions_ROI.Top_Left_X >= Dimensions_ROI.Bottom_Right_X or Dimensions_ROI.Top_Left_Y >= Dimensions_ROI.Bottom_Right_Y then
	 raise IMAGE_DIMENSION_ERROR with "given dimensions cannot be processed"; -- basically an error by the implementer...
         -- Note: This check does not take into account that track-data in adimageprocessor-image-trackdata.ads may be out of the bounds of the ROI
      end if;

      -- unset burst-transfer mode
      SOCKETCOMM.SettingsManager.Burst_Transfer_Off;

      -- construct request_string
      Request_String := OperationIdentifiers.ToString(operation => OperationIdentifiers.Request_Next_Image)
        & Process_Image_Size(Dimensions_ROI.Top_Left_X)
        & Process_Image_Size(Dimensions_ROI.Top_Left_Y)
        & Process_Image_Size(Dimensions_ROI.Bottom_Right_X)
        & Process_Image_Size(Dimensions_ROI.Bottom_Right_Y);

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
               Return_Array      : constant STREAMLIB.Stream_Element_Array := Receive_Data;
               Process_Indicator : constant OperationIdentifiers.phase1_operations := OperationIdentifiers.ToEnumeration(operation => Return_Array(1..2));
               Return_String     : String (1 .. 4);
            begin
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
                  Received_Data : constant STREAMLIB.Stream_Element_Array := Receive_Data;
                  Process_Indicator : constant OperationIdentifiers.phase2_operations := OperationIdentifiers.ToEnumeration(operation => Received_Data(1..2));
               begin
                  case Process_Indicator is
                     when OperationIdentifiers.Request_Chunks =>
                        exit Initialize_Loop; -- fine, lets proceed
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
         Chunk_Receive_Loop :
         for Index_A in Number_Of_Chunks'First .. Chunks loop
            declare
               package SOCKETCOMMRCV renames Adaimageprocessor.Network.Socket.Receive;
               Received_Data                  : constant STREAMLIB.Stream_Element_Array := SOCKETCOMMRCV.Receive_Data;
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

      -- we do some overloading here, which is safe because of different return
      -- types and careful assignment in the respective functions
      --
      -- conversion to an enumerated-type might slow down the program slightly
      -- but has the advantage to keep all valid values at a central point and
      -- let Ada warn you if not all cases are handled
      function ToEnumeration(operation: in STREAMLIB.Stream_Element_Array) return phase1_operations is
         StringRepresentation : String(1..2);
      begin
         StringRepresentation := Streamconverter.ToString(Input => operation);
         if StringRepresentation = OPcode_NextImage then
            return OperationIdentifiers.Request_Next_Image;
         else
            raise COMMUNICATION_ERROR with "Server did not answer image request correctly";
         end if;
      end ToEnumeration;

      function ToEnumeration(operation: in STREAMLIB.Stream_Element_Array) return phase2_operations is
         StringRepresentation : String(1..2);
      begin
         StringRepresentation := Streamconverter.ToString(Input => operation);
         if StringRepresentation = OPcode_Chunks then
            return OperationIdentifiers.Request_Chunks;
         else
            raise COMMUNICATION_ERROR with "Server did not answer image request correctly";
         end if;
      end ToEnumeration;

      function ToEnumeration(operation: in STREAMLIB.Stream_Element_Array) return error_operations is
         StringRepresentation : String(1..2);
      begin
         StringRepresentation := Streamconverter.ToString(Input => operation);
         if StringRepresentation = OPcode_Error then
            return OperationIdentifiers.Error;
         else
            return OperationIdentifiers.No_Error;
         end if;
      end ToEnumeration;

   end OperationIdentifiers;


   function Process_Image_Size ( Size : in Natural ) return String is
      Return_String : String (1 .. 4);
      package STRINGFIXEDLIB renames Ada.Strings.Fixed;
      package STRINGLIB renames Ada.Strings;
   begin
      -- first two characters contain sign (+/-) and space, we omit them here
      STRINGFIXEDLIB.Move(Source => Natural'Image(Size)(2 .. Natural'Image(Size)'Last),
			  Target => Return_String,
			  Drop => STRINGLIB.Error,
			  Justify => STRINGLIB.Right,
			  Pad => '0');
      return Return_String;
   end Process_Image_Size;

   function Receive_Data return STREAMLIB.Stream_Element_Array is
      package SOCKETCOMMRCV renames Adaimageprocessor.Network.Socket.Receive;
      Return_Array : constant STREAMLIB.Stream_Element_Array := SOCKETCOMMRCV.Receive_Data;
      Process_Indicator : constant OperationIdentifiers.error_operations := OperationIdentifiers.ToEnumeration(operation => Return_Array(1..2));
   begin
    -- FIXME; is there a chance that we can somehow hide SOCKETCOMM.Receive_Data away from outside this function?!

      case Process_Indicator is
         when OperationIdentifiers.No_Error =>
            -- no "ER", we return the entire array
            return Return_Array;
         when OperationIdentifiers.Error =>
            -- first two characters contain "ER" and the last one is a nullterminator. Remove them.
            raise CAMERA_ERROR with StreamConverter.ToString(Input => Return_Array(3..Return_Array'Last-1));
      end case;

   end Receive_Data;


end Adaimageprocessor.Network.Protocol;
