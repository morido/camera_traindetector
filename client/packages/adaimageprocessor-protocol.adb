private with Ada.Streams;
private with Ada.Strings.Unbounded; -- for Camera_Error()

package body Adaimageprocessor.Protocol is
   COMMUNICATION_ERROR : exception;  
   
   package body Image_Max_Dimensions is
      Max_X : Natural := 0;
      Max_Y : Natural := 0;
      Size_Retrieved : Boolean := False;
      
      function X return Natural is
      begin
	 if Size_Retrieved /= True then
	    Set;
	 end if;
	 return Max_X;
      end X;
	 
      function Y return Natural is
      begin
	 if Size_Retrieved /= True then
	    Set;
	 end if;
	 return Max_Y;
      end Y;
      
      -- private
      
      procedure Set is
	 Return_Array : Image_Chunk_Data;
	 Return_String : String (1 .. 8);
      begin
	 SOCKETCOMM.Send_String("IS");
	 Return_Array := SOCKETCOMM.Receive_Data;
	 
	 -- process result
	 if Character'Val(Return_Array(1)) = 'I' and
	   Character'Val(Return_Array(2)) = 'S' then
	    -- OK
	    Return_String := Character'Val(Return_Array(3))
	      & Character'Val(Return_Array(4))
	      & Character'Val(Return_Array(5))
	      & Character'Val(Return_Array(6))
	      & Character'Val(Return_Array(7))
	      & Character'Val(Return_Array(8))
	      & Character'Val(Return_Array(9))
	      & Character'Val(Return_Array(10));
	    Max_X := Natural'Value(Return_String(1..4));
	    Max_Y := Natural'Value(Return_String(5..8));
	    Size_Retrieved := True;
	 elsif Character'Val(Return_Array(1)) = 'E' and
	   Character'Val(Return_Array(2)) = 'R' then
	    -- specific error message from server
	    raise COMMUNICATION_ERROR with Camera_Error(Return_Array);
	 else
	    -- unspecific error message from server
	    raise COMMUNICATION_ERROR with "Server did not answer image request correctly";
	 end if;
      end Set;
   end Image_Max_Dimensions;
   
   function Request_Next_Image ( Subimage_Dimensions : Image_Dimensions )
			       return Number_Of_Chunks is
      Request_String : String(1 .. 18);
      Return_Array : Image_Chunk_Data;
      Return_String : String (1 .. 4);
      IMAGE_DIMENSION_ERROR: exception;
   begin
      if Subimage_Dimensions.Top_Left_X >= Subimage_Dimensions.Bottom_Right_X or Subimage_Dimensions.Top_Left_Y >= Subimage_Dimensions.Bottom_Right_Y then
	 raise IMAGE_DIMENSION_ERROR with "given dimensions cannot be processed";
      end if;
      
      -- "IN" is the identifier for this operation
      Request_String := "IN" 
	& Process_Image_Size(Subimage_Dimensions.Top_Left_X) 
	& Process_Image_Size(Subimage_Dimensions.Top_Left_Y) 
	& Process_Image_Size(Subimage_Dimensions.Bottom_Right_X) 
	& Process_Image_Size(Subimage_Dimensions.Bottom_Right_Y); 
      
      -- data transmission
      SOCKETCOMM.Send_String(Request_String);
      Return_Array := SOCKETCOMM.Receive_Data;
            
      -- process result
      if Character'Val(Return_Array(1)) = 'I' and
	Character'Val(Return_Array(2)) = 'N' then
	 -- OK
	 Return_String := Character'Val(Return_Array(3))
	   & Character'Val(Return_Array(4))
	   & Character'Val(Return_Array(5))
	   & Character'Val(Return_Array(6));
	 return Natural'Value(Return_String);
      elsif Character'Val(Return_Array(1)) = 'E' and
	Character'Val(Return_Array(2)) = 'R' then
	 -- specific error message from server
	 raise COMMUNICATION_ERROR with Camera_Error(Return_Array);
      else
	 -- unspecific error message from server
	 raise COMMUNICATION_ERROR with "Server did not answer image request correctly";
      end if;
   end Request_Next_Image;
   
   function Request_Chunks ( Chunks : in Number_Of_Chunks ) return Image_Chunks is
      Return_Array : Image_Chunks ( Number_Of_Chunks'First .. Chunks );
      Received_Data : Image_Chunk_Data;
      Current_Chunk_Number : Number_Of_Chunks;
      Current_Chunk_Number_As_String : String(1..4);
   begin
      -- start transfer
      SOCKETCOMM.Send_String("IC");
      
      -- populate Return_Array
      for Index_A in Number_Of_Chunks'First .. Chunks loop
	 Received_Data := SOCKETCOMM.Receive_Data;
	 
	 Current_Chunk_Number_As_String := Character'Val(Received_Data(1))
	   & Character'Val(Received_Data(2))
	   & Character'Val(Received_Data(3))
	   & Character'Val(Received_Data(4));
	 Current_Chunk_Number := Number_Of_Chunks'Value(Current_Chunk_Number_As_String);
	 
	 -- automatically sort correctly
	 Return_Array(Current_Chunk_Number) := Received_Data(5..Image_Chunk_Data'Last); 
      end loop;
      return Return_Array;
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
   
