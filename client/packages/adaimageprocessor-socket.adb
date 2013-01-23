package body Adaimageprocessor.Socket is
   -- fixme: add exception handling
   
   procedure Open_Socket(Server_IP : in String; Server_Port : in Positive) is
   begin
      GSOCK.Initialize;
      GSOCK.Create_Socket(Sockethandler, GSOCK.Family_Inet, GSOCK.Socket_Datagram);
      Server.Addr := GSOCK.Inet_Addr(Server_IP);
      Server.Port := GSOCK.Port_Type(Server_Port);
   end Open_Socket;
   
   procedure Close_Socket is
   begin
      GSOCK.Close_Socket (Sockethandler);
   end Close_Socket;
   
   procedure Send_Data(Data_To_Send : Character) is
      Offset : Ada.Streams.Stream_Element_Offset; -- FIXME: what is this good for?   
      Data_To_Send_As_Array : Ada.Streams.Stream_Element_Array ( 1 .. SEND_DATA_LENGTH );
   begin
      Data_To_Send_As_Array := ( 1 => Character'Pos(Data_To_Send) ); -- FIXME: buggy if length > 1
      GSOCK.Send_Socket(Socket => Sockethandler,
		  Item => Data_To_Send_As_Array,
		  Last => Offset,
		  To => Server);
    end Send_Data;
    
    -- is String enough or do we need a Wide_String or similar?!
    function Receive_Data return String is
       ResultData : Ada.Streams.Stream_Element_Array (1 .. RECEIVE_DATA_LENGTH);       
       Offset : Ada.Streams.Stream_Element_Offset; -- FIXME: what is this good for? / same above
       ReturnValue : SU.Unbounded_String;
       use type Ada.Streams.Stream_Element_Count; -- FIXME: what does this do?
    begin
       GSOCK.Receive_Socket(Socket => Sockethandler,
			    Item => ResultData,
			    Last => Offset,
			    From => Server);
       for Index in ResultData'Range loop
	  SU.Append(ReturnValue, Character'Val (ResultData (Index)));
       end loop;
       return SU.To_String(ReturnValue);
    end Receive_Data;
end Adaimageprocessor.Socket;
