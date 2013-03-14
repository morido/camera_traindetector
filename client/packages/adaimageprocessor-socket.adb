package body Adaimageprocessor.Socket is
   -- fixme: add exception handling

   procedure Open_Socket(Server_IP : in String; Server_Port : in Positive) is
   begin
      GSOCK.Initialize;
      GSOCK.Create_Socket(Sockethandler, GSOCK.Family_Inet, GSOCK.Socket_Datagram);
      GSOCK.Set_Socket_Option(Socket => Sockethandler,
			      Option => (Name    => GNAT.Sockets.Receive_Timeout,
					 Timeout => SOCKET_TIMEOUT));
      Server.Addr := GSOCK.Inet_Addr(Server_IP);
      Server.Port := GSOCK.Port_Type(Server_Port);
      SocketIsSetUp := True;
   end Open_Socket;

   procedure Close_Socket is
   begin
      if SocketIsSetUp then
	 GSOCK.Close_Socket (Sockethandler);
	 SocketIsSetUp := False;
      end if;
   end Close_Socket;

   procedure Send_String(String_To_Send : in String) is
      Array_Temp : Ada.Streams.Stream_Element_Array(0 .. Ada.Streams.Stream_Element_Offset(String_To_Send'Last));
      -- to avoid indexing errors
      use type Ada.Streams.Stream_Element_Offset;
      Array_Temp_Indexer : Ada.Streams.Stream_Element_Offset := Array_Temp'First;
      LENGTH_EXCEPTION : exception;
   begin
      CheckSocketSetUp;

      -- FIXME: check about "* 8" -- correct here?!
      if String_To_Send'Size > MAX_PACKET_SIZE * 8 then
	 raise LENGTH_EXCEPTION with "Given string is too long to transmit.";
      end if;

      -- cast the string to the stream array
      for Index in String_To_Send'Range loop
	 Array_Temp(Array_Temp_Indexer) := Character'Pos(String_To_Send(Index));
	 Array_Temp_Indexer := Array_Temp_Indexer +1;
      end loop;

      -- call the actual data transmission routine
      Send_Data(Array_Temp);

   end Send_String;

   function Receive_Data return Ada.Streams.Stream_Element_Array is
       subtype Valid_Connection_Retries is Natural range 0 .. MAX_CONNECTION_RETRIES;
       While_Index : Valid_Connection_Retries := Valid_Connection_Retries'First;
   begin
      CheckSocketSetUp;

       -- try to receive some data
       while While_Index < MAX_CONNECTION_RETRIES loop
	  begin
            AllowShutdown;
            declare
               Return_Value : Ada.Streams.Stream_Element_Array := Raw_Receiver;
            begin
               return Return_Value;
            exception
               when GSOCK.SOCKET_ERROR =>
                  While_Index := While_Index + 1;
            end;
	  end;
       end loop;

       raise CONNECTION_ERROR with "No replies from server. Giving up...";
    end Receive_Data;


-- private part

   procedure Send_Data(Data_To_Send : in Ada.Streams.Stream_Element_Array) is
      Offset : Ada.Streams.Stream_Element_Offset;
   begin
      GSOCK.Send_Socket(Socket => Sockethandler,
		  Item => Data_To_Send,
		  Last => Offset,
		  To => Server);
    end Send_Data;

   function Raw_Receiver return Ada.Streams.Stream_Element_Array is
      Received_Data : Transmittable_Data_Array;
      Offset : Ada.Streams.Stream_Element_Offset;
      use type Ada.Streams.Stream_Element_Offset; -- for arithmetics
   begin
      GSOCK.Receive_Socket(Socket => Sockethandler,
			   Item => Received_Data,
			   Last => Offset,
                           From => Server);
      -- last Byte is Null-Terminator, we truncate it here
      return Received_Data( 1 .. Offset-1 );

      -- Exception : Socket_Error propagated!
   end Raw_Receiver;

   procedure CheckSocketSetUp is
   begin
      if not SocketIsSetUp then
	 raise CONNECTION_ERROR with "Socket not set up.";
      end if;
   end CheckSocketSetUp;

end Adaimageprocessor.Socket;
