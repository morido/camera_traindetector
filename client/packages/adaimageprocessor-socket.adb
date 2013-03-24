package body Adaimageprocessor.Socket is
   -- fixme: add exception handling

   procedure Open_Socket(Server_IP : in String; Server_Port : in Positive) is
   begin
      GSOCK.Initialize;
      GSOCK.Create_Socket(Sockethandler, GSOCK.Family_Inet, GSOCK.Socket_Datagram);
	-- FIXME: hier war mal das timeout setting
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
      Array_Temp : STREAMLIB.Stream_Element_Array(0 .. STREAMLIB.Stream_Element_Offset(String_To_Send'Last));
      -- to avoid indexing errors
      use type STREAMLIB.Stream_Element_Offset;
      Array_Temp_Indexer : STREAMLIB.Stream_Element_Offset := Array_Temp'First;
      LENGTH_EXCEPTION : exception;
   begin
      CheckSocketSetUp;

      if String_To_Send'Size > MAX_PACKET_SIZE then
	 raise LENGTH_EXCEPTION with "Given string is too long to transmit.";
      end if;

      -- FIXME faster alternative available?
      -- cast the string to the stream array
      for Index in String_To_Send'Range loop
	 Array_Temp(Array_Temp_Indexer) := Character'Pos(String_To_Send(Index));
	 Array_Temp_Indexer := Array_Temp_Indexer +1;
      end loop;

      -- call the actual data transmission routine
      Send_Data(Array_Temp);

   end Send_String;

   function Receive_Data return STREAMLIB.Stream_Element_Array is
      subtype Valid_Connection_Tries is Positive range Positive'First .. SettingsManager.Get_Tries;
      While_Index : Valid_Connection_Tries := Valid_Connection_Tries'First;
   begin
      CheckSocketSetUp;

      -- try to receive some data
      loop
         begin
            return Raw_Receiver;
         exception
            when GSOCK.Socket_Error =>
               if While_Index < Valid_Connection_Tries'Last then
                  While_Index := While_Index + 1;
               else
                  raise CONNECTION_ERROR with "No replies from server. Giving up...";
               end if;
         end;
      end loop;

   end Receive_Data;


   package body SettingsManager is

      procedure Burst_Transfer ( Activate : in Boolean ) is
      begin
         if Activate then
            GSOCK.Set_Socket_Option(Socket => Sockethandler,
                                    Option => (Name    => GNAT.Sockets.Receive_Timeout,
                                               Timeout => SOCKET_TIMEOUT_MIN));
            connection_tries := CONNECTION_TRIES_MIN;
         else
            GSOCK.Set_Socket_Option(Socket => Sockethandler,
                                    Option => (Name    => GNAT.Sockets.Receive_Timeout,
                                               Timeout => SOCKET_TIMEOUT_MAX));
            connection_tries := CONNECTION_TRIES_MAX;
         end if;
      end Burst_Transfer;

      function Get_Tries return Positive is
      begin
         return connection_tries;
      end Get_Tries;

   end SettingsManager;


-- private

   procedure Send_Data(Data_To_Send : in STREAMLIB.Stream_Element_Array) is
      Offset : STREAMLIB.Stream_Element_Offset;
   begin
      GSOCK.Send_Socket(Socket => Sockethandler,
		  Item => Data_To_Send,
		  Last => Offset,
		  To => Server);
    end Send_Data;

   function Raw_Receiver return STREAMLIB.Stream_Element_Array is
      Received_Data : Transmittable_Data_Array;
      Offset : STREAMLIB.Stream_Element_Offset;
      use type STREAMLIB.Stream_Element_Offset; -- for arithmetics
   begin
      GSOCK.Receive_Socket(Socket => Sockethandler,
			   Item => Received_Data,
			   Last => Offset,
                           From => Server);
      -- Exception : Socket_Error propagated!

      -- last Byte is Null-Terminator, we truncate it here
      return Received_Data( Transmittable_Data_Array'First .. Offset-1 );
   end Raw_Receiver;

   procedure CheckSocketSetUp is
   begin
      if not SocketIsSetUp then
	 raise CONNECTION_ERROR with "Socket not set up.";
      end if;
   end CheckSocketSetUp;

end Adaimageprocessor.Socket;
