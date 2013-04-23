package body Adaimageprocessor.Network.Socket is

   procedure Open_Socket(Server_IP : in String; Server_Port : in Positive) is
   begin
      GSOCK.Create_Socket(Sockethandler, GSOCK.Family_Inet, GSOCK.Socket_Datagram);
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
      LENGTH_EXCEPTION : exception;
   begin
      CheckSocketSetUp;

      if String_To_Send'Size > MAX_PACKET_SIZE then
	 raise LENGTH_EXCEPTION with "Given string is too long to transmit.";
      end if;

      -- call the actual data transmission routine
      Send_Data(Data_To_Send => Streamconverter.ToStream(Input => String_To_Send));

   end Send_String;


   package body SettingsManager is

      procedure Burst_Transfer_On (Chunknumber : in Number_Of_Chunks) is
      begin
         GSOCK.Set_Socket_Option(Socket => Sockethandler,
                                 Option => (Name    => GSOCK.Receive_Timeout,
                                            Timeout => SOCKET_TIMEOUT_MIN));
         connection_tries := CONNECTION_TRIES_MIN;

         -- calculate the receive-buffer, it shall be as big as one image plus
         -- the protocol overhead
         -- this is a little conservative as the very last packet may be shorter
         -- than MAX_PACKET_SIZE
         -- Note: Setting this buffer large enough seems to be very vital for
         -- Windows as Raw_Receiver may block indefinetly if the buffer
         -- overflows. No such behaviour on *nix, though.
         GSOCK.Set_Socket_Option(Socket => Sockethandler,
                                 Option => (Name    => GSOCK.Receive_Buffer,
                                            Size    => (Chunknumber * MAX_PACKET_SIZE)));
      end Burst_Transfer_On;


      procedure Burst_Transfer_Off is
      begin
         GSOCK.Set_Socket_Option(Socket => Sockethandler,
                                 Option => (Name    => GSOCK.Receive_Timeout,
                                            Timeout => SOCKET_TIMEOUT_MAX));
         connection_tries := CONNECTION_TRIES_MAX;
      end Burst_Transfer_Off;

      function Get_Roundtrip_Tries return Positive is
      begin
         return ROUNDTRIP_TRIES;
      end Get_Roundtrip_Tries;

      function Get_Connection_Tries return Positive is
      begin
         return connection_tries;
      end Get_Connection_Tries;

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


   procedure CheckSocketSetUp is
   begin
      if not SocketIsSetUp then
	 raise CONNECTION_ERROR with "Socket not set up.";
      end if;
   end CheckSocketSetUp;

end Adaimageprocessor.Network.Socket;
