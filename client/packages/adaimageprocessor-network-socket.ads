--------------------------------------------------------------------------------
-- Package: Adaimageprocessor.Network.Socket
--
-- Purpose:
--   Handle the raw communication between the server (i.e. the camera) and the
--   client (this program)
--
-- Effects:
--   * <Adaclient> transmits data using this package in the following way:
--   1. - call <Open_Socket> to set up a new socket for communication
--   2. - Send an initial request through <Send_Data>
--   3. - Receive the answer to that request through <Receive_Data>
--   4. - repeat steps 2. and 3. until data transfer is complete.
--   5. - Close the socket used for 2. and 3. with <Close_Socket>
--
-- Performance:
--   * The performance of this package is heavily influenced by the underlying
--   network hardware. The camera only supports 100 Mbit connections.
--   * UDP is used to minimize overhead. However the <MAX_PACKET_SIZE> is set to
--   conform with FIXME, and hence is very conservative. Speed could be
--   gradually improved if this was a higher number. The possible loss of data
--   in UDP is a tradeoff for speed here.
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Headers: Adaimageprocessor.Network.Socket
-- GNAT.Sockets - Socket communication
--------------------------------------------------------------------------------
with GNAT.Sockets;

package Adaimageprocessor.Network.Socket is

   package GSOCK renames GNAT.Sockets;

   -----------------------------------------------------------------------------
   -- Constants: Adaimageprocessor.Network.Socket
   -- MAX_PACKET_SIZE - the maximum size in bytes of a of user-data within a
   -- packet that will definitely not result in further splitting by the
   -- underlying IP-Layer
   -----------------------------------------------------------------------------
   MAX_PACKET_SIZE: constant Natural := 548;

   -----------------------------------------------------------------------------
   -- Types: Adaimageprocessor.Network.Socket
   --
   -- Transmittable_Data_Array_Size - only used to make the following
   -- <Transmittable_Data_Array>-expression shorter
   -- Transmittable_Data_Array - an array to hold the data of an individual
   -- network-packet
   -- Number_Of_Chunks - how many individual parts (chunks) may a data transfer
   -- be split into. Important for buffer-calculation in
   -- <SettingsManager.Burst_Transfer_On>. Its 'Last may not be greater than
   -- 9999.
   -----------------------------------------------------------------------------
   subtype Transmittable_Data_Array_Size is STREAMLIB.Stream_Element_Offset range 1 .. STREAMLIB.Stream_Element_Offset(MAX_PACKET_SIZE);
   subtype Transmittable_Data_Array is STREAMLIB.Stream_Element_Array(Transmittable_Data_Array_Size'Range);
   subtype Number_Of_Chunks is Positive range 1 .. 6790; -- FIXME make server-dependant, relevant for Image_Chunk_Data! Max 4 digits for 'Last

   -----------------------------------------------------------------------------
   -- Variables: Adaimageprcessor.Socket
   --
   -- CONNECTION_ERROR - default exception raised by subprograms of this package
   -----------------------------------------------------------------------------
   CONNECTION_ERROR : exception;


   -----------------------------------------------------------------------------
   -- Procedure: Open_Socket
   -- Purpose:
   --   Open an UDP socket for communication
   --
   -- Parameters:
   --   Server_IP - The IP of the server (camera) to communicate with
   --   Server_Port - The port to be used for the communication
   --
   -- Returns:
   --   Nothing.
   --
   -- Exceptions:
   --   Socket_Error
   -----------------------------------------------------------------------------
   procedure Open_Socket(Server_IP : in String; Server_Port : in Positive);

   -----------------------------------------------------------------------------
   -- Procedure: Close_Socket
   -- Purpose:
   --   Close the socket opened by <Open_Socket> and make it available for
   --   other processes
   --
   -- Parameters:
   --   None.
   --
   -- Returns:
   --   Nothing.
   --
   -- Exceptions:
   --   None.
   -----------------------------------------------------------------------------
   procedure Close_Socket;

   -----------------------------------------------------------------------------
   -- Procedure: Send_String
   -- Purpose:
   --  Send a string to the server
   --
   -- Parameters:
   --  String_To_Send - the string to transmit
   --
   -- Returns:
   --  Nothing.
   --
   -- Exceptions:
   --  LENGTH_EXCEPTION - The string was too long to be transmitted.
   -----------------------------------------------------------------------------
   procedure Send_String (String_To_Send : in String);

   -----------------------------------------------------------------------------
   -- Function: Receive_Data
   -- Purpose:
   --   Wrapper for
   --   <Raw_Receiver>
   --
   -- Effects:
   --   attempts <SettingsManger.Get_Tries> times to receive data
   --
   -- Parameters:
   --   None.
   --
   -- Returns:
   --   A stream containing the data received.
   --
   -- Exceptions:
   --  CONNECTION_ERROR - raised if no data was received after
   --  <SettingsManager.Get_Tries>
   -----------------------------------------------------------------------------
   function Receive_Data return STREAMLIB.Stream_Element_Array;

   -----------------------------------------------------------------------------
   -- Package: SettingsManager
   --
   -- Purpose:
   --   Manage timeouts and retries applicable to the UDP-Transfer
   -----------------------------------------------------------------------------
   package SettingsManager is

      --------------------------------------------------------------------------
      -- Function: Burst_Transfer_On
      --
      -- Purpose:
      --   Sets send/receive-timeout, a retry-count and appropriate buffers for
      --   the image-transfer.
      --
      -- Parameters:
      --   Chunknumber - how many chunks are to be transfered; used for
      --   buffer-calculation
      --
      -- Returns:
      --   Nothing.
      --
      --------------------------------------------------------------------------
      procedure Burst_Transfer_On ( Chunknumber : in Number_Of_Chunks );

      --------------------------------------------------------------------------
      -- Function: Burst_Transfer_Off
      --
      -- Purpose:
      --   Sets send/receive-timeout, a retry-count and appropriate buffers for
      --   everything but the actual image-transfer.
      --
      -- Parameters:
      --   None.
      --
      -- Returns:
      --   Nothing.
      --
      --------------------------------------------------------------------------
      procedure Burst_Transfer_Off;

      --------------------------------------------------------------------------
      -- Function: Get_Tries
      --
      -- Purpose:
      --   Getter-Function for the current value of connection-tries
      --
      -- Parameters:
      --   None.
      --
      -- Returns:
      --   <connection_tries>
      --------------------------------------------------------------------------
      function Get_Tries return Positive;


   private
      --------------------------------------------------------------------------
      -- Section: Private
      --------------------------------------------------------------------------

      --------------------------------------------------------------------------
      -- Constants: SettingsManager
      --
      -- SOCKET_TIMEOUT_MAX - How long to wait for a datagram to arrive; in
      -- seconds; non-burst-mode
      -- SOCKET_TIMEOUT_MIN - How long to wait for a datagram to arrive; in
      -- seconds; burst-mode
      -- CONNECTION_TRIES_MAX - How often the program should try to establish a
      -- connection to the server; non-burst-mode
      -- CONNECTION_TRIES_MIN - How often the program should try to establish a
      -- connection to the server; burst-mode
      --------------------------------------------------------------------------
      SOCKET_TIMEOUT_MAX : constant Duration := 0.5;
      --SOCKET_TIMEOUT_MIN : constant Duration := 0.01; --FIXME good value?
      SOCKET_TIMEOUT_MIN : constant Duration := 5.0;
      CONNECTION_TRIES_MAX : constant Positive := 5;
      CONNECTION_TRIES_MIN : constant Positive := 2;

      --------------------------------------------------------------------------
      -- Variables: SettingsManager
      --
      -- connection_tries - the currently effective value for the number of
      -- connection tries
      --------------------------------------------------------------------------
      connection_tries : Positive := CONNECTION_TRIES_MAX;

   end SettingsManager;

private
   -----------------------------------------------------------------------------
   -- Section: Private
   -----------------------------------------------------------------------------

   -----------------------------------------------------------------------------
   -- Variables: Adaimageprocessor.Network.Socket
   -- Server - A record containing the IP and port of the server to communicate
   -- with; set by <Adaimageprocessor.Network.Socket.Open_Socket> and used by all other
   -- subprograms in this package.
   -- Sockethandler - Handler to the socket; set by
   -- <Adaimageprocessor.Network.Socket.Open_Socket> and used by all other subprograms
   -- in this package to access the socket.
   -- SocketIsSetUp - Variable indicating whether the socket has been
   -- initialized, set by <Adaimageprocessor.Network.Socket.Open_Socket>, unset by
   -- <Adaimageprocessor.Network.Socket.Close_Socket>
   -----------------------------------------------------------------------------
   Server : GSOCK.Sock_Addr_Type;
   Sockethandler : GSOCK.Socket_Type;
   SocketIsSetUp : Boolean := False;

   -----------------------------------------------------------------------------
   -- Procedure: Send_Data
   -- Purpose:
   --   Send data to the server using the socket created by
   --   <Adaimageprocessor.Network.Socket.Open_Socket>
   --
   -- Parameters:
   --   Data_To_Send - The raw data to be transmitted
   --   Last_Byte_To_Send - determines the size of the packet
   --
   -- Returns:
   --   Nothing.
   --
   -- Exceptions:
   --   Socket_Error.
   -----------------------------------------------------------------------------
   procedure Send_Data(Data_To_Send : in STREAMLIB.Stream_Element_Array);

   -----------------------------------------------------------------------------
   -- Function: Raw_Receiver
   -- Purpose:
   --   Handle the actual data reception; counterpart to <Send_Data>
   --
   -- Effects:
   --   Not called directly but rather by
   --   <Adaimageprocessor.Network.Socket.Receive_Data>
   --
   -- Parameters:
   --   None.
   --
   -- Returns:
   --  Received_Data_Array - A Stream containing the received data or an error
   --  if reception failed
   --
   -- Exceptions:
   --   None.
   -----------------------------------------------------------------------------
   function Raw_Receiver return STREAMLIB.Stream_Element_Array;

   -----------------------------------------------------------------------------
   -- Function: Raw_Receiver
   -- Purpose:
   --   Check if the connection is set up properly. I.e. if <SocketIsSetUp> is
   --   true.
   --
   -- Parameters:
   --   None.
   --
   -- Returns:
   --   nothing.
   --
   -- Exceptions:
   --   CONNECTION_ERROR - raised if connection is not set up properly.
   -----------------------------------------------------------------------------
   procedure CheckSocketSetUp;

end Adaimageprocessor.Network.Socket;
