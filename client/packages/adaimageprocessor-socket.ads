--------------------------------------------------------------------------------
-- Package: Adaimageprocessor.Socket
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
-- Headers: Adaimageprocessor.Protocol.Socket
-- GNAT.Sockets - Socket communication
-- Ada.Streams - Work with arbitrary streamed data
--------------------------------------------------------------------------------
with GNAT.Sockets;
with Ada.Streams;

package Adaimageprocessor.Socket is
   
   package GSOCK renames GNAT.Sockets;
   
   -----------------------------------------------------------------------------
   -- Constants: Adaimageprocessor.Socket
   -- MAX_PACKET_SIZE - the maximum size in bytes of a of user-data within a
   -- packet that will definitely not result in further splitting by the
   -- underlying IP-Layer
   -----------------------------------------------------------------------------
   MAX_PACKET_SIZE: constant Natural := 548;
   
   -----------------------------------------------------------------------------
   -- Types: Adaimageprocessor.Socket
   --
   -----------------------------------------------------------------------------
   subtype Transmittable_Data_Array is Ada.Streams.Stream_Element_Array(1 .. Ada.Streams.Stream_Element_Offset(MAX_PACKET_SIZE));

   
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
   --   attempts <MAX_CONNECTION_RETRIES> times to receive data
   --
   -- Parameters:
   --   None.
   --
   -- Returns:
   --   A stream containing the data received.
   --
   -- Exceptions:
   --  CONNECTION_ERROR - raised if no data was received after
   --  <MAX_CONNECTION_RETRIES> retries
   -----------------------------------------------------------------------------
   function Receive_Data return Transmittable_Data_Array;
   
private
   -----------------------------------------------------------------------------
   -- Section: Private
   -----------------------------------------------------------------------------
   
   -----------------------------------------------------------------------------
   -- Constants: Adaimageprocessor.Socket
   -- SOCKET_TIMEOUT - How long to wait for a datagram to arrive; in seconds
   -- MAX_CONNECTION_RETRIES : the maximum number of connection retries before
   --   the program terminates with an error message
   -----------------------------------------------------------------------------
   SOCKET_TIMEOUT : constant Duration := 0.5;
   MAX_CONNECTION_RETRIES : constant Natural := 5;
   
   -----------------------------------------------------------------------------
   -- Variables: Adaimageprocessor.Socket
   -- Server - A record containing the IP and port of the server to communicate 
   -- with; set by <Adaimageprocessor.Socket.Open_Socket> and used by all other 
   -- subprograms in this package.
   -- Sockethandler - Handler to the socket; set by
   -- <Adaimageprocessor.Socket.Open_Socket> and used by all other subprograms
   -- in this package to access the socket.
   -----------------------------------------------------------------------------
   Server : GSOCK.Sock_Addr_Type;
   Sockethandler : GSOCK.Socket_Type;
   
   
   -----------------------------------------------------------------------------
   -- Procedure: Send_Data
   -- Purpose:
   --   Send data to the server using the socket created by 
   --   <Adaimageprocessor.Socket.Open_Socket>
   --
   -- Parameters:
   --   Data_To_Send - The raw data to be transmitted
   --
   -- Returns:
   --   Nothing.
   --
   -- Exceptions:
   --   Socket_Error.
   -----------------------------------------------------------------------------
   procedure Send_Data(Data_To_Send : in Transmittable_Data_Array);
   
   -----------------------------------------------------------------------------
   -- Function: Raw_Receiver
   -- Purpose:
   --   Handle the actual data reception; counterpart to <Send_Data>
   --
   -- Effects:
   --   Not called directly but rather by
   --   <Adaimageprocessor.Socket.Receive_Data>
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
   function Raw_Receiver return Transmittable_Data_Array;
   
end Adaimageprocessor.Socket;
