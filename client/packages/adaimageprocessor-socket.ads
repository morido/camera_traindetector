--------------------------------------------------------------------------------
-- Package: Adaimageprocessor.Socket
-- Purpose:
--   Handle the communication between the server (i.e. the camera) and the
--   client (this program)
--
-- Effects:
--   * <Adaclient> transmits data using this package in the following way:
--   1. - call <Open_Socket> to set up a new socket for communication
--   2. - Send an initial request through <Send_Data>
--   3. - Receive the answer to that reqeust through <Receive_Data>
--   4. - repeat steps 2. and 3. until data transfer is complete.
--   5. - Close the socket used for 2. and 3. with <Close_Socket>
--
-- Performance:
--   * The performance of this package is heavily influenced by the underlying
--   network hardware. The camera only supports 100 Mbit connections.
--   * UDP is used to minimize overhead. However the <MAX_DATA_LENGTH> is set to
--   conform with FIXME, and hence is very conservative. Speed could be
--   gradually improved if this was a higher number. The possible loss of data
--   in UDP is a tradeoff for speed here.
--------------------------------------------------------------------------------

-- Headers: Adaimageprocessor.Socket
-- GNAT.Sockets - Socket communication
-- Ada.Streams - Helper functions for socket communication
-- Ada.Strings.Unbounded - Handle strings of unknown length
with GNAT.Sockets;
with Ada.Streams;
with Ada.Strings.Unbounded;

package Adaimageprocessor.Socket is
   
   -----------------------------------------------------------------------------
   -- Procedure: Open_Socket
   -- Purpose:
   --   Open a UDP socket for communication
   -- 
   -- Parameters:
   --   Server_IP - The IP of the server (camera) to communicate with
   --   Server_Port - The port to be used for the communication
   --
   -- Returns:
   --   Nothing.
   --
   -- Exceptions:
   --   FIXME which exceptions can this thing throw?
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
   --   FIXME which exceptions can this thing throw?
   -----------------------------------------------------------------------------
   procedure Close_Socket;
   
   -----------------------------------------------------------------------------
   -- Procedure: Send_Data
   -- Purpose:
   --   Send data to the server using the socket created by <Open_Socket>
   --
   -- Parameters:
   --   Data_To_Send - The raw data to be transmitted
   --
   -- Returns:
   --   Nothing.
   --
   -- Exceptions:
   --   None since this is an UDP-transfer.
   -----------------------------------------------------------------------------
   procedure Send_Data(Data_To_Send : Character);
   
   -----------------------------------------------------------------------------
   -- Function: Receive_Data
   -- Purpose:
   --   Receive an answer to a request made by <Send_Data> using the socket
   --   created by <Open_Socket>
   --
   -- Parameters:
   --   None.
   --
   -- Returns:
   --   A string contraining the data received
   --
   -- Exceptions:
   --   FIXME does this thing throw anything?
   -----------------------------------------------------------------------------
   function Receive_Data return String;
   
private
   -----------------------------------------------------------------------------
   -- Section: Private
   -----------------------------------------------------------------------------

   package SU renames Ada.Strings.Unbounded;
   package GSOCK renames GNAT.Sockets;
   
   -----------------------------------------------------------------------------
   -- Constants: Adaimageprocessor.Socket
   -- SEND_DATA_LENGTH - FIXME
   -- RECEIVE_DATA_LENGTH
   -----------------------------------------------------------------------------

   SEND_DATA_LENGTH : constant Ada.Streams.Stream_Element_Offset := 1;
   RECEIVE_DATA_LENGTH : constant Ada.Streams.Stream_Element_Offset := 548;
   
   -----------------------------------------------------------------------------
   -- Variables: Adaimageprocessor.Socket
   -- Server - FIXME
   -- Sockethandler - Handler set by <Adaimageprocessor.Socket.Open_Socket> and used by all other
   -- subprograms to access the socket
   -----------------------------------------------------------------------------
   Server : GSOCK.Sock_Addr_Type;
   Sockethandler : GSOCK.Socket_Type;
   
   
end Adaimageprocessor.Socket;
