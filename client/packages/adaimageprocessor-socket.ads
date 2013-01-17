with Ada.Streams;
with GNAT.Sockets;
with Ada.Strings.Unbounded;

package Adaimageprocessor.Socket is
   procedure Open_Socket(Server_IP : in String; Server_Port : in Positive);
   procedure Close_Socket;
   procedure Send_Data(Data_To_Send : Character);
   function Receive_Data return String;
   
private
   package SU renames Ada.Strings.Unbounded;
   package GSOCK renames GNAT.Sockets;
   
   SEND_DATA_LENGTH : constant Ada.Streams.Stream_Element_Offset := 1;
   RECEIVE_DATA_LENGTH : constant Ada.Streams.Stream_Element_Offset := 12;
   
   -- save connection settings during program run with these two variables
   Server : GSOCK.Sock_Addr_Type;
   Sockethandler : GSOCK.Socket_Type;
   
   
end Adaimageprocessor.Socket;
