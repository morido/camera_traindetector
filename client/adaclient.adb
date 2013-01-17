with Ada.Text_IO;
with GNAT.Sockets; use GNAT.Sockets;
with Ada.Strings.Unbounded;
with Ada.Streams;


-- packages of this very program
with Adaimageprocessor.Generic_Functions;

use type Ada.Streams.Stream_Element_Count;

procedure Adaclient is   
   package IO renames Ada.Text_IO;
   
   Sockethandler : Socket_Type;
   Server : Sock_Addr_Type;
   Offset : Ada.Streams.Stream_Element_Offset;
   
   SEND_DATA_LENGTH : constant := 1;
   RECEIVE_DATA_LENGTH : constant := 12;
   RequestData: Ada.Streams.Stream_Element_Array (1 .. SEND_DATA_LENGTH);
   ResultData: Ada.Streams.Stream_Element_Array (1 .. RECEIVE_DATA_LENGTH);
begin
   Gnat.Sockets.Initialize;
   Create_Socket(Sockethandler, Family_Inet, Socket_Datagram);
   Server.Addr := Inet_Addr("127.0.0.1");
   Server.Port := 12345;
   -- Make request for new image data
   RequestData := ( 1 => Character'Pos('1') );
   Send_Socket(Socket => Sockethandler,
	       Item => RequestData,
	       Last => Offset,
	       To => Server);
   Receive_Socket(Socket => Sockethandler,
		  Item => ResultData,
		  Last => Offset,
		  From => Server);
   for Index in ResultData'Range loop
      IO.Put (Character'Val (ResultData (Index)));
   end loop;
   GNAT.Sockets.Close_Socket (Sockethandler);
   
exception
   -- extend possibly with more defined exceptions
   when Error: others =>
      AdaImageprocessor.Generic_Functions.Error(Error);
end Adaclient;
