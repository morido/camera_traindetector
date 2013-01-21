pragma Profile (Ravenscar);
with Ada.Text_IO;


-- packages of this very program
with Adaimageprocessor.Generic_Functions;
with Adaimageprocessor.Socket;

procedure Adaclient is   
   package IO renames Ada.Text_IO;
   package SOCKET_COMM renames Adaimageprocessor.Socket;
   
   RequestData : Character;
   Server_IP : constant String := "127.0.0.1";
   Server_Port : constant Positive := 12345;
begin
   SOCKET_COMM.Open_Socket(Server_IP, Server_Port);
   -- Make request for new image data
   RequestData := '1';
   SOCKET_COMM.Send_Data(RequestData);
   IO.Put_Line(SOCKET_COMM.Receive_Data);
   SOCKET_COMM.Close_Socket;
   
exception
   -- extend possibly with more defined exceptions
   when Error: others =>
      AdaImageprocessor.Generic_Functions.Error(Error);
end Adaclient;
