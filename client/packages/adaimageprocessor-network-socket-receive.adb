package body Adaimageprocessor.Network.Socket.Receive is

   function Receive_Data return STREAMLIB.Stream_Element_Array is
      subtype Valid_Connection_Tries is Positive range Positive'First .. SettingsManager.Get_Connection_Tries;
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

   -- private

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

      -- FIXME; what if the above blocks forever (happens on Windows if the receive-buffer is full)
      -- if we are aiming for higher SIL we should probably have a second thread supervising this

      -- last Byte is Null-Terminator, we truncate it here
      return Received_Data( Transmittable_Data_Array'First .. Offset-1 );
   end Raw_Receiver;

end Adaimageprocessor.Network.Socket.Receive;
