--------------------------------------------------------------------------------
-- Package: Adaimageprocessor.Network.Socket.Receive
--
-- Purpose:
-- Separate the <Receive_Data>-function from <Adaimageprocessor.Network.Socket>
-- so it is not automatically visible in <Adaimageprocessor.Network.Protocol>
--------------------------------------------------------------------------------

package Adaimageprocessor.Network.Socket.Receive is


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

private
   -----------------------------------------------------------------------------
   -- section: private
   -----------------------------------------------------------------------------

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


end Adaimageprocessor.Network.Socket.Receive;
