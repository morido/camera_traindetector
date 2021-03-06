--------------------------------------------------------------------------------
-- Package: Adaimageprocessor.Network.Protocol
-- Purpose:
-- Implement the primitive UDP-based communication protocol between the server
-- (camera) and the client (this program)
--
-- Effects:
-- Abstracts the underlying <Adaimageprocessor.Network.Protocol.Socket>
-- package.
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Headers: Adaimageprocessor.Network.Protocol
-- Ada.Strings.Fixed - Functions for Strings with fixed length
-- Adaimageprocessor.Network.Socket - Handles the raw UDP
-- socket communication
-- Adaimageprocessor.Network.Socket.Receive - Handles the receive part of the
-- communication
--------------------------------------------------------------------------------
private with Ada.Strings.Fixed;
with Adaimageprocessor.Network.Socket;
private with Adaimageprocessor.Network.Socket.Receive;

package Adaimageprocessor.Network.Protocol is
   package SOCKETCOMM renames Adaimageprocessor.Network.Socket;

   -----------------------------------------------------------------------------
   -- Package: Adaimageprocessor.Network.Protocol
   -----------------------------------------------------------------------------

   -----------------------------------------------------------------------------
   -- Types: Adaimageprocessor.Network.Protocol
   --  Number_Of_Chunks - an Integer subtype covering all natural numbers up
   --  to 6728, which is the maximum number to be expected for an image
   --  Image_Chunk_Data - same as
   --  <Adaimageprocessor.Network.Socket.Transmittable_Data_Array>
   --  Image_Chunk_Data_NoNumber - the same as <Image_Chunk_Data> but only the
   --  data without the sequence number
   --  Image_Chunks_Type - a type defining an array of Chunks with the
   --  dimension of <Number_Of_Chunks> that can hold the actual image chunks
   --  Image_Chunks - A record which holds an <Image_Chunks_Type> plus the size
   --  of the very last chunk (which may be smaller than the others)
   -----------------------------------------------------------------------------
   subtype Number_Of_Chunks is SOCKETCOMM.Number_Of_Chunks;



   use type STREAMLIB.Stream_Element_Offset;
   subtype Image_Chunk_Data is SOCKETCOMM.Transmittable_Data_Array;
   subtype Image_Chunk_Data_NoNumber is STREAMLIB.Stream_Element_Array(SOCKETCOMM.Transmittable_Data_Array'First+4 .. SOCKETCOMM.Transmittable_Data_Array'Last-1);

   type Image_Chunks_type is array ( Number_Of_Chunks'Range ) of Image_Chunk_Data_NoNumber;
   type Chunk_Data is record
      Image_Chunks : Image_Chunks_type;
      Last_Chunk_Offset : STREAMLIB.Stream_Element_Offset;
   end record;


   -----------------------------------------------------------------------------
   -- Function: Request_Next_Image
   -- Purpose:
   --   Ask the server to internally grab a new image from the camera and
   --   prepare it for transmission (i.e. crop it to the given size)
   --
   -- Parameters:
   --  None.
   --
   -- Returns:
   --  The number of chunks this image will be split into.
   --
   -- Exceptions:
   --  IMAGE_DIMENSION_ERROR - The dimensions of the requested imaged do not
   --  make sense (i.e. the lower right corner is left of/higher up than its
   --  upper left counterpart)
   --  COMMUNICATION_ERROR - The server did not answer the request correctly or
   --  transmitted a specific errormessage
   --  SOCKETCOMM.CONNECTION_ERROR - no data was received within a given timeout
   -----------------------------------------------------------------------------
   function Request_Next_Image return Number_Of_Chunks;

   -----------------------------------------------------------------------------
   -- Function: Request_Chunks
   --
   -- Purpose:
   --   Requests all chunks (= the image-data in small parts) subsequently.
   --
   -- Parameters:
   --   Chunks - How many chunks to receive
   --
   -- Returns:
   --   The image chunks in an array.
   --
   -- Exceptions:
   --   SOCKETCOMM.CONNECTION_ERROR - no data was received within a given
   --   timeout
   --   COMMUNICATION_ERROR - the received data was malformed or the server
   --   transmitted a specific errormessage
   -----------------------------------------------------------------------------
   function Request_Chunks ( Chunks : in Number_Of_Chunks ) return Chunk_Data;

private
   -----------------------------------------------------------------------------
   -- Section: private
   -----------------------------------------------------------------------------


   -----------------------------------------------------------------------------
   -- Variables : Adaimageprocessor.Network.Protocol
   --
   -- COMMUNICATION_ERROR - raised if data was received but the result was not
   -- as expected
   -----------------------------------------------------------------------------
   COMMUNICATION_ERROR : exception;

   -----------------------------------------------------------------------------
   -- Package: OperationIdentifiers
   --
   -- Purpose:
   --   Manage the two character-long strings that identify each network
   --   operation
   -----------------------------------------------------------------------------
   package OperationIdentifiers is

      --------------------------------------------------------------------------
      -- types: OperationIdentifiers
      --
      -- operations - enumeration type for all possible operations whic can be
      -- performed via network
      -- phase1_operations - enumeration type for all possible operations which
      -- can be performed during the first phase (Image-Size settings) of the
      -- transfer
      -- phase2_operations - enumeration type for all possible operations which
      -- can be performed during the second phase (Bulk-Transfer) of the
      -- transfer
      -- error_operations - enuermation type for all possible operations which
      -- signalize any malfunction
      --------------------------------------------------------------------------
      type operations is (
                          Request_Chunks,
                          Request_Next_Image,
                          Error
                         );
      type phase1_operations is (
                                  Request_Next_Image
                                 );
      type phase2_operations is (
                                  Request_Chunks
                                 );
      type error_operations is  (
                                 Error,
                                 No_Error
                                );

      --------------------------------------------------------------------------
      -- Function: ToString
      --
      -- Purpose:
      -- Convert the enumeration type <operations> into a string representation
      -- suitable for transmission via the socket
      --
      -- Parameters:
      -- operation - one of the <operations>
      --
      -- Returns:
      -- A two-character string.
      --------------------------------------------------------------------------
      function ToString (operation : in operations) return String;


      --------------------------------------------------------------------------
      -- Function: ToEnumeration
      --
      -- Purpose:
      -- Convert a two-element array received from the socket into its
      -- enumerated representation of <phase1_operations> suitable for a case
      -- statement
      --
      -- Parameters:
      -- operation - a two-character long array
      --
      -- Returns:
      -- A <phase1_operations>-equivalent
      --------------------------------------------------------------------------
      function ToEnumeration (operation: in STREAMLIB.Stream_Element_Array) return phase1_operations;


      --------------------------------------------------------------------------
      -- Function: ToEnumeration
      --
      -- Purpose:
      -- Convert a two-element array received from the socket into its
      -- enumerated representation of <phase2_operations> suitable for a case
      -- statement
      --
      -- Parameters:
      -- operation - a two-character long array
      --
      -- Returns:
      -- A <phase2_operations>-equivalent
      --------------------------------------------------------------------------
      function ToEnumeration (operation: in STREAMLIB.Stream_Element_Array) return phase2_operations;

      --------------------------------------------------------------------------
      -- Function: ToEnumeration
      --
      -- Purpose:
      -- Convert a two-element array received from the socket into its
      -- enumerated representation of <error_operations> suitable for a case
      -- statement
      --
      -- Parameters:
      -- operation - a two-character long array
      --
      -- Returns:
      -- An <error_operations>-equivalent
      --------------------------------------------------------------------------
      function ToEnumeration(operation: in STREAMLIB.Stream_Element_Array) return error_operations;


   private

      --------------------------------------------------------------------------
      -- section: private
      --------------------------------------------------------------------------

      --------------------------------------------------------------------------
      -- Constants: OperationIdentifiers
      --
      -- OPcode_NextImage - the two-letter identifier used for the first phase
      -- of the transmission process to initiate the transfer; short for
      -- _I_mage _N_ext
      -- OPcode_Chunks - the two-letter identifier used for the second phase of
      -- the transmission process to transfer the actual imagedata; short for
      -- _I_mage _C_hunks
      -- OPcode_Error - the two-letter identifier indicating an error at the
      -- server-side; may occur during both phases
      --------------------------------------------------------------------------
      OPcode_NextImage : constant String := "IN";
      OPcode_Chunks : constant String := "IC";
      OPcode_Error : constant String := "ER";

   end OperationIdentifiers;


   -----------------------------------------------------------------------------
   -- Function: Process_Image_Size
   --
   -- Purpose:
   --   Helper function to pad the specified image size correctly to
   --   four characters, i.e. do a special integer to string casting
   --
   -- Parameters:
   --   Size - The number to be padded
   --
   -- Returns:
   --   A four character string containing the value of _Size_
   --   padded with zeroes from the left.
   --
   -- Exceptions:
   --   Length_Error - The input parameter _Size_ has a value
   --   greater than 9999
   -----------------------------------------------------------------------------
   function Process_Image_Size ( Size : in Natural ) return String;

   -----------------------------------------------------------------------------
   -- Function: Receive_Data
   --
   -- Purpose:
   --   Provides a data receiver which does check for server-side error
   --   messages, thus abstracs <Adaimageprocessor.Network.Socket.Receive_Data>
   --
   -- Parameters:
   --   None.
   --
   -- Returns:
   --   A Stream_Element_Array with the received data.
   --
   -- Exceptions:
   --   CAMERA_ERROR - raised if a server-side error was received
   -----------------------------------------------------------------------------
   function Receive_Data return STREAMLIB.Stream_Element_Array;

end Adaimageprocessor.Network.Protocol;
