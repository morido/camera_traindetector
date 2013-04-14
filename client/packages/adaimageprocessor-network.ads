--------------------------------------------------------------------------------
-- Package: Adaimageprocessor.Network
--
-- Purpose:
--  This package gives home to all network-related functions of this program and
--  provides some helper functions, which, unlike those in <Adaimageprocessor>,
--  are only relevant to network communication.
--------------------------------------------------------------------------------
package Adaimageprocessor.Network is

   -----------------------------------------------------------------------------
   -- Package: Streamconverter
   --
   -- Purpose:
   --  Provide functions to convert to and from an
   --  Ada.Stream.Stream_Element_Array
   -----------------------------------------------------------------------------
   package Streamconverter is
      --------------------------------------------------------------------------
      -- Variables: Streamconverter
      --
      -- CONVERSION_ERROR - Raised whenever an unchecked conversion does not
      -- work as expected
      --------------------------------------------------------------------------
      CONVERSION_ERROR : exception;

      --------------------------------------------------------------------------
      -- Function: ToStream
      --
      -- Purpose:
      --  Convert a String to an Ada.Stream.Stream_Element_Array
      --
      -- Performance:
      --  This uses an unchecked conversion and thus copies the values
      --  physically. It should be faster than using a for-loop together with
      --  Character'Pos but slower than a conversion on access types or an
      --  address overlay, which work without copying.
      --  See https://groups.google.com/forum/?fromgroups=#!search/comp.lang.ada$20Stream_Element_Array$202$20String/comp.lang.ada/S0wzGdmZj2s/mv_cx6kC6REJ
      --
      -- Parameters:
      --   Input - A String to be converted.
      --
      -- Returns:
      --   An Ada.Stream_Element_Array with as many elements as the input had
      --  characters
      --
      -- Exceptions:
      --  CONVERSION_ERROR - see <CONVERSION_ERROR>
      --------------------------------------------------------------------------
      function ToStream (Input: in String) return STREAMLIB.Stream_Element_Array;

      --------------------------------------------------------------------------
      -- Function: ToString
      --
      -- Purpose:
      --  Convert an Ada.Stream.Stream_Element_Array to a String
      --
      -- Performance:
      --  This uses an unchecked conversion and thus copies the values
      --  physically. It should be faster than using a for-loop together with
      --  Character'Pos but slower than a conversion on access types or an
      --  address overlay, which work without copying.
      --  See https://groups.google.com/forum/?fromgroups=#!search/comp.lang.ada$20Stream_Element_Array$202$20String/comp.lang.ada/S0wzGdmZj2s/mv_cx6kC6REJ
      --
      -- Parameters:
      --   Input - An Ada.Stream_Element_Array
      --
      -- Returns:
      --   A String with as many characters as the input had
      --  elements
      --
      -- Exceptions:
      --  CONVERSION_ERROR - see <CONVERSION_ERROR>
      --------------------------------------------------------------------------
      function ToString (Input: in STREAMLIB.Stream_Element_Array) return String;
   end Streamconverter;

end Adaimageprocessor.Network;
