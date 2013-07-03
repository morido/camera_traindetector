--------------------------------------------------------------------------------
-- Package: Adaimageprocessor.Image
--
-- Purpose:
-- Implements all processing done on the camera-image
--------------------------------------------------------------------------------

-- this is needed for our protected task;
-- see https://groups.google.com/forum/#!topic/comp.lang.ada/7bnNT7GynzE
pragma Restrictions(No_Implicit_Heap_Allocations);

with Ada.Unchecked_Conversion;

package Adaimageprocessor.Image is

   -----------------------------------------------------------------------------
   -- types:
   --
   -- storage_for_image - a 2d arraw able to accomodate an entire image
   -----------------------------------------------------------------------------
   type storage_for_image is array (Width_Of_Image'Range) of
     STREAMLIB.Stream_Element_Array(STREAMLIB.Stream_Element_Offset(Height_Of_Image'First)..STREAMLIB.Stream_Element_Offset(Height_Of_Image'Last));


   -----------------------------------------------------------------------------
   -- group: Imagedata
   --
   -- Purpose:
   -- Protected task to ensure non-concurrent read and write access to the
   -- current image of the camera
   -----------------------------------------------------------------------------
   protected Imagedata is
      entry Read (data: out storage_for_image);
      procedure Write (data : in storage_for_image);
      procedure Shutdown;
   private
      Current_Image : storage_for_image := (others => (others => STREAMLIB.Stream_Element(0)));
      Ready_For_Read : Boolean := False;
   end Imagedata;


   -----------------------------------------------------------------------------
   -- package: Streamconverter
   --
   -- Purpose:
   --  Provide functions to convert to and from an
   --  Ada.Stream.Stream_Element_Array; see also
   --  <Adaimageprocessor.Network.Streamconverter>
   -----------------------------------------------------------------------------
   package Streamconverter is

      --------------------------------------------------------------------------
      -- Variables: Streamconverter
      --
      -- CONVERSION_ERROR - raised whenever an unchecked conversion does not
      -- work as expected
      --------------------------------------------------------------------------
      CONVERSION_ERROR : exception;

      --------------------------------------------------------------------------
      -- Function: ToNatural
      --
      -- Purpose:
      --  Convert an Ada.Stream.Stream_Element to a Natural-Number
      --
      -- Performance:
      --  This uses an unchecked conversion and thus copies the values
      --  physically. It should be faster than using a for-loop together with
      --  Character'Pos but slower than a conversion on access types or an
      --  address overlay, which work without copying.
      --  See https://groups.google.com/forum/?fromgroups=#!search/comp.lang.ada$20Stream_Element_Array$202$20String/comp.lang.ada/S0wzGdmZj2s/mv_cx6kC6REJ
      --
      -- Parameters:
      --   Input - An Ada.Stream_Element
      --
      -- Returns:
      --   The numerical value representing the byte in the Stream_Element
      --
      -- Exceptions:
      --  CONVERSION_ERROR - see <CONVERSION_ERROR>
      --------------------------------------------------------------------------
      function ToNatural (Input: in STREAMLIB.Stream_Element) return Natural;


   end Streamconverter;


end Adaimageprocessor.Image;
