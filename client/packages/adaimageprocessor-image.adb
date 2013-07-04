package body Adaimageprocessor.Image is

   protected body Imagedata is
      entry Read (data: out storage_for_image) when Ready_For_Read is
      begin
         data := Current_Image;
         Ready_For_Read := false;
      end Read;

      procedure Write (data : in storage_for_image) is
      begin
         Current_Image := data;
         Ready_For_Read := true;
      end Write;

      procedure Shutdown is
      begin
         -- this does not not do anything but to unblock the read operation so
         -- all task depending on this can kill themselves
         Ready_For_Read := true;
         -- return a black image on next read
         Current_Image := (others => (others => STREAMLIB.Stream_Element(0)));
      end Shutdown;
   end Imagedata;



   package body Streamconverter is

      function ToNatural (Input: in STREAMLIB.Stream_Element) return Natural is
         subtype Source     is STREAMLIB.Stream_Element;
         type Naturaloutput is new Natural range 0..255;
         for Naturaloutput'Size use STREAMLIB.Stream_Element'Size; -- ensure same size on both sides of the conversion; kinda redundant here but would cause compiler error if Stream_Element'Size has a strange value...
         subtype Target     is Naturaloutput;
         function convert   is new Ada.Unchecked_Conversion(Source, Target);
         Result: Natural;
      begin
         Result := Natural(convert(Input));
         return Result;
      exception
         when others =>
            raise CONVERSION_ERROR with "Can not convert to numerical value.";
      end ToNatural;

   end Streamconverter;

end Adaimageprocessor.Image;
