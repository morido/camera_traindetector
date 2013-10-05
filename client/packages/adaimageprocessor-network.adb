private with Ada.Unchecked_Conversion;

package body Adaimageprocessor.Network is

   package body Streamconverter is

      function ToStream (Input : in String) return STREAMLIB.Stream_Element_Array is
         subtype Source is String(Input'Range);
         subtype Target is STREAMLIB.Stream_Element_Array(STREAMLIB.Stream_Element_Offset(Input'First)..STREAMLIB.Stream_Element_Offset(Input'Last));
         function convert is new Ada.Unchecked_Conversion(Source, Target);
         Result : Target;
      begin
         Result := convert(Input);
         return Result;
      exception
         when others =>
            raise CONVERSION_ERROR with "Cannot convert to stream";
      end ToStream;

      function ToString (Input: in STREAMLIB.Stream_Element_Array) return String is
         subtype Source is STREAMLIB.Stream_Element_Array(Input'First..Input'Last);
         subtype Target is String(Integer(Input'First)..Integer(Input'Last));
         function convert is new Ada.Unchecked_Conversion(Source, Target);
         Result : Target;
      begin
         Result := convert(Input);
         return Result;
      exception
         when others =>
            raise CONVERSION_ERROR with "Cannot convert to string";
      end ToString;

   end Streamconverter;

end Adaimageprocessor.Network;
