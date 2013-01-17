package body Adaimageprocessor.Generic_Functions is
   -- contains functions/procedures used in the entire client program

   
   procedure Error ( Errormessage: in EXCEPT.Exception_Occurrence ) is
      -- called via exception
      Completemessage : SU.Unbounded_String;
   begin
      -- FIXME: Add timestamp
      
      Completemessage := SU.To_Unbounded_String("Timestamp: " & EXCEPT.Exception_Name(Errormessage) & ": " & EXCEPT.Exception_Message(Errormessage));
      IO.Put_Line (File => IO.Standard_Error, Item => SU.To_String(Completemessage));
   end Error;
   
end Adaimageprocessor.Generic_Functions;
