-- Headers: Adaimageprocessor
-- Ada.Text_IO - Text output, used for printing error messages
-- Ada.Strings.Unbounded - String handling, used to compose error messages
-- Ada.Command_Line - Return exit codes
-- GNAT.Time_Stamp - Prints an ISO-conformant time stamp,
-- note: _Ada.Calendar_ cannot be used here due to Ravenscar-restrictions
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with GNAT.Time_Stamp;


package body Adaimageprocessor is
   package IO renames Ada.Text_IO;
   package SU renames Ada.Strings.Unbounded;
   package CL renames Ada.Command_Line;
   package TIMESTAMP renames GNAT.Time_Stamp;
   
   procedure Error ( Errormessage: in EXCEPT.Exception_Occurrence ) is
      -- called via exception
      Completemessage : SU.Unbounded_String;
   begin
      Completemessage := SU.To_Unbounded_String(TIMESTAMP.Current_Time & " " & EXCEPT.Exception_Name(Errormessage) & ": " & EXCEPT.Exception_Message(Errormessage));
      IO.Put_Line (File => IO.Standard_Error, Item => SU.To_String(Completemessage));
   end Error;
   
   procedure FatalError ( Errormessage: in EXCEPT.Exception_Occurrence ) is
   begin
      IO.Put_Line ("*** Fatal Error encountered. ***");
      Error(Errormessage);
      IO.Put_Line ("*** Will now halt. Goodbye. ***");
      CL.Set_Exit_Status (1);
   end FatalError;
   
end Adaimageprocessor;
