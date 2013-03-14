--------------------------------------------------------------------------------
-- Headers: Adaimageprocessor
-- Ada.Text_IO - Text output, used for printing error messages
-- Ada.Strings.Unbounded - String handling, used to compose error messages
-- Ada.Command_Line - Return exit codes
-- GNAT.Time_Stamp - Prints an ISO-conformant time stamp,
-- note: _Ada.Calendar_ cannot be used here due to Ravenscar-restrictions
--------------------------------------------------------------------------------
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with GNAT.Time_Stamp;


package body Adaimageprocessor is

   procedure Error ( Errormessage: in EXCEPT.Exception_Occurrence ) is
      -- called via exception

      package IO renames Ada.Text_IO;
      package SU renames Ada.Strings.Unbounded;
      package CL renames Ada.Command_Line;
      package TIMESTAMP renames GNAT.Time_Stamp;

      Completemessage : SU.Unbounded_String;
   begin
      Completemessage := SU.To_Unbounded_String(TIMESTAMP.Current_Time & " " & EXCEPT.Exception_Name(Errormessage) & ": " & EXCEPT.Exception_Message(Errormessage));
      IO.Put_Line (File => IO.Standard_Error, Item => SU.To_String(Completemessage));
      CL.Set_Exit_Status (CL.Failure);
   end Error;

   procedure AllowShutdown is
   begin
      if InterruptController.Shutdown_Requested then
	 raise END_TASK;
      end if;
   end AllowShutdown;

   -- private

   protected body InterruptController is
      function Shutdown_Requested return Boolean is
      begin
         return ShutdownFlag;
      end Shutdown_Requested;

      -- private
      procedure InterruptHandler is
      begin
	 ShutdownFlag := True;
      end InterruptHandler;
   end InterruptController;


end Adaimageprocessor;
