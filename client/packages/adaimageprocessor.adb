private with Ada.Text_IO;
private with Ada.Strings.Unbounded;
private with Ada.Command_Line;
private with GNAT.Time_Stamp;
private with Adaimageprocessor.Image;


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
      --manually simulate CTRL+C; this is necessary because we Ravenscar forbids
      -- to use abort-statements and hence requires "normal" termination of all
      -- tasks
      InterruptController.InterruptHandler;
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
         -- allow the image-dependant tasks to terminate as well
         Adaimageprocessor.Image.Imagedata.Shutdown;
      end InterruptHandler;
   end InterruptController;


end Adaimageprocessor;
