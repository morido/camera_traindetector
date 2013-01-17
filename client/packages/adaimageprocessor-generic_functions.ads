with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Calendar;
with GNAT.Calendar.Time_IO;

package Adaimageprocessor.Generic_Functions is
   package EXCEPT renames Ada.Exceptions;
   procedure Error ( Errormessage : in EXCEPT.Exception_Occurrence );
   
   private
      package IO renames Ada.Text_IO;
      package SU renames Ada.Strings.Unbounded;
      package TIME renames Ada.Calendar;
      package TIMEFORMAT renames GNAT.Calendar.Time_IO;
      
end Adaimageprocessor.Generic_Functions;
