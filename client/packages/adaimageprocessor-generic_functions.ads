with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with GNAT.Time_Stamp;


package Adaimageprocessor.Generic_Functions is
   package EXCEPT renames Ada.Exceptions;
   procedure Error ( Errormessage : in EXCEPT.Exception_Occurrence );
   
   private
      package IO renames Ada.Text_IO;
      package SU renames Ada.Strings.Unbounded;
      package TIMESTAMP renames GNAT.Time_Stamp;
      
end Adaimageprocessor.Generic_Functions;
