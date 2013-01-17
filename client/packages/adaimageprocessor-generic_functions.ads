with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Exceptions;

package Adaimageprocessor.Generic_Functions is
   package IO renames Ada.Text_IO;
   package SU renames Ada.Strings.Unbounded;
   package EXCEPT renames Ada.Exceptions;
   
   procedure Error ( Errormessage : in EXCEPT.Exception_Occurrence );
end Adaimageprocessor.Generic_Functions;
