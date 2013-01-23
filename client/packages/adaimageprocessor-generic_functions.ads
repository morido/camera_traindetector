--------------------------------------------------------------------------------
-- Package: Adaimageprocessor.Generic_Functions
-- Purpose:
--   Provide subprograms used throughout the entire <Adaclient> program
--
-- Effects:
--   This package is not supposed to be used seperately.
--
--------------------------------------------------------------------------------

-- Headers: Adaimageprocessor.Generic_Functions
-- Ada.Text_IO - Text output, used for printing error messages
-- Ada.Strings.Unbounded - String handling, used to compose error messages
-- Ada.Exceptions - Handle the raised exceptions in Error()
-- GNAT-Time_Stamp - Prints an ISO-conformant time stamp,
-- note: _Ada.Calendar_ cannot be used here due to Ravenscar-restrictions
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with GNAT.Time_Stamp;


package Adaimageprocessor.Generic_Functions is
   package EXCEPT renames Ada.Exceptions;
   
   -----------------------------------------------------------------------------
   -- Procedure: Error
   -- Purpose:
   --   Print an errormessage together with the current timestamp (IS0 8601 conformant).
   -- 
   -- Parameters:
   --   Errormessage - record which contrains the complete error description.
   --   This includes:
   --   * name (ID) of the exception
   --   * an explanatory message (set via _with_)
   --
   -- Exceptions:
   --   None.
   -----------------------------------------------------------------------------
   procedure Error ( Errormessage : in EXCEPT.Exception_Occurrence );
   
   private
      package IO renames Ada.Text_IO;
      package SU renames Ada.Strings.Unbounded;
      package TIMESTAMP renames GNAT.Time_Stamp;
      
end Adaimageprocessor.Generic_Functions;
