--------------------------------------------------------------------------------
-- Package: Adaimageprocessor
-- Purpose:
--   Provide subprograms used throughout the entire <Adaclient> program
--
-- Effects:
--   This package is not supposed to be used seperately.
--
--------------------------------------------------------------------------------

-- Headers: Adaimageprocessor
-- Ada.Exceptions - Handle the raised exceptions in Error()
with Ada.Exceptions;


package Adaimageprocessor is
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
   
   
   -----------------------------------------------------------------------------
   -- Procedure FatalError
   -- Purpose:
   --   Same as Error(). Terminates the program afterwards.
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
   procedure FatalError ( Errormessage : in EXCEPT.Exception_Occurrence );
   
end Adaimageprocessor;


