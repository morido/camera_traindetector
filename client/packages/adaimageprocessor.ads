--------------------------------------------------------------------------------
-- Package: Adaimageprocessor
-- Purpose:
--   Provide subprograms used throughout the entire <Adaclient> program
--
-- Effects:
--   This package is not supposed to be used seperately.
--
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Headers: Adaimageprocessor
-- Ada.Exceptions - Handle the raised exceptions in Error()
-- Ada.Interrupts.Names - Interface to named interrupts
-- System - For checks of the platform this program runs on
--------------------------------------------------------------------------------
with Ada.Exceptions;
with Ada.Interrupts.Names;
with System;

package Adaimageprocessor is
   
   -----------------------------------------------------------------------------
   -- Pragmas:
   --  Unreserve_All_Interrupts - Prevent GNAT from handling Interrupts, esp.
   --  SIGINT
   -----------------------------------------------------------------------------
   pragma Unreserve_All_Interrupts;
   
   -----------------------------------------------------------------------------
   -- Variables:
   --  ShutdownFlag - A Flag telling <Adaclient> cease operation.
   --  END_TASK - An exception to signalize the need to shut down
   -----------------------------------------------------------------------------
   ShutdownFlag : Boolean := False;
   END_TASK : exception;
   
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
   -- Procedure: FatalError
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
   
   -----------------------------------------------------------------------------
   -- Procedure: AllowShutdown
   -- Purpose:
   --   Can be called by arbitrary subprograms to signalize their safe state for
   --   possible shutdown.
   --
   -- Parameters:
   --   none.
   --
   -- Exceptions:
   --   Raises <END_TASK> if interrupt has been received.
   -----------------------------------------------------------------------------
   procedure AllowShutdown;
   
private
   -----------------------------------------------------------------------------
   -- Section: Private
   -----------------------------------------------------------------------------
   
   -----------------------------------------------------------------------------
   -- Group: Interrupt_Contoller
   -- Purpose:
   --  Protected task to handle interrupts
   -----------------------------------------------------------------------------

   protected InterruptController is
      --------------------------------------------------------------------------
      -- Procedure: InterruptHandler
      --
      -- Purpose: 
      --   Handle Various interrupts
      --
      -- Parameters:
      --   None.
      --
      -- Returns:
      --   Nothing.
      --
      -- Exceptions:
      --   None.
      --------------------------------------------------------------------------
      procedure InterruptHandler;
      
      --------------------------------------------------------------------------
      -- Pragmas:
      --  Attach_Handler - attach the <InterruptHandler> to SIGINT, SIGTERM and
      --  SIGHUP
      --------------------------------------------------------------------------
      pragma Attach_Handler(InterruptHandler, Ada.Interrupts.Names.SIGINT);
      pragma Attach_Handler(InterruptHandler, Ada.Interrupts.Names.SIGTERM);
      pragma Attach_Handler(InterruptHandler, Ada.Interrupts.Names.SIGHUP);
      
   end InterruptController;
   
end Adaimageprocessor;


