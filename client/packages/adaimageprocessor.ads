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
   -- Variables:
   --  ShutdownFlag - A Flag telling <Adaclient> cease operation.
   --  END_TASK - An exception to signalize the need to shut down
   -----------------------------------------------------------------------------

   END_TASK : exception;

   -----------------------------------------------------------------------------
   -- Pragmas:
   --  Unreserve_All_Interrupts - Prevent GNAT from handling Interrupts, esp.
   --  SIGINT
   -----------------------------------------------------------------------------
   pragma Unreserve_All_Interrupts;

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
      --  Pragmas:
      --  Priority - set the priority of this task to the lowest possible value
      --------------------------------------------------------------------------
      pragma Priority(System.Priority'First);

      --------------------------------------------------------------------------
      -- Function: Shutdown_Requested
      --
      -- Purpose:
      --   Provides a Getter-Function to obtain <ShutdownFlag>
      --
      -- Parameters:
      --   None.
      --
      -- Returns:
      --   A boolean value representing the current state of <ShutdownFlag>,
      --   hence this returns *true* if an interrupt has occured and *false* if
      --   not
      --
      -- Exceptions:
      --   None.
      --------------------------------------------------------------------------
      function Shutdown_Requested return Boolean;

      --------------------------------------------------------------------------
      -- section: Private
      --------------------------------------------------------------------------
   private


      --------------------------------------------------------------------------
      -- Procedure: InterruptHandler
      --
      -- Purpose:
      --   Handle various system-interrupts (CTRL-C, ...)
      --
      -- Effects:
      --   Causes all other tasks to terminate at the next possible point.
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

      --------------------------------------------------------------------------
      -- Variables:
      --
      -- ShutdownFlag - internal variable indicating if an interrupt has occured
      --------------------------------------------------------------------------
      ShutdownFlag : Boolean := False;

   end InterruptController;

end Adaimageprocessor;


