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
-- Ada.Streams - not used in this file; but stores imagedata in various other
-- files
--------------------------------------------------------------------------------
with Ada.Exceptions;
private with Ada.Interrupts.Names;
with System;
with Ada.Streams;

package Adaimageprocessor is


   -----------------------------------------------------------------------------
   -- Types:
   --
   --  Width_Of_Image - an Integer subtype covering all natural numbers up to
   --  960, which is the maximum width of an image captured by the camera
   --  Height_Of_Image - an Integer subtype covering all natural numbers up to
   --  1280, which is the maximum height of an image captured by the camera
   -----------------------------------------------------------------------------
   subtype Width_Of_Image is Natural range 0 .. 571;
   subtype Height_Of_Image is Natural range 0 .. 723;


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
   package STREAMLIB renames Ada.Streams;

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
   -- Section: private
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
      -- Section: private
      --------------------------------------------------------------------------
   private

      --------------------------------------------------------------------------
      -- Pragmas: InterruptContoller

      --  Attach_Handler - attach the <InterruptHandler> to SIGINT, SIGTERM and
      --  SIGHUP
      --------------------------------------------------------------------------
      pragma Attach_Handler(InterruptHandler, Ada.Interrupts.Names.SIGINT);
      pragma Attach_Handler(InterruptHandler, Ada.Interrupts.Names.SIGTERM);
      pragma Attach_Handler(InterruptHandler, Ada.Interrupts.Names.SIGHUP);

      --------------------------------------------------------------------------
      -- Variables: InterruptController
      --
      -- ShutdownFlag - internal variable indicating if an interrupt has occured
      --------------------------------------------------------------------------
      ShutdownFlag : Boolean := False;

   end InterruptController;

end Adaimageprocessor;


