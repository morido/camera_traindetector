--------------------------------------------------------------------------------
-- Package: Adaimageprocessor.Output
--
-- Purpose:
-- Handle the regular status output of the program
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Headers: Adaimageprocessor.Output
--
-- Ada.Text_IO - For printing to the console
-- Ada.Real_Time - Ravenscar-compliant stopwatch-facilities
-- Adaimageprocessor.Image.Analyze - Link to the needed input data
--------------------------------------------------------------------------------
private with Ada.Text_IO;
private with Ada.Real_Time;
private with Adaimageprocessor.Image.Analyze;

package Adaimageprocessor.Output is

   -----------------------------------------------------------------------------
   -- group: Print
   --
   -- Purpose:
   -- A separate task to handle all the regular output (i.e. printing the
   -- position of the train on the image) of the program
   -----------------------------------------------------------------------------
   task Print;

   -----------------------------------------------------------------------------
   -- section: private
   -----------------------------------------------------------------------------
private

   -----------------------------------------------------------------------------
   -- Package: Timer
   --
   -- Purpose:
   -- Provide a stopwatch for <Adaimageprocessor.Output.Print> so the execution
   -- time can be measured
   -----------------------------------------------------------------------------
   package Timer is
      --------------------------------------------------------------------------
      -- Procedure: Start
      --
      -- Purpose:
      -- Start the stopwatch.
      --
      -- Parameters:
      -- None.
      --
      -- Returns:
      -- Nothing.
      --------------------------------------------------------------------------
      procedure Start;

      --------------------------------------------------------------------------
      -- Function: Stop
      --
      -- Purpose:
      -- Stop the Stopwatch.
      --
      -- Parameters:
      -- None.
      --
      -- Returns:
      -- A formatted string of the inverse of the time (= the frequency
      -- = the frames per second) that has elapsed since the last call to
      -- <Start>
      --------------------------------------------------------------------------
      function Stop return String;
   private
      starttime, stoptime : Ada.Real_Time.Time;
   end Timer;



end Adaimageprocessor.Output;
