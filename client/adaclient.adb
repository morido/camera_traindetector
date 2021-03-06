--------------------------------------------------------------------------------
-- Package: Adaclient
-- Purpose:
--   This is the main client file which performs the image-requests from the
--   server, applies the detection algorithm and finally returns the presence
--   and position of a train within the received picture.
--   It is expected to run this as a service on a dedicated machine
--
-- Performance:
--   This program has been optimized for real-time applications.
--   - The image transfer should complete within FIXME ms
--   - The train detection takes FIXME ms
--------------------------------------------------------------------------------

pragma Profile (Ravenscar);
pragma Ada_05;
with Adaimageprocessor.Network.Protocol.Imagetransfer;
with Adaimageprocessor.Image.Analyze;
with Adaimageprocessor.Output;

--------------------------------------------------------------------------------
-- Procedure: Adaclient
--
-- Purpose:
--  Main program, does nothing. Actual work is done within tasks, which are
--  implicitely startet by this environment-task.
--------------------------------------------------------------------------------



procedure Adaclient is
begin

   -- all the work is done inside tasks which are implicitly started upon
   -- execution of this main procedure; therefore we do absolutely nothing here

  null;


end Adaclient;
