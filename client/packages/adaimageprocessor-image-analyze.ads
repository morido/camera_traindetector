--------------------------------------------------------------------------------
-- Headers: Adaimageprocessor.Image.Analyze
-- Adaimageprocessor.Image.Trackdata - information on the position of the track
-- on the images
--------------------------------------------------------------------------------
with Adaimageprocessor.Image.Trackdata;
with Ada.Synchronous_Task_Control;


package Adaimageprocessor.Image.Analyze is

   package TRACK renames Adaimageprocessor.Image.Trackdata;
   package STASKC renames Ada.Synchronous_Task_Control;

   -----------------------------------------------------------------------------
   -- Types: Adaimageprocessor.Image.Analyze
   --
   -- railnumber - a natural able to hold the current rail we are working on
   -----------------------------------------------------------------------------
   subtype railnumber is Natural range 1..2;



-- FIXME documentation missing
   protected Resulting_Position is
      procedure Write (position: in Integer);
      entry Read(position: out Integer);
   private
      position_of_train_in_pixels : Integer := -1;
      position_available : Boolean := False;
      rail_counter : Natural range railnumber'First-1..railnumber'Last := railnumber'First-1;
   end Resulting_Position;

   task Acquire_Imagedata;

   task type Analyze_Rail (id : railnumber; rail_to_process : access constant TRACK.Slices);

   -----------------------------------------------------------------------------
   -- Function: detect_train
   --
   -- Purpose:
   -- Main train detection function
   --
   -- Parameters:
   -- input - The image of interest
   --
   -- Returns:
   -- an integer indicating the position of the train on the image
   --
   -- Exceptions:
   -- None.
   -----------------------------------------------------------------------------
   function detect_train (input: in storage_for_image) return Integer;

private

   -----------------------------------------------------------------------------
   -- Section: Private
   -----------------------------------------------------------------------------

   -----------------------------------------------------------------------------
   -- Types: Adaimageprocessor.Image.Analyze
   --
   -- grayvalue - a type to hold a single pixel of the image
   -- float_for_mean - a float-type wth sufficient accuracy to calculate
   -- meanvalues
   -- slice_meanvalues - an unconstrained array for the meanvalues of the points
   -- of all slices for one track
   -- double_grayvalue - a type used for numerical differentiation between two
   -- variables of <grayvalue>
   -- detection_thresholds - a record to hold all data for thresholding the
   -- rails
   -- detection_thresholds.derivation_upper - the difference between a
   -- slopevalue n and n-1 has to exceed or equal this value so n is considered
   -- a possible train head
   -- detection_thresholds.shadow_lower - the lowest possible difference between
   -- a slopevalue n-1 and n which is still considered a "valid shadow gradient"
   -- detection_thresholds.shadow_upper - the highest possible difference
   -- between a slopevalue n-1 and n which is still considered a
   -- "valid shadow gradient"
   -- detection_thresholds.shadow_min_sections - for how many sections in a row
   -- do the <detection_thresholds.shadow_lower> and
   -- <detection_thresholds.shadow_upper> constraints have to hold true.
   -- detection_thresholds.delta_minimum_absolut_local - the maximum difference
   -- between the local and absolute minimum
   -- detection_thresholds.local_minimum_zero_counter - how many consecutive
   -- zeros in the gradient may be tolerated while trying to find the local
   -- minimum
   -----------------------------------------------------------------------------
   subtype grayvalue is Natural range 0..255;
   type float_for_mean is digits 3;
   type slice_meanvalues is array (Positive range <>) of grayvalue;
   subtype double_grayvalue is Integer range -grayvalue'Last..grayvalue'Last;
   type detection_thresholds is record
         derivation_upper : double_grayvalue;
         shadow_lower : double_grayvalue;
         shadow_upper : double_grayvalue;
         shadow_min_sections : Positive;
         delta_minimum_absolut_local : grayvalue;
         local_minimum_zero_counter : Natural;
   end record;

   -----------------------------------------------------------------------------
   -- Constants: Adaimageprocessor.Image.Analyze
   --
   -- threshold - the actual thresholding data used for the detection, derived
   -- from <detection_thresholds>
   -----------------------------------------------------------------------------
   threshold : constant detection_thresholds := (
                                                 derivation_upper => 6,
                                                 shadow_lower => -3,
                                                 shadow_upper => 9,
                                                 shadow_min_sections => 20,
                                                 delta_minimum_absolut_local => 6,
                                                 local_minimum_zero_counter => 3
                                                );


   -----------------------------------------------------------------------------
   -- Variables: Adaimageprocessor.Image.Analyze
   --
   -- Analyze_Semaphore - An array of semaphores to synchronize the Image
   -- aquisition task FIXME NAME and the image processing tasks FIXME Name
   -----------------------------------------------------------------------------
   Analyze_Semaphore : array (railnumber'Range) of STASKC.Suspension_Object;

   -- FIXME documentation
   package Local_Imagedata is
      procedure Set_Imagedata (data : in storage_for_image);
      function Get_Imagedata return storage_for_image;
   private
      imagedata_to_analyze : storage_for_image;
   end Local_Imagedata;




   -----------------------------------------------------------------------------
   -- Function: detect_train_one_rail
   --
   -- Purpose:
   -- Perform a train detection based only on one single rail
   --
   -- Parameters:
   -- input - the image to be processed
   -- rail_of_interest - the rail to be processed
   --
   -- Returns:
   -- An integer value indicating the position of the train on this particular
   -- rail.
   --
   -- Exceptions:
   -- None.
   -----------------------------------------------------------------------------
   function detect_train_one_rail (input : in storage_for_image; rail_of_interest : in TRACK.Slices) return Integer;

   -----------------------------------------------------------------------------
   -- Function: get_slice_mean
   --
   -- Purpose:
   -- Get the mean value of all imagepixels belonging to a given slice.
   --
   -- Parameters:
   -- input - the image to be processed
   -- points_of_interest - (a pointer to) the points of the slice
   --
   -- Returns:
   -- the mean of all pixels of the slice as a grayvalue.
   --
   -- Exceptions:
   -- None.
   -----------------------------------------------------------------------------
   function get_slice_mean (input : in storage_for_image; points_of_interest : in TRACK.Points_Access) return grayvalue;

   -----------------------------------------------------------------------------
   -- Function: get_slice_moving_average
   --
   -- Purpose:
   -- Get a moving average (a mean value of the means of n slices relative to
   -- current slice) of the current slice.
   --
   -- Effects:
   -- This is a simple way of smoothing the mean-values form <get_slice_mean>.
   --
   -- Parameters:
   -- Current_Slice - the slice for which the mean is to be calculated
   -- meanvalue - an array of all meanvalues of the Slices ahead of
   -- Current_Slice (i.e. of all Slices which have already been processed)
   -- rail_of_interest - which rail to work on
   --
   -- Returns:
   -- the mean of all n last slices as a <float_for_mean>.
   --
   -- Exceptions:
   -- None.
   -----------------------------------------------------------------------------
   function get_slice_moving_average (Current_Slice: in Positive; meanvalue: in slice_meanvalues; rail_of_interest: in TRACK.Slices) return grayvalue;

   -----------------------------------------------------------------------------
   -- Function: Slice_To_Pixel
   --
   -- Purpose:
   -- Returns the Y-value of the first point in the Slice given by Slicenumber
   --
   -- Parameters:
   -- rail_of_interest - the rail to be processed
   -- Slicenumber - the slice of the rail we are interested in
   --
   -- Returns:
   -- an integer with the value of the Y-coordinate of the first point in the
   -- given slice.
   --
   -- Exceptions:
   -- Constraint_Error - The Slicenumber was incorrect.
   -----------------------------------------------------------------------------
   function Slice_To_Pixel (rail_of_interest : in TRACK.Slices; Slicenumber : in Integer) return Integer;


   -----------------------------------------------------------------------------
   -- Procedure: Cleanup_Acquire_Imagedata
   --
   -- Purpose:
   -- Terminate the <Adaimageprocessor.Image.Analyze.Acquire_Imagedata>-task.
   --
   -- Parameters:
   -- None.
   --
   -- Returns:
   -- Nothing.
   --
   -- Exceptions:
   -- KILL - an exception always raised by this procedure.
   -----------------------------------------------------------------------------
   procedure Cleanup_Acquire_Imagedata;

   -----------------------------------------------------------------------------
   -- Procedure: Cleanup_Analyze_Rail
   --
   -- Purpose:
   -- Terminate the <Adaimageprocessor.Image.Analyze.Analyze_Rail>-tasks.
   --
   -- Parameters:
   -- id - the id of the rail whose worker is to be terminated
   --
   -- Returns:
   -- Nothing.
   --
   -- Exceptions:
   -- KILL - an exception always raised by this procedure.
   -----------------------------------------------------------------------------
   procedure Cleanup_Analyze_Rail(id : in railnumber);

end Adaimageprocessor.Image.Analyze;
