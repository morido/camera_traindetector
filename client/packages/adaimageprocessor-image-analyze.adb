package body Adaimageprocessor.Image.Analyze is


   protected body Resulting_Position is
      procedure Write (position : in Integer) is
      begin
         -- we are returning our last (=furthest away from the lower edge of the image) guess on the position of the train
         if position_of_train_in_pixels = -1 or position < position_of_train_in_pixels then
            position_of_train_in_pixels := position;
         end if;

         -- result from one more rail received
         rail_counter := rail_counter + 1;
         if rail_counter = railnumber'Last then
            for current_semaphore in Analyze_Semaphore'Range loop
               STASKC.Set_False(S => Analyze_Semaphore(current_semaphore));
            end loop;
            -- unblock read-operation, i.e. we are done
            position_available := True;
         end if;
      end Write;

      entry Read (position: out Integer) when position_available is
      begin
         position := position_of_train_in_pixels;
         position_available := false;
      end Read;
   end Resulting_Position;

   task body Acquire_Imagedata is
	tmp_imagedata : storage_for_image;
   begin
      loop
         AllowShutdown;
         Imagedata.Read(data => tmp_imagedata);
         Local_imagedata.Set_Imagedata(data => tmp_imagedata);
         for current_semaphore in Analyze_Semaphore'Range loop
            STASKC.Set_True(S => Analyze_Semaphore(current_semaphore));
         end loop;
      end loop;
   exception
      when Error: END_TASK =>
         Cleanup_Acquire_Imagedata;
      when Error: others =>
         Adaimageprocessor.Error(Error);
   end Acquire_Imagedata;



   task body Analyze_Rail is
      slicenumber : Integer;
      output : Integer;
      rail_of_interest : TRACK.Slices := rail_to_process.all;
   begin
      loop
         AllowShutdown;
         STASKC.Suspend_Until_True(S => Analyze_Semaphore(id));
         slicenumber := detect_train_one_rail(input => Local_imagedata.Get_Imagedata, rail_of_interest => rail_of_interest);
         output := Slice_To_Pixel(rail_of_interest => rail_of_interest, Slicenumber => slicenumber);
         Resulting_Position.Write(position => output);
         exit when InterruptController.Shutdown_Requested;
      end loop;
   exception
      when Error: END_TASK =>
         Cleanup_Analyze_Rail(id);
      when Error: others =>
         Adaimageprocessor.Error(Error);
   end Analyze_Rail;

   Left_Rail_Worker : Analyze_Rail(1, TRACK.Left_Rail'Access);
   Right_Rail_Worker : Analyze_Rail(2, TRACK.Right_Rail'Access);


   function detect_train (input: in storage_for_image) return Integer is
      output : Integer;
   begin
      Resulting_Position.Read(position => output);
      return output;
   end detect_train;

   -- private

   package body Local_imagedata is
      procedure Set_Imagedata (data : in storage_for_image) is
      begin
         imagedata_to_analyze := data;
      end Set_Imagedata;

      function Get_Imagedata return storage_for_image is
      begin
         return imagedata_to_analyze;
      end Get_Imagedata;
   end Local_imagedata;


   function detect_train_one_rail (input: in storage_for_image; rail_of_interest : in TRACK.Slices) return Integer is
      meanvalue, slopevalue : slice_meanvalues(rail_of_interest'First..rail_of_interest'Last);
      absolute_minimum : grayvalue := grayvalue'Last;
      first_derivation : double_grayvalue;

   begin
      for Slice in rail_of_interest'Range loop
         meanvalue(Slice) := get_slice_mean(input => input, points_of_interest => rail_of_interest(Slice).Point);
         slopevalue(Slice) := get_slice_moving_average(Current_Slice => Slice, meanvalue => meanvalue, rail_of_interest => rail_of_interest);

         if slopevalue(Slice) < absolute_minimum then
            absolute_minimum := slopevalue(Slice);
         end if;

         numerical_derivation : begin
            first_derivation := slopevalue(Slice) - slopevalue(Slice-1);
         exception
            when Constraint_Error =>
               first_derivation := 0;
         end numerical_derivation;

         if first_derivation >= threshold.derivation_upper then
            -- steep slope, we may have hit the nose of a train
            -- look backwards to confirm this


            possible_train_found : declare
               offset_local_minimum : Natural := 0;
            begin
               find_local_minimum : declare
                  zero_counter : Natural := 0;
               begin
                  outer_compare_loop: while slopevalue(Slice-offset_local_minimum-1)-slopevalue(Slice-offset_local_minimum) <= 0 loop
                     while slopevalue(Slice-offset_local_minimum-1)-slopevalue(Slice-offset_local_minimum) < 0 loop
                        offset_local_minimum := offset_local_minimum +1;
                        zero_counter := 0;
                     end loop;
                     -- allow a 0 in the slope delta for threshold.local_minimum_zero_counter-times
                     if zero_counter <= threshold.local_minimum_zero_counter and slopevalue(Slice-offset_local_minimum-1)-slopevalue(Slice-offset_local_minimum) = 0 then
                        zero_counter := zero_counter + 1;
                        offset_local_minimum := offset_local_minimum +1;
                     else
                        exit outer_compare_loop;
                     end if;
                  end loop outer_compare_loop;

               exception
                  when Constraint_Error =>
                     null; -- out of bounds of slopevalue: the first point must be the local minimum
                     -- FIXME we have to break out from here somehow since subsequent tests dont make sense
               end find_local_minimum;

               if slopevalue(Slice-offset_local_minimum) <= absolute_minimum + threshold.delta_minimum_absolut_local then
                  check_for_shadow_ahead_of_train : declare
                     offset_shadow : Natural := 0;
                  begin
                     while slopevalue(Slice-offset_local_minimum-offset_shadow-1)-slopevalue(Slice-offset_local_minimum-offset_shadow) <= threshold.shadow_upper
                       and slopevalue(Slice-offset_local_minimum-offset_shadow-1)-slopevalue(Slice-offset_local_minimum-offset_shadow) >= threshold.shadow_lower loop
                        offset_shadow := offset_shadow +1;
                        if offset_shadow = threshold.shadow_min_sections then
                           return Slice-offset_local_minimum;
                        end if;
                     end loop;
                  exception
                     when Constraint_Error =>
                        null; -- out of bounds of slopevalue: what to do? lower detection precision
                  end check_for_shadow_ahead_of_train;
               end if;
            end possible_train_found;
         end if;
      end loop;

      -- no train detected
      return -1;
   end detect_train_one_rail;


   function get_slice_mean (input : in storage_for_image;  points_of_interest : in TRACK.Points_Access) return grayvalue is
      sum_of_points : Natural := 0;
      mean : float_for_mean range 0.0..255.0;
   begin
      for Current_Point in points_of_interest'Range loop
         sum_of_points := sum_of_points + Streamconverter.ToNatural(input(points_of_interest(Current_Point).X)(STREAMLIB.Stream_Element_Offset(points_of_interest(Current_Point).Y)));
      end loop;
      mean := float_for_mean(sum_of_points) / float_for_mean(points_of_interest'Length);
      return grayvalue(float_for_mean'Rounding(mean));

   end get_slice_mean;

   function get_slice_moving_average (Current_Slice: in Positive; meanvalue: in slice_meanvalues; rail_of_interest: in TRACK.Slices) return grayvalue is
      sum_of_points : Integer := 0;
      iterations : Natural := 10;
      mean : float_for_mean;
   begin
      for mean_of_interest in 0..iterations-1 loop
         begin
            sum_of_points := sum_of_points + meanvalue(Current_Slice-mean_of_interest);
         exception
            when Constraint_Error => -- this means: Current_Slice < iterations
               iterations := iterations-1; -- "trial and error" but since iterations should be fairly small, this ought to be faster than an if in each call to the function
         end;
      end loop;
      mean := float_for_mean(sum_of_points) / float_for_mean(iterations);
      return grayvalue(float_for_mean'Rounding(mean));

   end get_slice_moving_average;


   function Slice_To_Pixel (rail_of_interest : in TRACK.Slices; Slicenumber : in Integer) return Integer is
   begin
      return rail_of_interest(Slicenumber).Point(rail_of_interest(Slicenumber).Point'First).Y;
   exception
      when Constraint_Error =>
         return -1; --equals "No train found"
   end Slice_To_Pixel;

   procedure Cleanup_Acquire_Imagedata is
      KILL : exception;
   begin
      raise KILL with "Internal image copying: Shutdown. Goodbye.";
   end Cleanup_Acquire_Imagedata;


   procedure Cleanup_Analyze_Rail(id : in railnumber) is
      KILL : exception;
      output : String := "Analyze Rail X: Shutdown. Goodbye.";
   begin
      -- insert the current rail into the string; this number is in no way
      -- linked to the actual task to be terminated. Thus it is only for
      -- informative purposes and not safety-relevant.
      output(14) := railnumber'Image(id)(2);
      raise KILL with output;
   end Cleanup_Analyze_Rail;


end Adaimageprocessor.Image.Analyze;
