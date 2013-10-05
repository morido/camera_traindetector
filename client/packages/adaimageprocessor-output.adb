package body Adaimageprocessor.Output is

   task body Print is
      output : integer;
      stop : Exception;
   begin
      loop
         AllowShutdown;
         Timer.Start;
         Adaimageprocessor.Image.Analyze.Resulting_Position.Read(position => output);
         declare
            time : constant String := Timer.Stop;
         begin
            if output = -1 then
               Ada.text_io.put_line(time & "No train detected.");
            else
               Ada.text_io.put_line(time & "Position in Y-pixels:" & Integer'Image(output));
            end if;
         end;

        --raise stop with "Shutting down...";
      end loop;
   exception
      when Error: stop =>
         Adaimageprocessor.Error(Error);
   end Print;

   -- private

   package body Timer is
      procedure Start is
      begin
         starttime := Ada.Real_Time.Clock;
      end Start;

      function Stop return String is
         difference : Ada.Real_Time.Time_Span;
         fps : float digits 3;
      begin
         stoptime := Ada.Real_Time.Clock;
         difference := Ada.Real_Time."-"(stoptime, starttime);
         fps := float(1.0 / Ada.Real_Time.To_Duration (difference));
         return integer'Image(integer(float'Rounding(fps))) & " fps | ";
      end Stop;
   end Timer;

end Adaimageprocessor.Output;
