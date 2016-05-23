with Ada.Text_IO;
with Measures; use Measures;

package body Emergency
   with SPARK_Mode
is
   
   procedure ContactEmergency(Wearer : in UserID;
			      Vitals : in BPM;
                              Location : in GPSLocation)
   -- SPARK mode is off because Ada.Text_IO has no contract
   with SPARK_Mode=>Off
   is
   begin
      null;
      Ada.Text_IO.Put_Line("Emergency call placed ");
      Ada.Text_IO.Put_Line("  Wearer: " & Measures.UserID'Image(Wearer));
      Ada.Text_IO.Put_Line("  Vitals: " & Measures.BPM'Image(Vitals));
      Ada.Text_IO.Put_Line("  Location: (" & 
			     Measures.Latitude'Image(Location.Lat) & ", " &
			     Measures.Longitude'Image(Location.Long) & " )");
   end ContactEmergency;
end Emergency;
