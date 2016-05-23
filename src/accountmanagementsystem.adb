with Measures; use Measures;

package body AccountManagementSystem   
   with SPARK_Mode
is   
   procedure Init is
   begin
      Users := (others => False);
      Insurers := (others => UserID'First);
      Friends := (others => UserID'First);
      Vitals := (others => BPM'First);
      MFootsteps := (others => Footsteps'First);
      Locations := (others => (0.0, 0.0));
   end Init;

   procedure CreateUser(NewUser : out UserID) is
   begin
      LatestUser := LatestUser + 1;
      Users(LatestUser) := True;
      NewUser := LatestUser;
   end CreateUser;
   
   procedure SetInsurer(Wearer : in UserID; Insurer : in UserID) is
   begin
      Insurers(Wearer) := Insurer;
   end SetInsurer;
     
   procedure RemoveInsurer(Wearer : in UserID) is
   begin
      Insurers(Wearer) := UserID'First;
   end RemoveInsurer;

   procedure SetFriend(Wearer : in UserID; Friend : in UserID) is
   begin
      Friends(Wearer) := Friend;
   end SetFriend;
     
   procedure RemoveFriend(Wearer : in UserID) is
   begin
      Friends(Wearer) := UserID'First;
   end RemoveFriend;

   function ReadVitals_Alt(Requester : in UserID; TargetUser : in UserID)
                           return BPM 
   is
   begin
      if Friends(TargetUser) = Requester then
         return Vitals(TargetUser);
      else 
         return BPM'First;
      end if;
   end ReadVitals_Alt;
   
   procedure UpdateVitals(Wearer : in UserID; NewVitals : in BPM) is 
   begin
      Vitals(Wearer) := NewVitals;
   end UpdateVitals;
     
   procedure UpdateFootsteps(Wearer : in UserID; NewFootsteps : in Footsteps) is
   begin
      MFootsteps(Wearer) := NewFootsteps;
   end UpdateFootsteps;
     
   procedure UpdateLocation(Wearer : in UserID; NewLocation : in GPSLocation) is
   begin
      Locations(Wearer) := NewLocation;
   end UpdateLocation;
  
--     procedure UpdateVitalsPermissions(Wearer : in UserID;
--  				     Other : in UserID;
--  				     Allow : in Boolean) is 
--     begin
--     end UpdateVitalsPermissions;
--     
--     procedure UpdateFootstepsPermissions(Wearer : in UserID;
--  					Other : in UserID;
--  					Allow : in Boolean) is
--     begin
--     end UpdateFootstepsPermissions;
--     
--     procedure UpdateLocationPermissions(Wearer : in UserID;
--  				       Other : in UserID;
--  				       Allow : in Boolean) is 
--     begin
--     end UpdateLocationPermissions;
--     
end AccountManagementSystem;
