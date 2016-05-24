with Measures; use Measures;

package body AccountManagementSystem   
   with SPARK_Mode
is   
   procedure Init is
   begin
      
      Users := (others => False);
      Insurers := (others => Null_UserID);
      Friends := (others => Null_UserID);
      Vitals := (others => Null_BPM );
      MFootsteps := (others => Null_Footsteps);
      Locations := (others => Null_Location);
      permiOfStepsForInsurer := (others => True);
      permiOfVitalsForInsurer := (others => False);
      permiOfLocasForInsurer := (others => False);
      
      permiOfStepsForFriend := (others => False);
      permiOfVitalsForFriend := (others => False);
      permiOfLocasForFriend := (others => False);
      
      permiOfStepsForEmerg := (others => False);
      permiOfVitalsForEmerg := (others => False);
      permiOfLocasForEmerg := (others => False);
      
   end Init;

   procedure CreateUser(NewUser : out UserID) is
   begin
      if(LatestUser>EmergencyID and LatestUser<UserID'Last)then 
         LatestUser := LatestUser + 1;
         Users(LatestUser) := True;
         NewUser := LatestUser;
      else
         NewUser := Null_UserID;
      end if;
       
   end CreateUser;
   
   procedure SetInsurer(Wearer : in UserID; Insurer : in UserID) is
   begin
      if(Wearer in Users'Range and  Wearer /= Null_UserID and 
           Wearer /= EmergencyID and Insurer /= EmergencyID and
           Insurer in Users'Range and Insurer /= Null_UserID and
           Users(Wearer) = True and Users(Insurer) = True  and
           Wearer /= Insurer) then
         Insurers(Wearer) := Insurer;
      end if;
      
   end SetInsurer;
     
   procedure RemoveInsurer(Wearer : in UserID) is
   begin
      if (Wearer in Users'Range and  Wearer /= Null_UserID and 
           Wearer /= EmergencyID and Users(Wearer) = true
          and Insurers(Wearer) in Users'Range 
          and Users(ReadInsurer(Wearer)) = true) then
         permiOfStepsForInsurer := (Insurers(Wearer) => True);
         permiOfVitalsForInsurer := (Insurers(Wearer) => False);
         permiOfLocasForInsurer := (Insurers(Wearer) => False);
         Insurers(Wearer) := Null_UserID;
      end if;
   end RemoveInsurer;

   procedure SetFriend(Wearer : in UserID; Friend : in UserID) is
   begin
      if(Wearer in Users'Range and  Wearer /= Null_UserID and 
           Wearer /= EmergencyID and Friend /= EmergencyID 
           and Friend in Users'Range and Friend /= Null_UserID and
           Users(Wearer) = True and Users(Friend) = True  and
           Wearer /= Friend) then
         Friends(Wearer) := Friend;
      end if;
   end SetFriend;
     
   procedure RemoveFriend(Wearer : in UserID) is
   begin
      if (Wearer in Users'Range and  Wearer /= Null_UserID and 
           Wearer /= EmergencyID and Users(Wearer) = true 
          and Friends(Wearer) in Users'Range 
          and Users(ReadFriend(Wearer)) = true) then
         permiOfStepsForFriend := (others => False);
         permiOfVitalsForFriend := (others => False);
         permiOfLocasForFriend := (others => False);
         Friends(Wearer) := Null_UserID;
      end if;
   end RemoveFriend;

   function ReadVitals(Requester : in UserID; TargetUser : in UserID)
                           return BPM 
   is
   begin
      if (Requester in Users'Range and TargetUser in Users'Range and
            Users(Requester) = True and Users(TargetUser) = True 
          and Requester /= Null_UserID and TargetUser /= EmergencyID
          and TargetUser not in Insurers'First..Insurers'Last) then
         if ((Friends(TargetUser) = Requester and 
               permiOfVitalsForFriend(TargetUser) = True) or 
            (Insurers(TargetUser) = Requester and 
                 permiOfVitalsForInsurer(TargetUser) = True) or
            (Requester = EmergencyID and 
                 permiOfVitalsForEmerg(TargetUser) = True))
         then 
            return Vitals(TargetUser);
         else return Null_BPM;
         end if;
         
      else 
         return Null_BPM;
      end if;
   end ReadVitals;
   
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
     procedure UpdateFootstepsPermissions(Wearer : in UserID;
  					Other : in UserID;
  					Allow : in Boolean) is
   begin
      permiOfStepsForFriend(Wearer) := Allow;
     end UpdateFootstepsPermissions;
--     
--     procedure UpdateLocationPermissions(Wearer : in UserID;
--  				       Other : in UserID;
--  				       Allow : in Boolean) is 
--     begin
--     end UpdateLocationPermissions;

   procedure ContactEmergency(Wearer : in UserID; 
                              Location : in GPSLocation; 
                              Vital : in BPM) is
   begin
      Vitals(Wearer) := Vital;
   end ContactEmergency;
    
end AccountManagementSystem;
