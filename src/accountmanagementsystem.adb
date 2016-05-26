with Measures; use Measures;
with Emergency; use Emergency;
with Ada.Text_IO;
-- This package is a SPARK implementation of the assignment 3 of our group.
-- The implementation is also strictly based on requirement described in
-- assignment 1. In this implementation, insurers and wearers are created 
-- by CreateUser. The maximum number of history record which can be stored 
-- is MAX_HISTORY.

package body AccountManagementSystem   
   with SPARK_Mode
is   
   -- Initialization of all needed arrays, record and variables.
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
      
      Null_Record.user := Null_UserID;
      Null_Record.GeoLocation := Null_Location;
      Null_Record.HeartBeat := Null_BPM;
      HistoryRecord := (others => Null_Record);
      
      LatestUser := EmergencyID;
      nextRecordIndex := 0;
   end Init;

   -- Users are created here. The LatestUser will be auto-incremented.
   -- Then the Users(LatestUser) is set to true. If created successful, return
   -- The created userID, otherwise return the Null_UserID.
   procedure CreateUser(NewUser : out UserID) is
   begin
      if(LatestUser>=EmergencyID and LatestUser<UserID'Last)then 
         LatestUser := LatestUser + 1;
         Users(LatestUser) := True;
         NewUser := LatestUser;
         --Ada.Text_IO.Put_Line("create user"& UserID'Image(NewUser));
      else
         NewUser := Null_UserID;
      end if;
   end CreateUser;
   
   -- Insurers are set here. The procedure checks the vadility of the wearer
   -- and the insurer. If pass the check, set the wearer's insurer to the
   -- input insurer and also re-initialize the permission of the insurer.
   procedure SetInsurer(Wearer : in UserID; Insurer : in UserID) is
   begin
      if(Wearer in Users'Range and  Wearer /= Null_UserID and 
           Wearer /= EmergencyID and Insurer /= EmergencyID and
           Insurer in Users'Range and Insurer /= Null_UserID and
           Users(Wearer) = True and Users(Insurer) = True  and
           Wearer /= Insurer and Insurers(Wearer) /= Insurer and
           Insurer in Insurers'First..Insurers'Last) then
         Insurers(Wearer) := Insurer;
         permiOfStepsForInsurer(Wearer) := True;
         permiOfVitalsForInsurer(Wearer) := False;
         permiOfLocasForInsurer(Wearer) := False;
      end if;
   end SetInsurer;
   
   -- This function is for returning the insurer of a wearer.
   function ReadInsurer(Wearer : in UserID) return UserID
   is
   begin
      if(Wearer in Users'Range and  Wearer /= Null_UserID and 
            Wearer /= EmergencyID and Users(Wearer) = True) then
         return Insurers(Wearer);
      else
         return Null_UserID;
      end if;
   end ReadInsurer;
   
   -- This procedure removes a wearer큦 insurer by setting it to Null_UserID.
   -- The procedure first checks the wearer's vadility. If pass the check,
   -- Set the permission of the insurer to default value.
   procedure RemoveInsurer(Wearer : in UserID) is
   begin
      if (Wearer in Users'Range and  Wearer /= Null_UserID and 
           Wearer /= EmergencyID and Users(Wearer) = true) then
         permiOfStepsForInsurer(Wearer) := True;
         permiOfVitalsForInsurer(Wearer):= False;
         permiOfLocasForInsurer(Wearer):= False;
         Insurers(Wearer) := Null_UserID;
      end if;
   end RemoveInsurer;

   -- This function is for returning the friend of a wearer.
   function ReadFriend(Wearer : in UserID) return UserID
   is
   begin
      if(Wearer in Users'Range and  Wearer /= Null_UserID and 
            Wearer /= EmergencyID and Users(Wearer) = True) then
         return Friends(Wearer);
      else
         return Null_UserID;
      end if;
   end ReadFriend;
   
   -- Friends are set here. The procedure checks the vadility of the wearer
   -- and the Friend. If pass the check, set the wearer's Friend to the
   -- input Friend and also re-initialize the permission of the Friend.
   procedure SetFriend(Wearer : in UserID; Friend : in UserID) is
   begin
      if(Wearer in Users'Range and  Wearer /= Null_UserID and 
           Wearer /= EmergencyID and Friend /= EmergencyID 
           and Friend in Users'Range and Friend /= Null_UserID and
           Users(Wearer) = True and Users(Friend) = True  and
           Wearer /= Friend and Friends(Wearer) /= Friend and
           Friend in Friends'First..Friends'Last) then
         Friends(Wearer) := Friend;
         permiOfStepsForFriend(Wearer):= False;
         permiOfVitalsForFriend(Wearer):= False;
         permiOfLocasForFriend(Wearer):= False;
      end if;
   end SetFriend;
   
   -- This procedure removes a wearer큦 friend by setting it to Null_UserID.
   -- The procedure first checks the wearer's vadility. If pass the check,
   -- Set the permission of the friend to default value.
   procedure RemoveFriend(Wearer : in UserID) is
   begin
      if (Wearer in Users'Range and  Wearer /= Null_UserID and 
           Wearer /= EmergencyID and Users(Wearer) = true) then
         permiOfStepsForFriend(Wearer) := False;
         permiOfVitalsForFriend(Wearer) := False;
         permiOfLocasForFriend(Wearer) := False;
         Friends(Wearer) := Null_UserID;
      end if;
   end RemoveFriend;

   -- This function returns a target user큦 footstep.
   -- It first checks the vadility of the requester as well as the 
   -- targetUser. The footstep can be returned only if the check is passed.
   -- Especially, the insurer with of the targetUser can always
   -- read the targetUser's footstep. Otherwise, return Null_Footsteps.
   function ReadFootsteps(Requester : in UserID; TargetUser : in UserID) 
                          return Footsteps   
   is
   begin
      if (Requester in Users'Range and TargetUser in Users'Range and
            Users(TargetUser) = True and Requester /= Null_UserID 
          and (Users(Requester) = True or Requester = EmergencyID)
          and TargetUser /= EmergencyID and TargetUser /= Null_UserID) then
         if ((Friends(TargetUser) = Requester and 
               permiOfStepsForFriend(TargetUser) = True) or 
            (Insurers(TargetUser) = Requester ) or
            (Requester = EmergencyID and 
               permiOfStepsForEmerg(TargetUser) = True)or
            (Requester = TargetUser)) then 
            return MFootsteps(TargetUser);
         else return Null_Footsteps;
         end if;
      else 
         return Null_Footsteps;
      end if;
   end ReadFootsteps;

   -- This function returns a target user큦 location.
   -- It first checks the vadility of the requester as well as the 
   -- targetUser. The location can be returned only if the check is passed.
   -- Especially, the insurer with of the targetUser can always
   -- read the targetUser's location. Otherwise, return Null_Location.
   function ReadLocation(Requester : in UserID; TargetUser : in UserID)
                         return GPSLocation
   is
   begin
   if (Requester in Users'Range and TargetUser in Users'Range and
         (Users(Requester) = True or Requester = EmergencyID) 
       and Users(TargetUser) = True 
          and Requester /= Null_UserID and TargetUser /= EmergencyID) then
         if ((Friends(TargetUser) = Requester and 
               permiOfLocasForFriend(TargetUser) = True) or 
            (Insurers(TargetUser) = Requester and 
                 permiOfLocasForInsurer(TargetUser) = True) or
            (Requester = EmergencyID and 
               permiOfLocasForEmerg(TargetUser) = True)or
            (Requester = TargetUser)) then 
            return Locations(TargetUser);
         else return Null_Location;
         end if;
      else 
         return Null_Location;
      end if;
   end ReadLocation;

   -- This function returns a target user큦 vital.
   -- It first checks the vadility of the requester as well as the 
   -- targetUser. The vital can be returned only if the check is passed.
   -- Especially, the insurer with of the targetUser can always
   -- read the targetUser's vital. Otherwise, return Null_BPM.
   function ReadVitals(Requester : in UserID; TargetUser : in UserID)
                           return BPM 
   is
   begin
      if (Requester in Users'Range and TargetUser in Users'Range and
            Users(TargetUser) = True and 
            (Users(Requester) = True or Requester = EmergencyID)
          and Requester /= Null_UserID and TargetUser /= EmergencyID) then
         if ((Friends(TargetUser) = Requester and 
               permiOfVitalsForFriend(TargetUser) = True) or 
            (Insurers(TargetUser) = Requester and 
                 permiOfVitalsForInsurer(TargetUser) = True) or
            (Requester = EmergencyID and 
               permiOfVitalsForEmerg(TargetUser) = True) or
            (Requester = TargetUser)) then 
            return Vitals(TargetUser);
         else return Null_BPM;
         end if;
      else 
         return Null_BPM;
      end if;
   end ReadVitals;
   
   -- This procedure update the vital of a wearer.
   -- The procedure first check the vadility of the wearer and the newVitals.
   -- The vital can be saved to one wearer only if the check is passed.
   procedure UpdateVitals(Wearer : in UserID; NewVitals : in BPM) is 
   begin
      if(Wearer in Users'Range and  Wearer /= Null_UserID and 
           Wearer /= EmergencyID and Users(Wearer) = True and
         NewVitals /= Null_BPM) then
         Vitals(Wearer) := NewVitals;
      end if;
   end UpdateVitals;
   
   -- This procedure update the step of a wearer.
   -- The procedure first check the vadility of the wearer and the NewFootsteps.
   -- The step can be saved to one wearer only if the check is passed.
   procedure UpdateFootsteps(Wearer : in UserID; NewFootsteps : in Footsteps) is
   begin
      if(Wearer in Users'Range and  Wearer /= Null_UserID and 
           Wearer /= EmergencyID and Users(Wearer) = True 
         and NewFootsteps /= Null_Footsteps) then
         MFootsteps(Wearer) := NewFootsteps;
      end if;
   end UpdateFootsteps;
   
   -- This procedure update the location of a wearer.
   -- The procedure first check the vadility of the wearer and the NewLocation.
   -- The location can be saved to one wearer only if the check is passed.
   procedure UpdateLocation(Wearer : in UserID; NewLocation : in GPSLocation) is
   begin
      if(Wearer in Users'Range and  Wearer /= Null_UserID and 
           Wearer /= EmergencyID and Users(Wearer) = True 
         and NewLocation /= Null_Location) then
         Locations(Wearer) := NewLocation;
      end if;  
   end UpdateLocation;
   
   -- This procedure set the permission of reading vitals of a wearer.
   -- The procedure first check the vadility of the wearer and the other.
   -- If pass the check, the permission would be set to Allow.
   -- Permission owners include emergency, friend and insurer.
   procedure UpdateVitalsPermissions(Wearer : in UserID;
                                     Other : in UserID;
                                     Allow : in Boolean) is 
   begin
      if(Wearer in Users'Range and Users(Wearer) = True and
           Wearer /= Null_UserID and Wearer /= EmergencyID and
             Other in Users'Range  and Wearer /= Other) then 
         if(Other = Friends(Wearer) ) then
            permiOfVitalsForFriend(Wearer) := Allow;
         end if;
         if (Other = Insurers(Wearer)) then
            permiOfVitalsForInsurer(Wearer) := Allow;
         end if;
         if (Other = EmergencyID) then
            permiOfVitalsForEmerg(Wearer) := Allow;
         end if;
      end if;
   end UpdateVitalsPermissions;
   
   -- This procedure set the permission of reading footsteps of a wearer.
   -- The procedure first check the vadility of the wearer and the other.
   -- If pass the check, the permission would be set to Allow. Especially,
   -- when one wearer set this permission of his/her insurer to be false, which
   -- is against the usage of the system, the insurer is removed to avoid wrong
   -- operation and further side consequence.
   -- Permission owners include emergency, friend and insurer.
   procedure UpdateFootstepsPermissions(Wearer : in UserID;
                                        Other : in UserID;
                                        Allow : in Boolean) is
   begin
      if(Wearer in Users'Range and Users(Wearer) = True and
           Wearer /= Null_UserID and Wearer /= EmergencyID and
             Other in Users'Range and Other /= Null_UserID 
            and Wearer /= Other) then 
         if(Other = Friends(Wearer)) then
            permiOfStepsForFriend(Wearer) := Allow;
         end if;
         if Other = Insurers(Wearer) then
            if(Allow = False)then
               Insurers(Wearer) := Null_UserID; 
               permiOfStepsForInsurer(Wearer) := True;
               permiOfVitalsForInsurer(Wearer) := False;
               permiOfLocasForInsurer(Wearer) := False;
            end if;
         end if;    
         if (Other = EmergencyID) then
            permiOfStepsForEmerg(Wearer) := Allow;
         end if;
      end if;
     end UpdateFootstepsPermissions;

   -- This procedure set the permission of reading the location of a wearer.
   -- The procedure first check the vadility of the wearer and the other.
   -- If pass the check, the permission would be set to Allow. 
   -- Permission owners include emergency, friend and insurer.   
   procedure UpdateLocationPermissions(Wearer : in UserID;
                                       Other : in UserID;
                                       Allow : in Boolean) is 
   begin
      if(Wearer in Users'Range and Users(Wearer) = True and
           Wearer /= Null_UserID and Wearer /= EmergencyID and
             Other in Users'Range and Other /= Null_UserID 
             and Wearer /= Other) then 
         if(Other = Friends(Wearer) ) then
            permiOfLocasForFriend(Wearer) := Allow;
         end if;
         if (Other = Insurers(Wearer)) then
            permiOfLocasForInsurer(Wearer) := Allow;
         end if;
         if (Other = EmergencyID) then
            permiOfLocasForEmerg(Wearer) := Allow;
         end if;
      end if;
   end UpdateLocationPermissions;

   -- The procedure is created to call Emergency.ContactEmergency.
   -- It first checks the vadility of the wearer, location and vital.
   -- Then it checks the emergency큦 permission to read the wearer큦 vitals.
   -- The emergency is called only when the emergency has permission
   -- to read the wearer큦 vitals. After that, the info of tis emergency is 
   -- recorded.
   procedure ContactEmergency(Wearer : in UserID; 
                              Location : in GPSLocation; 
                              Vital : in BPM) is
      thisRecord : EmergencyRecord;
   begin
      if(Wearer in Users'Range and Users(Wearer) = True and 
           Wearer /= Null_UserID and Wearer /= EmergencyID and 
             Vital /= Null_BPM and
             Location /= Null_Location and permiOfVitalsForEmerg(Wearer)= True
         and nextRecordIndex <= MAX_HISTORY and nextRecordIndex >= 0) then 
         ContactEmergency(Wearer,Vital,Location);
         thisRecord.user := Wearer;
         thisRecord.GeoLocation := Location;
         thisRecord.HeartBeat := Vital;
         HistoryRecord(nextRecordIndex):= thisRecord;
         nextRecordIndex := nextRecordIndex + 1;
      end if;
      
   end ContactEmergency;
    
end AccountManagementSystem;
