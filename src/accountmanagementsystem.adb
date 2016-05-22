with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Measures; use Measures;
with Emergency; use Emergency;

package body AccountManagementSystem    
   with SPARK_Mode => On
is

   
   -- Initialization. Companies are created from MAX_USERID-n+1 to MAX_USERID
   -- in the array of Users.
   procedure Init is
      n : Integer;
      thisWearer: Wearers;
   begin
      n := NoI;
      while n >0 loop
         thisWearer.ID := UserID(MAX_USERID-n+1);
         Users(UserID(MAX_USERID-n+1)) := thisWearer;
         n := n - 1;
      end loop;
   end Init;
   
   -- Users are created here. The userID will be auto-incremented.
   -- Each user is assigned to the list of Users. return -1 means invalid.
   function ReturnUser return UserID is
      thisWearer : Wearers;
      auto : UserID := AutoIncrementalBase;
   begin
    if Integer(auto) < MAX_USERID-NoI+1 then 
         return auto;
    else 
         return -1;
    end if; 
   end ReturnUser;
   
   procedure CreateUser is 
       thisWearer : Wearers;
   begin
      if Integer(AutoIncrementalBase) < MAX_USERID-NoI+1 then
      Users(AutoIncrementalBase).ID := AutoIncrementalBase;
      AutoIncrementalBase := AutoIncrementalBase + 1;
      end if;
   end CreateUser;
   
     
   -- Insurers are set here. The procedure first checks if the wearer exists.
   -- If InsurerID is in the range of insurer, 
   -- the user is able to set this id as his insurer.
   procedure SetInsurer(Wearer : in UserID; Insurer : in UserID) is
   begin
      if Wearer<=AutoIncrementalBase-1 and Wearer /= 0 and Wearer /= -1 then
         if Integer(Insurer) > MAX_USERID-NoI and 
           Integer(Insurer) < MAX_USERID+1 then
            Users(Wearer).Insurance := Insurer;
         end if;
      end if;
   end SetInsurer;

   -- This function returns the ID of a wearer큦 insurer.
   -- The function first checks if the wearer exists, if not returns -1.
   function ReadInsurer(Wearer : in UserID) return UserID is
   begin
      if Wearer>AutoIncrementalBase-1 or Wearer = 0 or Wearer = -1 then
         return -1;
      else 
         return Users(Wearer).Insurance;
      end if;
   end ReadInsurer;
   
   -- This precedure removes a wearer큦 insurer by setting it to -1.
   -- The precedure first checks if the wearer exists.
   procedure RemoveInsurer(Wearer : in UserID) is
   begin
      if Wearer<=AutoIncrementalBase-1 and Wearer /= 0 and Wearer /= -1 then
         Users(Wearer).Insurance := -1;
         Users(Wearer).InsurancePermissionHB := False;
         Users(Wearer).InsurancePermissionGL := False;
         Users(Wearer).InsurancePermissionSN := False;
      end if;
   end RemoveInsurer;
   
   -- Friends are set here. The procedure first check if the wearer exists.
   -- Then it checks if NewFriend is a potential
   -- friend of the wearer, that is, NewFriend should not be himself, 
   -- emergency, an insurer, or anyone who is not a wearer in the system.
   procedure SetFriend(Wearer : in UserID; NewFriend : in UserID)is
     
   begin
      if Wearer<=AutoIncrementalBase-1 and Wearer /= 0 and Wearer /= -1 then
      
         if NewFriend /= Wearer and NewFriend /= 0 then
               if not (Integer(NewFriend) > MAX_USERID-NoI 
                 and Integer(NewFriend) < MAX_USERID+1)then
                  
                  if not (NewFriend > AutoIncrementalBase 
                    and Integer(NewFriend) < MAX_USERID-NoI+1 )then
                     
                     Users(Wearer).Friend := NewFriend;
                  end if;
               end if;
         end if;
      end if;
   end SetFriend;
   
   -- This function returns the ID of a wearer큦 friends.
   -- The function first checks if the wearer exists, if not returns -1.
   function ReadFriend(Wearer : in UserID) return UserID is
   begin
      if Wearer>AutoIncrementalBase-1 or Wearer = 0 or Wearer = -1 then
         return -1;
      else 
         return Users(Wearer).Friend;
      end if;
   end ReadFriend;
   
   -- This function removes a wearer큦 friend by setting it to -1.
   -- The precedure first checks if the wearer exists.
   procedure RemoveFriend(Wearer : in UserID) is
   begin
      if Wearer<=AutoIncrementalBase-1 and Wearer /= 0 and Wearer /= -1 then
         Users(Wearer).Friend := -1; 
         Users(Wearer).FriendPermissionHB := False;
         Users(Wearer).FriendPermissionGL := False;
         Users(Wearer).FriendPermissionSN := False;
      end if;
   end RemoveFriend;
   
   -- This procedure set the permission to read vitals of a wearer.
   -- The procedure first check if the wearer exists.
   -- Permission owners include emergency, friend and insurer.
   procedure UpdateVitalsPermissions(Wearer : in UserID;
                                     Other : in UserID;
                                     Allow : in Boolean) is 
   begin
      if Wearer<=AutoIncrementalBase-1 and Wearer /= 0 and Wearer /= -1 then
         if Other = 0 then
            Users(Wearer).EmergencyPermissionHB := Allow;
         else
            if Other /= Wearer then
               if Other = Users(Wearer).Insurance then
                  Users(Wearer).InsurancePermissionHB := Allow;
               else 
                  if Other = Users(Wearer).Friend then
                     Users(Wearer).FriendPermissionHB := Allow;
                  end if;
               end if;
            end if;
         end if;
      end if;
   end UpdateVitalsPermissions;
   
   -- This procedure set the permission to read footSteps of a wearer.
   -- The procedure first check if the wearer exists.
   -- Permission owners include emergency, friend and insurer.
   procedure UpdateFootstepsPermissions(Wearer : in UserID;
                                        Other : in UserID;
                                        Allow : in Boolean) is
   begin
      if Wearer<=AutoIncrementalBase-1 and Wearer /= 0 and Wearer /= -1 then
  
         if Other /= Wearer then
            if Other = Users(Wearer).Insurance then
               Users(Wearer).InsurancePermissionSN := Allow;
            else 
               if Other = Users(Wearer).Friend then
                  Users(Wearer).FriendPermissionSN := Allow;
               else
                  if Other = 0 then
                     Users(Wearer).EmergencyPermissionSN := Allow;
                  end if;
               end if;
            end if;
         end if;
      end if;
   end UpdateFootstepsPermissions;
   
   -- This procedure set the permission to read locations of a wearer.
   -- The procedure first check if the wearer exists.
   -- Permission owners include emergency, friend and insurer.
   procedure UpdateLocationPermissions(Wearer : in UserID;
                                       Other : in UserID;
                                       Allow : in Boolean) is 
   begin
      if Wearer<=AutoIncrementalBase-1 and Wearer /= 0 and Wearer /= -1 then
         if Other = 0 then
            Users(Wearer).EmergencyPermissionGL := Allow;
         else
            if Other /= Wearer then
               if Other = Users(Wearer).Insurance then
                  Users(Wearer).InsurancePermissionGL := Allow;
               else 
                  if Other = Users(Wearer).Friend then
                     Users(Wearer).FriendPermissionGL := Allow;
                  end if;
               end if;
            end if;
         end if;
      end if;
   end UpdateLocationPermissions;
   
   -- This procedure update the vital of a wearer.
   -- The procedure first check if the wearer exists.
   -- The vital can be saved to one wearer only if he is a wearer.
   procedure UpdateVitals(Wearer : in UserID; NewVitals : in BPM) is 
    
   begin
      if not ((Wearer > AutoIncrementalBase-1 and 
         Integer(Wearer) < MAX_USERID-NoI+1) or Wearer = -1 )then
         
         if not (Integer(Wearer) > MAX_USERID-NoI and 
            Integer(Wearer) < MAX_USERID+1 )then
          
            if Wearer /= 0 then 
               Users(Wearer).HeartBeat := NewVitals;
            end if;
         end if;
      end if;
   end UpdateVitals;
   
   -- This procedure update the footStep of a wearer.
   -- The procedure first check if the wearer exists.
   -- The footStep can be saved to one wearer only if he is a wearer.
   procedure UpdateFootsteps
     (Wearer : in UserID; NewFootsteps : in Footsteps) is
   begin
      if not ((Wearer > AutoIncrementalBase-1 and 
         Integer(Wearer) < MAX_USERID-NoI+1) or Wearer = -1 )then
         
         if not (Integer(Wearer) > MAX_USERID-NoI and 
            Integer(Wearer) < MAX_USERID+1) then
            if Wearer /= 0 then
               Users(Wearer).StepNum := NewFootsteps;
            end if;
         end if;
      end if;
   end UpdateFootsteps;
   
   -- This procedure update the location of a wearer.
   -- The procedure first check if the wearer exists.
   -- The location can be saved to one wearer only if he is a wearer.
   procedure UpdateLocation
     (Wearer : in UserID; NewLocation : in GPSLocation) is
   begin
      if not ((Wearer > AutoIncrementalBase-1 and 
         Integer(Wearer) < MAX_USERID-NoI+1) or Wearer = -1 )then
         if not (Integer(Wearer) > MAX_USERID-NoI and 
            Integer(Wearer) < MAX_USERID+1 )then
            if Wearer /= 0 then
               Users(Wearer).GeoLocation := NewLocation;
            end if;
         end if;
      end if;
   end UpdateLocation;
   
   -- This function returns a target user큦 vital.
   -- It first checks if the target wearer exists, then checks the 
   -- requester큦 permission. The vital can be returned only if the target
   -- wearer exists and the permission is true. 
   -- If cannot read, return invalidVital.
   function ReadVitals(Requester : in UserID; TargetUser : in UserID)
                       return BPM is
   begin
      if TargetUser > 0 and TargetUser < AutoIncrementalBase and 
        (Requester = TargetUser or (Requester = 0 and 
         Users(TargetUser).EmergencyPermissionSN = True)
         or Requester = TargetUser or (Requester = Users(TargetUser).Friend
         and Users(TargetUser).FriendPermissionHB = True)
         or (Requester = Users(TargetUser).Insurance and 
         Users(TargetUser).InsurancePermissionHB = True)) then
         return Users(TargetUser).HeartBeat;
      else
         return invalidVital;
      end if;
   end ReadVitals;
   
   -- This function returns a target user큦 footstep.
   -- It first checks if the target wearer exists, then checks the 
   -- requester큦 permission. The footstep can be returned only if the target
   -- wearer exists and the permission is true. 
   -- If cannot read, return invalidFootStep.
   function ReadFootsteps(Requester : in UserID; TargetUser : in UserID)
                          return Footsteps is 
   begin
      if TargetUser > 0 and TargetUser < AutoIncrementalBase and 
        (Requester = TargetUser or (Requester = 0 and 
         Users(TargetUser).EmergencyPermissionSN = True)
         or Requester = TargetUser or (Requester = Users(TargetUser).Friend
         and Users(TargetUser).FriendPermissionHB = True)
         or (Requester = Users(TargetUser).Insurance and 
         Users(TargetUser).InsurancePermissionHB = True)) then
         return Users(TargetUser).StepNum;
      else
         return invalidFootStep;
      end if;
   end ReadFootsteps;
   
   -- This function returns a target user큦 location.
   -- It first checks if the target wearer exists, then checks the 
   -- requester큦 permission. The location can be returned only if the target
   -- wearer exists and the permission is true. 
   -- If cannot read, return invalidLocation.
   function ReadLocation(Requester : in UserID; TargetUser : in UserID)
                         return GPSLocation is
   begin
      if TargetUser > 0 and TargetUser < AutoIncrementalBase and 
        (Requester = TargetUser or (Requester = 0 and 
         Users(TargetUser).EmergencyPermissionSN = True)
         or Requester = TargetUser or (Requester = Users(TargetUser).Friend
         and Users(TargetUser).FriendPermissionHB = True)
         or (Requester = Users(TargetUser).Insurance and 
         Users(TargetUser).InsurancePermissionHB = True)) then
         return Users(TargetUser).GeoLocation;
      else
         return invalidLocation;
      end if;
   end ReadLocation;

   -- The procedure is to call Emergency.ContactEmergency.
   -- It first checks if the target wearer exists, then check the emergency큦
   -- permission to read the wearer큦 vitals.
   -- The emergency is called only when the emergency has permission
   -- to read the wearer큦 vitals.
   procedure ContactEmergency(Wearer : in UserID; 
                              Location : in GPSLocation; 
                              Vitals : in BPM) is
   begin
      if Wearer > 0 and Wearer < AutoIncrementalBase and 
         Users(Wearer).EmergencyPermissionHB = True then 
         ContactEmergency(Wearer,Vitals,Location);
         emergHistory(nextRecordIndex).user := Wearer;
         emergHistory(nextRecordIndex).GeoLocation := Location;
         emergHistory(nextRecordIndex).HeartBeat := Vitals;
         nextRecordIndex := nextRecordIndex +1;
      end if;
   end ContactEmergency;
   
end AccountManagementSystem;
