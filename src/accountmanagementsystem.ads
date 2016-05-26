with Measures; use Measures;

-- NOTE: This package exposes the types and variables of the package.
-- This makes the assignment a bit simpler, but is generally not
-- considered good style.
-- For an example that hides these while maintaining abstraction and
-- elegence, see the example at
-- {SPARK_2014_HOME}/share/examples/spark/natural/natural_set.ads
package AccountManagementSystem
with SPARK_Mode
is
   -- An array indicating whether a user ID is taken
   type UsersArray is array(UserID) of Boolean;
   
   -- A map from users to other users
   type UserUserArray is array(UserID) of UserID;
   
   -- Arrays for wearer data
   type VitalsArray is array(UserID) of BPM;
   type FootstepsArray is array(UserID) of Footsteps;
   type LocationsArray is array(UserID) of GPSLocation;
   
   -- kaiqi Arrays for the historic record
   MAX_HISTORY : constant Integer := 200;
   
   type EmergencyRecord is
      record
         user :UserID;
         GeoLocation :GPSLocation;
         HeartBeat :BPM;
      end record;

   type EmergencyArray is array (0 .. MAX_HISTORY) of EmergencyRecord;

   -- kaiqi to point to the nex avaliable Index for record
   nextRecordIndex : Integer := 0;
   HistoryRecord :EmergencyArray;

   -- kaiqi Array for the permissions 
   type PermissionArray is array (UserID) of Boolean;
   
   --kaiqi permission for insurer
   permiOfStepsForInsurer : PermissionArray;
   permiOfVitalsForInsurer : PermissionArray;
   permiOfLocasForInsurer : PermissionArray;
	
   --kaiqi permissons for friends
   permiOfStepsForFriend : PermissionArray;
   permiOfVitalsForFriend : PermissionArray;
   permiOfLocasForFriend : PermissionArray;
	
   --kaiqi permissons for emergency
   permiOfStepsForEmerg : PermissionArray;
   permiOfVitalsForEmerg : PermissionArray;
   permiOfLocasForEmerg : PermissionArray;
   
   --kaiqi the constants needed
   Null_UserID : constant UserID := -1;
   Null_Footsteps : constant Footsteps := 0;
   Null_BPM : constant BPM := -1;
   Null_Location : constant GPSLocation := (0.0,0.0);
   EmergencyID: constant UserID := 0;
   
   -- The list of users, and the latest user
   Users : UsersArray; 
   LatestUser : UserID := EmergencyID;

   -- Each users' insurer and friend
   Insurers : UserUserArray;
   Friends : UserUserArray ;
   
   -- Each users' personal data
   Vitals : VitalsArray;
   MFootsteps : FootstepsArray;
   Locations : LocationsArray;
   
   -- Create and initialise the account management system
   procedure Init with 
     Post => (for all I in Users'Range => Users(I) = False) and

     (for all I in Friends'Range => Friends(I) = Null_UserID) and
     (for all I in Vitals'Range => Vitals(I) = Null_BPM) and
     (for all I in MFootsteps'Range => MFootsteps(I) = Null_Footsteps) and
     (for all I in Locations'Range => Locations(I) = Null_Location) and
     
     (for all I in permiOfStepsForInsurer'Range => permiOfStepsForInsurer(I) = True) and
     (for all I in permiOfVitalsForInsurer'Range => permiOfVitalsForInsurer(I) = False) and
     (for all I in permiOfLocasForInsurer'Range => permiOfLocasForInsurer(I) = False) and
     
     (for all I in permiOfStepsForFriend'Range => permiOfStepsForFriend(I) = False) and
     (for all I in permiOfVitalsForFriend'Range => permiOfVitalsForFriend(I) = False) and
     (for all I in permiOfLocasForFriend'Range => permiOfLocasForFriend(I) = False) and
     
     (for all I in permiOfStepsForEmerg'Range => permiOfStepsForEmerg(I) = False) and
     (for all I in permiOfVitalsForEmerg'Range => permiOfVitalsForEmerg(I) = False) and
     (for all I in permiOfLocasForEmerg'Range => permiOfLocasForEmerg(I) = False) and
     
     (for all I in HistoryRecord'Range => HistoryRecord(I) = (Null_UserID,Null_Location,Null_BPM)) and
     
     (LatestUser = EmergencyID) and 
     (nextRecordIndex = 0);
     
   procedure CreateUser(NewUser : out UserID) with
     
     Pre => (LatestUser < UserID'Last),
     Post => (if(NewUser > EmergencyID and NewUser <= UserID'Last and Users'Old(NewUser) = False) then
       (Users = Users'Old'Update(NewUser => True)) and
     (permiOfLocasForEmerg = permiOfLocasForEmerg'Old'Update(NewUser => False)) and
     (permiOfVitalsForEmerg = permiOfVitalsForEmerg'Old'Update(NewUser => False)) and
                (permiOfStepsForEmerg = permiOfStepsForEmerg'Old'Update(NewUser => False))
             else (NewUser = Null_UserID)
             );
   
   procedure SetInsurer(Wearer : in UserID; Insurer : in UserID) with

     Pre => Wearer in Users'Range and Insurer in Users'Range ,
     Post => (if(Insurers'Old(Wearer) /= Insurer and 
     (Users(Wearer) = True) and (Users(Insurer) = True) and 
     (Wearer /= Null_UserID) and (Insurer /= Null_UserID) and 
     (Wearer /= EmergencyID) and (Insurer /= EmergencyID)) then
     (Insurers = Insurers'Old'Update(Wearer => Insurer)) and 
     (permiOfStepsForInsurer = permiOfStepsForInsurer'Old'Update(Wearer => True)) and
     (permiOfVitalsForInsurer = permiOfVitalsForInsurer'Old'Update(Wearer => False)) and
     (permiOfLocasForInsurer = permiOfLocasForInsurer'Old'Update(Wearer => False)));
              
   function ReadInsurer(Wearer : in UserID) return UserID 
   is (Insurers(Wearer));

   procedure RemoveInsurer(Wearer : in UserID) with
     Pre => Insurers(Wearer) /= UserID'First and (Users(Wearer) = True) ,
     Post => (Insurers = Insurers'Old'Update(Wearer => Null_UserID)) and
     (permiOfStepsForInsurer = permiOfStepsForInsurer'Old'Update(Wearer => True)) and
     (permiOfVitalsForInsurer = permiOfVitalsForInsurer'Old'Update(Wearer => False)) and
     (permiOfLocasForInsurer = permiOfLocasForInsurer'Old'Update(Wearer => False));

   procedure SetFriend(Wearer : in UserID; Friend : in UserID) with
     Pre =>(Wearer in Users'Range and  Wearer /= Null_UserID and 
           Wearer /= EmergencyID and Friend /= EmergencyID 
           and Friend in Users'Range and Friend /= Null_UserID and
           Users(Wearer) = True and Users(Friend) = True  and
           Wearer /= Friend),
     Post => Friends = Friends'Old'Update(Wearer => Friend);
              
   function ReadFriend(Wearer : in UserID) return UserID
   is (Friends(Wearer));

   procedure RemoveFriend(Wearer : in UserID) with
     Pre => Friends(Wearer) /= UserID'First,
     Post => (Friends = Friends'Old'Update(Wearer => UserID'First));

   procedure UpdateVitals(Wearer : in UserID; NewVitals : in BPM) with
     Pre => Wearer in Users'Range and  (Users(Wearer) = True) and (Wearer /= Null_UserID),
     Post => 
             (if(Wearer in Users'Range and  Wearer /= Null_UserID and 
           Wearer /= EmergencyID and Users(Wearer) = True ) then
       Vitals = Vitals'Old'Update(Wearer => NewVitals));
   
   procedure UpdateFootsteps(Wearer : in UserID; NewFootsteps : in Footsteps)
     with
     Pre => Wearer in Users'Range,
     Post => MFootsteps = MFootsteps'Old'Update(Wearer => NewFootsteps);
     
   procedure UpdateLocation(Wearer : in UserID; NewLocation : in GPSLocation) 
     with
     Pre => Wearer in Users'Range,
     Post => Locations = Locations'Old'Update(Wearer => NewLocation);
     
   -- An partial, incorrect specification.
   -- Note that there is no need for a corresponding body for this function. 
   -- These are best suited for functions that have simple control flow
   --function ReadVitals(Requester : in UserID; TargetUser : in UserID) return BPM 
   --is (if Friends(TargetUser) = Requester then
   --       Vitals(TargetUser)
   --    else BPM'First);
   
   
   
   -- An alternative specification using postconditions. These require package
   -- bodies, and are better suited to functions with non-trivial control flow,
   -- and are required for functions with preconditions
   --function ReadVitals_Alt(Requester : in UserID; TargetUser : in UserID)
     --                      return BPM 
   --with Post => ReadVitals_Alt'Result = (if Friends(TargetUser) = Requester then
     --     Vitals(TargetUser)
       --                                      else BPM'First);
   
   
      function ReadVitals(Requester : in UserID; TargetUser : in UserID)
                           return BPM 
     with 
       Pre => (Users(Requester) = True and Requester /= Null_UserID and TargetUser /= EmergencyID ),
       Post => (if (Insurers(TargetUser) = Requester and permiOfVitalsForInsurer(TargetUser) = True) or
                   (Friends(TargetUser) = Requester and permiOfVitalsForFriend(TargetUser) = True) or
                   (TargetUser = EmergencyID and permiOfVitalsForEmerg(TargetUser) = True)
                   
                  then
                      ReadVitals'Result = Vitals(TargetUser)
                      else
                        ReadVitals'Result = Null_BPM);

   

   
 
   --function ReadFootsteps(Requester : in UserID; TargetUser : in UserID) 
     --                     return Footsteps;
   
   
   
   
-- function ReadLocation(Requester : in UserID; TargetUser : in UserID)
--    return GPSLocation;
--        
-- procedure UpdateVitalsPermissions(Wearer : in UserID; 
--  				     Other : in UserID;
-- 				     Allow : in Boolean);
--
   procedure UpdateFootstepsPermissions(Wearer : in UserID; 
    					Other : in UserID;
                                        Allow : in Boolean)
            with 
       Pre => (Users(Wearer) = True and Wearer /= Null_UserID and Wearer /= EmergencyID ),
     Post => (if Insurers'Old(Wearer) = Other then
                (if Allow = False then 
                       Insurers(Wearer) = Null_UserID and 
                     (permiOfStepsForInsurer = permiOfStepsForInsurer'Old'Update(Wearer => True)) and
                     (permiOfVitalsForInsurer = permiOfVitalsForInsurer'Old'Update(Wearer => False)) and
                     (permiOfLocasForInsurer = permiOfLocasForInsurer'Old'Update(Wearer => False))
                      )
                  else (if EmergencyID = Other then
               permiOfStepsForEmerg = permiOfStepsForEmerg'Old'Update(Wearer => Allow)
               
               else (if Friends(Wearer) = Other then
                      permiOfStepsForFriend = permiOfStepsForFriend'Old'Update(Wearer => Allow)
               )));
   
   
--     
-- procedure UpdateLocationPermissions(Wearer : in UserID;
-- 				       Other : in UserID;
--  				       Allow : in Boolean);
--
 procedure ContactEmergency(Wearer : in UserID; 
                            Location : in GPSLocation; 
                            Vital : in BPM)
     with
           Post => (if permiOfVitalsForEmerg(Wearer) = True then
                    HistoryRecord = HistoryRecord'Old'Update(nextRecordIndex'Old => 
                      HistoryRecord'Old(nextRecordIndex'Old)'Update(user => Wearer,
                                                                      GeoLocation => Location,
                                                                      HeartBeat => Vital)) and 
                  nextRecordIndex = nextRecordIndex'Old +1
                  else HistoryRecord = HistoryRecord'Old and 
                    nextRecordIndex = nextRecordIndex'Old);

end AccountManagementSystem;
