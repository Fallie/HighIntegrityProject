with Measures; use Measures;

-- This package exposes the types and variables of the package. 
-- The SPARK preconditions and postconditions are implemented here.
-- The SPARK contracts are implemented based on the Alloy specification from
-- assignment 3 of our group. All global variables are declared in this package.

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
   
   -- The maximum number of emergency record that can be stored.
   MAX_HISTORY : constant Integer := 200;
   
   -- A record consists of userID, GPSLocation and BPM 
   -- of the wearer in an emergency event. 
   type EmergencyRecord is
      record
         user :UserID;
         GeoLocation :GPSLocation;
         HeartBeat :BPM;
      end record;

   -- The array for storing all emergency record.
   type EmergencyArray is array (0 .. MAX_HISTORY) of EmergencyRecord;

   -- Pointers to the next avaliable index for emergency record.
   nextRecordIndex : Integer := 0;
   
   -- The emergency record list.
   HistoryRecord :EmergencyArray;
   
   -- Array for the permissions 
   type PermissionArray is array (UserID) of Boolean;
   
   -- Permission for insurer
   permiOfStepsForInsurer : PermissionArray;
   permiOfVitalsForInsurer : PermissionArray;
   permiOfLocasForInsurer : PermissionArray;
	
   -- Permissons for friends
   permiOfStepsForFriend : PermissionArray;
   permiOfVitalsForFriend : PermissionArray;
   permiOfLocasForFriend : PermissionArray;
	
   -- Permissons for emergency
   permiOfStepsForEmerg : PermissionArray;
   permiOfVitalsForEmerg : PermissionArray;
   permiOfLocasForEmerg : PermissionArray;
   
   -- Constants needed to present the null value of the userID, Footsteps,
   -- BPM, Location and emergency record.
   Null_UserID : constant UserID := -1;
   Null_Footsteps : constant Footsteps := 0;
   Null_BPM : constant BPM := -1;
   Null_Location : constant GPSLocation := (0.0,0.0);
   Null_Record : EmergencyRecord;
   
   -- The constant emergency with the userID to be zero.
   EmergencyID: constant UserID := 0;
   
 
   -- The list of users, and the latest userID.
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
     (for all I in Insurers'Range => Insurers(I) = Null_UserID) and
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
     Post => (if(LatestUser'Old >= EmergencyID and LatestUser'Old < UserID'Last) then
                  (Users = Users'Old'Update(NewUser => True) and LatestUser = LatestUser'Old +1 )  
                    else (NewUser = Null_UserID));
   
   procedure SetInsurer(Wearer : in UserID; Insurer : in UserID) with

     Pre => Wearer in Users'Range and Insurer in Users'Range and 
     (Users(Wearer) = True) and (Users(Insurer) = True) and 
     (Wearer /= Null_UserID) and (Insurer /= Null_UserID) and 
     (Wearer /= EmergencyID) and (Insurer /= EmergencyID) and 
     Wearer /= Insurer,
     
     Post => (if(Insurers'Old(Wearer) /= Insurer ) then
                (Insurers = Insurers'Old'Update(Wearer => Insurer)) and 
                  (permiOfStepsForInsurer = permiOfStepsForInsurer'Old'Update(Wearer => True)) and
                (permiOfVitalsForInsurer = permiOfVitalsForInsurer'Old'Update(Wearer => False)) and
                  (permiOfLocasForInsurer = permiOfLocasForInsurer'Old'Update(Wearer => False)));
              
   function ReadInsurer(Wearer : in UserID) return UserID with
   pre =>(Wearer in Users'Range and  Wearer /= Null_UserID and 
            Wearer /= EmergencyID and Users(Wearer) = True ),
     post => ReadInsurer'Result = Insurers(Wearer);
   
   procedure RemoveInsurer(Wearer : in UserID) with
     Pre =>  Wearer in Users'Range and Users(Wearer) = True and (Wearer /= Null_UserID) and Wearer /= EmergencyID ,
     
     Post => (if (Insurers(Wearer) /= Null_UserID) then
                (Insurers = Insurers'Old'Update(Wearer => Null_UserID)) and
                  (permiOfStepsForInsurer = permiOfStepsForInsurer'Old'Update(Wearer => True)) and
                (permiOfVitalsForInsurer = permiOfVitalsForInsurer'Old'Update(Wearer => False)) and
                  (permiOfLocasForInsurer = permiOfLocasForInsurer'Old'Update(Wearer => False)));

   procedure SetFriend(Wearer : in UserID; Friend : in UserID) with
     Pre =>(Wearer in Users'Range and  Wearer /= Null_UserID and 
              Wearer /= EmergencyID and Friend /= EmergencyID 
            and Friend in Users'Range and Friend /= Null_UserID and
              Users(Wearer) = True and Users(Friend) = True  and
                Wearer /= Friend),
       Post => (if Friends(Wearer) /= Friend then
                      Friends = Friends'Old'Update(Wearer => Friend) and
                  (permiOfStepsForFriend = permiOfStepsForFriend'Old'Update(Wearer => False)) and
                    (permiOfVitalsForFriend = permiOfVitalsForFriend'Old'Update(Wearer => False)) and
                  (permiOfLocasForFriend = permiOfLocasForFriend'Old'Update(Wearer => False))) ;
              
   function ReadFriend(Wearer : in UserID) return UserID with
   pre =>(Wearer in Users'Range and  Wearer /= Null_UserID and 
            Wearer /= EmergencyID and Users(Wearer) = True ),
       post => ReadFriend'Result = Friends(Wearer);

   procedure RemoveFriend(Wearer : in UserID) with
     Pre =>  (Wearer in Users'Range and Users(Wearer) = True) and (Wearer /= Null_UserID) and Wearer /= EmergencyID ,
     
     Post => (if (Friends(Wearer) /= Null_UserID) then
                (Friends = Friends'Old'Update(Wearer => Null_UserID)) and
                  (permiOfStepsForFriend = permiOfStepsForFriend'Old'Update(Wearer => False)) and
                (permiOfVitalsForFriend = permiOfVitalsForFriend'Old'Update(Wearer => False)) and
                  (permiOfLocasForFriend = permiOfLocasForFriend'Old'Update(Wearer => False)));


   procedure UpdateVitals(Wearer : in UserID; NewVitals : in BPM) with
     Pre => Wearer in Users'Range and Users(Wearer) = True and (Wearer /= Null_UserID) and Wearer /= EmergencyID 
     and NewVitals /= Null_BPM,
     Post => Vitals = Vitals'Old'Update(Wearer => NewVitals);
   
   procedure UpdateFootsteps(Wearer : in UserID; NewFootsteps : in Footsteps)
     with
       Pre => Wearer in Users'Range and Users(Wearer) = True and (Wearer /= Null_UserID) and Wearer /= EmergencyID 
     and NewFootsteps /= Null_Footsteps,
     Post => MFootsteps = MFootsteps'Old'Update(Wearer => NewFootsteps);
     
   procedure UpdateLocation(Wearer : in UserID; NewLocation : in GPSLocation) 
     with
       Pre => Wearer in Users'Range and Users(Wearer) = True and (Wearer /= Null_UserID) and Wearer /= EmergencyID 
     and NewLocation /= Null_Location,
     Post => Locations = Locations'Old'Update(Wearer => NewLocation);
   
   
   function ReadVitals(Requester : in UserID; TargetUser : in UserID)
                         return BPM 
     with 
       Pre => (Users(Requester) = True and Requester /= Null_UserID and TargetUser /= EmergencyID and 
                     Requester in Users'Range and (Users(Requester) = True or Requester = EmergencyID) and (Requester /= Null_UserID) and 
                     TargetUser in Users'Range and Users(TargetUser) = True and TargetUser /= Null_UserID and TargetUser /= EmergencyID 
               and TargetUser not in Insurers'First..Insurers'Last),
         
     Post => (if (Insurers(TargetUser) = Requester and permiOfVitalsForInsurer(TargetUser) = True) or
                (Friends(TargetUser) = Requester and permiOfVitalsForFriend(TargetUser) = True) or
                  (TargetUser = EmergencyID and permiOfVitalsForEmerg(TargetUser) = True) or 
                (Requester = TargetUser)
                   
                  then
                    ReadVitals'Result = Vitals(TargetUser)
                  else
                    ReadVitals'Result = Null_BPM);

   

   
 
   function ReadFootsteps(Requester : in UserID; TargetUser : in UserID) 
                          return Footsteps     
     with 
       Pre => (Users(Requester) = True and Requester /= Null_UserID and TargetUser /= EmergencyID and 
                     Requester in Users'Range and (Users(Requester) = True or Requester = EmergencyID) and (Requester /= Null_UserID) and 
                     TargetUser in Users'Range and Users(TargetUser) = True and TargetUser /= Null_UserID and TargetUser /= EmergencyID ),
         
       Post => (if (Insurers(TargetUser) = Requester ) or
                  (Friends(TargetUser) = Requester and permiOfStepsForFriend(TargetUser) = True) or
                    (Requester = EmergencyID and permiOfStepsForEmerg(TargetUser) = True) or 
                  (Requester = TargetUser)
                   
                      then
                        ReadFootsteps'Result = MFootsteps(TargetUser)
                      else
                        ReadFootsteps'Result = Null_Footsteps);
   
   
   
   
   function ReadLocation(Requester : in UserID; TargetUser : in UserID)
                         return GPSLocation
     with
       Pre => (Users(Requester) = True and Requester /= Null_UserID and TargetUser /= EmergencyID and 
                     Requester in Users'Range and (Users(Requester) = True or Requester = EmergencyID) and (Requester /= Null_UserID) and 
                     TargetUser in Users'Range and Users(TargetUser) = True and TargetUser /= Null_UserID and TargetUser /= EmergencyID ),
       Post => (if (Insurers(TargetUser) = Requester and permiOfLocasForInsurer(TargetUser) = True) or
                  (Friends(TargetUser) = Requester and permiOfLocasForFriend(TargetUser) = True) or
                    (Requester = EmergencyID and permiOfLocasForEmerg(TargetUser) = True) or 
                  (Requester = TargetUser)
                   
                      then
                        ReadLocation'Result = Locations(TargetUser)
                      else
                        ReadLocation'Result = Null_Location);
   
   --
   procedure UpdateFootstepsPermissions(Wearer : in UserID; 
                                        Other : in UserID;
                                        Allow : in Boolean)
     with 
       Pre =>(Wearer in Users'Range and  Wearer /= Null_UserID and 
                Wearer /= EmergencyID
              and Other in Users'Range and Other /= Null_UserID and
                Users(Wearer) = True   and
                    Wearer /= Other),
         
         Post => (if Insurers'Old(Wearer) = Other then
                    (if Allow = False then 
                             Insurers(Wearer) = Null_UserID and 
                         permiOfStepsForInsurer = permiOfStepsForInsurer'Old'Update(Wearer => True) and
                         permiOfVitalsForInsurer = permiOfVitalsForInsurer'Old'Update(Wearer => False) and
                         permiOfLocasForInsurer = permiOfLocasForInsurer'Old'Update(Wearer => False)))
     and (if EmergencyID = Other then
            permiOfStepsForEmerg = permiOfStepsForEmerg'Old'Update(Wearer => Allow))
     and (if Friends(Wearer) = Other then
              permiOfStepsForFriend = permiOfStepsForFriend'Old'Update(Wearer => Allow));
       
   procedure UpdateVitalsPermissions(Wearer : in UserID; 
                                     Other : in UserID;
                                     Allow : in Boolean)
     with 
       Pre =>(Wearer in Users'Range and  Wearer /= Null_UserID and 
                Wearer /= EmergencyID
              and Other in Users'Range and Other /= Null_UserID and
                Users(Wearer) = True  and
                    Wearer /= Other),
         
         Post => (if Insurers'Old(Wearer) = Other then
                          permiOfVitalsForInsurer = permiOfVitalsForInsurer'Old'Update(Wearer => Allow))
         and (if EmergencyID = Other then
                permiOfVitalsForEmerg = permiOfVitalsForEmerg'Old'Update(Wearer => Allow))
         and (if Friends(Wearer) = Other then
                      permiOfVitalsForFriend = permiOfVitalsForFriend'Old'Update(Wearer => Allow));

   
     
   procedure UpdateLocationPermissions(Wearer : in UserID;
                                       Other : in UserID;
                                       Allow : in Boolean)
     with 
       Pre =>(Wearer in Users'Range and  Wearer /= Null_UserID and 
                Wearer /= EmergencyID
              and Other in Users'Range and Other /= Null_UserID and
                Users(Wearer) = True  and
                    Wearer /= Other),
         
         Post => (if Insurers'Old(Wearer) = Other then
                          permiOfLocasForInsurer = permiOfLocasForInsurer'Old'Update(Wearer => Allow))
         and (if EmergencyID = Other then
                permiOfLocasForEmerg = permiOfLocasForEmerg'Old'Update(Wearer => Allow))
         and (if Friends(Wearer) = Other then
                      permiOfLocasForFriend = permiOfLocasForFriend'Old'Update(Wearer => Allow));

   procedure ContactEmergency(Wearer : in UserID; 
                              Location : in GPSLocation; 
                              Vital : in BPM)
     with
       Pre => Wearer in Users'Range and  Wearer /= Null_UserID and 
     Wearer /= EmergencyID and 
     Users(Wearer) = True and Location /= Null_Location and Vital /= Null_BPM 
     and nextRecordIndex <= MAX_HISTORY and nextRecordIndex >= 0,

     Post => (if permiOfVitalsForEmerg(Wearer) = True then
                  HistoryRecord = HistoryRecord'Old'Update(nextRecordIndex'Old => (Wearer,Location,Vital)) and 
                nextRecordIndex = nextRecordIndex'Old +1
                  else HistoryRecord = HistoryRecord'Old and 
                    nextRecordIndex = nextRecordIndex'Old);

end AccountManagementSystem;
