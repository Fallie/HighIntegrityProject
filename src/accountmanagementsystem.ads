with Measures; use Measures;

-- This package defines a set of procedures and functions for the account 
-- management system of FatBat. And the system also update vitals, locations  
-- and footsteps of a wearer. Furthermore, it calls the 
-- Emergency.ContactEmergency in case of passed the check of the 
-- procedure called ContactEmergency.

package AccountManagementSystem with
  SPARK_Mode,
  Initializes => (NoI, AutoIncrementalBase, minUser,
                  invalidVital,invalidFootStep,invalidLocation,nextRecordIndex,
                  Users,MAX_HISTORY),
  Initial_Condition => (AutoIncrementalBase = 1 and nextRecordIndex = 0)
is
     
   -- This stands for the number of insurers.                             
   NoI : constant Integer := 3;    
   
   -- Note that in my system, insurance companies are initialized in Init
   -- procedure. And the number of insurance company is a constant variable.
   -- Insurers occupy the last NoI spaces of Users array that record all users.
   -- Thus, I do not have to create insurers via calling procedures.
   
   -- This is the basis of the auto-incremental UserID, starting from 1.
   AutoIncrementalBase : UserID := 1;
   
   -- The wearers' userID starts from the value of minUser.--?
   minUser : UserID := 1;          
   
   -- The invalid return if cannot reach a vital.
   invalidVital : BPM := -1;
   
   -- The invalid return if cannot reach a footstep.
   invalidFootStep : Footsteps := 0;
   
   -- The invalid return if cannot reach a location.
   invalidLocation : GPSLocation := (0.0, 0.0);
   
   -- The number of emergency information that can be stored per person.
   --EventStorageNum : constant Positive := 10;     
    
   -- The list of storing location in case of emergency.
   --type EventLocationList is array (Positive range <>) of GPSLocation; 
   
   -- The list of storing vitals in case of emergency.
   --type EventVitalList is array (Positive range <>) of BPM;
   
   --
   
   MAX_HISTORY : Integer := 100;
    type EmergencyRecord is
      record
         user :UserID;
         GeoLocation :GPSLocation;
         HeartBeat :BPM;
      end record;

   type EmergencyList is array (0 .. MAX_HISTORY) of EmergencyRecord;

   
   
   -- to point to the nex avaliable Index for record
   nextRecordIndex : Integer := 0;
   emergHistory :EmergencyList;
   
   --The data structure of a wearer.
   type Wearers is
      record
         ID : UserID := -1;
         
         -- The insurance and its permission. HB is heart beat. GL is 
         -- geoLocation.SN is stepNumber. Same for friend and emergency.
         Insurance : UserID := -1;
         InsurancePermissionHB : Boolean := False;
         InsurancePermissionGL : Boolean := False;
         InsurancePermissionSN : Boolean := True;
         
         Friend : UserID := -1;
         FriendPermissionHB : Boolean := False;
         FriendPermissionGL : Boolean := False;
         FriendPermissionSN : Boolean := False;
         
         Emergency : UserID := 0;
         EmergencyPermissionHB : Boolean := False;
         EmergencyPermissionGL : Boolean := False;
         EmergencyPermissionSN : Boolean := False;
         
         HeartBeat : BPM := -1;
         GeoLocation : GPSLocation := (0.0, 0.0);
         StepNum : Footsteps := 0;
         
      end record;
   
   -- The users array stored the created users.    
   Users : array (UserID range UserID(1)..UserID(MAX_USERID)) of Wearers;
   
   procedure Init 
     with Post => (AutoIncrementalBase = 1 and nextRecordIndex = 0);
   
   --remembre the emergency id must be 0
   function CreateUser return UserID 
     with 
      Global => (AutoIncrementalBase, Users),
      Pre => Integer(AutoIncrementalBase) <= MAX_USERID,
      Post => (AutoIncrementalBase'Old = CreateUser'Result and 
                 AutoIncrementalBase = CreateUser'Result);
   
   procedure SetInsurer(Wearer : in UserID; Insurer : in UserID)
      with 
      Global => ( In_Out => Users, Input => AutoIncrementalBase),
      Pre => (Wearer < AutoIncrementalBase and Insurer < AutoIncrementalBase),
      Post => (if Wearer /= Insurer then
                Users = Users'Old'Update(Wearer => Users'Old(Wearer)'Update
               (InsurancePermissionHB => False,
               InsurancePermissionGL => False, 
               InsurancePermissionSN => True,
                Insurance => Insurer))
                else
                  Users = Users'Old
             );

   function ReadInsurer(Wearer : in UserID) return UserID
     with 
       Global => (Input => (Users, AutoIncrementalBase)),
       Pre => Wearer < AutoIncrementalBase,
       Post => ReadInsurer'Result = Users(Wearer).Insurance;
   
   procedure RemoveInsurer(Wearer : in UserID) 
     with 
       Global => ( In_Out => Users, Input => AutoIncrementalBase),
       Pre => (Wearer < AutoIncrementalBase),
       Post => Users = Users'Old'Update(Wearer => Users'Old(Wearer)'Update
               (InsurancePermissionHB => False,
               InsurancePermissionGL => False, 
               InsurancePermissionSN => True,
                Insurance => -1));
   
   procedure SetFriend(Wearer : in UserID; NewFriend : in UserID);
   function ReadFriend(Wearer : in UserID) return UserID;
   procedure RemoveFriend(Wearer : in UserID);
   
   procedure UpdateVitalsPermissions(Wearer : in UserID; 
				     Other : in UserID;
                                     Allow : in Boolean)
     with 
       Global => ( In_Out => Users, Input => AutoIncrementalBase),
       Pre => (Wearer < AutoIncrementalBase and Other < AutoIncrementalBase),
       Post => (if Users(Wearer).Insurance = Other then
                 Users = Users'Old'Update(Wearer => Users'Old(Wearer)'Update
                 (InsurancePermissionHB => Allow))
               else (if Users(Wearer).Emergency = Other then
                 Users = Users'Old'Update(Wearer => Users'Old(Wearer)'Update
                 (EmergencyPermissionHB => Allow))
               else (if Users(Wearer).Friend = Other then
                  Users = Users'Old'Update(Wearer => Users'Old(Wearer)'Update
                 (FriendPermissionHB => Allow))
               else
                 Users = Users'Old)));


   procedure UpdateFootstepsPermissions(Wearer : in UserID; 
					Other : in UserID;
                                        Allow : in Boolean)
       with 
       Global => ( In_Out => Users, Input => AutoIncrementalBase),
       Pre => (Wearer < AutoIncrementalBase and Other < AutoIncrementalBase),
     Post => (if Users'Old(Wearer).Insurance = Other then
                 (if Allow = False then Users = Users'Old'Update
                 (Wearer => Users'Old(Wearer)'Update(
                   InsurancePermissionHB => False,
                   InsurancePermissionGL => False, 
                   InsurancePermissionSN => True,
                   Insurance => -1)))
               else (if Users(Wearer).Emergency = Other then
                 Users = Users'Old'Update(Wearer => Users'Old(Wearer)'Update
                 (EmergencyPermissionSN => Allow))
               else (if Users(Wearer).Friend = Other then
                  Users = Users'Old'Update(Wearer => Users'Old(Wearer)'Update
                 (FriendPermissionSN => Allow))
               else
                 Users = Users'Old)));
   
   
   procedure UpdateLocationPermissions(Wearer : in UserID;
				       Other : in UserID;
                                       Allow : in Boolean);
   
   procedure UpdateVitals(Wearer : in UserID; NewVitals : in BPM)     
     with 
       Global => ( In_Out => Users, Input => AutoIncrementalBase),
       Pre => (Wearer < AutoIncrementalBase and NewVitals /= -1),
       Post => Users = Users'Old'Update(Wearer => 
                          Users'Old(Wearer)'Update( HeartBeat => NewVitals));
   procedure UpdateFootsteps(Wearer : in UserID; NewFootsteps : in Footsteps); 
   procedure UpdateLocation(Wearer : in UserID; NewLocation : in GPSLocation);
   
   function ReadVitals(Requester : in UserID; TargetUser : in UserID) 
                       return BPM
     with 
       Global => (Input => (Users, AutoIncrementalBase, invalidVital)),
       Pre => (Requester < AutoIncrementalBase and TargetUser < AutoIncrementalBase),
       Post => (if Users(TargetUser).Insurance = Requester and Users(TargetUser).InsurancePermissionHB = True then
                 ReadVitals'Result = Users(TargetUser).HeartBeat
               else (if Users(TargetUser).Emergency = Requester and Users(TargetUser).EmergencyPermissionHB = True then
                 ReadVitals'Result = Users(TargetUser).HeartBeat
               else (if Users(TargetUser).Friend = Requester and Users(TargetUser) .FriendPermissionHB = True then
                 ReadVitals'Result = Users(TargetUser).HeartBeat
               else
                 ReadVitals'Result = invalidVital)));
   function ReadFootsteps(Requester : in UserID; TargetUser : in UserID) 
                          return Footsteps;
   function ReadLocation(Requester : in UserID; TargetUser : in UserID) 
                         return GPSLocation;
   procedure ContactEmergency(Wearer : in UserID; Location : in GPSLocation;
                              Vitals : in BPM)
     with
       Global => (In_out => (emergHistory, nextRecordIndex), Input => (Users, AutoIncrementalBase, invalidVital, invalidLocation)),
       Pre => (Wearer < AutoIncrementalBase and Location /= invalidLocation and Vitals /= invalidVital),
       Post => (if Users(Wearer).EmergencyPermissionHB = True then
                    emergHistory = emergHistory'Old'Update(nextRecordIndex'Old => 
                      emergHistory'Old(nextRecordIndex'Old)'Update(user => Wearer,
                                                                      GeoLocation => Location,
                                                                      HeartBeat => Vitals)) and 
                  nextRecordIndex = nextRecordIndex'Old +1
                  else emergHistory = emergHistory'Old and 
                    nextRecordIndex = nextRecordIndex'Old);

end AccountManagementSystem;
