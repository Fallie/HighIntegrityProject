with Measures; use Measures;

-- This package defines a set of procedures and functions for the account 
-- management system of FatBat. And the system also update vitals, locations  
-- and footsteps of a wearer. Furthermore, it calls the 
-- Emergency.ContactEmergency in case of passed the check of the 
-- procedure called ContactEmergency.

package AccountManagementSystem is
   
   procedure Init;
   function CreateUser return UserID;
   procedure SetInsurer(Wearer : in UserID; Insurer : in UserID);
   function ReadInsurer(Wearer : in UserID) return UserID;
   procedure RemoveInsurer(Wearer : in UserID);
   procedure SetFriend(Wearer : in UserID; NewFriend : in UserID);
   function ReadFriend(Wearer : in UserID) return UserID;
   procedure RemoveFriend(Wearer : in UserID);
   procedure UpdateVitalsPermissions(Wearer : in UserID; 
				     Other : in UserID;
                                     Allow : in Boolean);
   procedure UpdateFootstepsPermissions(Wearer : in UserID; 
					Other : in UserID;
                                        Allow : in Boolean);
   procedure UpdateLocationPermissions(Wearer : in UserID;
				       Other : in UserID;
                                       Allow : in Boolean);
   procedure UpdateVitals(Wearer : in UserID; NewVitals : in BPM);
   procedure UpdateFootsteps(Wearer : in UserID; NewFootsteps : in Footsteps); 
   procedure UpdateLocation(Wearer : in UserID; NewLocation : in GPSLocation);
   function ReadVitals(Requester : in UserID; TargetUser : in UserID) 
                       return BPM;
   function ReadFootsteps(Requester : in UserID; TargetUser : in UserID) 
                          return Footsteps;
   function ReadLocation(Requester : in UserID; TargetUser : in UserID) 
                         return GPSLocation;
   procedure ContactEmergency(Wearer : in UserID; Location : in GPSLocation;
                              Vitals : in BPM);

end AccountManagementSystem;
