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
   
   -- The list of users, and the latest user
   Users : UsersArray; 
   LatestUser : UserID := UserID'First;

   -- Each users' insurer and friend
   Insurers : UserUserArray;
   Friends : UserUserArray;
   
   -- Each users' personal data
   Vitals : VitalsArray;
   MFootsteps : FootstepsArray;
   Locations : LocationsArray;
   
   -- Create and initialise the account management system
   procedure Init with 
     Post => (for all I in Users'Range => Users(I) = False) and
     (for all I in Friends'Range => Friends(I) = UserID'First) and
     (for all I in Vitals'Range => Vitals(I) = BPM'First) and
     (for all I in MFootsteps'Range => MFootsteps(I) = Footsteps'First) and
     (for all I in Locations'Range => Locations(I) = (0.0, 0.0));
     
   procedure CreateUser(NewUser : out UserID) with
     Pre => LatestUser < UserID'Last,
     Post => Users(NewUser) = True;
   
   procedure SetInsurer(Wearer : in UserID; Insurer : in UserID) with
     Pre => Wearer in Users'Range and Insurer in Users'Range,
     Post => (Insurers = Insurers'Old'Update(Wearer => Insurer));
              
   function ReadInsurer(Wearer : in UserID) return UserID
   is (Insurers(Wearer));

   procedure RemoveInsurer(Wearer : in UserID) with
     Pre => Insurers(Wearer) /= UserID'First,
     Post => (Insurers = Insurers'Old'Update(Wearer => UserID'First));

   procedure SetFriend(Wearer : in UserID; Friend : in UserID) with
     Pre => Wearer in Users'Range and Friend in Users'Range,
     Post => Friends = Friends'Old'Update(Wearer => Friend);
              
   function ReadFriend(Wearer : in UserID) return UserID
   is (Friends(Wearer));

   procedure RemoveFriend(Wearer : in UserID) with
     Pre => Friends(Wearer) /= UserID'First,
     Post => (Friends = Friends'Old'Update(Wearer => UserID'First));

   procedure UpdateVitals(Wearer : in UserID; NewVitals : in BPM) with
     Pre => Wearer in Users'Range,
     Post => Vitals = Vitals'Old'Update(Wearer => NewVitals);
   
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
   function ReadVitals(Requester : in UserID; TargetUser : in UserID) return BPM 
   is (if Friends(TargetUser) = Requester then
          Vitals(TargetUser)
       else BPM'First);
   
   -- An alternative specification using postconditions. These require package
   -- bodies, and are better suited to functions with non-trivial control flow,
   -- and are required for functions with preconditions
   function ReadVitals_Alt(Requester : in UserID; TargetUser : in UserID)
                           return BPM 
   with Post => ReadVitals_Alt'Result = (if Friends(TargetUser) = Requester then
          Vitals(TargetUser)
       else BPM'First);
 
-- function ReadFootsteps(Requester : in UserID; TargetUser : in UserID) 
--    return Footsteps;
-- function ReadLocation(Requester : in UserID; TargetUser : in UserID)
--    return GPSLocation;
--        
-- procedure UpdateVitalsPermissions(Wearer : in UserID; 
--  				     Other : in UserID;
-- 				     Allow : in Boolean);
--
-- procedure UpdateFootstepsPermissions(Wearer : in UserID; 
--  					Other : in UserID;
--  					Allow : in Boolean);
--     
-- procedure UpdateLocationPermissions(Wearer : in UserID;
-- 				       Other : in UserID;
--  				       Allow : in Boolean);
--
-- procedure ContactEmergency(Wearer : in UserID; 
--                            Location : in GPSLocation; 
--                            Vitals : in BPM);

end AccountManagementSystem;
