with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Measures; use Measures;
with AccountManagementSystem; use AccountManagementSystem;

procedure FatBatExample is

   -- Two wearers.
   Wearer1 : UserID;
   Wearer2 : UserID;
   Wearer3 : UserID;

   -- Note that in my system, insurance companies are initialized in Init
   -- procedure. And the number of insurance company is a constant variable.
   -- Insurers occupy the last NoI spaces of Users array that record all users.
   -- Thus, I do not have to create insurers via calling procedures.
   -- I designed to have 3 insurance companies in this system.
   -- And that means following 3 insurance companies are ready to be use:
   -- Insurance1 : UserID := 98;
   -- Insurance2 : UserID := 99;
   -- Insurance3 : UserID := 100;
   -- We use Insurance1 and Insurance2 here to demonstrate the system.
   Insurance1 : UserID := 98;
   Insurance2 : UserID := 99;

   -- The emergency.
   emergency : UserID := 0;
   -- The location of a sample wearer.
   Location : GPSLocation := (50.0, 60.0);

   -- The vital of a sample wearer.
   Vitals : BPM := 75;

   -- The footStep of a sample wearer.
   FootStep : Footsteps := 9000;

   --The vital for testing emergency call.
   EmergencyVitals : BPM := 20;
begin

   Init;

   -- Creating two wearers.
   CreateUser;
   Wearer1 := ReturnUser;
   CreateUser;
   Wearer2 := ReturnUser;
   CreateUser;
   Wearer3 := ReturnUser;

   -- Setting wearer2 as wearer1큦 friend.
   SetFriend(Wearer1,Wearer2);

   -- Changing wearer1큦 friend to wearer3.
   SetFriend(Wearer1,Wearer3);

   -- Setting Insurance1 as wearer1큦 insurer.
   SetInsurer(Wearer1,Insurance1);

   -- Changing wearer1큦 insurer to Insurance2.
   SetInsurer(Wearer1,Insurance2);

   -- Updating vital for wearer1.
   UpdateVitals(Wearer1,Vitals);

   -- Updating footsteps for wearer1.
   UpdateFootsteps(Wearer1,FootStep);

   -- Setting the insurance permission of wearer1.
   UpdateVitalsPermissions(Wearer1,Insurance2,True);
   UpdateFootstepsPermissions(Wearer1,Insurance2,True);
   UpdateLocationPermissions(Wearer1,Insurance2,True);

   -- Changing the insuance permission to read location of wearer1.
   UpdateLocationPermissions(Wearer1,Insurance2,False);

   -- Setting the emergency permission of wearer1.
   UpdateVitalsPermissions(Wearer1,emergency,True);
   UpdateFootstepsPermissions(Wearer1,emergency,False);
   UpdateLocationPermissions(Wearer1,emergency,False);

   -- Setting the emergency permission of wearer2.
   UpdateVitalsPermissions(Wearer2,emergency,false);
   UpdateFootstepsPermissions(Wearer2,emergency,True);
   UpdateLocationPermissions(Wearer2,emergency,True);

   -- Contacting emergency for wear1,
   -- whose emergency permission to read vital is true.
   ContactEmergency(Wearer1,Location,EmergencyVitals);

   -- Contacting emergency for wear2,
   -- whose emergency permission to read vital is false.
   ContactEmergency(Wearer2,Location,EmergencyVitals);

end FatBatExample;
