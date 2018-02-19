
generic
   type Real is digits <>;

package Quaternions is
   pragma Elaborate_Body;

   type Quaternion is record
      A, B, C, D : Real;
   end record;

   function "abs" (Left : Quaternion) return Real;
   function Conj (Left : Quaternion) return Quaternion;
   function "-" (Left : Quaternion) return Quaternion;
   function "+" (Left, Right : Quaternion) return Quaternion;
   function "-" (Left, Right : Quaternion) return Quaternion;
   function "*" (Left : Quaternion; Right : Real) return Quaternion;
   function "*" (Left : Real; Right : Quaternion) return Quaternion;
   function "*" (Left, Right : Quaternion) return Quaternion;
   function Image (Left : Quaternion) return String;
   function Normalized (Left : Quaternion) return Quaternion;

end Quaternions;
