
with Ada.Text_IO;  use Ada.Text_IO;

with GL.Types;

with Quaternions;

procedure Test_Quaternion is
   package Single_Quaternion is new Quaternions (GL.Types.Single);
   use Single_Quaternion;
   use GL.Types;
   q  : Quaternion := (1.0, 2.0, 3.0, 4.0);
   q1 : Quaternion := (2.0, 3.0, 4.0, 5.0);
   q2 : Quaternion := (3.0, 4.0, 5.0, 6.0);
   r  : Single     := 7.0;
begin
   Put_Line ("q = "       & Image (q));
   Put_Line ("q1 = "      & Image (q1));
   Put_Line ("q2 = "      & Image (q2));
   Put_Line ("r ="        & Single'Image (r));
   Put_Line ("abs q ="    & Single'Image (abs q));
   Put_Line ("abs q1 ="   & Single'Image (abs q1));
   Put_Line ("abs q2 ="   & Single'Image (abs q2));
   Put_Line ("-q = "      & Image (-q));
   Put_Line ("conj q = "  & Image (Conj (q)));
   Put_Line ("q1 + q2 = " & Image (q1 + q2));
   Put_Line ("q2 + q1 = " & Image (q2 + q1));
   Put_Line ("q * r = "   & Image (q * r));
   Put_Line ("r * q = "   & Image (r * q));
   Put_Line ("q1 * q2 = " & Image (q1 * q2));
   Put_Line ("q2 * q1 = " & Image (q2 * q1));

-- Expected results
--  q =  1.00000E+00 + 2.00000E+00i + 3.00000E+00j + 4.00000E+00k
--  q1 =  2.00000E+00 + 3.00000E+00i + 4.00000E+00j + 5.00000E+00k
--  q2 =  3.00000E+00 + 4.00000E+00i + 5.00000E+00j + 6.00000E+00k
--  r = 7.00000E+00
--  abs q = 5.47723E+00
--  abs q1 = 7.34847E+00
--  abs q2 = 9.27362E+00
--  -q = -1.00000E+00 +-2.00000E+00i +-3.00000E+00j +-4.00000E+00k
--  conj q =  1.00000E+00 +-2.00000E+00i +-3.00000E+00j +-4.00000E+00k
--  q1 + q2 =  5.00000E+00 + 7.00000E+00i + 9.00000E+00j + 1.10000E+01k
--  q2 + q1 =  5.00000E+00 + 7.00000E+00i + 9.00000E+00j + 1.10000E+01k
--  q * r =  7.00000E+00 + 1.40000E+01i + 2.10000E+01j + 2.80000E+01k
--  r * q =  7.00000E+00 + 1.40000E+01i + 2.10000E+01j + 2.80000E+01k
--  q1 * q2 = -5.60000E+01 + 1.60000E+01i + 2.40000E+01j + 2.60000E+01k
--  q2 * q1 = -5.60000E+01 + 1.80000E+01i + 2.00000E+01j + 2.80000E+01k

end Test_Quaternion;
