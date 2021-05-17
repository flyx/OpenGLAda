--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.Types;

private with GL.Low_Level;

package GL.Culling is
   pragma Preelaborate;

   type Face_Selector is (Front, Back, Front_And_Back);

   use GL.Types;

   procedure Set_Front_Face (Face : Orientation);
   function Front_Face return Orientation;

   procedure Set_Cull_Face (Selector : Face_Selector);
   function Cull_Face return Face_Selector;

private

   for Face_Selector use (Front          => 16#0404#,
                          Back           => 16#0405#,
                          Front_And_Back => 16#0408#);
   for Face_Selector'Size use Low_Level.Enum'Size;

   pragma Convention (StdCall, Set_Cull_Face);
   pragma Convention (StdCall, Set_Front_Face);
end GL.Culling;
