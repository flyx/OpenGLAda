--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Interfaces.C.Pointers;

package GL.Types.Colors is
   pragma Preelaborate;

   type Color_Index is (R, G, B, A);
   subtype Basic_Color_Index is Color_Index range R .. B;

   subtype Component is Single range 0.0 .. 1.0;

   type Color is array (Color_Index) of aliased Component;
   type Basic_Color is array (Basic_Color_Index) of Component;

   pragma Convention (C, Color);
   pragma Convention (C, Basic_Color);

   type Color_Array is array (Size range <>) of aliased Color;
   type Basic_Color_Array is array (Size range <>) of aliased Basic_Color;

   package Color_Pointers is new Interfaces.C.Pointers
     (Size, Color, Color_Array, Color'(others => 0.0));
   package Basic_Color_Pointers is new Interfaces.C.Pointers
     (Size, Basic_Color, Basic_Color_Array, Basic_Color'(others => 0.0));
end GL.Types.Colors;
