--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

generic
   type Index_Type is (<>);
   type Element_Type is private;
   with function "+" (Left, Right : Element_Type) return Element_Type is <>;
   with function "-" (Left, Right : Element_Type) return Element_Type is <>;
   with function "-" (Left        : Element_Type) return Element_Type is <>;
   with function "*" (Left, Right : Element_Type) return Element_Type is <>;
   with function "/" (Left, Right : Element_Type) return Element_Type is <>;
package GL.Vectors is
   pragma Preelaborate;

   type Vector is array (Index_Type) of aliased Element_Type;
   pragma Convention (C, Vector);

   function "+" (Left, Right : Vector) return Vector;

   function "-" (Left, Right : Vector) return Vector;
   function "-" (Left        : Vector) return Vector;

   function "*" (Left : Vector;       Right : Element_Type) return Vector;
   function "*" (Left : Element_Type; Right : Vector)       return Vector;

   function "/" (Left : Vector; Right : Element_Type) return Vector;

   function Dot_Product   (Left, Right : Vector) return Element_Type;

   pragma Inline ("+");
   pragma Inline ("-");
   pragma Inline ("*");
   pragma Inline ("/");
end GL.Vectors;
