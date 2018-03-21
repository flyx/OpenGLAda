--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with System;
with Interfaces.C;

with GL.Low_Level;

-- This package is incomplete. As I do not develop or test under Linux, this
-- has very low priority. Perhaps someone wants to help out...
package GL.GLX is
   pragma Preelaborate;

   --  needed types from Xlib
   type XID is new Interfaces.C.unsigned_long;

   type GLX_Context is new System.Address;

   type GLX_Drawable    is new XID;
   type Screen_Depth    is new Natural;
   type Screen_Number   is new Natural;
   type Visual_ID       is new XID;

   type Display_Pointer is new System.Address;

   type X_Visual_Info is record
      Visual        : System.Address;
      Visual_Ident  : Visual_ID;
      Screen        : Screen_Number;
      Depth         : Screen_Depth;
      Class         : Integer;
      Red_Mask      : Long_Integer;
      Green_Mask    : Long_Integer;
      Blue_Mask     : Long_Integer;
      Colormap_Size : Natural;
      Bits_Per_RGB  : Natural;
   end record;
   pragma Convention (C_Pass_By_Copy, X_Visual_Info);
   type X_Visual_Info_Pointer is access all X_Visual_Info;
   pragma Convention (C, X_Visual_Info_Pointer);

   function Create_Context (Display    : Display_Pointer;
                            Visual     : X_Visual_Info_Pointer;
                            Share_List : GLX_Context;
                            Direct     : Low_Level.Bool)
                            return GLX_Context;

   function Make_Current (Display  : Display_Pointer;
                          Drawable : GLX_Drawable;
                          Context  : GLX_Context)
                          return Low_Level.Bool;

   function Make_Context_Current (Display  : Display_Pointer;
                                  Draw     : GLX_Drawable;
                                  Read     : GLX_Drawable;
                                  Context  : GLX_Context)
                                  return Low_Level.Bool;

   function Get_Current_Context return System.Address;

   function Get_Current_Display return System.Address;

   function Get_Proc_Address (Name : Interfaces.C.char_array)
     return System.Address;

private

   pragma Import (C, Create_Context, "glXCreateContext");
   pragma Import (C, Make_Current, "glXMakeCurrent");
   pragma Import (C, Make_Context_Current, "glXMakeContextCurrent");
   pragma Import (C, Get_Current_Context, "glXGetCurrentContext");
   pragma Import (C, Get_Current_Display, "glXGetCurrentDisplay");
   pragma Import (C, Get_Proc_Address, "glXGetProcAddress");

end GL.GLX;
