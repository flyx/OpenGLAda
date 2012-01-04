with Interfaces.C;

package GL.GLX is
   --  needed types from Xlib
   type XID is new Interfaces.C.unsigned_long;
   type Bool is new Boolean;

   type GLX_Context is new System.Address;
   Null_Context : constant GLX_Context := GLX_Context (System.Null_Address);

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

   function GLX_Create_Context (Display    : Display_Pointer;
                                Visual     : X_Visual_Info_Pointer;
                                Share_List : GLX_Context;
                                Direct     : Bool)
                                return GLX_Context;

   function GLX_Make_Current (Display  : Display_Pointer;
                              Drawable : GLX_Drawable;
                              Context  : GLX_Context)
                              return Bool;

   function GLX_Make_Context_Current (Display  : Display_Pointer;
                                      Draw     : GLX_Drawable;
                                      Read     : GLX_Drawable;
                                      Context  : GLX_Context)
                                      return Bool;

private
   for Bool use (False => 0, True => 1);
   for Bool'Size use Interfaces.C.int'Size;

   pragma Import (C, GLX_Create_Context, "glXCreateContext");
   pragma Import (C, GLX_Make_Current, "glXMakeCurrent");
   pragma Import (C, GLX_Make_Context_Current, "glXMakeContextCurrent");

end GL.GLX;
