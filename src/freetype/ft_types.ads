
with System;
with System.Address_To_Access_Conversions;
with Interfaces.C; use Interfaces.C;

with GL.Types;

package FT_Types is
   package Unsigned_Char_To_Address is new
       System.Address_To_Access_Conversions (Interfaces.C.unsigned_char);

   type FT_Char_Map is private;
   type FT_Driver is private;
   type FT_Face_Internal is private;
   type FT_Generic is private;
   type FT_Glyph_Slot_Ptr is new System.Address;
   type FT_List_Node is private;
   type FT_List_Record is private;
   type FT_Library is new System.Address;
   type FT_Size_Ptr is private;
   type FT_Slot_Internal is private;
   type FT_Subglyph is private;

   type Load_Flag is (Load_Default, Load_No_Scale, Load_No_Hinting, Load_Render,
                      Load_No_Bitmap, Load_Vertical_Layout, Load_Force_Autohint,
                      Load_Crop_Bitmap, Load_Pedantic, Load_Advance_Only,
                      Load_Ignore_Gloabl_Advance_Width, Load_No_Recourse,
                      Load_Ignore_Transform, Load_Monochrome, Load_Linear_Design,
                      Load_SBits_Only, Load_No_Autohint, Load_Load_Colour,
                      Load_Compute_Metrics, Load_Bitmap_Metrics_Only);

   type FT_Render_Mode is (Render_Mode_Normal, Render_Mode_Light,
                           Render_Mode_Mono, Render_Mode_LCD,
                           Render_Mode_LCD_V, Render_Mode_Max);

   subtype FT_Bool is unsigned_char;
   subtype FT_Error is GL.Types.int;
   subtype FT_String is char;
   subtype FT_ULong is unsigned_long;

   FT_Exception : exception;

private

   type FT_Char_Map is new System.Address;
   type FT_Driver is new System.Address;
   type FT_Face_Internal is new System.Address;

   type FT_Generic_Finalizer is access procedure (theFinalizer : System.Address);
   pragma Convention (C, FT_Generic_Finalizer);

   type FT_Generic is record
      Data      : System.Address;
      Finalizer : FT_Generic_Finalizer;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_Generic);

   type FT_List_Node is new System.Address;
   type FT_List_Record is record
      head : FT_List_Node;
      tail : FT_List_Node;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_List_Record);

   type FT_Size_Ptr is new System.Address;
   type FT_Slot_Internal is new System.Address;
   type FT_Subglyph is new System.Address;

   for Load_Flag use
       (Load_Default => 16#000000#,
        Load_No_Scale => 16#000001#,
        Load_No_Hinting => 16#000002#,
        Load_Render => 16#000004#,
        Load_No_Bitmap => 16#000008#,
        Load_Vertical_Layout => 16#000010#,
        Load_Force_Autohint => 16#000020#,
        Load_Crop_Bitmap => 16#000040#,
        Load_Pedantic => 16#000080#,
        Load_Advance_Only => 16#000100#,
        Load_Ignore_Gloabl_Advance_Width => 16#000200#,
        Load_No_Recourse => 16#000400#,
        Load_Ignore_Transform => 16#000800#,
        Load_Monochrome => 16#001000#,
        Load_Linear_Design => 16#002000#,
        Load_SBits_Only => 16#0004000#,
        Load_No_Autohint => 16#008000#,
        Load_Load_Colour => 16#100000#,
        Load_Compute_Metrics => 16#200000#,
        Load_Bitmap_Metrics_Only => 16#400000#);

end FT_Types;
