
with System;
with Interfaces.C; use Interfaces.C;

with GL.Types;

package FT.Types is

   type Char_Map_Ptr is private;
   type Driver_Ptr is private;
   type Face_Internal_Ptr is private;
   type Generic_Record is private;
   type List_Record is private;
   type Size_Ptr is private;
   type Slot_Internal_Ptr is private;
   type Subglyph_Ptr is private;

   type Load_Flag is (Load_Default, Load_No_Scale, Load_No_Hinting, Load_Render,
                      Load_No_Bitmap, Load_Vertical_Layout, Load_Force_Autohint,
                      Load_Crop_Bitmap, Load_Pedantic, Load_Advance_Only,
                      Load_Ignore_Gloabl_Advance_Width, Load_No_Recourse,
                      Load_Ignore_Transform, Load_Monochrome, Load_Linear_Design,
                      Load_SBits_Only, Load_No_Autohint, Load_Load_Colour,
                      Load_Compute_Metrics, Load_Bitmap_Metrics_Only);

   type Render_Mode is (Render_Mode_Normal, Render_Mode_Light,
                           Render_Mode_Mono, Render_Mode_LCD,
                           Render_Mode_LCD_V, Render_Mode_Max);

   subtype FT_Bool is unsigned_char;
   subtype FT_Error is GL.Types.int;
   subtype FT_String is char;
   subtype FT_ULong is unsigned_long;

   FT_Exception : exception;

private

   type Char_Map_Ptr is new System.Address;
   type Driver_Ptr is new System.Address;
   type Face_Internal_Ptr is new System.Address;

   type Generic_Finalizer is access procedure (theFinalizer : System.Address);
   pragma Convention (C, Generic_Finalizer);

   type Generic_Record is record
      Data      : System.Address;
      Finalizer : Generic_Finalizer;
   end record;
   pragma Convention (C_Pass_By_Copy, Generic_Record);

   type List_Node is new System.Address;
   type List_Record is record
      head : List_Node;
      tail : List_Node;
   end record;
   pragma Convention (C_Pass_By_Copy, List_Record);

   type Size_Ptr is new System.Address;
   type Slot_Internal_Ptr is new System.Address;
   type Subglyph_Ptr is new System.Address;

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

end FT.Types;
