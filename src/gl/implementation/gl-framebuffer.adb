--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.API;
with GL.Enums.Getter;

package body GL.Framebuffer is

   procedure Set_Clamp_Read_Color (Enabled : Boolean) is
   begin
      API.Clamp_Color (Enums.Clamp_Read_Color, Low_Level.Bool (Enabled));
   end Set_Clamp_Read_Color;

   procedure Set_Read_Buffer (Value : Read_Buffer_Selector) is
   begin
      API.Read_Buffer (Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Read_Buffer;

   function Read_Buffer return Read_Buffer_Selector is
      Ret : aliased Read_Buffer_Selector;
   begin
      API.Get_Read_Buffer_Selector (Enums.Getter.Read_Buffer, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Read_Buffer;

   procedure Read_Pixels (X, Y : Int;
                          Width, Height : Size;
                          Format : Pixels.Framebuffer_Format;
                          Data_Type : Pixels.Data_Type;
                          Data : out Array_Type) is
   begin
      API.Read_Pixels
        (X, Y, Width, Height, Format, Data_Type, Data (Data'First)'Address);
      Raise_Exception_On_OpenGL_Error;
   end Read_Pixels;

   procedure Set_Logic_Op_Mode (Value : Logic_Op) is
   begin
      API.Logic_Op (Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Logic_Op_Mode;

   function Logic_Op_Mode return Logic_Op is
      Ret : aliased Logic_Op;
   begin
      API.Get_Logic_Op (Enums.Getter.Logic_Op_Mode, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Logic_Op_Mode;

end GL.Framebuffer;
