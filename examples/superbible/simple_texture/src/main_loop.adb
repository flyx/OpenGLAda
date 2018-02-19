
with Interfaces;

with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
with GL.Pixels;
with GL.Types;
with GL.Types.Colors;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Program_Loader;
with Utilities;

--  ------------------------------------------------------------------------

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is
   type Data is array (GL.Types.Int range <>) of GL.Types.Single;

   Rendering_Program  : GL.Objects.Programs.Program;
   Vertex_Array       : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   theTexture         : GL.Objects.Textures.Texture;

   --  ------------------------------------------------------------------------

   procedure Generate_Texture (Texture_Data : in out Data; Width, Height : GL.Types.Int) is
      use GL.Types;
      use Interfaces;
      Col_32     : Unsigned_32;
      Row_32     : Unsigned_32;
   begin
      for Col in 0 .. Height - 1 loop
         Col_32 := Unsigned_32 (Col);
         for Row in 0 .. Width - 1 loop
            Row_32 := Unsigned_32 (Row);
            Texture_Data ((Col * Width + Row) * 4 + 1) :=
              Single ((Row_32 and Col_32) and 16#FF#) / 255.0;
            Texture_Data ((Col * Width + Row) * 4 + 2) :=
              Single ((Row_32 or Col_32) and 16#FF#) / 255.0;
            Texture_Data ((Col * Width + Row) * 4 + 3) :=
              Single ((Row_32 xor Col_32) and 16#FF#) / 255.0;
            Texture_Data ((Col * Width + Row) * 4 + 4) := 1.0;
         end loop;
      end loop;
   exception
      when others =>
         Put_Line ("An exceptiom occurred in Generate_Texture.");
         raise;
   end Generate_Texture;

   --  ------------------------------------------------------------------------

   procedure Render is
      use GL.Types;

      Back_Colour : constant GL.Types.Colors.Color := (0.0, 0.25, 0.0, 1.0);
   begin
      Utilities.Clear_Background_Colour (Back_Colour);

      GL.Objects.Programs.Use_Program (Rendering_Program);
      GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, 3);

   exception
      when others =>
         Put_Line ("An exceptiom occurred in Render.");
         raise;
   end Render;

   --  ----------------------------------------------------------------------------

   procedure Setup is
      use GL.Objects.Shaders;
      use GL.Objects.Textures.Targets;
      use GL.Types;
      use Program_Loader;
      Width        : constant Int := 256;
      Height       : constant Int := 256;
      MIP_Levels   : constant Int := 8;
      X_Offset     : constant Int := 0;
      Y_Offset     : constant Int := 0;
      Texture_Data : aliased Data (1 .. Width * Height * 4);
      Image_Data   : constant GL.Objects.Textures.Image_Source :=
        GL.Objects.Textures.Image_Source (Texture_Data'Address);
   begin
      theTexture.Initialize_Id;
      Texture_2D.Bind (theTexture);
      Texture_2D.Storage (MIP_Levels, GL.Pixels.RGBA32F, Width, Height);
      Generate_Texture (Texture_Data, Width, Height);

      --  Assume the texture is already bound to the GL_TEXTURE_2D target
      Texture_2D.Load_Sub_Image_From_Data (0, X_Offset, Y_Offset,  Width, Height,
                                           GL.Pixels.RGBA, GL.Pixels.Float,
                                           Image_Data);
      Rendering_Program := Program_From
        ((Src ("src/shaders/st_vertex_shader.glsl", Vertex_Shader),
          Src ("src/shaders/st_fragment_shader.glsl", Fragment_Shader)));

      Vertex_Array.Initialize_Id;
      Vertex_Array.Bind;
      Utilities.Show_Shader_Program_Data (Rendering_Program);
   exception
      when others =>
         Put_Line ("An exceptiom occurred in Setup.");
         raise;
   end Setup;

   --  ----------------------------------------------------------------------------

   use Glfw.Input;
   Running : Boolean := True;
begin
   Setup;
   while Running loop
      Render;
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Glfw.Input.Poll_Events;
      Running := Running and not
        (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
      Running := Running and not Main_Window.Should_Close;
   end loop;
exception
   when Program_Loader.Shader_Loading_Error =>
      -- message was already written to stdout
      null;
end Main_Loop;
