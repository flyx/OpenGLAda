--  part of OpenGLAda, (c) 2022 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Ada.Text_IO;

with GL.Compute;
with GL.Files;
with GL.Memory;
with GL.Objects.Buffers;
with GL.Objects.Shaders;
with GL.Objects.Programs;
with GL.Types;

with GL_Test.Display_Backend;

procedure GL_Test.Compute is
   use GL.Types;

   procedure Load_Singles is new GL.Objects.Buffers.Load_To_Buffer
     (Single_Pointers);

   procedure Map_Singles is new GL.Objects.Buffers.Map
     (Single_Pointers);

   Data_Count : constant := 1024;

   Iteration_Count : Natural := 0;

   Buffer          : GL.Objects.Buffers.Buffer;
   Invalid         : GL.Objects.Buffers.Buffer;
   Compute_Shader  : GL.Objects.Shaders.Shader
     (Kind => GL.Objects.Shaders.Compute_Shader);
   Program         : GL.Objects.Programs.Program;
begin
   Display_Backend.Init;
   Display_Backend.Open_Window (Width => 500, Height => 500);

   Buffer.Initialize_Id;
   Invalid.Initialize_Id;
   Compute_Shader.Initialize_Id;
   Program.Initialize_Id;

   --  initialize an SSBO with numbers from 1 to `Data_Count`
   declare
      use GL.Objects.Buffers;

      Values : Single_Array (1 .. Data_Count);
   begin
      for I In Values'Range loop
         Values (I) := Single (I);
      end loop;
      Shader_Storage_Buffer.Bind (Buffer);
      Load_Singles
        (Shader_Storage_Buffer,
         Values,
         Dynamic_Draw);
      --  The following dummy bind is necessary for the Bind_Buffer_Base
      --  to be effective. TODO: determine if it's a bug or not.
      Shader_Storage_Buffer.Bind (Invalid);
      Shader_Storage_Buffer.Bind_Buffer_Base (0, Buffer);
   end;

   -- load shader sources and compile shaders
   GL.Files.Load_Shader_Source_From_File
     (Compute_Shader, "../src/gl/gl_test-compute-shader.glsl");

   Compute_Shader.Compile;

   if not Compute_Shader.Compile_Status then
      Ada.Text_IO.Put_Line ("Compilation of compute shader failed. log:");
      Ada.Text_IO.Put_Line (Compute_Shader.Info_Log);
   end if;

   -- set up program
   Program.Attach (Compute_Shader);
   Program.Link;
   if not Program.Link_Status then
      Ada.Text_IO.Put_Line ("Program linking failed. Log:");
      Ada.Text_IO.Put_Line (Program.Info_Log);
      return;
   end if;

   -- check various constant
   declare
      use GL.Compute;
   begin
      for Index in Index_Type loop
         Ada.Text_IO.Put ("Max compute work group count " & Index'Image & " :");
         Ada.Text_IO.Put_Line (Int'Image (Max_Compute_Work_Group_Count (Index)));
      end loop;

      for Index in Index_Type loop
         Ada.Text_IO.Put ("Max compute work group size " & Index'Image & " :");
         Ada.Text_IO.Put_Line (Int'Image (Max_Compute_Work_Group_Size (Index)));
      end loop;

      Ada.Text_IO.Put ("Max compute work group invocations :");
      Ada.Text_IO.Put_Line (Int'Image (Max_Compute_Work_Group_Invocations));
   end;

   Program.Use_Program;

   -- dispatch compute shader 5 times
   while Iteration_Count < 5 loop
      --  local size for x axis is 32 as specified in the shader
      GL.Compute.Dispatch_Compute (Data_Count / 32, 1, 1);
      GL.Memory.Barrier ((Shader_Storage => True, others => False));
      Iteration_Count := Iteration_Count + 1;
   end loop;

   --  print the final result to compare to the expected value
   declare
      use GL.Objects;
      use GL.Objects.Buffers;

      CPU_Total : Single := 0.0;
      GPU_Total : Single := 0.0;

      Value_Ptr : Single_Pointers.Pointer;
   begin
      Map_Singles (Shader_Storage_Buffer, Read_Only, Value_Ptr);

      for I in 1 .. Data_Count loop
         --  We run 5 iterations of the compute shader which simply doubles its
         --  previous value, so this is ultimately the same as multiplying the
         --  initial value by 2 ** 5 = 32.
         CPU_Total := CPU_Total + Single (I) * 32.0;
         GPU_Total := GPU_Total + Value_Ptr.all;
         Single_Pointers.Increment (Value_Ptr);
      end loop;

      Unmap (Shader_Storage_Buffer);

      Ada.Text_IO.Put_Line ("CPU Total : " & CPU_Total'Image);
      Ada.Text_IO.Put_Line ("GPU Total : " & GPU_Total'Image);
   end;

   Display_Backend.Shutdown;
end GL_Test.Compute;
