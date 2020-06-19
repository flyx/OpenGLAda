with Ada.Calendar;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;

with GID;
with GL.Types;
with GL.Objects.Textures.Targets;

package body GL.Images is
   generic
      type Component is mod <>;
      Num_Components : Types.Int;
      Width, Height  : Types.Size;
   package Loader is
      type Raw_Data is array (Types.Int range <>) of Component;
      type Raw_Data_Access is access Raw_Data;
         
      procedure To_Array (
        Source : in out GID.Image_descriptor;
        Data : out Raw_Data_Access);
        
      procedure Free_Data is new Ada.Unchecked_Deallocation
        (Raw_Data, Raw_Data_Access);
   end Loader;
     
   package body Loader is
      procedure To_Array (
        Source : in out GID.Image_descriptor;
        Data : out Raw_Data_Access) is
         use type Types.Int;
           
         Cur_Pos : Types.Int := 0;
         
         procedure Set_X_Y (X, Y : Natural) is
         begin
            Cur_Pos := GL.Types.Int (Y) * Width * Num_Components
              + GL.Types.Int (X) * Num_Components;
         end Set_X_Y;
         
         procedure Put_Pixel (R, G, B, A : Component) is
            Input : constant array (Types.Int'(0) .. Types.Int'(3)) of Component := (R, G, B, A);
         begin
            for I in 0 .. Num_Components - 1 loop
               Data (Cur_Pos + I) := Input (I);
            end loop;
            Cur_Pos := Cur_Pos + Num_Components;
         end Put_Pixel;
         
         procedure Feedback (Percents : Natural) is null;
         
         procedure Content_Loader is new GID.Load_image_contents
           (Component, Set_X_Y, Put_Pixel, Feedback, GID.nice);
           
         Next_Frame : Ada.Calendar.Day_Duration;
      begin
         Data := new Raw_Data (0 .. Width * Height * Num_Components - 1);
         Content_Loader (Source, Next_Frame);
      end To_Array;
   end Loader;
   
   procedure Load_Image_To_Texture (
     Source         : in out Ada.Streams.Root_Stream_Type'Class;
     Texture        : in out GL.Objects.Textures.Texture'Class;
     Texture_Format : GL.Pixels.Internal_Format;
     Try_TGA        : Boolean := False) is
      Descriptor    : GID.Image_descriptor;
      Width, Height : Types.Size;
      
      use GL.Pixels;
      use GL.Objects.Textures.Targets;
      
      Needs_Wide_Component : Boolean := False;
      Num_Components : GL.Types.Int;
      Format : Pixels.Data_Format;
   begin
      GID.Load_image_header (Descriptor, Source, Try_TGA);
      Width := Types.Size (GID.Pixel_width (Descriptor));
      Height := Types.Size (GID.Pixel_height (Descriptor));
      case Texture_Format is
         when Depth_Component | Red | Alpha | Luminance | Alpha4 | Alpha8 |
              Luminance4 | Luminance8 | Intensity | Intensity4 | Intensity8 |
              Compressed_Red | R8 | R8I | R8UI | Compressed_Alpha |
              Compressed_Luminance | Compressed_Intensity | SLuminance |
              SLuminance8 | Compressed_Red_RGTC1 | Compressed_Signed_Red_RGTC1 |
              R8_SNorm =>
            Num_Components := 1;
            Format := Pixels.Red;
         when Alpha12 | Alpha16 | Luminance12 | Luminance16 |
              Intensity12 | Intensity16 | Depth_Component16 |
              Depth_Component24 | Depth_Component32 | R16 | R16F | R32F |
              R16I | R16UI | R32I | R32UI | R16_SNorm =>
            Num_Components := 1;
            Format := Pixels.Red;
            Needs_Wide_Component := True;
         when Luminance_Alpha | Luminance4_Alpha4 | Luminance6_Alpha2 |
              Luminance8_Alpha8 | Compressed_RG | RG | RG8 | RG8I | RG8UI |
              Compressed_Luminance_Alpha | SLuminance_Alpha |
              SLuminance8_Alpha8 | Compressed_RG_RGTC2 |
              Compressed_Signed_RG_RGTC2 | RG8_SNorm =>
            Num_Components := 2;
            Format := Pixels.RG;
         when Luminance12_Alpha4 | Luminance12_Alpha12 | Luminance16_Alpha16 |
              RG16 | RG16F | RG32F | RG16I | RG16UI | RG32I | RG32UI |
              Depth24_Stencil8 | RG16_SNorm =>
            Num_Components := 2;
            Format := Pixels.RG;
            Needs_Wide_Component := True;
         when RGB | R3_G3_B2 | RGB4 | RGB5 | RGB8 | Compressed_RGB_S3TC_DXT1 |
              Compressed_RGB | SRGB | SRGB8 | Compressed_SRGB |
              Compressed_SRGB_S3TC_DXT1 | RGB8UI | RGB8I |
              Compressed_RGB_BPTC_Signed_Float |
              Compressed_RGB_BPTC_Unsigned_Float | RGB8_SNorm =>
            Num_Components := 3;
            Format := Pixels.RGB;
         when RGB10 | RGB12 | RGB16 | R11F_G11F_B10F | RGB9_E5 | RGB32UI |
              RGB16UI | RGB32I | RGB16I | RGB16_SNorm =>
            Num_Components := 3;
            Format := Pixels.RGB;
            Needs_Wide_Component := True;
         when RGBA | RGBA2 | RGBA4 | RGB5_A1 | RGBA8 |
              Compressed_RGBA_S3TC_DXT1 | Compressed_RGBA_S3TC_DXT3 |
              Compressed_RGBA_S3TC_DXT5 | Compressed_RGBA | SRGB_Alpha |
              SRGB8_Alpha8 | Compressed_SRGB_Alpha |
              Compressed_SRGB_Alpha_S3TC_DXT1 |
              Compressed_SRGB_Alpha_S3TC_DXT3 |
              Compressed_SRGB_Alpha_S3TC_DXT5 | RGBA8UI | RGBA8I |
              Compressed_RGBA_BPTC_Unorm | Compressed_SRGB_Alpha_BPTC_UNorm |
              RGBA8_SNorm | Compressed_RGBA_ASTC_4x4 |
              Compressed_RGBA_ASTC_5x4 | Compressed_RGBA_ASTC_5x5 |
              Compressed_RGBA_ASTC_6x5 | Compressed_RGBA_ASTC_6x6 |
              Compressed_RGBA_ASTC_8x5 | Compressed_RGBA_ASTC_8x6 |
              Compressed_RGBA_ASTC_8x8 | Compressed_SRGB8_Alpha8_ASTC_4x4 |
              Compressed_SRGB8_Alpha8_ASTC_5x4 | 
              Compressed_SRGB8_Alpha8_ASTC_5x5 |
              Compressed_SRGB8_Alpha8_ASTC_6x5 |
              Compressed_SRGB8_Alpha8_ASTC_6x6 |
              Compressed_SRGB8_Alpha8_ASTC_8x5 |
              Compressed_SRGB8_Alpha8_ASTC_8x6 |
              Compressed_SRGB8_Alpha8_ASTC_8x8 |
              Compressed_SRGB8_Alpha8_ASTC_10x5 |
              Compressed_SRGB8_Alpha8_ASTC_10x6 |
              Compressed_SRGB8_Alpha8_ASTC_10x8 |
              Compressed_SRGB8_Alpha8_ASTC_10x10 |
              Compressed_SRGB8_Alpha8_ASTC_12x10 |
              Compressed_SRGB8_Alpha8_ASTC_12x12 =>
            Num_Components := 4;
            Format := Pixels.RGBA;
         when RGB10_A2 | RGBA12 | RGBA16 | RGBA32F | RGB32F | RGBA16F | RGB16F |
              RGBA32UI | RGBA16UI | RGBA32I | RGBA16I | RGBA16_SNorm |
              RGB10_A2UI | Compressed_RGBA_ASTC_10x5 |
              Compressed_RGBA_ASTC_10x6 | Compressed_RGBA_ASTC_10x8 |
              Compressed_RGBA_ASTC_10x10 | Compressed_RGBA_ASTC_12x10 |
              Compressed_RGBA_ASTC_12x12 =>
            Num_Components := 4;
            Format := Pixels.RGBA;
            Needs_Wide_Component := True;
      end case;
      if Needs_Wide_Component then
         declare
            package Impl is new Loader
              (GL.Types.UShort, Num_Components, Width, Height);
            use type Impl.Raw_Data_Access;
            Data : Impl.Raw_Data_Access := null;
         begin
            Impl.To_Array (Descriptor, Data);
            if not Texture.Initialized then
               Texture.Initialize_Id;
            end if;
            Texture_2D.Bind (Texture);
            Texture_2D.Load_From_Data (0, Texture_Format, Width, Height, Format,
              Pixels.Unsigned_Short, Objects.Textures.Image_Source
                (Data.all (Data.all'First)'Address));
            Impl.Free_Data (Data);
         exception when others =>
            if Data /= null then
               Impl.Free_Data (Data);
            end if;
            raise;
         end;
      else
         declare
            package Impl is new Loader
              (GL.Types.UByte, Num_Components, Width, Height);
            use type Impl.Raw_Data_Access;
            Data : Impl.Raw_Data_Access := null;
         begin
            Impl.To_Array (Descriptor, Data);
            if not Texture.Initialized then
               Texture.Initialize_Id;
            end if;
            Texture_2D.Bind (Texture);
            Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
            Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
            GL.Pixels.Set_Unpack_Alignment (GL.Pixels.Bytes);
            Texture_2D.Load_From_Data (0, Texture_Format, Width, Height, Format,
              Pixels.Unsigned_Byte, Objects.Textures.Image_Source
                (Data.all (Data.all'First)'Address));
            Impl.Free_Data (Data);
         exception when others =>
            if Data /= null then
               Impl.Free_Data (Data);
            end if;
            raise;
         end;
      end if;
   end Load_Image_To_Texture;
   
   procedure Load_File_To_Texture (
     Path           : String;
     Texture        : in out GL.Objects.Textures.Texture'Class;
     Texture_Format : GL.Pixels.Internal_Format;
     Try_TGA        : Boolean := False) is
      use Ada.Streams.Stream_IO;
        
      Input_File   : File_Type;
      Input_Stream : Stream_Access;
   begin
      Ada.Streams.Stream_IO.Open (Input_File, In_File, Path);
      Input_Stream := Stream (Input_File);
   
      Load_Image_To_Texture
        (Input_Stream.all, Texture, Texture_Format, Try_TGA);

      Ada.Streams.Stream_IO.Close (Input_File);
   end Load_File_To_Texture;
end GL.Images;