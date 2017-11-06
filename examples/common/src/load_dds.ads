
--  This version of DDS loading procedure only supports compressed
--  DDS files.

with GL.Objects.Textures;

    procedure Load_DDS (File_Name  : String;
                        theTexture : out GL.Objects.Textures.Texture);
