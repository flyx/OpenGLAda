
with GL.Objects.Textures;

package DDS_Loader is
    procedure Load_DDS (File_Name  : String;
                        theTexture : out GL.Objects.Textures.Texture);
end DDS_Loader;
