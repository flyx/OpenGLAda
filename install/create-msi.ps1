param (
  [switch]$wixonly = $false,
  [switch]$skipdownloads = $false
)

$ErrorActionPreference = "Stop"
Add-Type -Assembly System.IO.Compression.FileSystem

$target = "$PSScriptRoot\target"
$tmp = "$PSScriptRoot\tmp"
$heat = "${env:WIX}bin\heat.exe"
$candle = "${env:WIX}bin\candle.exe"
$light = "${env:WIX}bin\light.exe"

if (-Not ($wixonly -or $skipdownloads)) {
  Remove-Item -Path $tmp -Recurse
  Remove-Item -Path $target -Recurse

  mkdir -Force $tmp
  mkdir -Force "$target\lib"

  # necessary because by default, PowerShell uses TLS1.0
  [System.Net.ServicePointManager]::SecurityProtocol = `
      [System.Net.SecurityProtocolType]::Tls11 -bor [System.Net.SecurityProtocolType]::Tls12;

  # GLFW

  $glfw_url = "https://github.com/glfw/glfw/releases/download/3.2.1/glfw-3.2.1.bin.WIN32.zip"
  $glfw_destination = "$tmp\glfw-3.2.1.bin.WIN32.zip"
  Invoke-WebRequest -Uri $glfw_url -OutFile $glfw_destination
  $zip = [IO.Compression.ZipFile]::OpenRead($glfw_destination)
  $zip.Entries | where {$_.FullName -eq "glfw-3.2.1.bin.WIN32/lib-mingw/libglfw3.a"} | `
      foreach {[IO.Compression.ZipFileExtensions]::ExtractToFile($_, "$target\lib\libglfw3.a", $true)}
  $zip.Dispose()

  # FreeType

  $freetype_url = "https://github.com/ubawurinna/freetype-windows-binaries/raw/1089f5da22ddbdebbd72bfffab971e3159dad7cc/win32/freetype.dll"
  $freetype_destination = "$target\lib\freetype.dll"
  Invoke-WebRequest -Uri $freetype_url -OutFile $freetype_destination -UserAgent [Microsoft.PowerShell.Commands.PSUserAgent]::FireFox
}
if (-Not $wixonly) {

  # Build OpenGLAda

  $scenario = @("-XWindowing_System=windows", "-XGLFW_Version=3", "-XMode=release",
                "-XFreeType_Linker_Param=-l:freetype.dll", "-XGLFW_Linker_Param=-l:libglfw3.a")

  $env:LD_LIBRARY_PATH = "$target\lib"
  $project = "..\opengl-full.gpr"

  &"gprclean" $scenario $project
  &"gprbuild" -p $scenario $project
  &"gprinstall" -p --prefix=target $scenario $project
}

# Create file lists for WiX

$heatArgs = @("-ag", "-scom", "-sreg", "-sfrag", "-srd")
&$heat dir $target\lib\opengl -cg CoreLibFiles $heatArgs -dr libOpenglFolder -out $tmp\CoreFragments.wxs -var var.libOpengl
&$heat dir $target\lib\opengl.glfw -cg GlfwLibFiles $heatArgs -dr libOpenglGlfwFolder -out $tmp\GlfwFragments.wxs -var var.libOpenglGlfw
&$heat dir $target\lib\opengl.soil -cg SoilLibFiles $heatArgs -dr libOpenglSoilFolder -out $tmp\SoilFragments.wxs -var var.libOpenglSoil
&$heat dir $target\lib\opengl.text -cg TextLibFiles $heatArgs -dr libOpenglTextFolder -out $tmp\TextFragments.wxs -var var.libOpenglText
&$heat dir $target\lib\freetype -cg FreetypeLibFiles $heatArgs -dr libFreetypeFolder -out $tmp\FreetypeFragments.wxs -var var.libFreetype

&$heat dir $target\include\opengl -cg CoreIncludeFiles $heatArgs -dr includeOpenglFolder -out $tmp\CoreIncludes.wxs -var var.includeOpengl
&$heat dir $target\include\opengl.glfw -cg GlfwIncludeFiles $heatArgs -dr includeOpenglGlfwFolder -out $tmp\GlfwIncludes.wxs -var var.includeOpenglGlfw
&$heat dir $target\include\opengl.soil -cg SoilIncludeFiles $heatArgs -dr includeOpenglSoilFolder -out $tmp\SoilIncludes.wxs -var var.includeOpenglSoil
&$heat dir $target\include\opengl.text -cg TextIncludeFiles $heatArgs -dr includeOpenglTextFolder -out $tmp\TextIncludes.wxs -var var.includeOpenglText
&$heat dir $target\include\freetype -cg FreetypeIncludeFiles $heatArgs -dr includeFreetypeFolder -out $tmp\FreetypeIncludes.wxs -var var.includeFreetype

# Build the installer

&$candle OpenGLAda.wix $tmp\CoreFragments.wxs $tmp\FreetypeFragments.wxs $tmp\GlfwFragments.wxs $tmp\SoilFragments.wxs $tmp\TextFragments.wxs `
                       $tmp\CoreIncludes.wxs $tmp\FreetypeIncludes.wxs $tmp\GlfwIncludes.wxs $tmp\SoilIncludes.wxs $tmp\TextIncludes.wxs `
         -dlib="$target\lib" -dgpr="$target\share\gpr" `
         -dlibOpengl="$target\lib\opengl" -dlibOpenglGlfw="$target\lib\opengl.glfw" `
         -dlibOpenglSoil="$target\lib\opengl.soil" `
         -dlibOpenglText="$target\lib\opengl.text" -dlibFreetype="$target\lib\freetype" `
         -dincludeOpengl="$target\include\opengl" -dincludeOpenglGlfw="$target\include\opengl.glfw" `
         -dincludeOpenglSoil="$target\include\opengl.soil" `
         -dincludeOpenglText="$target\include\opengl.text" -dincludeFreetype="$target\include\freetype"
&$light -ext WixUIExtension OpenGLAda.wixobj CoreFragments.wixobj FreetypeFragments.wixobj `
        GlfwFragments.wixobj SoilFragments.wixobj TextFragments.wixobj `
        CoreIncludes.wixobj FreetypeIncludes.wixobj `
        GlfwIncludes.wixobj SoilIncludes.wixobj TextIncludes.wixobj -o OpenGLAda-win32.msi `
        -dWixUILicenseRtf="$PSScriptRoot\license.rtf"
