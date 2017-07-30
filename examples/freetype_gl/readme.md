Author: Roger Mc Murtrie
These examples are loosely based on text rendering code that uses shaders from various sources.
Most text rendering tutorials and examples that I could find do not use shaders but use the now-deprecated immediate pipeline.
A useful tutorial is Learn OpenGL>In Practice>Text Rendering at https://learnopengl.com/#!In-Practice/Text-Rendering. However, although this is one of the few tutorials that uses shaders it uses FreeType directly rather than FTGL to load fonts.
The version numbers in the shader programs may need modification depending on your
platform.

GNAT Programming Studio (GPS) Considerations
Use Tools->Views->Scenarios to display the Scenario panel.
In the Scenario panel set GLFW Version to 3.
For Mac, in the Scenario panel set Windowing System to quartz.
In Preferences, ensure that Editor->Fonts & Colors->Default is set to a fixed size font.
In Preferences->Editor->Ada set appropriate format selectors.


