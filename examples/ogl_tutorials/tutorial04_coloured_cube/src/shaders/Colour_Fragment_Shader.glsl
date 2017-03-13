#version 410 core

// Interpolated values from the vertex shaders
in vec3 Fragment_Colour;

out vec3 color;

void main()
{
	color = Fragment_Colour;
}
