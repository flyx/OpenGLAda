#version 410 core

// Interpolated values from the vertex shaders
in vec3 fragment_colour;

out vec3 color;

void main()
{
	color = fragment_colour;
}
