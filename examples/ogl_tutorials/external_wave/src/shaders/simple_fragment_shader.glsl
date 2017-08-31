#version 410 core

in vec3 colour_vs;
out vec4 colour;

void main()
{
	colour = vec4 (colour_vs, 1.0);
}
