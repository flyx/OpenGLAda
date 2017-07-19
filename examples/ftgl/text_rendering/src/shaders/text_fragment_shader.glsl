#version 410 core

in vec2 texture_coords;
out vec4 colour;

uniform sampler2D texture_sampler;
uniform vec3 texture_colour;

void main()
{
	vec4 sampled = vec4 (1.0, 1.0, 1.0, texture(texture_sampler, texture_coords).r);
    colour = vec4(texture_colour, 1.0) * sampled;
}
