#version 410

in vec2 texture_coords;
out vec4 colour;

uniform sampler2D text_sampler;  //   Mono-colored bitmap image
uniform vec3 text_colour;

void main()
{
	vec4 sampled = vec4 (1.0, 1.0, 1.0, texture(text_sampler, texture_coords).r);
    colour = vec4(text_colour, 1.0) * sampled;
}
