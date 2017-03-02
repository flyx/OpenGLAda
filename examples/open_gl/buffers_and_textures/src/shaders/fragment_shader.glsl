#version 410

in vec2 image_coords_vs;
in vec3 triangle_colour;

out vec4 Frag_Colour;

uniform sampler2D image;

void main()
{
    Frag_Colour = texture(image, image_coords_vs) * vec4(triangle_colour, 1.0);
}
