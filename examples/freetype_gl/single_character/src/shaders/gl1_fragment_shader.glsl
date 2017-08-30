#version 410 core

in vec2 texture_coords;
out vec4 frag_colour;

uniform sampler2D bitmap_image;  //   Mono-colored bitmap image
uniform vec3 text_colour;

void main()
{
    vec2 flipped_coords = vec2(texture_coords.x, 1.0 - texture_coords.y);
    vec4 sampled = vec4 (1.0, 1.0, 1.0, texture(bitmap_image, flipped_coords).r);
    frag_colour = vec4(text_colour, 1.0) * sampled;
}
