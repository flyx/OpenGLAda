#version 410 core

in vec2 texture_coords;

out vec4 frag_colour;

uniform sampler2D bitmap_image;  //   Mono-colored bitmap image
uniform vec3 text_colour;

void main()
{
    vec2 coords = vec2(0.2, 0.2);
  //  vec4 sampled = vec4 (1.0, 1.0, 1.0, texture(bitmap_image, texture_coords).r);
    vec4 sampled = vec4 (texture(bitmap_image, coords));
  //  frag_colour = vec4(text_colour, 1.0) * sampled;
  frag_colour = vec4(sampled.r, sampled.b, sampled.g, 1.0);
}
