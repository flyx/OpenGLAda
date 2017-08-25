#version 410

in vec2 texture_coords;

out vec4 frag_colour;

uniform sampler2D bitmap_image;  //   Mono-colored bitmap image
uniform vec3 text_colour;

void main()
{
    vec4 sampled = vec4 (1.0, 1.0, 1.0, texture(bitmap_image, texture_coords).r);
    frag_colour = vec4(text_colour, 1.0) * sampled;
    // vec2 flipped_texcoord = vec2(texcoord.x, 1.0 - texcoord.y);
    /* frag_colour = mix(
         texture(textures[0], flipped_texcoord),
         texture(textures[1], flipped_texcoord),
         fade_factor); */
}
