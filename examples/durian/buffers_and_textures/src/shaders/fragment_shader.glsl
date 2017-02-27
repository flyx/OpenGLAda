#version 410

uniform float fade_factor;
uniform sampler2D textures[2];

in vec2 texcoord;

out vec4 Frag_Colour;

void main()
{
    vec2 flipped_texcoord = vec2(texcoord.x, 1.0 - texcoord.y);
    Frag_Colour = mix(
         texture(textures[0], flipped_texcoord),
         texture(textures[1], flipped_texcoord),
         fade_factor);
}
