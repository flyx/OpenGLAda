#version 410 core

uniform sampler2D sampler;
out vec4 colour;

void main()
{
   colour = texture(sampler, gl_FragCoord.xy / textureSize(sampler, 0));
}
