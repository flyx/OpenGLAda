#version 410 core

// Interpolated values from the vertex shaders
in vec2 UV;

out vec3 colour;

// Values that stay constant for the whole mesh.
uniform sampler2D myTextureSampler;

void main()
{
    colour = texture(myTextureSampler, UV).rgb;
}
