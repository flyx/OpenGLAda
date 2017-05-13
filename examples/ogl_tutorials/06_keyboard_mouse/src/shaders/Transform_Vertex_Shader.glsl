#version 410 core

// Input vertex data, different for all executions of this shader.
layout(location = 0) in vec3 vertexPosition_modelspace;
layout(location = 1) in vec2 vertexUV;

// Output data ; will be interpolated for each fragment.
out vec2 UV;

// Values that stay constant for the whole mesh.
uniform mat4 MVP_5;

void main()
{
    gl_Position =  MVP_5 * vec4(vertexPosition_modelspace,1.0);
    UV = vertexUV;
}

