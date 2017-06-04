#version 410 core

layout(location = 0) in vec3 vertex_Position;
layout(location = 1) in vec3 vertex_Colour;

out vec3 colour_vs;

uniform mat4 MVP_Matrix;

void main()
{
    gl_Position =  MVP_Matrix * vec4(vertex_Position, 1.0);
    colour_vs = vertex_Colour;
}

