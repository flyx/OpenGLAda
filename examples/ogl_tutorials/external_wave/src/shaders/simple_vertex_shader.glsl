#version 410 core

layout(location = 0) in vec3 vertex_Position;
layout(location = 1) in vec3 vertex_Colour;

out vec3 Colour;

void main()
{
    gl_Position.xyz = vertex_Position;
    gl_Position.w = 1.0;
    Colour = vertex_Colour;
}

