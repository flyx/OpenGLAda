#version 410 core

layout(location = 0) in vec3 vertex_model_position
layout(location = 1) in vec 3 vertex_colour

out vec3 fragmant_colour

uniform mat4 MVP_Matrix

void main()
{
	gl_Position = MVP_Matrix * vec4(vertex_model_position,1.0);
	fragment_colour = vertex_colour;
}
