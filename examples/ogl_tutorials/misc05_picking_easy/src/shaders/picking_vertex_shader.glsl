#version 410 core

// Input vertex data, different for all executions of this shader.
layout(location = 0) in vec3 Vertex_Position_Modelspace;

// Values that stay constant for the whole mesh.
uniform mat4 MVP;

void main()
    {
	// Output position of the vertex, in clip space : MVP * position
	gl_Position =  MVP * vec4(Vertex_Position_Modelspace, 1);
	}

