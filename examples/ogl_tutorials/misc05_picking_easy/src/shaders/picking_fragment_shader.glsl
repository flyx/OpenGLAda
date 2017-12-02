#version 410 core

// Ouput data
out vec4 colour;

// Values that stay constant for the whole mesh.
uniform vec4 Picking_Colour;

void main()
    {
	colour = Picking_Colour;
    }
