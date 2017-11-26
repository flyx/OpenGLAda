#version 410 core

// Ouput data
out vec4 color;

// Values that stay constant for the whole mesh.
uniform vec4 Picking_Colour;

void main()
    {
	color = Picking_Colour;
    }
