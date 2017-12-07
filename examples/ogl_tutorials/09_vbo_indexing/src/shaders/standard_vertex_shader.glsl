#version 410 core

// Input vertex data, different for all executions of this shader.
layout(location = 0) in vec3 Vertex_Position_Modelspace;
layout(location = 1) in vec2 Vertex_UV;
layout(location = 2) in vec3 Vertex_Normal_Modelspace;

// Output data ; will be interpolated for each fragment.
out vec2 UV;
out vec3 Position_Worldspace;
out vec3 Normal_Camera_Space;
out vec3 Eye_Direction_Camera_Space;
out vec3 Light_Direction_Camera_Space;

// Values that stay constant for the whole mesh.
uniform mat4 MVP;
uniform mat4 V;
uniform mat4 M;
uniform vec3 Light_Position_Worldspace;

void main()
    {
	// Output position of the vertex, in clip space : MVP * position
	gl_Position =  MVP * vec4(Vertex_Position_Modelspace, 1);
	
	// Position of the vertex, in worldspace : M * position
	Position_Worldspace = (M * vec4(Vertex_Position_Modelspace, 1)).xyz;
	
	// Vector that goes from the vertex to the camera, in camera space.
	// In camera space, the camera is at the origin (0,0,0).
	vec3 Vertex_Position_Camera_Space = ( V * M * vec4(Vertex_Position_Modelspace,1)).xyz;
	Eye_Direction_Camera_Space = vec3(0, 0, 0) - Vertex_Position_Camera_Space;

	// Vector that goes from the vertex to the light, in camera space. M is ommited because it's identity.
	vec3 LightPosition_cameraspace = ( V * vec4(Light_Position_Worldspace, 1)).xyz;
	Light_Direction_Camera_Space = Light_Direction_Camera_Space + Eye_Direction_Camera_Space;
	
	// Normal of the the vertex, in camera space
	Normal_Camera_Space = ( V * M * vec4(Vertex_Normal_Modelspace, 0)).xyz; // Only correct if ModelMatrix does not scale the model ! Use its inverse transpose if not.
	
	// UV of the vertex. No special space for this one.
	UV = Vertex_UV;
    }

