#version 410 core

layout (location = 0) out vec4 colour;

in VERTEX
{
    vec3    normal;
    vec4    colour;
} vertex;

void main(void)
   {
  // colour = vertex.colour * (0.1 + abs(vertex.normal.z)) + vec4(0.8, 0.9, 0.7, 1.0) * pow(abs(vertex.normal.z), 40.0);
   colour = vertex.colour * (0.8 + 1.0 * abs(vertex.normal.z))
       + vec4(0.8, 0.9, 0.7, 1.0) * pow(abs(vertex.normal.z), 40.0);
   }
