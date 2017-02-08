#version 410
layout (triangles) in;
layout (points, max_vertices = 3) out;

void main()
{
  int index;

for (index = 0; index < gl_in.length(); index++)
    {
     gl_Position = gl_in[index].gl_Position;
     EmitVertex();
    }
}
