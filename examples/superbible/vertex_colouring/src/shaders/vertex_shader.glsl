#version 410

 layout (location = 0) in vec4 offset;

 out vec4 vs_colour;

void main()
{
  const vec4 vertices[3] = vec4[3] (vec4( 0.25, -0.25, 0.5, 1.0),
                                                      vec4(-0.25, -0.25, 0.5, 1.0),
                                    vec4( 0.25, 0.25, 0.5, 1.0));
  const vec4 colours[] = vec4[3] (vec4(1.0, 0.0, 0.0, 1.0),
                                      vec4(0.0, 1.0, 0.0, 1.0),
                                      vec4(0.0,  0.0, 1.0, 1.0));
  vs_colour = colours[gl_VertexID];
  gl_Position = vertices[gl_VertexID] + offset;
}
