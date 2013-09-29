#version 140

in  vec3 in_Position;
in  vec3 in_Color;
out vec3 ex_Color;

void main(void) {
   gl_Position = vec4(in_Position, 1.0);
   ex_Color = in_Color;
}