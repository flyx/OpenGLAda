#version 410

 in VS_OUT
{
    vec4 colour;
} fs_in;

 out vec4 fragment_colour;

void main()
{
     fragment_colour = fs_in.colour;
}
