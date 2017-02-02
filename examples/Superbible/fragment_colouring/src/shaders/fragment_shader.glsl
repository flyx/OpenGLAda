#version 410

 out vec4 fragment_colour;

void main()
{
    // fragment_colour = vs_colour;
    fragment_colour = vec4(0.5*(1.0 + sin(gl_FragCoord.x * 0.25)),
                           0.5*(1.0 + cos(gl_FragCoord.y * 0.25)),
                           sin(gl_FragCoord.x * 0.15) * cos(gl_FragCoord.y * 0.15),
                           1.0);
}
