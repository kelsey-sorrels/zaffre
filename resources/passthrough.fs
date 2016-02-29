#version 330 core

uniform sampler2D uFb;

in vec2 vTextureCoord;
out vec4 outcolor;

void main(void){
    outcolor = texture(uFb, vTextureCoord);
}
