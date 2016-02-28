#version 330

uniform sampler2D uFb;

in vec2 vTextureCoord;
out vec3 outcolor;

vec4 sample(vec2 co) {
    vec4 sum = vec4(0.0);
    vec2 hstep = vec2(0.0012, 0.0);
    vec2 vstep = vec2(0.0, 0.0012);

    sum += texture(uFb, vTextureCoord + co);
    sum += texture(uFb, vTextureCoord + co + hstep) * 0.125;
    sum += texture(uFb, vTextureCoord + co - hstep) * 0.125;
    sum += texture(uFb, vTextureCoord + co + vstep) * 0.125;
    sum += texture(uFb, vTextureCoord + co - vstep) * 0.125;
    sum += texture(uFb, vTextureCoord + co + hstep + vstep) * 0.0625;
    sum += texture(uFb, vTextureCoord + co + hstep - vstep) * 0.0625;
    sum += texture(uFb, vTextureCoord + co - hstep + vstep) * 0.0625;
    sum += texture(uFb, vTextureCoord + co - vstep - vstep) * 0.0625;

    return sum / 1.3;
}
    
void main(void){
    float scanline = vTextureCoord.y * 24 * 14 / 2;
    float wavePos = cos((fract(scanline) - 0.5) * 3.14);
    
    float red   = sample(vec2( 0.0003, 0.0)).x;
    float green = sample(vec2( 0.0000, 0.0)).y;
    float blue  = sample(vec2(-0.0003, 0.0)).z;
    
    outcolor.r = mix(0.75 * red, red, wavePos);
    outcolor.g = mix(0.75 * green, green, wavePos);
    outcolor.b = mix(0.75 * blue, blue, wavePos);
}
