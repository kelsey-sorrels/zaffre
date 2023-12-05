#version 330 core

uniform sampler2D uFont;
uniform sampler2DArray uFg, uBg;
uniform usampler2DArray uGlyphs;
uniform vec2 fontSize, termDimensions, fontTextureDimensions, glyphTextureDimensions;
uniform int numLayers;

in vec2 vTextureCoord;
out vec4 outcolor;

void main(void) {
  // width and height of the screen in pixels
  ivec2 termXY = ivec2(floor(vTextureCoord.x * termDimensions.x),
                       floor(vTextureCoord.y * termDimensions.y));
  ivec2 charSize = ivec2(fontSize.x, fontSize.y);
  // row,column -> glyph texture uvs
  // look up in uGlphs the index in the font texture to use for the fragment

  vec4 result = vec4(0);
  //int numLayers = 1;
  for (uint i = 0u; i < uint(numLayers); i++) {
    ivec3 termXYZ = ivec3(termXY.x, termXY.y, (i + 0u));
    uvec3 glyphXYT = texelFetch(uGlyphs, termXYZ, 0).xyz;
    uint glyphType = glyphXYT.z;
    ivec2 fontIndex = ivec2(glyphXYT.xy);
    ivec2 fontXY = ivec2(int(fontIndex.x) * charSize.x, int(fontIndex.y) * charSize.y);
    // calc the position of the fragment relative to the terminal cell
    ivec2 charXY = ivec2(fract(vTextureCoord.x * termDimensions.x) * charSize.x,
                         (-fract(vTextureCoord.y * termDimensions.y) + 1) * charSize.y);
    vec4 fnt = texelFetch(uFont, fontXY + charXY, 0);

    vec4 fg  = texelFetch(uFg, termXYZ, 0);
    vec4 bg  = texelFetch(uBg, termXYZ, 0);
    uint r = uint(256u * result.r);


    // from https://www.w3.org/TR/compositing-1/#csscompositingrules_CSS

    // co: the premultiplied pixel value after compositing
    // Cs: the color value of the source graphic element being composited
    // αs: the alpha value of the source graphic element being composited
    // Cb: the color value of the backdrop
    // αb: the alpha value of the backdrop

    // Cs as
    vec4 Cs = mix(bg, fg, fnt.a);
    float Fa, Fb;
    vec3 blend;

    switch (glyphType) {
      case 0u:
        // src_over (normal)
        Fa = 1;
        Fb = 1 - Cs.a;
        blend = Cs.rgb;
        break;
      case 1u:
        // multiply
        Fa = 1;
        Fb = 1 - Cs.a;
        blend = result.rgb * Cs.rgb;
        break;
    }

    // Apply the blend in place
    Cs.rgb = (1 - result.a) * Cs.rgb + result.a * blend.rgb;
    // Composite
    result.rgb = Cs.a * Fa * Cs.rgb + result.a * Fb * result.rgb;
    // αo = αs + αb * (1 - αs)
    result.a = Cs.a + result.a * (1 - Cs.a);
  }
  outcolor = result;
}
