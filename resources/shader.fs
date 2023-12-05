#version 330 core

uniform sampler2D uFont, uColorTable;
uniform sampler2DArray uFg, uBg;
uniform usampler2DArray uGlyphs;
uniform vec2 fontSize, termDimensions, fontTextureDimensions, glyphTextureDimensions;
uniform int numLayers;

in vec2 vTextureCoord;
out vec4 outcolor;

float multiply(float Cb, float Cs) {
    return Cb * Cs;
}

vec3 multiply(vec3 Cb, vec3 Cs) {
    return Cb * Cs;
}

float screen(float Cb, float Cs) {
  return Cb + Cs - (Cb * Cs);
}

vec3 screen(vec3 Cb, vec3 Cs) {
  return Cb + Cs - (Cb * Cs);
}


float color_dodge(float Cb, float Cs) {
	if(Cb == 0.0) {
		return 0.0;
    } else if(Cs == 1) {
		return 1.0;
	} else {
		return min(1.0, Cb / (1.0 - Cs));
    }
}

vec3 color_dodge(vec3 Cb, vec3 Cs) {
    return vec3(color_dodge(Cb.r, Cs.r), color_dodge(Cb.g, Cs.g), color_dodge(Cb.b, Cs.b));
}

float color_burn(float Cb, float Cs) {
	if (Cb == 1.0) {
		return 1.0;
    } else if (Cs == 0.0) {
		return 0.0;
	} else {
		return 1.0 - min(1.0, (1.0 - Cb ) /  Cs);
    }
}

vec3 color_burn(vec3 Cb, vec3 Cs) {
    return vec3(color_burn(Cb.r, Cs.r), color_burn(Cb.g, Cs.g), color_burn(Cb.b, Cs.b));
}

float hard_light(float Cb, float Cs) {
    if (Cs <= 0.5) {
        return multiply(Cb, 2 * Cs);
    } else {
        return screen(Cb, 2 * Cs - 1);
    }
}

vec3 hard_light(vec3 Cb, vec3 Cs) {
    return vec3(hard_light(Cb.r, Cs.r), hard_light(Cb.g, Cs.g), hard_light(Cb.b, Cs.b));
}

vec3 overlay(vec3 Cb, vec3 Cs) {
    return hard_light(Cs, Cb);
}

float soft_light(float Cb, float Cs) {
    float D;
    if (Cb <= 0.25) {
        D = ((16 * Cb - 12) * Cb + 4) * Cb;
    } else {
        D = sqrt(Cb);
    }

    if (Cb <= 0.25) {
        return Cb - (1 - 2 * Cs) * Cb * (1 - Cb);
    } else {
        return Cb + (2 * Cs - 1) * (D - Cb);
    }
}

vec3 soft_light(vec3 Cb, vec3 Cs) {
    return vec3(soft_light(Cb.r, Cs.r), soft_light(Cb.g, Cs.g), soft_light(Cb.b, Cs.b));
}

vec3 difference(vec3 Cb, vec3 Cs) {
    return abs(Cb - Cs);
}

vec3 exclusion(vec3 Cb, vec3 Cs) {
    return Cb + Cs - 2 * Cb * Cs;
}

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
    // contains glyph_x, glyph_y, blend_mode, palette_index
    uvec4 glyphXYMP = texelFetch(uGlyphs, termXYZ, 0);
    ivec2 fontIndex = ivec2(glyphXYMP.xy);
    uint blendMode = glyphXYMP.z;
    uint paletteIndex = glyphXYMP.w;
    ivec2 fontXY = ivec2(int(fontIndex.x) * charSize.x, int(fontIndex.y) * charSize.y);
    // calc the position of the fragment relative to the terminal cell
    ivec2 charXY = ivec2(fract(vTextureCoord.x * termDimensions.x) * charSize.x,
                         (-fract(vTextureCoord.y * termDimensions.y) + 1) * charSize.y);
    vec4 fnt = texelFetch(uFont, fontXY + charXY, 0);

    vec4 fg  = texelFetch(uFg, termXYZ, 0);
    vec4 bg  = texelFetch(uBg, termXYZ, 0);
    if (paletteIndex != 0u) {
        uint colorIndex = uint(256u * fnt.r);
        fg = texelFetch(uColorTable, ivec2(colorIndex, paletteIndex - 1u), 0);
    }


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

    Fa = 1;
    Fb = 1 - Cs.a;
    switch (blendMode) {
      case 0u:
        // src_over (normal)
        blend = Cs.rgb;
        break;
      case 1u:
        // multiply
        blend = multiply(result.rgb, Cs.rgb);
        break;
      case 0x2u:
        // screen 
        blend = screen(result.rgb, Cs.rgb);
        break;
      case 0x3u:
        // overlay 
        blend = overlay(result.rgb, Cs.rgb);
        break;
      case 0x4u:
        // darken 
        blend = min(result.rgb, Cs.rgb);
        break;
      case 0x5u:
        // lighten 
        blend = max(result.rgb, Cs.rgb);
        break;
      case 0x6u:
        // color-dodge 
        blend = color_dodge(result.rgb, Cs.rgb);
        break;
      case 0x7u:
        // color-burn 
        blend = color_burn(result.rgb, Cs.rgb);
        break;
      case 0x8u:
        // hard-light 
        blend = hard_light(result.rgb, Cs.rgb);
        break;
      case 0x9u:
        // soft-light 
        blend = soft_light(result.rgb, Cs.rgb);
        break;
      case 0x10u:
        // difference 
        blend = difference(result.rgb, Cs.rgb);
        break;
      case 0x11u:
        // exclusion 
        blend = exclusion(result.rgb, Cs.rgb);
        break;
      /*case 0x12:
        // hue 
        blend = ;
        break;
      case 0x13:
        // saturation 
        blend = ;
        break;
      case 0x14:
        // color 
        blend = ;
        break;
      case 0x15}:
        // luminosity 
        blend = ;
        break;*/
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
