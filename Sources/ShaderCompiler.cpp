#include "stdafx.h"
#include <iostream>
#include <fstream>
#include <stdio.h>

#include "GLSLANG/ShaderLang.h"

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <vector>

#include "compiler/TranslatorGLSL.h"
#include "compiler/TranslatorESSL.h"
#include "compiler/TranslatorHLSL.h"
#include "TranslatorAGAL.h"

#ifdef SYS_WINDOWS
#include <Windows.h>
//#include <gl/GL.h>
#endif

#ifdef SYS_OSX
//#include <OpenGL/gl3.h>
//#include <OpenGL/gl3ext.h>
#include <unistd.h>
#endif

#ifdef SYS_LINUX
//#include<X11/X.h>
//#include<X11/Xlib.h>
//#include<GL/gl.h>
//#include<GL/glx.h>
#endif

TCompiler* ConstructCompiler(ShShaderType type, ShShaderSpec spec, ShShaderOutput output) {
	switch (output) {
	case SH_GLSL_OUTPUT:
		return new TranslatorGLSL(type, spec);
	case SH_ESSL_OUTPUT:
		return new TranslatorESSL(type, spec);
#ifdef SYS_WINDOWS
	case SH_HLSL_OUTPUT:
		return new TranslatorHLSL(type, spec, output);
#endif
	case SH_AGAL_OUTPUT:
		return new TranslatorAGAL(type, spec);
	default:
		return NULL;
	}
}

//
// Delete the compiler made by ConstructCompiler
//
void DeleteCompiler(TCompiler* compiler)
{
	delete compiler;
}

//
// Return codes from main.
//
enum TFailCode {
	ESuccess = 0,
	EFailUsage,
	EFailCompile,
	EFailCompilerCreate,
};

typedef std::vector<char*> ShaderSource;
static void usage();
static ShShaderType FindShaderType(const char* fileName);
static bool CompileFile(const ShaderSource& source, ShHandle compiler, int compileOptions);
static void LogMsg(const char* msg, const char* name, const int num, const char* logName);
static void PrintActiveVariables(ShHandle compiler, ShShaderInfo varType, bool mapLongVariableNames);

// If NUM_SOURCE_STRINGS is set to a value > 1, the input file data is
// broken into that many chunks.
const unsigned int NUM_SOURCE_STRINGS = 2;
static bool readShaderSource(const char* fileName, ShaderSource& source);
static void freeShaderSource(ShaderSource& source);

//
// Set up the per compile resources
//
void GenerateResources(ShBuiltInResources* resources)
{
	ShInitBuiltInResources(resources);

	resources->MaxVertexAttribs = 8;
	resources->MaxVertexUniformVectors = 128;
	resources->MaxVaryingVectors = 8;
	resources->MaxVertexTextureImageUnits = 0;
	resources->MaxCombinedTextureImageUnits = 8;
	resources->MaxTextureImageUnits = 8;
	resources->MaxFragmentUniformVectors = 16;
	resources->MaxDrawBuffers = 1;

	resources->OES_standard_derivatives = 0;
	resources->OES_EGL_image_external = 0;
}

namespace {
	struct Varying {
		Varying(GLenum type, const std::string &name, int size, bool array)
			: type(type), name(name), size(size), array(array), reg(-1), col(-1)
		{
		}

		GLenum type;
		std::string name;
		int size;   // Number of 'type' elements
		bool array;

		int reg;    // First varying register, assigned during link
		int col;    // First register element, assigned during link
	};

	std::vector<Varying> varyings;

	struct Attribute {
		Attribute() : type(GL_NONE), name("") {
		}

		Attribute(GLenum type, const std::string &name) : type(type), name(name) {
		}

		GLenum type;
		std::string name;
	};

	std::vector<Attribute> attributes;

	/* Uniform Types */
#define GL_FLOAT_VEC2                     0x8B50
#define GL_FLOAT_VEC3                     0x8B51
#define GL_FLOAT_VEC4                     0x8B52
#define GL_INT_VEC2                       0x8B53
#define GL_INT_VEC3                       0x8B54
#define GL_INT_VEC4                       0x8B55
#define GL_BOOL                           0x8B56
#define GL_BOOL_VEC2                      0x8B57
#define GL_BOOL_VEC3                      0x8B58
#define GL_BOOL_VEC4                      0x8B59
#define GL_FLOAT_MAT2                     0x8B5A
#define GL_FLOAT_MAT3                     0x8B5B
#define GL_FLOAT_MAT4                     0x8B5C
#define GL_SAMPLER_2D                     0x8B5E
#define GL_SAMPLER_CUBE                   0x8B60

	GLenum parseType(const std::string &type) {
		if (type == "float")
		{
			return GL_FLOAT;
		}
		else if (type == "float2")
		{
			return GL_FLOAT_VEC2;
		}
		else if (type == "float3")
		{
			return GL_FLOAT_VEC3;
		}
		else if (type == "float4")
		{
			return GL_FLOAT_VEC4;
		}
		else if (type == "float2x2")
		{
			return GL_FLOAT_MAT2;
		}
		else if (type == "float3x3")
		{
			return GL_FLOAT_MAT3;
		}
		else if (type == "float4x4")
		{
			return GL_FLOAT_MAT4;
		}
		else UNREACHABLE();

		return GL_NONE;
	}

	// This method needs to match OutputHLSL::decorate
	std::string decorateAttribute(const std::string &name) {
		if (name.compare(0, 3, "gl_") != 0 && name.compare(0, 3, "dx_") != 0) {
			return "_" + name;
		}

		return name;
	}

	int VariableRowCount(GLenum type) {
		switch (type)
		{
		  case GL_NONE:
			return 0;
		  case GL_BOOL:
		  case GL_FLOAT:
		  case GL_INT:
		  case GL_BOOL_VEC2:
		  case GL_FLOAT_VEC2:
		  case GL_INT_VEC2:
		  case GL_INT_VEC3:
		  case GL_FLOAT_VEC3:
		  case GL_BOOL_VEC3:
		  case GL_BOOL_VEC4:
		  case GL_FLOAT_VEC4:
		  case GL_INT_VEC4:
			return 1;
		  case GL_FLOAT_MAT2:
			return 2;
		  case GL_FLOAT_MAT3:
			return 3;
		  case GL_FLOAT_MAT4:
			return 4;
		  default:
			UNREACHABLE();
		}

		return 0;
	}

	int VariableColumnCount(GLenum type) {
		switch (type)
		{
		  case GL_NONE:
			return 0;
		  case GL_BOOL:
		  case GL_FLOAT:
		  case GL_INT:
			return 1;
		  case GL_BOOL_VEC2:
		  case GL_FLOAT_VEC2:
		  case GL_INT_VEC2:
		  case GL_FLOAT_MAT2:
			return 2;
		  case GL_INT_VEC3:
		  case GL_FLOAT_VEC3:
		  case GL_BOOL_VEC3:
		  case GL_FLOAT_MAT3:
			return 3;
		  case GL_BOOL_VEC4:
		  case GL_FLOAT_VEC4:
		  case GL_INT_VEC4:
		  case GL_FLOAT_MAT4:
			return 4;
		  default:
			UNREACHABLE();
		}

		return 0;
	}

	#if defined(_MSC_VER)
		#define snprintf _snprintf
	#endif

	std::string str(int i) {
		char buffer[20];
		snprintf(buffer, sizeof(buffer), "%d", i);
		return buffer;
	}
}

enum HLSLProfile {
	PS2Profile, PS3Profile, PS4Profile, PS5Profile,
	VS2Profile, VS3Profile, VS4Profile, VS5Profile
};

std::string augmentHLSLVertexShader(const char* buffer, int registers, const Varying *packing[][4], HLSLProfile profile, std::map<std::string, int>& attributeIndices) {
	std::string varyingSemantic = /*(mUsesPointSize && sm3) ? "COLOR" :*/ "TEXCOORD";

	std::string shader(buffer);
	shader += "struct VS_INPUT\n{\n";

	int semanticIndex = 0;
	for (std::vector<Attribute>::iterator attribute = attributes.begin(); attribute != attributes.end(); ++attribute) {
		switch (attribute->type) {
		case GL_FLOAT:      shader += "    float ";    break;
		case GL_FLOAT_VEC2: shader += "    float2 ";   break;
		case GL_FLOAT_VEC3: shader += "    float3 ";   break;
		case GL_FLOAT_VEC4: shader += "    float4 ";   break;
		case GL_FLOAT_MAT2: shader += "    float2x2 "; break;
		case GL_FLOAT_MAT3: shader += "    float3x3 "; break;
		case GL_FLOAT_MAT4: shader += "    float4x4 "; break;
		default:  UNREACHABLE();
		}

		shader += decorateAttribute(attribute->name) + " : TEXCOORD" + str(semanticIndex) + ";\n";
		attributeIndices[attribute->name] = semanticIndex;
		semanticIndex += VariableRowCount(attribute->type);
	}

	shader += "};\n\n";

	shader += "struct VS_OUTPUT\n{\n";

	if (profile == VS4Profile || profile == VS5Profile) {
		shader += "\tfloat4 gl_Position : SV_POSITION;\n";
	}
	else {
		shader += "\tfloat4 gl_Position : POSITION;\n";
	}

	for (int r = 0; r < registers; r++)
	{
		int registerSize = packing[r][3] ? 4 : (packing[r][2] ? 3 : (packing[r][1] ? 2 : 1));

		shader += "    float" + str(registerSize) + " v" + str(r) + " : " + varyingSemantic + str(r) + ";\n";
	}

	/*if (fragmentShader->mUsesFragCoord)
	{
		vertexHLSL += "    float4 gl_FragCoord : " + varyingSemantic + str(registers) + ";\n";
	}

	if (vertexShader->mUsesPointSize && sm3)
	{
		vertexHLSL += "    float gl_PointSize : PSIZE;\n";
	}*/

	shader += "};\n\n";

	shader += "VS_OUTPUT main(VS_INPUT input)\n{\n";

	for (std::vector<Attribute>::iterator attribute = attributes.begin(); attribute != attributes.end(); ++attribute) {
		shader += "    " + decorateAttribute(attribute->name) + " = ";

		if (VariableRowCount(attribute->type) > 1)   // Matrix
		{
			shader += "transpose";
		}

		shader += "(input." + decorateAttribute(attribute->name) + ");\n";
	}

	if (profile == HLSLProfile::VS4Profile || profile == VS5Profile) {
		shader += "\n"
				   "    gl_main();\n"
				   "\n"
				   "    VS_OUTPUT output;\n"
				   "    output.gl_Position.x = gl_Position.x;\n"
				   "    output.gl_Position.y = gl_Position.y;\n"
				   "    output.gl_Position.z = (gl_Position.z + gl_Position.w) * 0.5;\n"
				   "    output.gl_Position.w = gl_Position.w;\n";
	}
	else {
		shader += "\n"
				   "    gl_main();\n"
				   "\n"
				   "    VS_OUTPUT output;\n"
				   "    output.gl_Position.x = gl_Position.x - dx_ViewAdjust.x * gl_Position.w;\n"
				   "    output.gl_Position.y = gl_Position.y + dx_ViewAdjust.y * gl_Position.w;\n"
				   "    output.gl_Position.z = (gl_Position.z + gl_Position.w) * 0.5;\n"
				   "    output.gl_Position.w = gl_Position.w;\n";
	}

	/*
	if (vertexShader->mUsesPointSize && sm3)
	{
		vertexHLSL += "    output.gl_PointSize = gl_PointSize;\n";
	}

	if (fragmentShader->mUsesFragCoord)
	{
		vertexHLSL += "    output.gl_FragCoord = gl_Position;\n";
	}
	*/
	for (std::vector<Varying>::iterator varying = varyings.begin(); varying != varyings.end(); varying++)
	{
		if (varying->reg >= 0)
		{
			for (int i = 0; i < varying->size; i++)
			{
				int rows = VariableRowCount(varying->type);

				for (int j = 0; j < rows; j++)
				{
					int r = varying->reg + i * rows + j;
					shader += "    output.v" + str(r);

					bool sharedRegister = false;   // Register used by multiple varyings

					for (int x = 0; x < 4; x++)
					{
						if (packing[r][x] && packing[r][x] != packing[r][0])
						{
							sharedRegister = true;
							break;
						}
					}

					if(sharedRegister)
					{
						shader += ".";

						for (int x = 0; x < 4; x++)
						{
							if (packing[r][x] == &*varying)
							{
								switch(x)
								{
								  case 0: shader += "x"; break;
								  case 1: shader += "y"; break;
								  case 2: shader += "z"; break;
								  case 3: shader += "w"; break;
								}
							}
						}
					}

					shader += " = " + varying->name;

					if (varying->array)
					{
						shader += "[" + str(i) + "]";
					}

					if (rows > 1)
					{
						shader += "[" + str(j) + "]";
					}

					shader += ";\n";
				}
			}
		}
	}

	shader += "\n"
				   "    return output;\n"
				   "}\n";
	return shader;
}

std::string augmentHLSLFragmentShader(const char* buffer, int registers, const Varying *packing[][4], HLSLProfile profile) {
	std::string varyingSemantic = /*(mUsesPointSize && sm3) ? "COLOR" :*/ "TEXCOORD";

	std::string shader(buffer);
	shader += "struct PS_INPUT\n{\n";

	if (profile == PS4Profile || profile == PS5Profile) {
		shader += "\tfloat4 gl_Position : SV_POSITION;\n";
	}

	for (std::vector<Varying>::iterator varying = varyings.begin(); varying != varyings.end(); ++varying) {
		if (varying->reg >= 0)
		{
			for (int i = 0; i < varying->size; i++)
			{
				int rows = VariableRowCount(varying->type);
				for (int j = 0; j < rows; j++)
				{
					int r = varying->reg + i * rows + j;
					std::string n = str(r);
					int registerSize = packing[r][3] ? 4 : (packing[r][2] ? 3 : (packing[r][1] ? 2 : 1));
					shader += "    float" + str(registerSize) + " v" + n + " : " + varyingSemantic + n + ";\n";
				}
			}
		}
		else UNREACHABLE();
	}

	/*if (fragmentShader->mUsesFragCoord)
	{
		pixelHLSL += "    float4 gl_FragCoord : " + varyingSemantic + str(registers) + ";\n";
		if (sm3) {
			pixelHLSL += "    float2 dx_VPos : VPOS;\n";
		}
	}

	if (fragmentShader->mUsesPointCoord && sm3)
	{
		pixelHLSL += "    float2 gl_PointCoord : TEXCOORD0;\n";
	}

	if (fragmentShader->mUsesFrontFacing)
	{
		pixelHLSL += "    float vFace : VFACE;\n";
	}*/

	shader +=	"};\n"
				"\n"
				"struct PS_OUTPUT {\n";

	if (profile == PS4Profile || profile == PS5Profile) {
		shader += "\tfloat4 gl_Color[1] : SV_TARGET;\n";
	}
	else {
		shader += "\tfloat4 gl_Color[1] : COLOR;\n";
	}

	shader +=	"};\n"
				"\n"
				"PS_OUTPUT main(PS_INPUT input) {\n";

	/*if (fragmentShader->mUsesFragCoord)
	{
		pixelHLSL += "    float rhw = 1.0 / input.gl_FragCoord.w;\n";

		if (sm3)
		{
			pixelHLSL += "    gl_FragCoord.x = input.dx_VPos.x + 0.5;\n"
						  "    gl_FragCoord.y = input.dx_VPos.y + 0.5;\n";
		}
		else
		{
			// dx_Coord contains the viewport width/2, height/2, center.x and center.y. See Context::applyRenderTarget()
			pixelHLSL += "    gl_FragCoord.x = (input.gl_FragCoord.x * rhw) * dx_Coord.x + dx_Coord.z;\n"
						  "    gl_FragCoord.y = (input.gl_FragCoord.y * rhw) * dx_Coord.y + dx_Coord.w;\n";
		}

		pixelHLSL += "    gl_FragCoord.z = (input.gl_FragCoord.z * rhw) * dx_Depth.x + dx_Depth.y;\n"
					  "    gl_FragCoord.w = rhw;\n";
	}

	if (fragmentShader->mUsesPointCoord && sm3)
	{
		pixelHLSL += "    gl_PointCoord.x = input.gl_PointCoord.x;\n";
		pixelHLSL += "    gl_PointCoord.y = 1.0 - input.gl_PointCoord.y;\n";
	}

	if (fragmentShader->mUsesFrontFacing)
	{
		pixelHLSL += "    gl_FrontFacing = dx_PointsOrLines || (dx_FrontCCW ? (input.vFace >= 0.0) : (input.vFace <= 0.0));\n";
	}*/

	for (std::vector<Varying>::iterator varying = varyings.begin(); varying != varyings.end(); ++varying)
	{
		if (varying->reg >= 0)
		{
			for (int i = 0; i < varying->size; i++)
			{
				int rows = VariableRowCount(varying->type);
				for (int j = 0; j < rows; j++)
				{
					std::string n = str(varying->reg + i * rows + j);
					shader += "    " + varying->name;

					if (varying->array)
					{
						shader += "[" + str(i) + "]";
					}

					if (rows > 1)
					{
						shader += "[" + str(j) + "]";
					}

					switch (VariableColumnCount(varying->type))
					{
					  case 1: shader += " = input.v" + n + ".x;\n";   break;
					  case 2: shader += " = input.v" + n + ".xy;\n";  break;
					  case 3: shader += " = input.v" + n + ".xyz;\n"; break;
					  case 4: shader += " = input.v" + n + ";\n";     break;
					  default: UNREACHABLE();
					}
				}
			}
		}
		else UNREACHABLE();
	}

	shader += "\n"
				  "    gl_main();\n"
				  "\n"
				  "    PS_OUTPUT output;\n"
				  "    output.gl_Color[0] = gl_Color[0];\n"
				  "\n"
				  "    return output;\n"
				  "}\n";
	return shader;
}

// Packs varyings into generic varying registers, using the algorithm from [OpenGL ES Shading Language 1.00 rev. 17] appendix A section 7 page 111
// Returns the number of used varying registers, or -1 if unsuccesful
int packVaryings(const Varying *packing[][4]) { //InfoLog &infoLog, const Varying *packing[][4], FragmentShader *fragmentShader) {
	//Context *context = getContext();
	const int maxVaryingVectors = 8; //context->getMaximumVaryingVectors();

	for (std::vector<Varying>::iterator varying = varyings.begin(); varying != varyings.end(); ++varying) {
		int n = VariableRowCount(varying->type) * varying->size;
		int m = VariableColumnCount(varying->type);
		bool success = false;

		if (m == 2 || m == 3 || m == 4)
		{
			for (int r = 0; r <= maxVaryingVectors - n && !success; r++)
			{
				bool available = true;

				for (int y = 0; y < n && available; y++)
				{
					for (int x = 0; x < m && available; x++)
					{
						if (packing[r + y][x])
						{
							available = false;
						}
					}
				}

				if (available)
				{
					varying->reg = r;
					varying->col = 0;

					for (int y = 0; y < n; y++)
					{
						for (int x = 0; x < m; x++)
						{
							packing[r + y][x] = &*varying;
						}
					}

					success = true;
				}
			}

			if (!success && m == 2)
			{
				for (int r = maxVaryingVectors - n; r >= 0 && !success; r--)
				{
					bool available = true;

					for (int y = 0; y < n && available; y++)
					{
						for (int x = 2; x < 4 && available; x++)
						{
							if (packing[r + y][x])
							{
								available = false;
							}
						}
					}

					if (available)
					{
						varying->reg = r;
						varying->col = 2;

						for (int y = 0; y < n; y++)
						{
							for (int x = 2; x < 4; x++)
							{
								packing[r + y][x] = &*varying;
							}
						}

						success = true;
					}
				}
			}
		}
		else if (m == 1)
		{
			int space[4] = {0};

			for (int y = 0; y < maxVaryingVectors; y++)
			{
				for (int x = 0; x < 4; x++)
				{
					space[x] += packing[y][x] ? 0 : 1;
				}
			}

			int column = 0;

			for (int x = 0; x < 4; x++)
			{
				if (space[x] >= n && space[x] < space[column])
				{
					column = x;
				}
			}

			if (space[column] >= n)
			{
				for (int r = 0; r < maxVaryingVectors; r++)
				{
					if (!packing[r][column])
					{
						varying->reg = r;

						for (int y = r; y < r + n; y++)
						{
							packing[y][column] = &*varying;
						}

						break;
					}
				}

				varying->col = column;

				success = true;
			}
		}
		else UNREACHABLE();

		if (!success)
		{
			//infoLog.append("Could not pack varying %s", varying->name.c_str());

			return -1;
		}
	}

	// Return the number of used registers
	int registers = 0;

	for (int r = 0; r < maxVaryingVectors; r++)
	{
		if (packing[r][0] || packing[r][1] || packing[r][2] || packing[r][3])
		{
			registers++;
		}
	}

	return registers;
}

void parseVaryings(const char* hlsl) {
	varyings.clear();
	if (hlsl) {
		const char* input = strstr(hlsl, "// Varyings") + 12;
		if (input == (void*)12) return;

		while(true)
		{
			char varyingType[256];
			char varyingName[256];

			int matches = sscanf(input, "static %255s %255s", varyingType, varyingName);

			if (matches != 2)
			{
				break;
			}

			char *array = strstr(varyingName, "[");
			int size = 1;

			if (array)
			{
				size = atoi(array + 1);
				*array = '\0';
			}

			varyings.push_back(Varying(parseType(varyingType), varyingName, size, array != NULL));

			input = strstr(input, ";") + 2;
		}

		//mUsesFragCoord = strstr(mHlsl, "GL_USES_FRAG_COORD") != NULL;
		//mUsesFrontFacing = strstr(mHlsl, "GL_USES_FRONT_FACING") != NULL;
		//mUsesPointSize = strstr(mHlsl, "GL_USES_POINT_SIZE") != NULL;
		//mUsesPointCoord = strstr(mHlsl, "GL_USES_POINT_COORD") != NULL;
	}
}

void parseAttributes(const char* hlsl) {
	if (hlsl) {
		const char* input = strstr(hlsl, "// Attributes") + 14;
		if (input == (void*)14) return;

		while (true) {
			char attributeType[256];
			char attributeName[256];

			int matches = sscanf(input, "static %255s _%255s", attributeType, attributeName);

			if (matches != 2)
			{
				break;
			}

			attributes.push_back(Attribute(parseType(attributeType), attributeName));

			input = strstr(input, ";") + 2;
		}
	}
}

int compileGLSL(const ShaderSource& source, const char* to, ShShaderOutput output, HLSLProfile profile, std::map<std::string, int>& attributes) {
	TFailCode failCode = ESuccess;

	int compileOptions = SH_OBJECT_CODE | SH_ATTRIBUTES_UNIFORMS;
	int numCompiles = 0;
	ShHandle vertexCompiler = 0;
	ShHandle fragmentCompiler = 0;
	char* buffer = 0;
	size_t bufferLen = 0;
	int numAttribs = 0, numUniforms = 0;
	ShShaderSpec spec = SH_GLES2_SPEC;
	//ShShaderOutput output = SH_ESSL_OUTPUT;

	ShInitialize();

	ShBuiltInResources resources;
	GenerateResources(&resources);

	//argc--;
	//argv++;
	//for (; (argc >= 1) && (failCode == ESuccess); argc--, argv++) {
		/*if (argv[0][0] == '-') {
			switch (argv[0][1]) {
			case 'i': compileOptions |= SH_INTERMEDIATE_TREE; break;
			case 'm': compileOptions |= SH_MAP_LONG_VARIABLE_NAMES; break;
			case 'o': compileOptions |= SH_OBJECT_CODE; break;
			case 'u': compileOptions |= SH_ATTRIBUTES_UNIFORMS; break;
			case 'l': compileOptions |= SH_UNROLL_FOR_LOOP_WITH_INTEGER_INDEX; break;
			case 'e': compileOptions |= SH_EMULATE_BUILT_IN_FUNCTIONS; break;
			case 'd': compileOptions |= SH_DEPENDENCY_GRAPH; break;
			case 't': compileOptions |= SH_TIMING_RESTRICTIONS; break;
			case 's':
				if (argv[0][2] == '=') {
					switch (argv[0][3]) {
						case 'e': spec = SH_GLES2_SPEC; break;
						case 'w': spec = SH_WEBGL_SPEC; break;
						case 'c': spec = SH_CSS_SHADERS_SPEC; break;
						default: failCode = EFailUsage;
					}
				} else {
					failCode = EFailUsage;
				}
				break;
			case 'b':
				if (argv[0][2] == '=') {
					switch (argv[0][3]) {
					case 'e': output = SH_ESSL_OUTPUT; break;
					case 'g': output = SH_GLSL_OUTPUT; break;
					case 'h': output = SH_HLSL_OUTPUT; break;
					default: failCode = EFailUsage;
					}
				} else {
					failCode = EFailUsage;
				}
				break;
			case 'x':
				if (argv[0][2] == '=') {
					switch (argv[0][3]) {
					case 'i': resources.OES_EGL_image_external = 1; break;
					case 'd': resources.OES_standard_derivatives = 1; break;
					case 'r': resources.ARB_texture_rectangle = 1; break;
					default: failCode = EFailUsage;
					}
				} else {
					failCode = EFailUsage;
				}
				break;
			default: failCode = EFailUsage;
			}
		} else*/ //{
	ShHandle compiler = 0;
	switch (profile) {
	case VS2Profile:
	case VS3Profile:
	case VS4Profile:
	case VS5Profile:
		if (vertexCompiler == 0)
			vertexCompiler = ShConstructCompiler(
				SH_VERTEX_SHADER, spec, output, &resources);
		compiler = vertexCompiler;
		break;
	case PS2Profile:
	case PS3Profile:
	case PS4Profile:
	case PS5Profile:
		if (fragmentCompiler == 0)
			fragmentCompiler = ShConstructCompiler(
				SH_FRAGMENT_SHADER, spec, output, &resources);
		compiler = fragmentCompiler;
		break;
	default: break;
	}
	if (compiler) {
		bool compiled = CompileFile(source, compiler, compileOptions);

		//LogMsg("BEGIN", "COMPILER", numCompiles, "INFO LOG");
		ShGetInfo(compiler, SH_INFO_LOG_LENGTH, &bufferLen);
		buffer = (char*) realloc(buffer, bufferLen * sizeof(char));
		ShGetInfoLog(compiler, buffer);
		puts(buffer);
		//LogMsg("END", "COMPILER", numCompiles, "INFO LOG");
		//printf("\n\n");

		if (compiled && (compileOptions & SH_OBJECT_CODE)) {
			//LogMsg("BEGIN", "COMPILER", numCompiles, "OBJ CODE");
			ShGetInfo(compiler, SH_OBJECT_CODE_LENGTH, &bufferLen);
			buffer = (char*) realloc(buffer, bufferLen * sizeof(char));
			ShGetObjectCode(compiler, buffer);
			//puts(buffer);
			//LogMsg("END", "COMPILER", numCompiles, "OBJ CODE");
			//printf("\n\n");

			if (output == SH_HLSL_OUTPUT) {
				std::string shader;
				parseAttributes(buffer);
				parseVaryings(buffer);
				const int MAX_VARYING_VECTORS_SM3 = 10;
				const Varying *packing[MAX_VARYING_VECTORS_SM3][4] = {NULL};
				int registers = packVaryings(packing);
				if (profile == VS2Profile || profile == VS3Profile || profile == VS4Profile || profile == VS5Profile) {
					shader = augmentHLSLVertexShader(buffer, registers, packing, profile, attributes);
				}
				else {
					shader = augmentHLSLFragmentShader(buffer, registers, packing, profile);
				}
				std::ofstream file(to, std::ios_base::binary);
				file.write(shader.c_str(), shader.size());
			}
			else {
				std::ofstream file(to, std::ios_base::binary);
				size_t size = strlen(buffer);
				file.write(buffer, size);
			}
		}
		/*if (compiled && (compileOptions & SH_ATTRIBUTES_UNIFORMS)) {
			LogMsg("BEGIN", "COMPILER", numCompiles, "ACTIVE ATTRIBS");
			PrintActiveVariables(compiler, SH_ACTIVE_ATTRIBUTES, (compileOptions & SH_MAP_LONG_VARIABLE_NAMES) != 0);
			LogMsg("END", "COMPILER", numCompiles, "ACTIVE ATTRIBS");
			printf("\n\n");

			LogMsg("BEGIN", "COMPILER", numCompiles, "ACTIVE UNIFORMS");
			PrintActiveVariables(compiler, SH_ACTIVE_UNIFORMS, (compileOptions & SH_MAP_LONG_VARIABLE_NAMES) != 0);
			LogMsg("END", "COMPILER", numCompiles, "ACTIVE UNIFORMS");
			printf("\n\n");
		}*/
		if (!compiled)
			failCode = EFailCompile;
		++numCompiles;
	}
	else {
		failCode = EFailCompilerCreate;
	}
		//}
	//}

	if ((vertexCompiler == 0) && (fragmentCompiler == 0))
		failCode = EFailUsage;
	if (failCode == EFailUsage)
		usage();

	if (vertexCompiler)
		ShDestruct(vertexCompiler);
	if (fragmentCompiler)
		ShDestruct(fragmentCompiler);
	if (buffer)
		free(buffer);
	ShFinalize();

	return failCode;
}

//
//   print usage to stdout
//
void usage()
{
	printf("Usage: translate [-i -m -o -u -l -e -b=e -b=g -b=h -x=i -x=d] file1 file2 ...\n"
		"Where: filename : filename ending in .frag or .vert\n"
		"       -i       : print intermediate tree\n"
		"       -m       : map long variable names\n"
		"       -o       : print translated code\n"
		"       -u       : print active attribs and uniforms\n"
		"       -l       : unroll for-loops with integer indices\n"
		"       -e       : emulate certain built-in functions (workaround for driver bugs)\n"
		"       -t       : enforce experimental timing restrictions\n"
		"       -d       : print dependency graph used to enforce timing restrictions\n"
		"       -s=e     : use GLES2 spec (this is by default)\n"
		"       -s=w     : use WebGL spec\n"
		"       -s=c     : use CSS Shaders spec\n"
		"       -b=e     : output GLSL ES code (this is by default)\n"
		"       -b=g     : output GLSL code\n"
		"       -b=h     : output HLSL code\n"
		"       -x=i     : enable GL_OES_EGL_image_external\n"
		"       -x=d     : enable GL_OES_EGL_standard_derivatives\n"
		"       -x=r     : enable ARB_texture_rectangle\n");
}

std::string extractFilename(std::string path) {
	int i = path.size() - 1;
	for (; i > 0; --i) {
		if (path[i] == '/' || path[i] == '\\') {
			++i;
			break;
		}
	}
	return path.substr(i, std::string::npos);
}

std::string removeExtension(std::string filename) {
	int i = filename.size() - 1;
	for (; i > 0; --i) {
		if (filename[i] == '.') {
			break;
		}
	}
	if (i == 0) return filename;
	else return filename.substr(0, i);
}

bool isVertexShader(std::string filename) {
	if (filename.substr(filename.size() - 5) == ".vert") return true;
	std::string withoutext = removeExtension(filename);
	return withoutext.substr(withoutext.size() - 5, std::string::npos) == ".vert";
}

//
//   Deduce the shader type from the filename.  Files must end in one of the
//   following extensions:
//
//   .frag*    = fragment shader
//   .vert*    = vertex shader
//
ShShaderType FindShaderType(const char* fileName)
{
	assert(fileName);

	/*const char* ext = strrchr(fileName, '.');

	if (ext && strcmp(ext, ".sl") == 0)
		for (; ext > fileName && ext[0] != '.'; ext--);

	ext = strrchr(fileName, '.');
	if (ext) {
		if (strncmp(ext, ".frag", 4) == 0) return SH_FRAGMENT_SHADER;
		if (strncmp(ext, ".vert", 4) == 0) return SH_VERTEX_SHADER;
	}

	return SH_FRAGMENT_SHADER;*/
	if (isVertexShader(fileName)) return SH_VERTEX_SHADER;
	else return SH_FRAGMENT_SHADER;
}

//
//   Read a file's data into a string, and compile it using ShCompile
//
bool CompileFile(const ShaderSource& source, ShHandle compiler, int compileOptions)
{
	int ret = ShCompile(compiler, &source[0], source.size(), compileOptions);
	return ret ? true : false;
}

void LogMsg(const char* msg, const char* name, const int num, const char* logName)
{
	printf("#### %s %s %d %s ####\n", msg, name, num, logName);
}

void PrintActiveVariables(ShHandle compiler, ShShaderInfo varType, bool mapLongVariableNames)
{
	size_t nameSize = 0;
	switch (varType) {
		case SH_ACTIVE_ATTRIBUTES:
			ShGetInfo(compiler, SH_ACTIVE_ATTRIBUTE_MAX_LENGTH, &nameSize);
			break;
		case SH_ACTIVE_UNIFORMS:
			ShGetInfo(compiler, SH_ACTIVE_UNIFORM_MAX_LENGTH, &nameSize);
			break;
		default: assert(0);
	}
	if (nameSize <= 1) return;
	char* name = new char[nameSize];

	char* mappedName = NULL;
	if (mapLongVariableNames) {
		size_t mappedNameSize = 0;
		ShGetInfo(compiler, SH_MAPPED_NAME_MAX_LENGTH, &mappedNameSize);
		mappedName = new char[mappedNameSize];
	}

	size_t activeVars = 0;
	int size = 0;
	ShDataType type = SH_NONE;
	const char* typeName = NULL;
	ShGetInfo(compiler, varType, &activeVars);
	for (size_t i = 0; i < activeVars; ++i) {
		switch (varType) {
			case SH_ACTIVE_ATTRIBUTES:
				ShGetActiveAttrib(compiler, i, NULL, &size, &type, name, mappedName);
				break;
			case SH_ACTIVE_UNIFORMS:
				ShGetActiveUniform(compiler, i, NULL, &size, &type, name, mappedName);
				break;
			default: assert(0);
		}
		switch (type) {
			case SH_FLOAT: typeName = "GL_FLOAT"; break;
			case SH_FLOAT_VEC2: typeName = "GL_FLOAT_VEC2"; break;
			case SH_FLOAT_VEC3: typeName = "GL_FLOAT_VEC3"; break;
			case SH_FLOAT_VEC4: typeName = "GL_FLOAT_VEC4"; break;
			case SH_INT: typeName = "GL_INT"; break;
			case SH_INT_VEC2: typeName = "GL_INT_VEC2"; break;
			case SH_INT_VEC3: typeName = "GL_INT_VEC3"; break;
			case SH_INT_VEC4: typeName = "GL_INT_VEC4"; break;
			case SH_BOOL: typeName = "GL_BOOL"; break;
			case SH_BOOL_VEC2: typeName = "GL_BOOL_VEC2"; break;
			case SH_BOOL_VEC3: typeName = "GL_BOOL_VEC3"; break;
			case SH_BOOL_VEC4: typeName = "GL_BOOL_VEC4"; break;
			case SH_FLOAT_MAT2: typeName = "GL_FLOAT_MAT2"; break;
			case SH_FLOAT_MAT3: typeName = "GL_FLOAT_MAT3"; break;
			case SH_FLOAT_MAT4: typeName = "GL_FLOAT_MAT4"; break;
			case SH_SAMPLER_2D: typeName = "GL_SAMPLER_2D"; break;
			case SH_SAMPLER_CUBE: typeName = "GL_SAMPLER_CUBE"; break;
			case SH_SAMPLER_EXTERNAL_OES: typeName = "GL_SAMPLER_EXTERNAL_OES"; break;
			default: assert(0);
		}
		printf("%d: name:%s type:%s size:%d", i, name, typeName, size);
		if (mapLongVariableNames)
			printf(" mapped name:%s", mappedName);
		printf("\n");
	}
	delete [] name;
	if (mappedName)
		delete [] mappedName;
}

static bool readShaderSource(const char* fileName, ShaderSource& source) {
	FILE* in = fopen(fileName, "rb");
	if (!in) {
		printf("Error: unable to open input file: %s\n", fileName);
		return false;
	}

	// Obtain file size.
	fseek(in, 0, SEEK_END);
	int count = ftell(in);
	rewind(in);

	int len = (int)ceil((float)count / (float)NUM_SOURCE_STRINGS);
	source.reserve(NUM_SOURCE_STRINGS);
	// Notice the usage of do-while instead of a while loop here.
	// It is there to handle empty files in which case a single empty
	// string is added to vector.
	do {
		char* data = new char[len + 1];
		int nread = fread(data, 1, len, in);
		data[nread] = '\0';
		source.push_back(data);

		count -= nread;
	} while (count > 0);

	fclose(in);
	return true;
}

static void freeShaderSource(ShaderSource& source) {
	for (ShaderSource::size_type i = 0; i < source.size(); ++i) {
		delete[] source[i];
	}
	source.clear();
}

static void executeSync(const char* command) {
#ifdef SYS_WINDOWS
	STARTUPINFOA startupInfo;
	PROCESS_INFORMATION processInfo;
	memset(&startupInfo, 0, sizeof(startupInfo));
	memset(&processInfo, 0, sizeof(processInfo));
	startupInfo.cb = sizeof(startupInfo);
	CreateProcessA(nullptr, (char*)command, nullptr, nullptr, FALSE, CREATE_DEFAULT_ERROR_MODE, "PATH=%PATH%;.\\cygwin\\bin\0", nullptr, &startupInfo, &processInfo);
	WaitForSingleObject(processInfo.hProcess, INFINITE);
	CloseHandle(processInfo.hProcess);
	CloseHandle(processInfo.hThread);
#endif
	system(command);
}

extern int compileHLSLToD3D9(const char* from, const char* to, const std::map<std::string, int>& attributes);
extern int compileHLSLToD3D11(const char* from, const char* to, const std::map<std::string, int>& attributes);

static int compileGLSLToD3D9(std::string from, std::string to, std::string tempdir) {
#ifdef SYS_WINDOWS
	ShaderSource source;
	readShaderSource(from.c_str(), source);
	source.push_back("void main() { kore(); }");

	std::string totemp = tempdir + "/" + removeExtension(extractFilename(from)) + ".hlsl";
	std::map<std::string, int> attributes;
	int result = compileGLSL(source, totemp.c_str(), SH_HLSL_OUTPUT, isVertexShader(from.c_str()) ? VS2Profile : PS2Profile, attributes);
	if (result == ESuccess) result = compileHLSLToD3D9(totemp.c_str(), to.c_str(), attributes);
	return result;
#else
	return 0;
#endif
}

static int compileGLSLToD3D11(std::string from, std::string to, std::string tempdir) {
#ifdef SYS_WINDOWS
	ShaderSource source;
	readShaderSource(from.c_str(), source);
	source.push_back("void main() { kore(); }");

	std::string totemp = tempdir + "/" + removeExtension(extractFilename(from)) + ".hlsl";
	std::map<std::string, int> attributes;
	int result = compileGLSL(source, totemp.c_str(), SH_HLSL_OUTPUT, isVertexShader(from.c_str()) ? VS4Profile : PS4Profile, attributes);
	if (result == ESuccess) result = compileHLSLToD3D11(totemp.c_str(), to.c_str(), attributes);
	return result;
#else
	return 0;
#endif
}

static int compileGLSLToESSL(ShaderSource source, std::string to, HLSLProfile profile) {
	std::map<std::string, int> attributes;
	return compileGLSL(source, to.c_str(), SH_ESSL_OUTPUT, profile, attributes);
}

static int compileGLSLToESSL(std::string from, std::string to) {
	ShaderSource source;
	readShaderSource(from.c_str(), source);
	source.push_back("void main() { kore(); }");

	std::map<std::string, int> attributes;
	return compileGLSL(source, to.c_str(), SH_ESSL_OUTPUT, isVertexShader(from.c_str()) ? VS2Profile : PS2Profile, attributes);
}

bool glsl14 = false;

static int compileGLSLToGLSL(ShaderSource source, std::string to, HLSLProfile profile) {
	glsl14 = true;
	std::map<std::string, int> attributes;
	return compileGLSL(source, to.c_str(), SH_GLSL_OUTPUT, profile, attributes);
}

static int compileGLSLToGLSL(std::string from, std::string to) {
	ShaderSource source;
	readShaderSource(from.c_str(), source);
	source.push_back("void main() { kore(); }");

	return compileGLSLToGLSL(source, to, isVertexShader(from.c_str()) ? VS2Profile : PS2Profile);
}

static void copyFile(std::string from, std::string to) {
#ifdef SYS_WINDOWS
	CopyFileA((char*)from.c_str(), (char*)to.c_str(), FALSE);
#endif
#ifdef SYS_OSX
	pid_t pid = fork();
	if (pid == 0) {
		execl("/bin/cp", "/bin/cp", from.c_str(), to.c_str(), (char *)0);
	}
#endif
}

void compileShader(const char* inFilename, const char* outFilename, int mode, bool optimize, bool gles);

static int compileGLSLToAGAL(std::string from, std::string to, std::string tempdir) {
#if 0
	//std::string totemp = tempdir + "/" + extractFilename(from);

	ShaderSource source;
	readShaderSource(from.c_str(), source);
	/*if (isVertexShader(from.c_str())) {
		source.push_back("void main() {");
			source.push_back("kore();");
			source.push_back("vec4 pos = gl_Position;"); //hack to avoid https://github.com/adobe/glsl2agal/issues/24
			source.push_back("gl_Position.z = (pos.z + pos.w) * 0.5;");
			source.push_back("gl_Position.xy = pos.xy;");
			source.push_back("gl_Position.w = pos.w;");
		source.push_back("}");
	}
	else {
		source.push_back("void main() { kore(); }");
	}*/
	if (isVertexShader(from.c_str())) {
		source.push_back("void main() {");
			source.push_back("kore();");
			source.push_back("gl_Position.z = (gl_Position.z + gl_Position.w) * 0.5;");
		source.push_back("}");
	}
	else {
		source.push_back("void main() { kore(); }");
	}

	/*compileGLSLToGLSL(source, totemp, isVertexShader(from.c_str()) ? VS2Profile : PS2Profile);
#ifdef SYS_WINDOWS
	std::string command("glsl2agalopt.exe -optimize ");
#else
	std::string command("./glsl2agalopt -optimize ");
#endif
	if (isVertexShader(from.c_str())) command += "-v ";
	else command += "-f ";
	command += totemp;
	executeSync(command.c_str());
	copyFile(totemp + ".out", to);
	return 0;*/

	std::map<std::string, int> attributes;
	return compileGLSL(source, to.c_str(), SH_AGAL_OUTPUT, isVertexShader(from.c_str()) ? VS2Profile : PS2Profile, attributes);
#endif

	ShaderSource source;
	readShaderSource(from.c_str(), source);
	if (isVertexShader(from.c_str())) {
		source.push_back("void main() {");
		source.push_back("kore();");
		source.push_back("vec4 pos = gl_Position;"); //hack to avoid https://github.com/adobe/glsl2agal/issues/24
		source.push_back("gl_Position.z = (pos.z + pos.w) * 0.5;");
		source.push_back("gl_Position.xy = pos.xy;");
		source.push_back("gl_Position.w = pos.w;");
		source.push_back("}");
	}
	else {
		source.push_back("void main() { kore(); }");
	}

	std::string totemp = tempdir + "/" + extractFilename(from);
	compileGLSLToESSL(source, totemp, isVertexShader(from.c_str()) ? VS2Profile : PS2Profile);

	compileShader(totemp.c_str(), to.c_str(), isVertexShader(from.c_str()) ? 0 : 1, true, true);
	return 1;
}

int main(int argc, char* argv[]) {
	if (argc < 5) {
		std::cout << "Usage: Kg profile in out tempdir" << std::endl;
		return 1;
	}
	if (strcmp(argv[1], "d3d9") == 0) {
		return compileGLSLToD3D9(argv[2], argv[3], argv[4]);
	}
	else if (strcmp(argv[1], "d3d11") == 0) {
		return compileGLSLToD3D11(argv[2], argv[3], argv[4]);
	}
	else if (strcmp(argv[1], "glsl") == 0) {
		return compileGLSLToGLSL(argv[2], argv[3]);
	}
	else if (strcmp(argv[1], "essl") == 0) {
		return compileGLSLToESSL(argv[2], argv[3]);
	}
	else if (strcmp(argv[1], "agal") == 0) {
		return compileGLSLToAGAL(argv[2], argv[3], argv[4]);
	}
	else {
		std::cout << "Unknown profile " << argv[1] << std::endl;
		return 1;
	}
}
