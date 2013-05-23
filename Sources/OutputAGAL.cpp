#include "OutputAGAL.h"
#include "common/angleutils.h"
#include "compiler/debug.h"
#include "compiler/InfoSink.h"
#include "compiler/UnfoldShortCircuit.h"
#include "compiler/SearchSymbol.h"
#include "compiler/DetectDiscontinuity.h"
#include "agal/Parser.h"
#include "agal/Compiler.h"
#include "agal/RuntimeCompiler.h"
#include "agal/AgalCompiler.h"
#include <limits.h>
#include <stdio.h>
#include <algorithm>

namespace {
	TString str(int i) {
		char buffer[20];
		snprintf(buffer, sizeof(buffer), "%d", i);
		return buffer;
	}
}

OutputAGAL::OutputAGAL(TParseContext &context) : TIntermTraverser(false, true, false), mContext(context) {

}
 
OutputAGAL::~OutputAGAL() {

}

void OutputAGAL::output() {
	agal::Parser parser;
	auto parsed = parser.parse(mContext.treeRoot);
	agal::Compiler compiler;
	auto data = compiler.compile(parsed);
	agal::RuntimeCompiler rtCompiler;
	auto rtData = rtCompiler.compile(data);
	agal::AgalCompiler agalCompiler;
	auto finalData = agalCompiler.compile(rtData.shader);
	/*mContext.treeRoot->traverse(this);
	header();

	mContext.infoSink().obj << mHeader.c_str();
	mContext.infoSink().obj << mBody.c_str();*/
}

void OutputAGAL::header() {

}

void OutputAGAL::visitSymbol(TIntermSymbol* node) {
	TInfoSinkBase& out = mBody;

	TString name = node->getSymbol();

	if (name == "gl_FragColor") {
		out << "gl_Color[0]";
	}
	else if (name == "gl_FragData") {
		out << "gl_Color";
	}
	else if (name == "gl_DepthRange") {
		mUsesDepthRange = true;
		out << name;
	}
	else if (name == "gl_FragCoord") {
		mUsesFragCoord = true;
		out << name;
	}
	else if (name == "gl_PointCoord") {
		mUsesPointCoord = true;
		out << name;
	}
	else if (name == "gl_FrontFacing") {
		mUsesFrontFacing = true;
		out << name;
	}
	else if (name == "gl_PointSize") {
		mUsesPointSize = true;
		out << name;
	}
	else {
		TQualifier qualifier = node->getQualifier();

		if (qualifier == EvqUniform) {
			mReferencedUniforms.insert(name.c_str());
			out << name;
		}
		else if (qualifier == EvqAttribute) {
			mReferencedAttributes.insert(name.c_str());
			out << name;
		}
		else if (qualifier == EvqVaryingOut || qualifier == EvqInvariantVaryingOut || qualifier == EvqVaryingIn || qualifier == EvqInvariantVaryingIn) {
			mReferencedVaryings.insert(name.c_str());
			out << name;
		}
		else {
			out << name;
		}
	}
	out << "\n";
}

void OutputAGAL::visitConstantUnion(TIntermConstantUnion*) {
	TInfoSinkBase& out = mBody;
	out << "union!\n";
}

bool OutputAGAL::visitBinary(Visit visit, TIntermBinary* node) {
	TInfoSinkBase& out = mBody;
	switch (node->getOp()) {
	case EOpAssign:
		out << "assign:";
		break;
	case EOpInitialize:
		out << "init:";
		break;
	case EOpAddAssign:
		out << "add:";
		break;
	case EOpSubAssign:
		out << "sub:";
		break;
	case EOpMulAssign:
		out << "mul:";
		break;
	case EOpVectorTimesScalarAssign:
		out << "mul:";
		break;
	case EOpMatrixTimesScalarAssign:
		out << "mul:";
		break;
	case EOpVectorTimesMatrixAssign:
		out << "mul:";
		break;
	case EOpMatrixTimesMatrixAssign:
		out << "mul:";
		break;
	case EOpDivAssign:
		out << "div:";
		break;
	case EOpIndexDirect:
		out << "index:";
		break;
	case EOpIndexIndirect:
		out << "index:";
		break;
	case EOpIndexDirectStruct:
		out << "index:";
		break;
	case EOpVectorSwizzle:
		out << "swizzle:";
		break;
	case EOpAdd:
		out << "add:";
		break;
	case EOpSub:
		out << "sub:";
		break;
	case EOpMul:
		out << "mul:";
		break;
	case EOpDiv:
		out << "div:";
		break;
	case EOpEqual:
		out << "equal:";
		break;
	case EOpNotEqual:
		out << "notequal:";
		break;
	case EOpLessThan:
		out << "less:";
		break;
	case EOpGreaterThan:
		out << "greater:";
		break;
	case EOpLessThanEqual:
		out << "less:";
		break;
	case EOpGreaterThanEqual:
		out << "greater:";
		break;
	case EOpVectorTimesScalar:
		out << "mul:";
		break;
	case EOpMatrixTimesScalar:
		out << "mul:";
		break;
	case EOpVectorTimesMatrix:
		out << "mul:";
		break;
	case EOpMatrixTimesVector:
		out << "mul:";
		break;
	case EOpMatrixTimesMatrix:
		out << "mul:";
		break;
	case EOpLogicalOr:
		out << "or:";
		break;
	case EOpLogicalXor:
		out << "xor:";
		break;
	case EOpLogicalAnd:
		out << "and:";
		break;
	default: UNREACHABLE();
	}
	out << "\n";
	return true;
}

bool OutputAGAL::visitUnary(Visit visit, TIntermUnary* node) {
	TInfoSinkBase& out = mBody;
	switch (node->getOp()) {
	case EOpNegative:
		out << "negative";
		break;
	case EOpVectorLogicalNot:
		out << "not";
		break;
	case EOpLogicalNot:
		out << "not";
		break;
	case EOpPostIncrement:
		out << "postincrement";
		break;
	case EOpPostDecrement:
		out << "postdecrement";
		break;
	case EOpPreIncrement:
		out << "preincrement";
		break;
	case EOpPreDecrement:
		out << "predecrement";
		break;
	case EOpConvIntToBool:
		out << "inttobool";
		break;
	case EOpConvFloatToBool:
		out << "floattobool";
		break;
	case EOpConvBoolToFloat:
		out << "booltofloat";
		break;
	case EOpConvIntToFloat:
		out << "inttofloat";
		break;
	case EOpConvFloatToInt:
		out << "floattoint";
	case EOpConvBoolToInt:
		out << "booltoint";
		break;
	case EOpRadians:
		out << "radians";
		break;
	case EOpDegrees:
		out << "degrees";
		break;
	case EOpSin:
		out << "sin";
		break;
	case EOpCos:
		out << "cos";
		break;
	case EOpTan:
		out << "tan";
		break;
	case EOpAsin:
		out << "asin";
		break;
	case EOpAcos:
		out << "acos";
		break;
	case EOpAtan:
		out << "atan";
		break;
	case EOpExp:
		out << "exp";
		break;
	case EOpLog:
		out << "log";
		break;
	case EOpExp2:
		out << "exp2";
		break;
	case EOpLog2:
		out << "log2";
		break;
	case EOpSqrt:
		out << "sqrt";
		break;
	case EOpInverseSqrt:
		out << "isqrt";
		break;
	case EOpAbs:
		out << "abs";
		break;
	case EOpSign:
		out << "sign";
		break;
	case EOpFloor:
		out << "floor";
		break;
	case EOpCeil:
		out << "ceil";
		break;
	case EOpFract:
		out << "fract";
		break;
	case EOpLength:
		out << "length";
		break;
	case EOpNormalize:
		out << "normalize";
		break;
	case EOpDFdx:
		out << "ddx";
		break;
	case EOpDFdy:
		out << "ddy";
		break;
	case EOpFwidth:
		out << "fwidth";
		break;
	case EOpAny:
		out << "any";
		break;
	case EOpAll:
		out << "all";
		break;
	default: UNREACHABLE();
	}
	out << "\n";
	return true;
}

bool OutputAGAL::visitSelection(Visit visit, TIntermSelection*) {
	TInfoSinkBase& out = mBody;
	out << "selection!\n";
	return true;
}

bool OutputAGAL::visitAggregate(Visit visit, TIntermAggregate* node) {
	TInfoSinkBase &out = mBody;
	switch (node->getOp()) {
	case EOpSequence:
		out << "sequence";
		break;
	case EOpDeclaration:
		out << "decleration";
		break;
	case EOpPrototype:
		out << "prototype";
		break;
	case EOpComma:
		out << "comma";
		break;
	case EOpFunction:
		out << "function";
		break;
	case EOpFunctionCall:
		out << "call";
		break;
	case EOpParameters:
		out << "parameters";
		break;
	case EOpConstructFloat:
		out << "float";
		break;
	case EOpConstructVec2:
		out << "vec2";
		break;
	case EOpConstructVec3:
		out << "vec3";
		break;
	case EOpConstructVec4:
		out << "vec4";
		break;
	case EOpConstructBool:
		out << "bool";
		break;
	case EOpConstructBVec2:
		out << "bvec2";
		break;
	case EOpConstructBVec3:
		out << "bvec3";
		break;
	case EOpConstructBVec4:
		out << "bvec4";
		break;
	case EOpConstructInt:
		out << "int";
		break;
	case EOpConstructIVec2:
		out << "ivec2";
		break;
	case EOpConstructIVec3:
		out << "ivec3";
		break;
	case EOpConstructIVec4:
		out << "ivec4";
		break;
	case EOpConstructMat2:
		out << "mat2";
		break;
	case EOpConstructMat3:
		out << "mat3";
		break;
	case EOpConstructMat4: 
		out << "mat4";
		break;
	case EOpConstructStruct:
		out << "struct";
		break;
	case EOpLessThan:
		out << "less";
		break;
	case EOpGreaterThan:
		out << "greater";
		break;
	case EOpLessThanEqual:
		out << "less";
		break;
	case EOpGreaterThanEqual:
		out << "greater";
		break;
	case EOpVectorEqual:
		out << "equal";
		break;
	case EOpVectorNotEqual:
		out << "notequal";
		break;
	case EOpMod:
		out << "mod";
		break;
	case EOpPow:
		out << "pow";
		break;
	case EOpAtan:
		out << "atan";
		break;
	case EOpMin:
		out << "min";
		break;
	case EOpMax:
		out << "max";
		break;
	case EOpClamp:
		out << "clamp";
		break;
	case EOpMix:
		out << "lerp";
		break;
	case EOpStep:
		out << "step";
		break;
	case EOpSmoothStep:
		out << "smoothstep";
		break;
	case EOpDistance:
		out << "distance";
		break;
	case EOpDot:
		out << "dot";
		break;
	case EOpCross:
		out << "cross";
		break;
	case EOpFaceForward:
		out << "faceforward";
		break;
	case EOpReflect:
		out << "reflect";
		break;
	case EOpRefract:
		out << "refract";
		break;
	case EOpMul:
		out << "mul";
		break;
	default: UNREACHABLE();
	}
	out << "\n";
	return true;
}

bool OutputAGAL::visitLoop(Visit visit, TIntermLoop*) {
	TInfoSinkBase& out = mBody;
	out << "loop!\n";
	return true;
}

bool OutputAGAL::visitBranch(Visit visit, TIntermBranch*) {
	TInfoSinkBase& out = mBody;
	out << "branch!\n";
	return true;
}
