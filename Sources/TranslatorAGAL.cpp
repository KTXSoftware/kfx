#include "TranslatorAGAL.h"

#include "compiler/InitializeParseContext.h"
#include "OutputAGAL.h"

TranslatorAGAL::TranslatorAGAL(ShShaderType type, ShShaderSpec spec) : TCompiler(type, spec) {

}

void TranslatorAGAL::translate(TIntermNode* root) {
	TParseContext& parseContext = *GetGlobalParseContext();
	OutputAGAL outputAGAL(parseContext);
	outputAGAL.output();
}
