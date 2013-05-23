#pragma once

#include "compiler/ShHandle.h"

class TranslatorAGAL : public TCompiler {
public:
    TranslatorAGAL(ShShaderType type, ShShaderSpec spec);
protected:
    virtual void translate(TIntermNode* root);
};
