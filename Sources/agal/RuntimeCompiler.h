#pragma once

#include "Data.h"
#include <map>
#include <string>
#include <vector>

namespace agal {
	struct VarProps2 {
		bool global;
		bool read;
		int write;
		Variable* newVar;
		bool isVertex;
		Const* value;
		Variable* ref;
	};

	/**
		The RuntimeCompiler is in charge of :
		- eliminating conditional constructs (If/Cond)
		- evaluating Row/Vector
		- inlining the For loops
		- allocating variables index
		- creating constant pool
		- adding extra padding to ensure platform compat
	**/
	class RuntimeCompiler {
		std::map<int, VarProps2> varProps;
		std::vector<Variable*> usedVars;
		std::vector<Variable*> constVars;
		struct Object {
			Variable* v;
			std::map<std::string, Variable*> fields;
		};
		std::map<int, Object> objectVars;
		int varId;
	
		// force replace of variables by their provided value
		bool isCond;
		// do not force constants to be replaced by variables
		bool isConst;
		Code cur;
		Position defPos;
	public:
		bool config_padWrites;
	
		RuntimeCompiler() {
			varId = 0;
			config_padWrites = true;
		}
	
		void error(std::string msg, Position p);	
		VarProps2& props(Variable* v);
	
		/**
			Compile the final shader : the params is a mapping of all the variables that will be applied at compilation time.
			The can be of the following value :
				* null
				* true/false for Bool
				* an Int base index in the paramsData table for all other types
		**/
		Data compile(Data data);
		bool isUnsupportedWriteMask(std::vector<Comp> s);
	
		struct UsedSize {
			Variable* v;
			int size;
		};

		UsedSize calculateUsedSize(Variable* v, int* index);
		void indexVars(Code c, std::vector<Variable*> constVars);
		Code compileCode(Code c);
		void checkVars();
		bool isGoodSwiz(std::vector<Comp> s);
		void padWrite(Variable* v);
		void addAssign(CodeValue* v, CodeValue e, Position p);
		Variable* allocVar(std::string name, VarKind k, VarType* t, Position p);
		CodeValue makeConst(int index, std::vector<Comp> swiz, Position p);
		std::vector<Comp> mergeSwiz(std::vector<Comp> from, std::vector<Comp> to);
		CodeValue allocConst(std::vector<float> cvals, Position p);	
		bool isTrue(Const* c);
		int swizBits(std::vector<Comp> s, VarType* t);
		int fullBits(VarType* t);	
		void compileAssign(CodeValue* v, CodeValue* e);
		Const* compileCond(CodeValue v);
		CodeValue compileConstValue(CodeValue v); // don't create a const-var
		CodeValue compileValueForce(CodeValue v);
		Variable* newVar(Variable* v, Position p);
		int* compare(Const* c1, Const* c2);
		CodeValueDecl* makeOp(CodeOp op, CodeValue e1, CodeValue e2);
		CodeValueDecl* makeUnop(CodeUnop op, CodeValue e);
		CodeValue* compileValue(CodeValue* e, bool isTarget = false);
		CodeValue compileVector(std::vector<CodeValue> values, Position p);
	};
}
