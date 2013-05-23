#pragma once

#include "Data.h"
#include <map>
#include <string>
#include <vector>

namespace agal {
	struct VarProps {
		bool read;
		bool global;
		int write;
		bool inferred;
		bool* readInShader;
	};

	class Compiler {
		Code cur;
		std::map<std::string, Variable*> vars;
		std::map<std::string, Variable*> namedVars;
		std::vector<Variable*> allVars;
		std::vector<VarProps> varProps;
		struct op {
			op(VarType* p1, VarType* p2, VarType* r) : p1(p1), p2(p2), r(r) { }
			VarType* p1;
			VarType* p2;
			VarType* r;
		};
		struct Operation {
			Operation(CodeOp o, std::vector<op> types) : o(o), types(types) { }
			CodeOp o;
			std::vector<op> types;
		};
		std::vector<std::vector<op> > ops;
		std::map<std::string, ParsedCode> helpers;
		CodeValue* ret;
		bool allowTextureRead;

	public:
		bool allowAllWMasks;

		int enumIndex(CodeOp op) {
			return (int)op;
		}

		Compiler() {
			allowAllWMasks = false;
			auto ops = initOps();
			for (auto o = ops.begin(); o != ops.end(); ++o)
				this->ops[enumIndex(o->o)] = o->types;
		}

		std::vector<Operation> initOps();
		void error(std::string msg, Position p);
		void warn(std::string msg, Position p) { }
		VarProps& props(Variable* v);
		Data compile(ParsedHxsl h);
		Code compileShader(ParsedCode c, bool vertex);
		std::map<std::string, Variable*> saveVars();
		void closeBlock(std::map<std::string, Variable*> old);
		void compileAssign(ParsedValue* v, ParsedValue* e, Position p);
		void addAssign(CodeValue v, CodeValue e, Position p);
		int swizBits(std::vector<Comp> s, VarType* t);
		int fullBits(VarType* t);
		Variable* allocVar(std::string name, VarKind k, VarType* t, Position p);

		/*function constSwiz(k,count) {
			var s = [];
			var e = [X, Y, Z, W][k];
			for( i in 0...count ) s.push(e);
			return s;
		}*/

		void checkVars();

		/*function isUnsupportedWriteMask( s : Array<Comp> ) {
			return s != null && s.length > 1 && (s[0] != X || s[1] != Y || (s.length > 2 && (s[2] != Z || (s.length > 3 && s[3] != W))));
		}*/

		void checkReadVar(Variable* v, std::vector<Comp> swiz, Position p, bool isCond);
		float* constValue(CodeValue v);	
		CodeValue compileValue(ParsedValue* e, bool* isTarget = nullptr, bool* isCond = nullptr);
		bool tryUnify(VarType* t1, VarType* t2);
		void unify(VarType* t1, VarType* t2, Position p);
		CodeValue makeOp(CodeOp op, ParsedValue e1, ParsedValue e2, Position p, bool isCond);
		CodeValue makeUnop(CodeUnop op, ParsedValue e, Position p, bool isCond);
		bool isFloat(VarType* t);
		bool isCompatible(VarType* t1, VarType* t2);
	};
}
