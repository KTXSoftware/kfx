#pragma once

#include "Data.h"
#include "compiler/Common.h"
#include <stdexcept>

class TIntermNode;
class TIntermAggregate;
typedef TVector<TIntermNode*> TIntermSequence;
class TIntermTyped;
class TType;

namespace agal {
	class Parser {
		TIntermAggregate* main;
		std::map<std::string, TIntermAggregate*> helpers;
		std::vector<ParsedVar> globals;
		ParsedCode cur;
		bool allowReturn;
	public:
		Parser() : main(nullptr), allowReturn(false) {
			
		}
		
		void error(std::string msg, Position p) {
			throw std::runtime_error(msg.c_str()); //, p);
		}

		ParsedHxsl parse(TIntermNode* e);

		/*Expr* includeFile(std::string file) {
			return null;
		}*/

		VarType* getType(TType* t, Position pos);
		ParsedVar allocVarDecl(std::string v, TType* t, Position p);
		ParsedVar allocVar(const char* v, TType* t, VarKind k, Position p);

		VarKind getKindFromName(std::string name, Position p) {
			if (name == "Input") return VInput;
			else if (name == "Var") return VVar;
			else if (name == "Const") return VConst;
			else if (name == "Param") return VParam;
			else error("Unrecognized kind: " + name, p);
		}

		void parseDecl(TIntermNode* e);
		ParsedCode buildShader(TIntermAggregate* f);
		
		void addAssign(ParsedValue e1, ParsedValue e2, Position p) {
			ParsedExpr expr;
			expr.v = new ParsedValue(e1);
			expr.e = new ParsedValue(e2);
			expr.p = p;
			cur.exprs.push_back(expr);
		}

		void parseExpr(TIntermSequence& e);
		ParsedValue parseValue(TIntermTyped* e);

		/*int* parseInt(Expr e) {
			return switch (e.expr) {
			case EConst(c): switch( c ) { case CInt(i): Std.parseInt(i); default: null; }
			case EUnop(op, _, e):
				if( op == OpNeg ) {
					var i = parseInt(e);
					if( i == null ) null else -i;
				} else
					null;
			default: null;
			}
		}*/

		ParsedValue makeUnop(CodeUnop op, ParsedValue e, Position p);
		ParsedValue makeOp(CodeOp op, ParsedValue e1, ParsedValue e2, Position p);
		ParsedValue makeCall(std::string n, std::vector<TIntermTyped*> params, Position p);
	};
}
