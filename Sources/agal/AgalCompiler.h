#pragma once

#include "Data.h"
#include <vector>

namespace agal {
	struct Temp {
		std::vector<int*> liveBits;
		std::vector<int> lastWritePos;
		std::vector<int*> assignedTo;
		std::vector<int*> assignedPos;
		int finalRegister;
		Swizzle assignedComps;
		std::vector<int> invAssignedComps;
	};

	struct AgalData {
		std::vector<Opcode*> code;
		bool fragmentShader;
	};

	class AgalCompiler {
		std::vector<Opcode*> code;
		int tempCount;
		int tempMax;
		std::map<int, Temp*> temps;
		std::vector<std::vector<Temp*> > regs;
		int codePos;
		int startRegister;
		bool packRegisters;
		bool vertex;
	public:
		AgalCompiler() { }
		void error(std::string msg, Position p);
		Reg allocTemp(VarType* t);
		std::vector<Comp> initSwiz(VarType* t);
		std::vector<Comp> convertSwiz(std::vector<Comp> swiz);
		Reg reg(Variable* v, std::vector<Comp> swiz);
		Reg delta(Reg r, int n, std::vector<Comp> s);
		Reg swizOpt(Reg r, std::vector<Comp> s);
		int swizBits(Swizzle s);
		AgalData compile(Code c);
		void compileExpr(CodeValue* e, CodeValue* v);
		
		// AGAL is using 1.3 shader profile, so does not allow exotic write mask
		/*function isUnsupportedWriteMask( r : Reg ) {
			var s = r.swiz;
			return s != null && s.length > 1 && (s[0] != X || s[1] != Y || (s.length > 2 && (s[2] != Z || (s.length > 3 && s[3] != W))));
		}*/

		bool assignRegisters(bool pack, bool vertex);
		std::vector<Opcode*> uniqueReg();
		void compileLiveness(void (AgalCompiler::*reg)(Reg, bool));
		void regLive(Reg r, bool write);
		void changeReg(Reg r, Temp* t);
		int bitCount(int i);
		void regAssign(Reg r, bool write);
		Opcode* project(Reg dst, Reg r1, Reg r2);
		Opcode* project3(Reg dst, Reg r1, Reg r2);
		Opcode* matrix44multiply(VarType* rt, Reg dst, Reg r1, Reg r2);
		Opcode* matrix33multiply(VarType* rt, Reg dst, Reg r1, Reg r2);
		void mov(Reg dst, Reg src, VarType* t);
		Opcode* matrixOp3(int num, Reg dst, Reg a, Reg b);
		Opcode* matrixOp4(int num, Reg dst, Reg a, Reg b);
		Opcode* modGenerate(Reg dst, Reg a, Reg b);
		void compileTo(Reg dst, CodeValue e);
		Reg compileSrc(CodeValue e);
	};
}
