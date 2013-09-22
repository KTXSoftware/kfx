#pragma once

#include <map>
#include <string>
#include <vector>

namespace agal {
	enum Comp {
		X,
		Y,
		Z,
		W
	};

	typedef std::vector<Comp> Swizzle; // length 1-4
	
	enum RegType {
		RAttr,
		RConst,
		RTemp,
		ROut,
		RVar
	};

	struct Const;

	struct RegAccess {
		RegType t;
		Comp comp;
		int offset;
	};

	struct Reg {
		RegType t;
		int index;
		Swizzle swiz;
		RegAccess* access;
	};

	struct TexFlag {
		virtual bool isMipMapDisable() { return false; }
		virtual bool isMipMapNearest() { return false; }
		virtual bool isMipMapLinear() { return false; }
		virtual bool isWrap() { return false; }
		virtual bool isClamp() { return false; }
		virtual bool isFilterNearest() { return false; }
		virtual bool isFilterLinear() { return false; }
		virtual bool isSingle() { return false; }
		virtual bool isLodBias() { return false; }
		virtual bool isCube() { return false; }
	};

	struct Tex {
		int index;
		std::vector<TexFlag*> flags;
	};

	struct Opcode {
		virtual bool isMov() { return false; }
		virtual bool isAdd() { return false; }
		virtual bool isSub() { return false; }
		virtual bool isMul() { return false; }
		virtual bool isDiv() { return false; }
		virtual bool isRcp() { return false; }
		virtual bool isMin() { return false; }
		virtual bool isMax() { return false; }
		virtual bool isFrc() { return false; }
		virtual bool isSqt() { return false; }
		virtual bool isRsq() { return false; }
		virtual bool isPow() { return false; }
		virtual bool isLog() { return false; }
		virtual bool isExp() { return false; }
		virtual bool isNrm() { return false; }
		virtual bool isSin() { return false; }
		virtual bool isCos() { return false; }
		virtual bool isCrs() { return false; }
		virtual bool isDp3() { return false; }
		virtual bool isDp4() { return false; }
		virtual bool isAbs() { return false; }
		virtual bool isNeg() { return false; }
		virtual bool isSat() { return false; }
		virtual bool isM33() { return false; }
		virtual bool isM44() { return false; }
		virtual bool isM34() { return false; }
		virtual bool isKil() { return false; }
		virtual bool isTex() { return false; }
		virtual bool isSge() { return false; }
		virtual bool isSlt() { return false; }
		virtual bool isUnused() { return false; }
		virtual bool isSeq() { return false; }
		virtual bool isSne() { return false; }
		virtual Opcode* copy() = 0;
	};

	struct DstVOpcode : Opcode {
		Reg dst;
		Reg v;
		DstVOpcode(Reg dst, Reg v) : dst(dst), v(v) { }
	};

	struct DstABOpcode : Opcode {
		Reg dst;
		Reg a;
		Reg b;
		DstABOpcode(Reg dst, Reg a, Reg b) : dst(dst), a(a), b(b) { }
	};
	
	struct OMov : DstVOpcode {
		OMov(Reg dst, Reg v) : DstVOpcode(dst, v) { }
		bool isMov() override { return true; }
		Opcode* copy() override { return new OMov(dst, v); }
	};

	struct OAdd : DstABOpcode {
		OAdd(Reg dst, Reg a, Reg b) : DstABOpcode(dst, a, b) { }
		bool isAdd() override { return true; }
		Opcode* copy() override { return new OAdd(dst, a, b); }
	};

	struct OSub : DstABOpcode {
		OSub(Reg dst, Reg a, Reg b) : DstABOpcode(dst, a, b) { }
		bool isSub() override { return true; }
		Opcode* copy() override { return new OSub(dst, a, b); }
	};

	struct OMul : DstABOpcode {
		OMul(Reg dst, Reg a, Reg b) : DstABOpcode(dst, a, b) { }
		bool isMul() override { return true; }
		Opcode* copy() override { return new OMul(dst, a, b); }
	};

	struct ODiv : DstABOpcode {
		ODiv(Reg dst, Reg a, Reg b) : DstABOpcode(dst, a, b) { }
		bool isDiv() override { return true; }
		Opcode* copy() override { return new ODiv(dst, a, b); }
	};

	struct ORcp : DstVOpcode {
		ORcp(Reg dst, Reg v) : DstVOpcode(dst, v) { }
		bool isRcp() override { return true; }
		Opcode* copy() override { return new ORcp(dst, v); }
	};
	
	struct OMin : DstABOpcode {
		OMin(Reg dst, Reg a, Reg b) : DstABOpcode(dst, a, b) { }
		bool isMin() override { return true; }
		Opcode* copy() override { return new OMin(dst, a, b); }
	};
	
	struct OMax : DstABOpcode {
		OMax(Reg dst, Reg a, Reg b) : DstABOpcode(dst, a, b) { }
		bool isMax() override { return true; }
		Opcode* copy() override { return new OMax(dst, a, b); }
	};
	
	struct OFrc : DstVOpcode {
		OFrc(Reg dst, Reg v) : DstVOpcode(dst, v) { }
		bool isFrc() override { return true; }
		Opcode* copy() override { return new OFrc(dst, v); }
	};
	
	struct OSqt : DstVOpcode {
		OSqt(Reg dst, Reg v) : DstVOpcode(dst, v) { }
		bool isSqt() override { return true; }
		Opcode* copy() override { return new OSqt(dst, v); }
	};
	
	struct ORsq : DstVOpcode {
		ORsq(Reg dst, Reg v) : DstVOpcode(dst, v) { }
		bool isRsq() override { return true; }
		Opcode* copy() override { return new ORsq(dst, v); }
	};
	
	struct OPow : DstABOpcode {
		OPow(Reg dst, Reg a, Reg b) : DstABOpcode(dst, a, b) { }
		bool isPow() override { return true; }
		Opcode* copy() override { return new OPow(dst, a, b); }
	};
	
	struct OLog : DstVOpcode {
		OLog(Reg dst, Reg v) : DstVOpcode(dst, v) { }
		bool isLog() override { return true; }
		Opcode* copy() override { return new OLog(dst, v); }
	};
	
	struct OExp : DstVOpcode {
		OExp(Reg dst, Reg v) : DstVOpcode(dst, v) { }
		bool isExp() override { return true; }
		Opcode* copy() override { return new OExp(dst, v); }
	};
	
	struct ONrm : DstVOpcode {
		ONrm(Reg dst, Reg v) : DstVOpcode(dst, v) { }
		bool isNrm() override { return true; }
		Opcode* copy() override { return new ONrm(dst, v); }
	};
	
	struct OSin : DstVOpcode {
		OSin(Reg dst, Reg v) : DstVOpcode(dst, v) { }
		bool isSin() override { return true; }
		Opcode* copy() override { return new OSin(dst, v); }
	};
	
	struct OCos : DstVOpcode {
		OCos(Reg dst, Reg v) : DstVOpcode(dst, v) { }
		bool isCos() override { return true; }
		Opcode* copy() override { return new OCos(dst, v); }
	};
	
	struct OCrs : DstABOpcode {
		OCrs(Reg dst, Reg a, Reg b) : DstABOpcode(dst, a, b) { }
		bool isCrs() override { return true; }
		Opcode* copy() override { return new OCrs(dst, a, b); }
	};
	
	struct ODp3 : DstABOpcode {
		ODp3(Reg dst, Reg a, Reg b) : DstABOpcode(dst, a, b) { }
		bool isDp3() override { return true; }
		Opcode* copy() override { return new ODp3(dst, a, b); }
	};
	
	struct ODp4 : DstABOpcode {
		ODp4(Reg dst, Reg a, Reg b) : DstABOpcode(dst, a, b) { }
		bool isDp4() override { return true; }
		Opcode* copy() override { return new ODp4(dst, a, b); }
	};
	
	struct OAbs : DstVOpcode {
		OAbs(Reg dst, Reg v) : DstVOpcode(dst, v) { }
		bool isAbs() override { return true; }
		Opcode* copy() override { return new OAbs(dst, v); }
	};
	
	struct ONeg : DstVOpcode {
		ONeg(Reg dst, Reg v) : DstVOpcode(dst, v) { }
		bool isNeg() override { return true; }
		Opcode* copy() override { return new ONeg(dst, v); }
	};
	
	struct OSat : DstVOpcode {
		OSat(Reg dst, Reg v) : DstVOpcode(dst, v) { }
		bool isSat() override { return true; }
		Opcode* copy() override { return new OSat(dst, v); }
	};
	
	struct OM33 : DstABOpcode {
		OM33(Reg dst, Reg a, Reg b) : DstABOpcode(dst, a, b) { }
		bool isM33() override { return true; }
		Opcode* copy() override { return new OM33(dst, a, b); }
	};
	
	struct OM44 : DstABOpcode {
		OM44(Reg dst, Reg a, Reg b) : DstABOpcode(dst, a, b) { }
		bool isM44() override { return true; }
		Opcode* copy() override { return new OM44(dst, a, b); }
	};
	
	struct OM34 : DstABOpcode {
		OM34(Reg dst, Reg a, Reg b) : DstABOpcode(dst, a, b) { }
		bool isM34() override { return true; }
		Opcode* copy() override { return new OM34(dst, a, b); }
	};
	
	struct OKil : Opcode {
		OKil(Reg v) : v(v) { }
		Reg v;
		bool isKil() override { return true; }
		Opcode* copy() override { return new OKil(v); }
	};
	
	struct OTex : Opcode {
		OTex(Reg dst, Reg pt, Tex tex) : dst(dst), pt(pt), tex(tex) { }
		Reg dst;
		Reg pt;
		Tex tex;
		bool isTex() override { return true; }
		Opcode* copy() override { return new OTex(dst, pt, tex); }
	};
	
	struct OSge : DstABOpcode {
		OSge(Reg dst, Reg a, Reg b) : DstABOpcode(dst, a, b) { }
		bool isSge() override { return true; }
		Opcode* copy() override { return new OSge(dst, a, b); }
	};
	
	struct OSlt : DstABOpcode {
		OSlt(Reg dst, Reg a, Reg b) : DstABOpcode(dst, a, b) { }
		bool isSlt() override { return true; }
		Opcode* copy() override { return new OSlt(dst, a, b); }
	};
	
	struct OUnused : Opcode {
		bool isUnused() override { return true; }
		Opcode* copy() override { return new OUnused; }
	};

	struct OSeq : DstABOpcode {
		OSeq(Reg dst, Reg a, Reg b) : DstABOpcode(dst, a, b) { }
		bool isSeq() override { return true; }
		Opcode* copy() override { return new OSeq(dst, a, b); }
	};
	
	struct OSne : DstABOpcode {
		OSne(Reg dst, Reg a, Reg b) : DstABOpcode(dst, a, b) { }
		bool isSne() override { return true; }
		Opcode* copy() override { return new OSne(dst, a, b); }
	};

	struct TMipMapDisable : TexFlag { // default
		bool isMipMapDisable() override { return true; }
	};
	
	struct TMipMapNearest : TexFlag {
		bool isMipMapDisable() override { return true; }
	};
	
	struct TMipMapLinear : TexFlag {
		bool isMipMapLinear() override { return true; }
	};
	
	struct TWrap : TexFlag {
		bool isWrap() override { return true; }
	};
	
	struct TClamp : TexFlag { // default
		bool isClamp() override { return true; }
	};
	
	struct TFilterNearest : TexFlag {
		bool isFilterNearest() override { return true; }
	};
	
	struct TFilterLinear : TexFlag { // default
		bool isFilterLinear() override { return true; }
	};
	
	struct TSingle : TexFlag {
		bool isSingle() override { return true; }
	};

	struct TLodBias : TexFlag {
		TLodBias(float v) : v(v) { }
		float v;
		bool isLodBias() override { return true; }
	};

	struct TCube : TexFlag {
		bool isCube() override { return true; }
	};

	enum TexParam {
		PMipMap,
		PWrap,
		PFilter,
		PLodBias,
		PSingle
	};

	enum VarKind {
		VUnknown,

		VConst,
		VVar,
		VInput,
		VOut,
		VTmp,
		VTexture,
		// compile time parameter : a VConst that also changes the shader flow
		VParam
	};

	struct VarType {
		virtual bool isNull() { return false; }
		virtual bool isBool() { return false; }
		virtual bool isFloat() { return false; }
		virtual bool isFloat2() { return false; }
		virtual bool isFloat3() { return false; }
		virtual bool isFloat4() { return false; }
		virtual bool isInt() { return false; }
		virtual bool isMatrix() { return false; }
		virtual bool isTexture() { return false; }
		virtual bool isArray() { return false; }
		virtual bool isObject() { return false; }
	};

	struct TNull : VarType {
		bool isNull() override { return true; }
	};

	struct TBool : VarType {
		bool isBool() override { return true; }
	};

	struct TFloat : VarType {
		bool isFloat() override { return true; }
	};

	struct TFloat2 : VarType {
		bool isFloat2() override { return true; }
	};

	struct TFloat3 : VarType {
		bool isFloat3() override { return true; }
	};

	struct TFloat4 : VarType {
		bool isFloat4() override { return true; }
	};

	struct TInt : VarType {
		bool isInt() override { return true; }
	};

	struct TMatrix : VarType {
		int r;
		int c;
		bool* transpose;
		TMatrix() : r(0), c(0), transpose(nullptr) { }
		bool isMatrix() override { return true; }
	};

	struct TTexture : VarType {
		bool cube;
		bool isTexture() override { return true; }
	};

	struct TArray : VarType {
		TArray(VarType* t, int size) : t(t), size(size) { }
		VarType* t;
		int size;
		bool isArray() override { return true; }
	};

	struct TObject : VarType {
		struct Field {
			std::string name;
			VarType* t;
		};

		std::vector<Field> fields;

		bool isObject() override { return true; }
	};

	typedef int Position;
	
	struct Variable {
		int id;
		std::string name;
		VarType* type;
		VarKind kind;
		int index;
		Position pos;

		//bool operator!=(const Variable& other) { return id != other.id; }
		Variable* copy() {
			Variable* var = new Variable;
			var->id = id;
			var->name = name;
			var->type = type;
			var->kind = kind;
			var->index = index;
			var->pos = pos;
			return var;
		}
	};

	enum CodeOp {
		CAdd,
		CSub,
		CMul,
		CDiv,
		CMin,
		CMax,
		CPow,
		CCross,
		CDot,
		CLt,
		CGt,
		CLte,
		CGte,
		CMod,
		CEq,
		CNeq,
		// Internal usage only
		CAnd,
		COr,
		CInterval
	};

	enum CodeUnop {
		CRcp,
		CSqrt,
		CRsq,
		CLog,
		CExp,
		CLen,
		CSin,
		CCos,
		CAbs,
		CNeg,
		CSat,
		CFrac,
		CInt,
		CNorm,
		CKill,
		CTrans,
		// Internal usage only
		CNot
	};

	// final hxsl

	struct Const {
		virtual bool isNull() { return false; }
		virtual bool isInt() { return false; }
		virtual bool isBool() { return false; }
		virtual bool isFloat() { return false; }
		virtual bool isFloats() { return false; }
		virtual bool isObject() { return false; }
		virtual bool isArray() { return false; }
		virtual bool equals(Const* other) = 0;
	};

	struct ConstNull : Const {
		bool isNull() override { return true; }
		bool equals(Const* other) override { return other->isNull(); }
	};
	
	struct ConstInt : Const {
		ConstInt(int i) : i(i) { }
		int i;
		bool isInt() override { return true; }
		bool equals(Const* other) override { return other->isInt() && dynamic_cast<ConstInt*>(other)->i == i; }
	};

	struct ConstBool : Const {
		ConstBool(bool b) : b(b) { }
		bool b;
		bool isBool() override { return true; }
		bool equals(Const* other) override { return other->isBool() && dynamic_cast<ConstBool*>(other)->b == b; }
	};

	struct ConstFloat : Const {
		float v;
		ConstFloat(float v) : v(v) { }
		bool isFloat() override { return true; }
		bool equals(Const* other) override { return other->isFloat() && dynamic_cast<ConstFloat*>(other)->v == v; }
	};

	struct ConstFloats : Const {
		std::vector<float> v;
		bool isFloats() override { return true; }
		bool equals(Const* other) override {
			if (!other->isFloats()) return false;
			auto floats = dynamic_cast<ConstFloats*>(other);
			if (v.size() != floats->v.size()) return false;
			for (unsigned i = 0; i < v.size(); ++i) {
				if (v[i] != floats->v[i]) return false;
			}
			return true;
		}
	};

	struct ConstObject : Const {
		std::map<std::string, Const*> fields;
		bool isObject() override { return true; }
		bool equals(Const* other) override {
			if (!other->isObject()) return false;
			auto obj = dynamic_cast<ConstObject*>(other);
			if (fields.size() != obj->fields.size()) return false;
			for (auto field : fields) {
				if (obj->fields.find(field.first) == obj->fields.end()) return false;
				if (!obj->fields[field.first]->equals(field.second)) return false;
			}
			return true;
		}
	};

	struct ConstArray : Const {
		std::vector<Const*> v;
		bool isArray() override { return true; }
		bool equals(Const* other) override {
			if (!other->isArray()) return false;
			auto arr = dynamic_cast<ConstArray*>(other);
			if (v.size() != arr->v.size()) return false;
			for (unsigned i = 0; i < v.size(); ++i) {
				if (!v[i]->equals(arr->v[i])) return false;
			}
			return true;
		}
	};

	struct CodeValueDecl {
		virtual bool isVar() { return false; }
		virtual bool isOp() { return false; }
		virtual bool isUnop() { return false; }
		virtual bool isAccess() { return false; }
		virtual bool isTex() { return false; }
		virtual bool isSwiz() { return false; }
		virtual bool isSubBlock() { return false; }
		virtual bool isConst() { return false; }
		virtual bool isIf() { return false; }
		virtual bool isCond() { return false; }
		virtual bool isFor() { return false; }
		virtual bool isVector() { return false; }
		virtual bool isRow() { return false; }
		virtual bool isField() { return false; }
	};

	struct CodeValue {
		CodeValueDecl* d;
		VarType* t;
		Position p;
	};

	struct Block {
		CodeValue* v;
		CodeValue e;
	};

	typedef std::vector<Block> CodeBlock;

	struct CVar : CodeValueDecl {
		CVar(Variable* v, std::vector<Comp> swiz) : v(v), swiz(swiz) { }
		Variable* v;
		std::vector<Comp> swiz;
		bool isVar() override { return true; }
	};

	struct COp : CodeValueDecl {
		COp(CodeOp op, CodeValue e1, CodeValue e2) : op(op), e1(e1), e2(e2) { }
		CodeOp op;
		CodeValue e1;
		CodeValue e2;
		bool isOp() override { return true; }
	};

	struct CUnop : CodeValueDecl {
		CUnop(CodeUnop op, CodeValue e) : op(op), e(e) { }
		CodeUnop op;
		CodeValue e;
		bool isUnop() override { return true; }
	};

	struct CAccess : CodeValueDecl {
		Variable* v;
		CodeValue idx;
		bool isAccess() override { return true; }
	};

	struct CodeTexFlag {
		virtual bool isFlag() { return false; }
		virtual bool isParam() { return false; }
	};

	struct TexMode {
		CodeTexFlag* f;
		Position p;
	};

	struct CTex : CodeValueDecl {
		CTex(Variable* v, CodeValue acc, std::vector<TexMode> mode) : v(v), acc(acc), mode(mode) { }
		Variable* v;
		CodeValue acc;
		std::vector<TexMode> mode;
		bool isTex() override { return true; }
	};

	struct CSwiz : CodeValueDecl {
		CodeValue e;
		std::vector<Comp> swiz;
		bool isSwiz() override { return true; }
	};

	struct CSubBlock : CodeValueDecl {
		CodeBlock tmpExpr;
		CodeValue v;
		bool isSubBlock() override { return true; }
	};

	// used in intermediate representation only
	
	struct CConst : CodeValueDecl {
		CConst(Const* c) : c(c) { }
		Const* c;
		bool isConst() override { return true; }
	};

	struct CIf : CodeValueDecl {
		CodeValue cond;
		CodeBlock eif;
		CodeBlock* eelse;
		bool isIf() override { return true; }
	};

	struct CCond : CodeValueDecl {
		CodeValue cond;
		CodeValue eif;
		CodeValue eelse;
		bool isCond() override { return true; }
	};

	struct CFor : CodeValueDecl {
		Variable* v;
		CodeValue it;
		CodeBlock exprs;
		bool isFor() override { return true; }
	};

	struct CVector : CodeValueDecl {
		std::vector<CodeValue> vals;
		bool isVector() override { return true; }
	};

	struct CRow : CodeValueDecl {
		CodeValue e1;
		CodeValue e2;
		bool isRow() override { return true; }
	};

	struct CField : CodeValueDecl {
		CodeValue e;
		std::string f;
		bool isField() override { return true; }
	};
	
	struct CTFlag : CodeTexFlag {
		CTFlag(TexFlag* t) : t(t) { }
		TexFlag* t;
		bool isFlag() override { return true; }
	};

	struct CTParam : CodeTexFlag {
		TexParam t;
		CodeValue value;
		bool isParam() override { return true; }
	};
	
	struct Code {
		bool vertex;
		Position pos;
		std::vector<Variable*> args;
		std::vector<std::vector<float>> consts;
		CodeBlock exprs;
		int tempSize;
	};

	struct Data {
		Code shader;
		std::vector<Variable*> globals;
	};

	// parsed hxsl

	struct ParsedVar {
		std::string n;
		VarType* t;
		VarKind k;
		Position p;
	};

	struct ParsedValueDecl {
		virtual bool isVar() { return false; }
		virtual bool isConst() { return false; }
		virtual bool isLocal() { return false; }
		virtual bool isOp() { return false; }
		virtual bool isUnop() { return false; }
		virtual bool isTex() { return false; }
		virtual bool isIf() { return false; }
		virtual bool isFor() { return false; }
		virtual bool isCond() { return false; }
		virtual bool isVector() { return false; }
		virtual bool isRow() { return false; }
		virtual bool isBlock() { return false; }
		virtual bool isReturn() { return false; }
		virtual bool isCall() { return false; }
		virtual bool isField() { return false; }
	};
	
	struct ParsedValue {
		ParsedValueDecl* v;
		Position p;
	};

	struct ParsedExpr {
		ParsedValue* v;
		ParsedValue* e;
		Position p;
	};

	struct PVar : ParsedValueDecl {
		std::string v;
		bool isVar() override { return true; }
	};

	struct PConst : ParsedValueDecl {
		Const* c;
		bool isConst() override { return true; }
	};
	
	struct PLocal : ParsedValueDecl {
		ParsedVar v;
		bool isLocal() override { return true; }
	};

	struct POp : ParsedValueDecl {
		CodeOp op;
		ParsedValue e1;
		ParsedValue e2;

		POp(CodeOp op, ParsedValue e1, ParsedValue e2) : op(op), e1(e1), e2(e2) { }
		bool isOp() override { return true; }
	};

	struct PUnop : ParsedValueDecl {
		CodeUnop op;
		ParsedValue e;

		PUnop(CodeUnop op, ParsedValue e) : op(op), e(e) { }
		bool isUnop() override { return true; }
	};

	struct PTex : ParsedValueDecl {
		std::string v;
		ParsedValue acc;
		//mode : Array<{ f : ParsedTexFlag, p : Position }> );
		bool isTex() override { return true; }
	};

	struct PIf : ParsedValueDecl {
		ParsedValue* cond;
		ParsedValue* eif;
		ParsedValue* eelse;
		bool isIf() override { return true; }
	};

	struct PFor : ParsedValueDecl {
		std::string v;
		ParsedValue iter;
		ParsedValue expr;
		bool isFor() override { return true; }
	};

	struct PCond : ParsedValueDecl {
		ParsedValue cond;
		ParsedValue eif;
		ParsedValue eelse;
		bool isCond() override { return true; }
	};

	struct PVector : ParsedValueDecl {
		std::vector<ParsedValue> el;
		bool isVector() override { return true; }
	};

	struct PRow : ParsedValueDecl {
		ParsedValue e;
		ParsedValue index;
		bool isRow() override { return true; }
	};

	struct PBlock : ParsedValueDecl {
		std::vector<ParsedExpr> el;
		bool isBlock() override { return true; }
	};

	struct PReturn : ParsedValueDecl {
		ParsedValue e;
		bool isReturn() override { return true; }
	};
		
	struct PCall : ParsedValueDecl {
		std::string n;
		std::vector<ParsedValue> vl;
		bool isCall() override { return true; }
	};

	struct PField : ParsedValueDecl {
		ParsedValue e;
		std::string field;
		bool isField() override { return true; }
	};
		
	enum ParsedTexFlag {
		PTFlag, //( t : TexFlag );
		PTParam //( t : TexParam, value : ParsedValue );
	};

	struct ParsedCode {
		Position pos;
		std::vector<ParsedVar> args;
		std::vector<ParsedExpr> exprs;
	};

	struct ParsedHxsl {
		Position pos;
		std::vector<ParsedVar> globals;
		ParsedCode shader;
		std::map<std::string, ParsedCode> helpers;
	};

	namespace Tools {
		/*public static function swizBits( s : Array<Comp>, t : VarType ) {
			if( s == null ) return fullBits(t);
			var b = 0;
			for( x in s )
				b |= 1 << Type.enumIndex(x);
			return b;
		}*/

		/*public static function fullBits( t : VarType ) {
			return (1 << Tools.floatSize(t)) - 1;
		}*/

		/*public static function isFloat( t : VarType ) {
			return switch( t ) {
			case TFloat, TFloat2, TFloat3, TFloat4, TInt: true;
			default: false;
			};
		}*/

		/*public static function typeStr( t : VarType )  {
			switch( t ) {
			case TMatrix(r, c, t):
				return "M" + r + "" + c + (t.t ? "T" : "");
			case TTexture(cube):
				return cube ? "CubeTexture" : "Texture";
			case TArray(t, size):
				return size == 0 ? "Array<" + typeStr(t)+">" : typeStr(t) + "<" + size + ">";
			case TObject(fields):
				return "{" + [for( f in fields ) f.name + " : " + typeStr(f.t)].join(", ") + "}";
			default:
				return Std.string(t).substr(1);
			}
		}*/
	
		int regSize(VarType* t);
		int floatSize(VarType* t);
		VarType* makeFloat(int i);
	
		/*public static function getAllVars( hx : Data ) {
			return hx.globals.concat(hx.vertex.args).concat(hx.fragment.args);
		}*/

		int getMaxTextures();

		struct RegProp {
			RegProp(bool read, bool write, int count) : read(read), write(write), count(count) { }
			bool read;
			bool write;
			int count;
		};

		RegProp getProps(RegType r, bool fragment);

		/*bytes ofString(std::string str) {
			var b : haxe.io.Bytes = haxe.Unserializer.run(str);
			#if flash9
			// force endianness
			b.getData().endian = flash.utils.Endian.LITTLE_ENDIAN;
			#end
			return b;
		}*/

		/*public static function regStr( r : Data.Reg ) {
			var str = Std.string(r.t).charAt(1).toLowerCase() + r.index;
			if( str == "o0" ) str = "out";
			var acc = r.access;
			if( acc != null )
				str = regStr( { t : acc.t, index : acc.offset, access : null, swiz : null } ) + "[" + str + "." + Std.string(acc.comp).toLowerCase() + "]";
			if( r.swiz != null )
				str += "." + r.swiz.join("").toLowerCase();
			return str;
		}*/

		/*public static function opStr( op : Data.Opcode ) {
			var pl = Type.enumParameters(op);
			var str = Type.enumConstructor(op).substr(1).toLowerCase() + " " + regStr(pl[0]);
			switch( op ) {
			case OKil(_): return str;
			case OTex(_, pt, tex): return str + ", tex" + tex.index + "[" + regStr(pl[1]) + "]" + (tex.flags.length == 0  ? "" : " <" + tex.flags.join(",") + ">");
			default:
			}
			str += ", " + regStr(pl[1]);
			if( pl[2] != null ) str += ", " + regStr(pl[2]);
			return str;
		}*/
	}
}
