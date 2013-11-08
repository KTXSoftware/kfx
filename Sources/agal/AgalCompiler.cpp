#include "AgalCompiler.h"
#include <stdexcept>

using namespace agal;

void AgalCompiler::error(std::string msg, Position p) {
	throw std::runtime_error(msg.c_str()); //, p);
}

AgalData AgalCompiler::compile(Code c) {
	code.clear();
	tempCount = c.tempSize;
	vertex = c.vertex;
	for (auto e : c.exprs)
		compileExpr(new CodeValue(e.e), e.v);

	auto old = code;
	if (!assignRegisters(false, c.vertex)) {
		code = old;
		if (!assignRegisters(true, c.vertex))
			error("This shader uses too many temporary variables for his calculus", c.pos);
	}

	// DEBUG
	/*#if (debug && shaderCompDebug)
	for( i in 0...temps.length ) {
		if ( temps[i] == null ) continue;
		var bits = temps[i].liveBits;
		var lifes = [];
		var p = 0;
		while( true ) {
			while( p < bits.length && bits[p] == null )
				p++;
			if( p >= bits.length ) break;
			var k = bits[p];
			var start = p;
			while( bits[p] == k )
				p++;
			if( start == p - 1 )
				lifes.push(start +" : " + k);
			else
				lifes.push(start + "-"+ (p - 1)+" : "+k);
		}
		trace("T" + i + " " + Std.string(lifes));
	}
	for( i in 0...code.length ) {
		var a = format.agal.Tools.opStr(old[i]);
		var b = format.agal.Tools.opStr(code[i]);
		trace("@"+i+"   "+StringTools.rpad(a," ",30) + (a == b ? "" : b));
	}
	#end*/

	// remove no-ops
	std::vector<Opcode*> newcode;
	int i = 0;
	for (auto op : code) {
		if (op->isMov()) {
			auto mov = dynamic_cast<OMov*>(op);
			if (mov->dst.index == mov->v.index && mov->dst.t == mov->v.t && swizBits(mov->dst.swiz) == swizBits(mov->v.swiz) && mov->v.access == nullptr) {
				continue;
			}
		}
		newcode.push_back(op);
		// TODO : group dp4/dp3 into m44/m34/m44 ?
	}
	code = newcode;

	AgalData data;
	data.fragmentShader = !c.vertex;
	data.code = code;
	return data;
}

namespace {
	int enumIndex(Comp c) {
		return (int)c;
	}
}

int AgalCompiler::swizBits(Swizzle s) {
	if (s.size() == 0)
		return 15;
	int b = 0;
	for (auto ss : s)
		b |= 1 << enumIndex(ss);
	return b;
}

void AgalCompiler::compileExpr(CodeValue* e, CodeValue* v) {
	if (v == nullptr) {
		// assume dest not check
		Reg reg;
		reg.t = ROut;
		reg.index = -1;
		reg.access = nullptr;
		compileTo(reg, *e);
		return;
	}
	Reg d;
	if (v->d->isVar()) {
		//case CVar(v, swiz):
		auto var = dynamic_cast<CVar*>(v->d);
		d = reg(var->v, var->swiz);
	}
	else error("assert", -1);
	// fragment shader does not allow direct operations to output
	if (!vertex && d.t == ROut) {
		if (e->d->isOp() || e->d->isTex() || e->d->isUnop()) {
			auto t = allocTemp(v->t);
			compileTo(t, *e);
			mov(d, t, v->t);
			return;
		}
		//else if (e->d->isVar() || e->d->isSwiz() || e->d->isAccess() || e->d->isSubBlock());
		else if (e->d->isConst() || e->d->isVector() || e->d->isIf() || e->d->isFor() || e->d->isRow() || e->d->isCond() || e->d->isField()) error("assert", -1);
	}
	compileTo(d, *e);
}

std::vector<Comp> AgalCompiler::initSwiz(VarType* t) {
	std::vector<Comp> ret;
	if (t->isFloat()) {
		ret.push_back(X);
	}
	else if (t->isFloat2()) {
		ret.push_back(X);
		ret.push_back(Y);
	}
	else if (t->isFloat3()) {
		ret.push_back(X);
		ret.push_back(Y);
		ret.push_back(Z);
	}
	return ret;
}

std::vector<Comp> AgalCompiler::convertSwiz(std::vector<Comp> swiz) {
	if (swiz.size() < 1) return std::vector<Comp>();
	std::vector<Comp> sz;
	for (auto s : swiz)
		switch (s) {
		case X:
			sz.push_back(X);
			break;
		case Y:
			sz.push_back(Y);
			break;
		case Z:
			sz.push_back(Z);
			break;
		case W:
			sz.push_back(W);
			break;
		}
	return sz;
}

Reg AgalCompiler::reg(Variable* v, std::vector<Comp> swiz_) {
	auto swiz = (swiz_.size() < 1) ? initSwiz(v->type) : convertSwiz(swiz_);
	RegType t;
	switch (v->kind) {
	case VConst:
		t = RConst;
		break;
	case VOut:
		t = ROut;
		break;
	case VTmp:
		t = RTemp;
		break;
	case VVar:
		t = RVar;
		break;
	case VInput:
		t = RAttr;
		break;
	case VTexture:
	case VParam:
		error("assert", -1);
	}
	Reg reg;
	reg.t = t;
	reg.index = v->index;
	reg.swiz = swiz;
	reg.access = nullptr;
	return reg;
}

bool AgalCompiler::assignRegisters(bool pack, bool vertex) {
	auto maxRegs = Tools::getProps(RTemp, !vertex).count;
	code = uniqueReg();
	temps.clear();
	regs.clear();
	regs.resize(maxRegs);
	for (int i = 0; i < maxRegs; ++i)
		regs[i].clear();
	startRegister = -1;
	packRegisters = pack;
	compileLiveness(&AgalCompiler::regLive);
	tempMax = maxRegs;
	startRegister = 0;
	compileLiveness(&AgalCompiler::regAssign);
	return tempMax <= maxRegs;
}

namespace {
	Reg cp(Reg r) {
		Reg reg;
		reg.t = r.t;
		reg.index = r.index;
		reg.swiz = r.swiz;
		reg.access = nullptr;
		if (r.access != nullptr) {
			reg.access = new RegAccess;
			reg.access->t = r.access->t;
			reg.access->offset = r.access->offset;
			reg.access->comp = r.access->comp;
		}
		return reg;
	}
}

std::vector<Opcode*> AgalCompiler::uniqueReg() {
	std::vector<Opcode*> c;
	for (unsigned i = 0; i < code.size(); ++i) {
		if (code[i]->isUnused()) {
			c.push_back(new OUnused);
		}
		else if (code[i]->isKil()) {
			//case OKil(r):
			auto kil = dynamic_cast<OKil*>(code[i]);
			c.push_back(new OKil(cp(kil->v)));
		}
		else if (code[i]->isTex()) {
			//case OTex(d, v, fl):
			auto tex = dynamic_cast<OTex*>(code[i]);
			c.push_back(new OTex(cp(tex->dst), cp(tex->pt), tex->tex));
		}
		else if (code[i]->isMov() || code[i]->isRcp() || code[i]->isFrc() || code[i]->isSqt() || code[i]->isRsq() || code[i]->isLog() || code[i]->isExp() || code[i]->isNrm()
			|| code[i]->isSin() || code[i]->isCos() || code[i]->isAbs() || code[i]->isNeg() || code[i]->isSat()) {
			//case OMov(d, v), ORcp(d, v), OFrc(d, v), OSqt(d, v), ORsq(d, v), OLog(d, v), OExp(d, v), ONrm(d, v), OSin(d, v), OCos(d, v), OAbs(d, v), ONeg(d, v), OSat(d, v):
			DstVOpcode* vop = dynamic_cast<DstVOpcode*>(code[i]->copy());
			vop->dst = cp(vop->dst);
			vop->v = cp(vop->v);
			c.push_back(vop); //Type.createEnum(Opcode, Type.enumConstructor(code[i]), [cp(d), cp(v)]));
		}
		else if (code[i]->isAdd() || code[i]->isSub() || code[i]->isMul() || code[i]->isDiv() || code[i]->isMin() || code[i]->isMax() || code[i]->isPow() || code[i]->isCrs() || code[i]->isDp3()
			|| code[i]->isSge() || code[i]->isSlt() || code[i]->isSne() || code[i]->isSeq() || code[i]->isDp4() || code[i]->isM33() || code[i]->isM44() || code[i]->isM34()) {
			//case OAdd(d, a, b), OSub(d, a, b), OMul(d, a, b), ODiv(d, a, b), OMin(d, a, b), OMax(d, a, b), OPow(d, a, b), OCrs(d, a, b), ODp3(d, a, b), OSge(d, a, b), OSlt(d, a, b), OSne(d,a,b), OSeq(d,a,b), ODp4(d,a,b), OM33(d, a, b),  OM44(d, a, b), OM34(d,a,b):
			DstABOpcode* abop = dynamic_cast<DstABOpcode*>(code[i]->copy());
			abop->dst = cp(abop->dst);
			abop->a = cp(abop->a);
			abop->b = cp(abop->b);
			c.push_back(abop); //Type.createEnum(Opcode, Type.enumConstructor(code[i]), [cp(d), cp(a), cp(b)]));
		}
	}
	return c;
}

void AgalCompiler::compileLiveness(void (AgalCompiler::*reg)(Reg, bool)) {
	for (unsigned i = 0; i < code.size(); ++i) {
		codePos = i;
		if (code[i]->isUnused()) {

		}
		else if (code[i]->isKil()) {
			auto kill = dynamic_cast<OKil*>(code[i]);
			(this->*reg)(kill->v, false);
		}
		else if (code[i]->isMov()) {
			//case OMov(d, v):
			auto move = dynamic_cast<OMov*>(code[i]);
			(this->*reg)(move->v, false);
			// small optimization in order to trigger no-op moves
			if (move->v.t == RTemp && move->dst.t == RTemp ) startRegister = move->v.index;
			(this->*reg)(move->dst, true);
		}
		else if (code[i]->isTex()) {
			auto tex = dynamic_cast<OTex*>(code[i]);
			(this->*reg)(tex->pt, false);
			(this->*reg)(tex->dst, true);
		}
		else if (code[i]->isRcp() || code[i]->isFrc() || code[i]->isSqt() || code[i]->isRsq() || code[i]->isLog() || code[i]->isExp() || code[i]->isNrm() || code[i]->isSin() || code[i]->isCos() || code[i]->isAbs() || code[i]->isNeg() || code[i]->isSat()) {
			//case OTex(d, v, _), ORcp(d, v), OFrc(d,v),OSqt(d,v), ORsq(d,v), OLog(d,v),OExp(d,v), ONrm(d,v), OSin(d,v), OCos(d,v), OAbs(d,v), ONeg(d,v), OSat(d,v):
			auto op = dynamic_cast<DstVOpcode*>(code[i]);
			(this->*reg)(op->v, false);
			(this->*reg)(op->dst, true);
		}
		else if (code[i]->isAdd() || code[i]->isSub() || code[i]->isMul() || code[i]->isDiv() || code[i]->isMin() || code[i]->isMax() ||
			code[i]->isPow() || code[i]->isCrs() || code[i]->isDp3() || code[i]->isSge() || code[i]->isSlt() || code[i]->isSne() || code[i]->isSeq() || code[i]->isDp4()) {
			//case OAdd(d, a, b), OSub(d, a, b), OMul(d, a, b), ODiv(d, a, b), OMin(d, a, b), OMax(d, a, b),
			//OPow(d, a, b), OCrs(d, a, b), ODp3(d, a, b), OSge(d, a, b), OSlt(d, a, b), OSne(d,a,b), OSeq(d,a,b), ODp4(d,a,b):
			auto op = dynamic_cast<DstABOpcode*>(code[i]);
			(this->*reg)(op->a, false);
			(this->*reg)(op->b, false);
			(this->*reg)(op->dst, true);
		}
		else if (code[i]->isM33() || code[i]->isM44() || code[i]->isM34()) {
			//case OM33(d, a, b),  OM44(d, a, b), OM34(d,a,b):
			auto op = dynamic_cast<DstABOpcode*>(code[i]);
			if (op->a.t == RTemp || op->b.t == RTemp ) error("assert", -1);
			(this->*reg)(op->dst, true);
		}
	}
}

void AgalCompiler::compileTo(Reg dst, CodeValue e) {
	if (e.d->isVar() || e.d->isSwiz() || e.d->isAccess()) {
		auto r = compileSrc(e);
		mov(dst, r, e.t);
	}
	else if (e.d->isOp()) {
		//case COp(op, e1, e2):
		auto cop = dynamic_cast<COp*>(e.d);
		if (dst.t == RVar)
			switch (cop->op) {
			// these operations cannot directly write to a var
			case CCross:
			case CPow: {
				auto t = allocTemp(e.t);
				compileTo(t, e);
				mov(dst, t, e.t);
				return;
			}
			}
		// some specific handling
		switch (cop->op) {
		case CLte: {
			auto tmp = cop->e2;
			cop->e2 = cop->e1;
			cop->e1 = tmp;
			cop->op = CGte;
		}
		case CGt: {
			auto tmp = cop->e2;
			cop->e2 = cop->e1;
			cop->e1 = tmp;
			cop->op = CLt;
		}
		}
		// -
		auto v1 = compileSrc(cop->e1);
		auto v2 = compileSrc(cop->e2);
		// it is not allowed to apply an operation on two constants or two vars at the same time : use a temp var
		if ((v1.t == RConst && v2.t == RConst) || (v1.t == RVar && v2.t == RVar)) {
			auto t = allocTemp(cop->e1.t);
			mov(t, v1, cop->e1.t);
			v1 = t;
		}
		switch (cop->op) {
		case CAdd:
			code.push_back(new OAdd(dst, v1, v2));
			break;
		case CDiv:
			code.push_back(new ODiv(dst, v1, v2));
			break;
		case CMin:
			code.push_back(new OMin(dst, v1, v2));
			break;
		case CMax:
			code.push_back(new OMax(dst, v1, v2));
			break;
		case CDot:
			if (cop->e1.t->isFloat4()) code.push_back(new ODp4(dst, v1, v2)); else code.push_back(new ODp3(dst, v1, v2));
		case CCross:
			code.push_back(new OCrs(dst, v1, v2));
			break;
		case CMul:
			if (cop->e2.t->isMatrix()) {
				if (cop->e1.t->isFloat4()) {
					if (v1.t == RTemp || v2.t == RTemp)
						code.push_back(matrixOp4(e.t->isFloat4() ? 4 : 3, dst, v1, v2));
					else if (e.t->isFloat4())
						code.push_back(new OM44(dst, v1, v2));
					else
						code.push_back(new OM34(dst, v1, v2));
				}
				else if (cop->e1.t->isFloat3()) {
					if (v1.t == RTemp || v2.t == RTemp) {
						if (e.t->isFloat4()) code.push_back(matrixOp4(3, dst, v1, v2));
						else code.push_back(matrixOp3(3, dst, v1, v2));
					}
					else if (e.t->isFloat4())
						code.push_back(new OM34(dst, v1, v2));
					else
						code.push_back(new OM33(dst, v1, v2));
				}
				else if (cop->e1.t->isMatrix()) {
					//case TMatrix(w, h, _):
					auto matrix = dynamic_cast<TMatrix*>(cop->e1.t);
					if (matrix->c == 4 && matrix->r == 4)
						code.push_back(matrix44multiply(e.t, dst, v1, v2));
					else if (matrix->c == 3 && matrix->r == 3)
						code.push_back(matrix33multiply(e.t, dst, v1, v2));
					else
						error("assert", -1);
				}
				else {
					error("assert", -1);
				}
			}
			else {
				code.push_back(new OMul(dst, v1, v2));
			}
			break;
		case CSub:
			code.push_back(new OSub(dst, v1, v2));
			break;
		case CPow:
			code.push_back(new OPow(dst, v1, v2));
			break;
		case CGte:
			code.push_back(new OSge(dst, v1, v2));
			break;
		case CEq:
			code.push_back(new OSeq(dst, v1, v2));
			break;
		case CNeq:
			code.push_back(new OSne(dst, v1, v2));
			break;
		case CLt:
			code.push_back(new OSlt(dst, v1, v2));
			break;
		case CMod:
			code.push_back(modGenerate(dst, v1, v2));
			break;
		case COr:
		case CAnd:
		case CLte:
		case CGt:
		case CInterval:
			error("assert", -1);
		}
	}
	else if (e.d->isUnop()) {
		//case CUnop(op, p):
		auto unop = dynamic_cast<CUnop*>(e.d);
		auto v = compileSrc(unop->e);
		switch (unop->op) {
		case CNorm:
			// normalize into a varying require temp var
			if (dst.t == RVar) {
				auto t = allocTemp(unop->e.t);
				code.push_back(new ONrm(t, v));
				mov(dst, t, unop->e.t);
				return;
			}
			break;
		case CLen: {
			// compile length(x) as sqrt(x.dot(x))
			auto t = allocTemp(unop->e.t);
			std::vector<Comp> comp;
			comp.push_back(X);
			auto tx = delta(t, 0, comp);
			mov(t, v, unop->e.t);
			code.push_back(unop->e.t->isFloat4() ? (Opcode*)new ODp4(tx, t, t) : (Opcode*)new ODp3(tx, t, t));
			code.push_back(new OSqt(dst, tx));
			return;
		}
		default:
			// if our parameter is a const, we need to copy to a temp var
			if (v.t == RConst) {
				auto t = allocTemp(unop->e.t);
				mov(t, v, unop->e.t);
				v = t;
			}
		}
		switch (unop->op) {
		case CRcp: code.push_back(new ORcp(dst, v)); break;
		case CSqrt: code.push_back(new OSqt(dst, v)); break;
		case CRsq: code.push_back(new ORsq(dst, v)); break;
		case CLog: code.push_back(new OLog(dst, v)); break;
		case CExp: code.push_back(new OExp(dst, v)); break;
		case CLen: error("assert", -1); break;
		case CNot: error("assert", -1); break;
		case CSin: code.push_back(new OSin(dst, v)); break;
		case CCos: code.push_back(new OCos(dst, v)); break;
		case CAbs: code.push_back(new OAbs(dst, v)); break;
		case CNeg: code.push_back(new ONeg(dst, v)); break;
		case CSat: code.push_back(new OSat(dst, v)); break;
		case CFrac: code.push_back(new OFrc(dst, v)); break;
		case CNorm: code.push_back(new ONrm(dst, v)); break;
		case CKill: code.push_back(new OKil(v)); break;
		case CInt:
		case CTrans: error("assert", -1); break;
		}
	}
	else if (e.d->isTex()) {
		//case CTex(v, acc, flags):
		auto tex = dynamic_cast<CTex*>(e.d);
		auto vtmp = compileSrc(tex->acc);
		// getting texture from a const is not allowed
		if (vtmp.t == RConst) {
			auto t = allocTemp(tex->acc.t);
			mov(t, vtmp, tex->acc.t);
			vtmp = t;
		}
		std::vector<TexFlag*> tflags;
		if (tex->v->type->isTexture()) {
			if (dynamic_cast<TTexture*>(tex->v->type)->cube) tflags.push_back(new TCube);
		}
		for (auto f : tex->mode) {
			if (f.f->isFlag()) {
				//case CTFlag(f):
				auto flag = dynamic_cast<CTFlag*>(f.f);
				if (flag->t->isSingle()) continue;
				if (flag->t->isMipMapDisable()) tflags.push_back(new TMipMapDisable);
				else if (flag->t->isMipMapNearest()) tflags.push_back(new TMipMapNearest);
				else if (flag->t->isMipMapLinear()) tflags.push_back(new TMipMapLinear);
				else if (flag->t->isMipMapDisable()) tflags.push_back(new TMipMapDisable);
				else if (flag->t->isMipMapNearest()) tflags.push_back(new TMipMapNearest);
				else if (flag->t->isMipMapLinear()) tflags.push_back(new TMipMapLinear);
				else if (flag->t->isWrap()) tflags.push_back(new TWrap);
				else if (flag->t->isClamp()) tflags.push_back(new TClamp);
				else if (flag->t->isFilterNearest()) tflags.push_back(new TFilterNearest);
				else if (flag->t->isFilterLinear()) tflags.push_back(new TFilterLinear);
				else if (flag->t->isLodBias()) tflags.push_back(new TLodBias(dynamic_cast<TLodBias*>(flag->t)->v));
				else if (flag->t->isSingle()) tflags.push_back(nullptr);
				
			}
			else if (f.f->isParam()) {
				error("assert", -1);
			}
		}
		Tex t;
		t.index = tex->v->index;
		t.flags = tflags;
		code.push_back(new OTex(dst, vtmp, t));
	}
	else if (e.d->isSubBlock()) {
		//case CSubBlock(el, v):
		auto subblock = dynamic_cast<CSubBlock*>(e.d);
		for (auto e : subblock->tmpExpr)
			compileExpr(&e.e, e.v);
		compileTo(dst, subblock->v);
	}
	else if (e.d->isConst() || e.d->isVector() || e.d->isIf() || e.d->isFor() || e.d->isRow() || e.d->isCond() || e.d->isField()) {
		error("assert", -1);
	}
}

Reg AgalCompiler::compileSrc(CodeValue e) {
	if (e.d->isVar()) {
		auto var = dynamic_cast<CVar*>(e.d);
		return reg(var->v, var->swiz);
	}
	else if (e.d->isSwiz()) {
		auto swiz = dynamic_cast<CSwiz*>(e.d);
		auto v = compileSrc(e);
		Reg reg;
		reg.t = v.t;
		reg.swiz = convertSwiz(swiz->swiz);
		reg.index = v.index;
		reg.access = v.access;
		return reg;
	}
	else if (e.d->isOp() || e.d->isTex() || e.d->isUnop()) {
		auto t = allocTemp(e.t);
		compileTo(t, e);
		return t;
	}
	else if (e.d->isAccess()) {
		auto access = dynamic_cast<CAccess*>(e.d);
		std::vector<Comp> swiz;
		auto r1 = reg(access->v, swiz);
		auto r2 = compileSrc(access->idx);
		Reg reg;
		reg.t = r2.t;
		reg.index = r2.index;
		reg.access = new RegAccess;
		reg.access->t = r1.t;
		reg.access->comp = r2.swiz[0];
		reg.access->offset = r1.index;
		//reg.access = { t : r1.t, comp : r2.swiz[0], offset : r1.index };
		reg.swiz = initSwiz(e.t);
		return reg;
	}
	else if (e.d->isSubBlock()) {
		auto subblock = dynamic_cast<CSubBlock*>(e.d);
		for (auto e : subblock->tmpExpr)
			compileExpr(new CodeValue(e.e), e.v);
		return compileSrc(subblock->v);
	}
	else if (e.d->isConst() || e.d->isVector() || e.d->isIf() || e.d->isFor() || e.d->isRow() || e.d->isCond() || e.d->isField()) {
		error("assert", -1); //+Type.enumConstructor(e.d);
	}
	Reg reg;
	reg.index = -1;
	reg.access = nullptr;
	return reg;
}

Reg AgalCompiler::allocTemp(VarType* t) {
	auto index = tempCount;
	tempCount += Tools::regSize(t);
	Reg reg;
	reg.t = RTemp;
	reg.index = index;
	reg.swiz = initSwiz(t);
	reg.access = nullptr;
	return reg;
}

void AgalCompiler::mov(Reg dst, Reg src, VarType* t) {
	if (t->isFloat()) {
		std::vector<Comp> swiz;
		swiz.push_back(X);
		code.push_back(new OMov(swizOpt(dst, swiz), src));
	}
	else if (t->isFloat2()) {
		std::vector<Comp> swiz;
		swiz.push_back(X);
		swiz.push_back(Y);
		code.push_back(new OMov(swizOpt(dst, swiz), src));
	}
	else if (t->isFloat3()) {
		std::vector<Comp> swiz;
		swiz.push_back(X);
		swiz.push_back(Y);
		swiz.push_back(Z);
		code.push_back(new OMov(swizOpt(dst, swiz), src));
	}
	else {
		for (int i = 0; i < Tools::regSize(t); ++i) {
			std::vector<Comp> swiz;
			code.push_back(new OMov(delta(dst, i, swiz), delta(src, i, swiz)));
		}
	}
}

Reg AgalCompiler::delta(Reg r, int n, std::vector<Comp> s) {
	if (r.access == nullptr) {
		Reg reg;
		reg.t = r.t;
		reg.index = r.index + n;
		reg.swiz = s.size() > 0 ? s : r.swiz;
		reg.access = nullptr;
		return reg;
	}
	else {
		auto acc = r.access;
		Reg reg;
		reg.t = r.t;
		reg.index = r.index;
		reg.swiz = s.size() > 0 ? s : r.swiz;
		reg.access = new RegAccess;
		reg.access->t = acc->t;
		reg.access->comp = acc->comp;
		reg.access->offset = acc->offset + n;
		return reg;
	}
}

Reg AgalCompiler::swizOpt(Reg r, std::vector<Comp> s) {
	if (r.swiz.size() == 0) r.swiz = s;
	return r;
}

Opcode* AgalCompiler::modGenerate(Reg dst, Reg a, Reg b) {
	code.push_back(new ODiv(dst, a, b));
	code.push_back(new OFrc(dst, dst));
	return new OMul(dst, dst, b);
}

void AgalCompiler::regLive(Reg r, bool write) {
	if (r.t != RTemp) return;
	Temp* t = NULL;
	if (temps.find(r.index) != temps.end()) t = temps[r.index];
	if (write) {
		// alloc register
		if (temps.find(r.index) == temps.end()) {
			Temp* temp = new Temp;
			for (int i = 0; i < 4; ++i) temp->lastWritePos.push_back(-1);
			temp->finalRegister = -1;
			temps[r.index] = temp;
		}		
		// set last-write per-component codepos
		if (r.access != nullptr) {
			// if we have an access, our index is good but our swiz is not
			t->lastWritePos[enumIndex(r.access->comp)] = codePos;
			t->assignedTo[enumIndex(r.access->comp)] = nullptr;
		}
		else if (r.swiz.size() == 0) {
			for (int i = 0; i < 4; ++i) {
				t->lastWritePos[i] = codePos;
				t->assignedTo[i] = nullptr;
			}
		}
		else {
			for (auto s : r.swiz) {
				t->lastWritePos[enumIndex(s)] = codePos;
				t->assignedTo[enumIndex(s)] = nullptr;
			}
		}
		// copy-propagation
		if( startRegister >= 0 ) {
			if (code[codePos]->isMov()) {
				//case OMov(d, v):
				auto mov = dynamic_cast<OMov*>(code[codePos]);
				if (mov->v.access == nullptr) {
					// build component swizzle map
					auto& s = mov->dst.swiz;
					if (s.size() == 0) {
						s.push_back(X);
						s.push_back(Y);
						s.push_back(Z);
						s.push_back(W);
					}
					auto& ss = mov->v.swiz;
					if (ss.size() == 0) {
						ss.push_back(X);
						ss.push_back(Y);
						ss.push_back(Z);
						ss.push_back(W);	
					}
						
					for (unsigned i = 0; i < s.size(); ++i) {
						auto si = enumIndex(s[i]);
						t->assignedTo[si] = new int(startRegister);
						t->assignedPos[si] = new int(codePos);
						t->assignedComps[si] = ss[i];
					}
				}
			}
			startRegister = -1;
		}
	}
	else {
		if (t == nullptr) error("assert", -1);
		// if we have an access, our index is good but our swiz is not
		std::vector<Comp> s;
		if (r.access != nullptr) s.push_back(r.access->comp);
		else if (r.swiz.size() == 0) {
			s.push_back(X);
			s.push_back(Y);
			s.push_back(Z);
			s.push_back(W);
		}
		else s = r.swiz;

		// if we need to read some components at some time
		// make sure that we reserve all the components as soon
		// as the first one is written
		int* minPos = nullptr;
		int mask = 0;
		for (auto s2 : s) {
			auto bit = enumIndex(s2);
			auto pos = t->lastWritePos[bit];
			if (minPos == nullptr || pos < *minPos) minPos = new int(pos);
			mask |= 1 << bit;
		}

		// copy-propagation
		int* copy = nullptr;
		for (auto c : s) {
			auto ci = enumIndex(c);
			auto from = t->assignedTo[ci];
			if (from != nullptr && (copy == nullptr || from == copy) ) {
				copy = from;

				auto fromTemp = temps[*from];
				auto cc = t->assignedComps[ci];
				if (fromTemp->lastWritePos[enumIndex(cc)] < *t->assignedPos[ci] ) {
					continue;
				}
			}
			copy = nullptr;
			break;
		}

		if (copy != nullptr) {
			r.index = *t->assignedTo[enumIndex(s[0])];
			r.swiz.clear();
			for (auto c : s)
				r.swiz.push_back(t->assignedComps[enumIndex(c)]);
			regLive(r, write);
			return;
		}
			
		if (minPos < 0) error("assert", -1);
			
		for (int p = *minPos + 1; p < codePos; ++p) {
			auto k = t->liveBits[p];
			if (k == nullptr) k = new int(0);
			t->liveBits[p] = new int(*k | mask);
		}
		t->liveBits[codePos] = 0; // mark that we use it
	}
}

void AgalCompiler::regAssign(Reg r, bool write) {
	if (r.t != RTemp) return;
	auto t = temps[r.index];
	// if we are reading or already live, use our current id
	if (!write || t->liveBits[codePos] > 0) {
		changeReg(r, t);
		return;
	}
	// transform mov to dead registers into no-ops
	if (code[codePos]->isMov()) {
		//case OMov(dst, src):
		auto mov = dynamic_cast<OMov*>(code[codePos]);
		if (mov->dst.t == RTemp && (mov->v.t == RTemp || mov->v.t == RConst) && mov->v.access == nullptr) {
			auto t = temps[mov->dst.index];
			if (t->liveBits[codePos + 1] == nullptr) {
				code[codePos] = new OMov(mov->dst, mov->dst); // no-op, will be removed later
				return;
			}
		}
	}
	// make sure that we reserve all the components we will write
	int mask = 0;
	for (int i = 0; i < 4; ++i)
		if (t->lastWritePos[i] >= codePos)
			mask |= 1 << i;
	auto ncomps = bitCount(mask);
	// allocate a new temp id by looking the other live variable components
	int* found = nullptr;
	int reservedMask = 0;
	int foundUsage = 10;
	for (int td = 0; td < tempMax; ++td) {
		auto rid = (startRegister + td) % tempMax;
		auto reg = regs[rid];

		// check current reserved components
		int rmask = 0;
		int available = 4;
		for (int i = 0; i < 4; ++i) {
			auto t = reg[i];
			if (t == nullptr) continue;
			auto b = t->liveBits[codePos];
			if (b == nullptr || ((*b & (1 << t->invAssignedComps[i])) == 0)) continue;
			rmask |= 1 << i;
			available--;
		}

		// not enough components available
		if (available < ncomps)
			continue;
		// not first X components available
		// this is necessary for write masks
		if (ncomps > 1 && (rmask & ((1 << ncomps) - 1)) != 0)
			continue;
		// if we have found a previous register that is better fit
		if (packRegisters && found != nullptr && foundUsage <= available - ncomps)
			continue;
		found = new int(rid);
		foundUsage = available - ncomps;
		reservedMask = rmask;
		// continue to look for best match
		if( !packRegisters ) break;
	}
	if (found == nullptr) {
		reservedMask = 0;
		found = new int(tempMax++);
		regs.push_back(std::vector<Temp*>());
	}
	auto reg = regs[*found];
	t->finalRegister = *found;
	// list free components
	std::vector<Comp> all;
	all.push_back(X);
	all.push_back(Y);
	all.push_back(Z);
	all.push_back(W);
	std::vector<Comp> comps;
	for (int i = 0; i < 4; ++i)
		if ((reservedMask & (1 << i)) == 0)
			comps.push_back(all[i]);
	// create component map
	t->assignedComps.clear();
	for (int i = 0; i < 4; ++i)
		if ((mask & (1 << i)) != 0) {
			// if one single component, allocate from the end to keep free first registers
			Comp c;
			if (ncomps == 1) {
				c = comps.back();
				comps.pop_back();
			}
			else {
				c = comps.front();
				comps.erase(comps.begin());
			}
			t->assignedComps[i] = c;
			t->invAssignedComps[enumIndex(c)] = i;
			reg[enumIndex(c)] = t;
		}
	changeReg(r, t);
	// next assign will most like use another register
	// even if this one is no longer used
	// this is supposed to favor parallelism
	if (packRegisters) startRegister = 0;
	else startRegister = *found + 1;
}

void AgalCompiler::changeReg(Reg r, Temp* t) {
	r.index = t->finalRegister;
	if (r.access != nullptr)
		r.access->comp = t->assignedComps[enumIndex(r.access->comp)];
	else if (r.swiz.size() != 0) {
		std::vector<Comp> s;
		for (auto c : r.swiz)
			s.push_back(t->assignedComps[enumIndex(c)]);
		r.swiz = s;
	}
}

int AgalCompiler::bitCount(int i) {
	int n = 0;
	while( i > 0 ) {
		n += i & 1;
		i >>= 1;
	}
	return n;
}

// we have to make sure that we don't output MXX macros when one of the sources is a temp var
// or else that might break our temp optimization algorithm because each column might be
// assigned to a different temporary, and since we can't read+write on the same source without
// causing issues
Opcode* AgalCompiler::matrixOp3(int num, Reg dst, Reg a, Reg b) {
	if (dst.index == a.index && dst.t == a.t) {
		auto t = allocTemp(num == 3 ? (VarType*)new TFloat3 : (VarType*)new TFloat4);
		code.push_back(new OMov(t, a));
		a = t;
	}
	if (dst.index == b.index && dst.t == b.t) {
		auto t = allocTemp(num == 3 ? (VarType*)new TFloat3 : (VarType*)new TFloat4);
		code.push_back(new OMov(t, b));
		b = t;
	}
	std::vector<Comp> all;
	all.push_back(X);
	all.push_back(Y);
	all.push_back(Z);
	all.push_back(W);
	for (int i = 0; i < num; ++i) {
		std::vector<Comp> swiz;
		swiz.push_back(all[i]);
		code.push_back(new ODp3(delta(dst, 0, swiz), a, delta(b, i, std::vector<Comp>())));
	}
	auto back = code.back();
	code.pop_back();
	return back;
}

Opcode* AgalCompiler::matrixOp4(int num, Reg dst, Reg a, Reg b) {
	if (dst.index == a.index && dst.t == a.t) {
		auto t = allocTemp(num == 3 ? (VarType*)new TFloat3 : (VarType*)new TFloat4);
		code.push_back(new OMov(t, a));
		a = t;
	}
	if (dst.index == b.index && dst.t == b.t) {
		auto t = allocTemp(num == 3 ? (VarType*)new TFloat3 : (VarType*)new TFloat4);
		code.push_back(new OMov(t, b));
		b = t;
	}
	std::vector<Comp> all;
	all.push_back(X);
	all.push_back(Y);
	all.push_back(Z);
	all.push_back(W);
	for (int i = 0; i < num; ++i) {
		std::vector<Comp> swiz;
		swiz.push_back(all[i]);
		code.push_back(new ODp4(delta(dst, 0, swiz), a, delta(b, i, std::vector<Comp>())));
	}
	auto back = code.back();
	code.pop_back();
	return back;
}

Opcode* AgalCompiler::project(Reg dst, Reg r1, Reg r2) {
	Reg reg1;
	reg1.t = dst.t;
	reg1.index = dst.index;
	reg1.swiz.push_back(X);
	reg1.access = nullptr;
	code.push_back(new ODp4(reg1, r1, r2));
	
	Reg reg2;
	reg2.t = dst.t;
	reg2.index = dst.index;
	reg2.swiz.push_back(Y);
	reg2.access = nullptr;
	code.push_back(new ODp4(reg2, r1, delta(r2, 1, std::vector<Comp>())));

	Reg reg3;
	reg3.t = dst.t;
	reg3.index = dst.index;
	reg3.swiz.push_back(Z);
	reg3.access = nullptr;
	code.push_back(new ODp4(reg3, r1, delta(r2, 2, std::vector<Comp>())));

	Reg reg4;
	reg4.t = dst.t;
	reg4.index = dst.index;
	reg4.swiz.push_back(W);
	reg4.access = nullptr;
	return new ODp4(reg4, r1, delta(r2, 3, std::vector<Comp>()));
}

Opcode* AgalCompiler::project3(Reg dst, Reg r1, Reg r2) {
	Reg reg1;
	reg1.t = dst.t;
	reg1.index = dst.index;
	reg1.swiz.push_back(X);
	reg1.access = nullptr;
	code.push_back(new ODp3(reg1, r1, r2));

	Reg reg2;
	reg2.t = dst.t;
	reg2.index = dst.index;
	reg2.swiz.push_back(Y);
	reg2.access = nullptr;
	code.push_back(new ODp3(reg2, r1, delta(r2, 1, std::vector<Comp>())));

	Reg reg3;
	reg3.t = dst.t;
	reg3.index = dst.index;
	reg3.swiz.push_back(Z);
	reg3.access = nullptr;
	return new ODp3(reg3, r1, delta(r2, 2, std::vector<Comp>()));
}

Opcode* AgalCompiler::matrix44multiply(VarType* rt, Reg dst, Reg r1, Reg r2) {
	if (rt->isMatrix()) {
		//case TMatrix(_,_,t):
		auto m = dynamic_cast<TMatrix*>(rt);
		if (*m->transpose) {
			// result must be transposed, let's inverse operands
			auto tmp = r1;
			r1 = r2;
			r2 = tmp;
		}
	}
	// for some reason, using four OM44 here trigger an error (?)
	code.push_back(project(dst,r1,r2));
	code.push_back(project(delta(dst, 1, std::vector<Comp>()), delta(r1, 1, std::vector<Comp>()), r2));
	code.push_back(project(delta(dst, 2, std::vector<Comp>()), delta(r1, 2, std::vector<Comp>()), r2));
	return project(delta(dst, 3, std::vector<Comp>()), delta(r1, 3, std::vector<Comp>()), r2);
}

Opcode* AgalCompiler::matrix33multiply(VarType* rt, Reg dst, Reg r1, Reg r2) {
	if (rt->isMatrix()) {
		//case TMatrix(_,_,t):
		auto m = dynamic_cast<TMatrix*>(rt);
		if (*m->transpose) {
			// result must be transposed, let's inverse operands
			auto tmp = r1;
			r1 = r2;
			r2 = tmp;
		}
	}
	// for some reason, using three OM33 here trigger an error (?)
	code.push_back(project3(dst,r1,r2));
	code.push_back(project3(delta(dst, 1, std::vector<Comp>()), delta(r1, 1, std::vector<Comp>()), r2));
	return project3(delta(dst, 2, std::vector<Comp>()), delta(r1, 2, std::vector<Comp>()), r2);
}
