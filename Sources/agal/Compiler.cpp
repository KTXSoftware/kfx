#include "Compiler.h"
#include "common/angleutils.h"
#include <stdexcept>
#include <typeinfo>

using namespace agal;

namespace {
	std::string str(int i) {
		char buffer[20];
		snprintf(buffer, sizeof(buffer), "%d", i);
		return buffer;
	}

	int enumIndex(Comp comp) {
		switch (comp) {
		case X:
			return 0;
		case Y:
			return 1;
		case Z:
			return 2;
		case W:
			return 3;
		default:
			return 0;
		}
	}
}

void Compiler::error(std::string msg, Position p) {
	throw std::runtime_error(msg.c_str()); //, p);
}

Data Compiler::compile(ParsedHxsl h) {
	auto out = allocVar("gl_Position", VOut, new TFloat4, h.pos);
	props(out).global = true;
	helpers = h.helpers;
	
	std::vector<Variable*> globals;
	for (auto it = h.globals.begin(); it != h.globals.end(); ++it) {
		auto v = allocVar(it->n, it->k, it->t, it->p);
		if (namedVars.find(v->name) != namedVars.end()) error("Duplicate variable " + v->name, v->pos);
		namedVars[v->name] = v;
		globals.push_back(v);
		props(v).global = true;
	}

	auto shader = compileShader(h.shader, true);

	Data data;
	data.globals = globals;
	data.shader = shader;
	return data;
}

VarProps& Compiler::props(Variable* v) {
	return varProps[v->id];
}

int Compiler::fullBits(VarType* t) {
	return (1 << Tools::floatSize(t)) - 1;
}

Variable* Compiler::allocVar(std::string name, VarKind k, VarType* t, Position p) {
	if (k == VUnknown) {
		if (t->isBool() || (t->isArray() && dynamic_cast<TArray*>(t)->size == 0)) k = VParam;
		else if (t->isTexture()) k = VTexture;
	}
	Variable* v = new Variable;
	v->id = allVars.size();
	v->name = name;
	v->type = t;
	v->kind = k;
	v->index = 0;
	v->pos = p;

	VarProps props;
	props.global = false;
	props.read = false;
	if (k == VUnknown) props.write = fullBits(t);
	else {
		switch (k) {
		case VInput:
		case VConst:
		case VParam:
			props.write = fullBits(t);
			break;
		default:
			props.write = 0;
			break;
		}
	}
	props.inferred = false;
	props.readInShader = nullptr;
	if (varProps.size() <= static_cast<unsigned>(v->id)) varProps.resize(v->id + 1);
	varProps[v->id] = props;

	vars[name] = v;
	allVars.push_back(v);
	return v;
}

Code Compiler::compileShader(ParsedCode c, bool vertex) {
	Code cur;
	cur.vertex = vertex;
	cur.pos = c.pos;
	cur.tempSize = 0;
	for (auto a = c.args.begin(); a != c.args.end(); ++a) {
		Variable* v;
		if (a->t->isTexture()) {
			if (cur.vertex) error("You can't use a texture inside a vertex shader", a->p);
			v = allocVar(a->n, VTexture, a->t, a->p);
			cur.args.push_back(v);
		}
		else {
			v = allocVar(a->n, VUnknown, a->t, a->p);
			// set Const but allow to refine as Param later
			if (v->kind == VUnknown) {
				v->kind = VConst;
				props(v).inferred = true;
			}
			cur.args.push_back(v);
		}
		if (namedVars.find(v->name) != namedVars.end())
			error("Duplicate variable " + v->name, v->pos);
		namedVars[v->name] = v;
	}

	for (auto e = c.exprs.begin(); e != c.exprs.end(); ++e)
		compileAssign(e->v, e->e, e->p);

	checkVars();

	// cleanup
	for (auto v = vars.begin(); v != vars.end(); ++v)
		if (!props(v->second).global)
			vars.erase(v->first);

	return cur;
}

void Compiler::checkVars() {
	auto shader = (cur.vertex ? std::string("vertex") : std::string("fragment")) + " shader";
	for (auto vv = vars.begin(); vv != vars.end(); ++vv) {
		auto v = vv->second;
		auto p = v->pos;
		auto vp = props(v);
		// not used var
		if (v->kind == VUnknown) {
			if (cur.vertex) continue;
			v->kind = VConst;
		}
		switch (v->kind) {
		case VOut:
		//	if (vp.write == 0) error("Output is not written by " + shader, p);
		//	if (vp.write != fullBits(v->type)) error("Some output components are not written by " + shader, p);
			vp.write = 0; // reset status between two shaders
			break;
		case VVar:
			if (cur.vertex) {
				if (vp.write == 0) {
					// delay error
				}
				else if (vp.write != fullBits(v->type))
					error("Some components of variable '" + v->name + "' are not written by vertex shader", p);
			}
			else {
				if (!vp.read && vp.write == 0)
					warn("Variable '" + v->name + "' is not used", p);
				else if (!vp.read)
					warn("Variable '" + v->name + "' is not read by " + shader, p);
				else if (vp.write == 0)
					error("Variable '" + v->name + "' is not written by vertex shader", p);
			}
			break;
		case VInput:
			if (!cur.vertex && !vp.read)
				warn("Input '" + v->name + "' is not used", p);
			break;
		case VTmp:
			if (!vp.read) warn("Unused local variable '" + v->name+"'", p);
			break;
		case VConst:
			if (!vp.read) warn("Constant '" + v->name + "' not used" + (vp.global ? "" :" by " + shader), p);
			break;
		case VParam:
			if (!cur.vertex && !vp.read) warn("Unused parameter '" + v->name + "'", p);
			break;
		case VTexture:
			if (!cur.vertex && !vp.read)
				warn("Unused texture " + v->name, p);
			break;
		}
	}
}

void Compiler::compileAssign(ParsedValue* v, ParsedValue* e, Position p) {
	if (v == nullptr) {
		auto decl = e->v;
		if (decl->isBlock()) {
			auto block = dynamic_cast<PBlock*>(decl);
			auto old = saveVars();
			for (auto e = block->el.begin(); e != block->el.end(); ++e)
				compileAssign(e->v, e->e, e->p);
			closeBlock(old);
			return;
		}
		else if (decl->isReturn()) {
			auto retur = dynamic_cast<PReturn*>(decl);
			//if (ret == nullptr) error("Unexpected return", retur->e.p);
			if (ret != nullptr) error("Duplicate return", retur->e.p);
			ret = new CodeValue(compileValue(new ParsedValue(retur->e), nullptr, nullptr));
			return;
		}
		else if (decl->isIf()) {
			auto pif = dynamic_cast<PIf*>(decl);
			auto cond = compileValue(pif->cond, new bool(false), new bool(true));
			unify(cond.t, new TBool, e->p);
			// save writes
			std::vector<int> oldWrite;
			for (auto p2 = varProps.begin(); p2 != varProps.end(); ++p2)
				oldWrite.push_back(p2->write);
			auto old = cur.exprs;
			cur.exprs;
			compileAssign(nullptr, pif->eif, p);
			auto vif = cur.exprs;
			CodeBlock* velse;
			if (pif->eelse == nullptr) {
				// restore writes
				for (unsigned i = 0; i < oldWrite.size(); ++i)
					varProps[i].write = oldWrite[i];
			}
			else {
				// save and restore writes
				std::vector<int> ifWrite;
				for (unsigned i = 0; i < oldWrite.size(); ++i) {
					auto p2 = varProps[i];
					ifWrite.push_back(p2.write);
					varProps[i].write = oldWrite[i];
				}
				compileAssign(nullptr, pif->eelse, p);
				velse = new CodeBlock(cur.exprs);
				// merge writes
				for (unsigned i = 0; i < oldWrite.size(); ++i)
					varProps[i].write = oldWrite[i] | (ifWrite[i] & varProps[i].write);
			}
			cur.exprs = old;
			Block block;
			block.v = nullptr;
			auto cif = new CIf;
			cif->cond = cond;
			cif->eif = vif;
			cif->eelse = velse;
			block.e.d = cif;
			block.e.t = new TNull;
			block.e.p = e->p;
			cur.exprs.push_back(block);
			return;
		}
		else if (decl->isFor()) {
			//case PFor(vname, it, loop):
			auto pfor = dynamic_cast<PFor*>(decl);
			VarType* vt = nullptr;
			CodeValue it;
			if (pfor->iter.v->isOp()) {
				//case POp(CInterval, first, last):
				auto pop = dynamic_cast<POp*>(pfor->iter.v);
				auto first = compileValue(new ParsedValue(pop->e1), new bool(false), new bool(true));
				auto last = compileValue(new ParsedValue(pop->e2), new bool(false), new bool(true));
				unify(first.t, new TFloat, first.p);
				unify(last.t, new TFloat, last.p);
				vt = new TFloat;

				auto cop = new COp(CInterval, first, last);
				it.d = cop;
				it.t = new TNull;
				it.p = e->p;
			}
			else {
				auto it2 = compileValue(new ParsedValue(pfor->expr), new bool(false), new bool(true));
				if (it2.t->isArray()) {
					vt = it2.t;
					it = it2;
				}
				else error("Can only unroll loop on variable length Array", it2.p);
			}

			auto vloop = allocVar(pfor->v, VParam, vt, e->p);
			props(vloop).write = 1;

			auto oldExprs = cur.exprs;
			std::vector<int> oldWrite;
			for (auto p2 = varProps.begin(); p2 != varProps.end(); ++p2)
				oldWrite.push_back(p2->write);
			cur.exprs.clear();
			compileAssign(nullptr, new ParsedValue(pfor->iter), p);
			auto eloop = cur.exprs;
			cur.exprs = oldExprs;
			for (unsigned i = 0; i < oldWrite.size(); ++i)
				varProps[i].write = oldWrite[i];
				
			vars.erase(vloop->name);

			Block block;
			block.v = nullptr;
			auto cfor = new CFor;
			cfor->v = vloop;
			cfor->it = it;
			cfor->exprs = eloop;
			block.e.d = cfor;
			block.e.t = new TNull;
			block.e.p = e->p;
			cur.exprs.push_back(block);
			return;
		}
		auto e2 = compileValue(e, nullptr, nullptr);
		/*if (e2.d->isUnop()) {
			CUnop* unop = dynamic_cast<CUnop*>(e2.d);
			if (unop->op == CKill) {
				Block block;
				block.v = nullptr;
				block.e = e2;
				cur.exprs.push_back(block);
				return;
			}
		}*/
		Block block;
		block.v = nullptr;
		block.e = e2;
		cur.exprs.push_back(block);
		return;
	}
	if (e == nullptr) {
		if (v->v->isLocal()) {
			auto v2 = dynamic_cast<PLocal*>(v->v);
			allocVar(v2->v.n, VTmp, v2->v.t, v2->v.p);
			return;
		}
		error("assert",p);
	}
	auto e2 = compileValue(e, nullptr, nullptr);
	if (v->v->isLocal()) {
		auto v2 = dynamic_cast<PLocal*>(v->v);
		if (v2->v.t == nullptr) v2->v.t = e2.t;
	}
	auto v2 = compileValue(v, new bool(true), nullptr);
	unify(e2.t, v2.t, e2.p);
	addAssign(v2, e2, p);
}

CodeValue Compiler::compileValue(ParsedValue* e, bool* isTarget, bool* isCond) {
	if (e->v->isBlock()) error("Unexpected block", e->p);
	if (e->v->isReturn()) error("Unexpected return", e->p);
	if (e->v->isVar()) {
		auto var = dynamic_cast<PVar*>(e->v);
		if (vars.find(var->v) == vars.end())
			error(std::string("Unknown variable '") + var->v + "'", e->p);
		auto v = vars[var->v];
		if (!isTarget)
			checkReadVar(v, std::vector<Comp>(), e->p, isCond == nullptr ? false : *isCond);
		CodeValue value;
		auto cvar = new CVar(v, std::vector<Comp>());
		value.d = cvar;
		value.t = v->type;
		value.p = e->p;
		return value;
	}
	else if (e->v->isConst()) {
		auto constant = dynamic_cast<PConst*>(e->v);
		VarType* t;
		if (constant->c->isNull()) t = new TNull;
		else if (constant->c->isInt()) t = new TInt;
		else if (constant->c->isFloat()) t = new TFloat;
		else if (constant->c->isBool()) t = new TBool;
		else if (constant->c->isFloats()) {
			auto floats = dynamic_cast<ConstFloats*>(constant->c);
			if (floats->v.size() == 0 || floats->v.size() > 4) error("Floats must contain 1-4 values", e->p);
			t = Tools::makeFloat(floats->v.size());
		}
		else if (constant->c->isObject()) error("Literal objects are not allowed", e->p);
		else if (constant->c->isArray()) error("Literal arrays are not allowed", e->p);
		CodeValue value;
		auto c = new CConst(constant->c);
		value.d = c;
		value.t = t;
		value.p = e->p;
		return value;
	}
	else if (e->v->isLocal()) {
		auto local = dynamic_cast<PLocal*>(e->v);
		auto v = allocVar(local->v.n, VTmp, local->v.t, local->v.p);
		CodeValue value;
		auto var = new CVar(v, std::vector<Comp>());
		value.d = var;
		value.t = v->type;
		value.p = e->p;
		return value;
	}
	else if (e->v->isOp()) {
		auto op = dynamic_cast<POp*>(e->v);
		return makeOp(op->op, op->e1, op->e2, e->p, isCond == nullptr ? false : *isCond);
	}
	else if (e->v->isUnop()) {
		auto op = dynamic_cast<PUnop*>(e->v);
		return makeUnop(op->op, op->e, e->p, isCond == nullptr ? false : *isCond);
	}
	else if (e->v->isTex()) {
		/*
		var v = vars.get(vname);
		if( v == null ) error("Unknown texture '" + vname + "'", e.p);
		props(v).read = true;
			
		var single = false;
		var tflags = [];
		var modes = [];
			
		for( f in flags ) {
			var param;
			switch( f.f ) {
			case PTFlag(fl):
				param = switch(fl) {
				case TMipMapDisable, TMipMapLinear, TMipMapNearest: PMipMap;
				case TWrap, TClamp: PWrap;
				case TFilterLinear, TFilterNearest: PFilter;
				case TSingle: single = true; PSingle;
				case TLodBias(_): PLodBias;
				}
				tflags.push( { f : CTFlag(fl), p : f.p } );
			case PTParam(p, v):
				var v = compileValue(v, false, true);
				param = p;
				var t = switch( p ) {
				case PLodBias: TFloat;
				case PMipMap, PSingle, PWrap, PFilter: TBool;
				}
				unify(v.t, t, v.p);
				tflags.push( { f : CTParam(p, v), p : f.p } );
			}
			if( modes[Type.enumIndex(param)] )
				error("Duplicate or conflicting texture flag", f.p);
			modes[Type.enumIndex(param)] = true;
		}
		var acc = compileValue(acc);
		switch( v.type ) {
		case TTexture(cube):
			unify(acc.t, cube?TFloat3:(single ? TFloat : TFloat2), acc.p);
		default: error("'"+vname + "' is not a texture", e.p);
		}
		return { d : CTex(v, acc, tflags), t : TFloat4, p : e.p };
		*/
	}
	else if (e->v->isCond()) {
		auto conditional = dynamic_cast<PCond*>(e->v);
		auto cond = compileValue(new ParsedValue(conditional->cond));
		unify(cond.t, new TBool, cond.p);
		auto e1 = compileValue(new ParsedValue(conditional->eif));
		auto e2 = compileValue(new ParsedValue(conditional->eelse));
		unify(e2.t, e1.t, e2.p);
		CodeValue value;
		auto c = new CCond;
		c->cond = cond;
		c->eif = e1;
		c->eelse = e2;
		value.d = c;
		value.t = e1.t;
		value.p = e->p;
		return value;
	}
	else if (e->v->isVector()) {
		auto vec = dynamic_cast<PVector*>(e->v);
		if (vec->el.size() == 0 || vec->el.size() > 4)
			error("Vector size should be 1-4", e->p);
		std::vector<CodeValue> exprs;
		for (auto v : vec->el) {
			auto e = compileValue(new ParsedValue(v));
			unify(e.t, new TFloat, e.p);
			exprs.push_back(e);
		}
		CodeValue value;
		auto cvec = new CVector;
		cvec->vals = exprs;
		value.d = cvec;
		value.t = Tools::makeFloat(exprs.size());
		value.p = e->p;
		return value;
	}
	else if (e->v->isRow()) {
		//case PRow(e1, e2):
		auto row = dynamic_cast<PRow*>(e->v);
		auto e1 = compileValue(new ParsedValue(row->e));
		auto e2 = compileValue(new ParsedValue(row->index));
		unify(e2.t, new TFloat, e2.p);
		if (e1.t->isMatrix()) {
			//case TMatrix(rows, cols, t):
			auto m = dynamic_cast<TMatrix*>(e1.t);
			if (m->transpose == nullptr) m->transpose = new bool(true);
			if (!*m->transpose) throw "You can't read a row from a not transposed matrix";
			auto c = constValue(e2);
			if (c != nullptr && (*c < 0 || *c >= m->r || float(int(*c)) != *c))
				error("Accessing matrix outside bounds", e2.p);
			CodeValue value;
			auto crow = new CRow;
			crow->e1 = e1;
			crow->e2 = e2;
			value.d = crow;
			value.t = Tools::makeFloat(m->c);
			value.p = e->p;
			return value;
		}
		else if (e1.t->isArray()) {
			//case TArray(t, size):
			auto arr = dynamic_cast<TArray*>(e1.t);
			if (arr->size == 0)
				error("Cannot access variable length array using [] : only for loops are allowed", e->p);
			auto c = constValue(e2);
			if (c != nullptr && (*c < 0 || *c >= arr->size || float(int(*c)) != *c))
				error("Accessing Array outside bounds", e2.p);
			CodeValue value;
			auto crow = new CRow;
			crow->e1 = e1;
			crow->e2 = e2;
			value.d = crow;
			value.t = arr->t;
			value.p = e->p;
			return value;
		}
		else {
			error(/*Tools::typeStr(e1.t) +*/ " cannot be accessed this way", e1.p);
			CodeValue value;
			value.d = nullptr;
			value.t = nullptr;
			value.p = e->p;
			return value;
		}
	}
	else if (e->v->isCall()) {
		//case PCall(n,vl):
		auto call = dynamic_cast<PCall*>(e->v);
		if (helpers.find(call->n) == helpers.end()) error("Unknown function '" + call->n + "'", e->p);
		auto h = helpers[call->n];
		std::vector<CodeValue> vals;
		allowTextureRead = true;
		for (auto v : call->vl)
			vals.push_back(compileValue(new ParsedValue(v)));
		allowTextureRead = false;
		if (h.args.size() != call->vl.size()) error("Function " + call->n + " requires " + str(h.args.size()) + " arguments", e->p);
		auto old = saveVars();
		// only allow access to globals/output from within out helper functions
		for (auto v : old)
			if (!props(v.second).global)
				vars.erase(vars.find(v.first));
		// init args
		for (unsigned i = 0; i < h.args.size(); ++i) {
			auto value = vals[i];
			auto a = h.args[i];
			unify(value.t, a.t, value.p);
			if (a.t->isTexture()) {
				if (value.d->isVar()) {
					// copy variable
					vars[a.n] = dynamic_cast<CVar*>(value.d)->v;
				}
				else {
					error("Invalid texture access", value.p);
				}
			}
			else {
				auto v = allocVar(a.n, VTmp, a.t, a.p);
				CodeValue value2;
				auto cvar = new CVar(v, std::vector<Comp>());
				value2.d = cvar;
				value2.t = v->type;
				value2.p = v->pos;
				addAssign(value2, value, value.p);
			}
		}
		// compile block
		auto rold = ret;
		ret = nullptr;
		for (auto e : h.exprs)
			compileAssign(e.v, e.e, e.p);
		auto v = ret;
		//if (v == nullptr)
		//	error("Missing return", h.pos);
		ret = rold;
		closeBlock(old);
		CodeValue value;
		if (v == nullptr) {
			value.d = nullptr;
			value.t = new TNull;
			value.p = 0;
		}
		else {
			value.d = v->d;
			value.t = v->t;
			value.p = e->p;
		}
		return value;
	}
	else if (e->v->isField()) {
		//case PField(e1, f):
		auto field = dynamic_cast<PField*>(e->v);
		auto e1 = compileValue(new ParsedValue(field->e), isTarget, isCond);
		if (e1.t->isMatrix() || e1.t->isTexture() || e1.t->isArray()) {
			// no swizzling
		}
		else if (e1.t->isObject()) {
			//case TObject(fields):
			auto obj = dynamic_cast<TObject*>(e1.t);
			for (auto fv : obj->fields)
				if (fv.name == field->field) {
					CodeValue value;
					auto cfield = new CField;
					cfield->e = e1;
					cfield->f = field->field;
					value.d = cfield;
					value.t = fv.t;
					value.p = e->p;
					return value;
				}
		}
		else {
			std::vector<Comp> swiz;
			for (unsigned i = 0; i < field->field.size(); ++i) {
				switch (field->field[i]) {
				case 'x':
				case 'r':
					swiz.push_back(X);
					break;
				case 'y':
				case 'g':
					swiz.push_back(Y);
					break;
				case 'z':
				case 'b':
					swiz.push_back(Z);
					break;
				case 'w':
				case 'a':
					swiz.push_back(W);
					break;
				default:
					swiz.clear();
					goto noswiz;
				}
			}
			noswiz:
			if (swiz.size() > 0) {
				auto v = e1;
				auto count = Tools::floatSize(v.t);
				// allow all components access on input and varying values only
				if (v.d->isVar()) {
					//case CVar(v, s), CField({ d : CVar(v,s) },_):
					auto var = dynamic_cast<CVar*>(v.d);
					if (var->swiz.size() == 0 && count > 0 && (var->v->kind == VInput || var->v->kind == VVar)) count = 4;
				}
				else if (v.d->isField()) {
					auto field = dynamic_cast<CField*>(v.d);
					if (field->e.d->isVar()) {
						auto var = dynamic_cast<CVar*>(field->e.d);
						if (var->swiz.size() == 0 && count > 0 && (var->v->kind == VInput || var->v->kind == VVar)) count = 4;
					}
				}
				// check that swizzling is correct
				for (auto s : swiz)
					if (::enumIndex(s) >= count)
						error("Invalid swizzling on " /*+ Tools::typeStr(v.t)*/, e->p);
				// build swizzling
				if (v.d->isVar()) {
					//case CVar(v, swiz2):
					auto var = dynamic_cast<CVar*>(v.d);
					std::vector<Comp> ns;
					if (var->swiz.size() == 0) {
						if (!isTarget)
							checkReadVar(var->v, swiz, e->p, isCond == nullptr ? false : *isCond);
						ns = swiz;
					} else {
						// combine swizzlings
						ns.clear();
						for (auto s : swiz)
							ns.push_back(var->swiz[::enumIndex(s)]);
					}
					CodeValue value;
					auto cvar = new CVar(var->v, ns);
					value.d = cvar;
					value.t = Tools::makeFloat(swiz.size());
					value.p = e->p;
					return value;
				}
				else {
					CodeValue value;
					auto cswiz = new CSwiz;
					cswiz->e = v;
					cswiz->swiz = swiz;
					value.d = cswiz;
					value.t = Tools::makeFloat(swiz.size());
					value.p = e->p;
					return value;
				}
			}
		}
		error(/*Tools::typeStr(e1.t) +*/ std::string(" has no field '") + field->field + "'", e->p);
	}
	else if (e->v->isIf() || e->v->isFor()) {
		error("assert", -1);
	}
	CodeValue value;
	value.d = nullptr;
	value.t = nullptr;
	value.p = 0;
	return value;
}

std::map<std::string, Variable*> Compiler::saveVars() {
	std::map<std::string, Variable*> old;
	for (auto v = vars.begin(); v != vars.end(); ++v)
		old[v->first] = vars[v->first];
	return old;
}

void Compiler::closeBlock(std::map<std::string, Variable*> old) {
	for (auto v = vars.begin(); v != vars.end(); ++v)
		if (v->second->kind == VTmp && old[v->second->name] != v->second && !props(v->second).read )
			warn("Unused local variable '" + v->second->name + "'", v->second->pos);
	vars = old;
}

void Compiler::unify(VarType* t1, VarType* t2, Position p) {
	if (!tryUnify(t1, t2)) {
		// if we only have the transpose flag different, let's print a nice error message
		if (t1->isMatrix()) {
			//case TMatrix(r, c, t):
			if (t2->isMatrix()) {
				//case TMatrix(r2, c2, t2):
				auto m1 = dynamic_cast<TMatrix*>(t1);
				auto m2 = dynamic_cast<TMatrix*>(t2);
				if (m1->r == m2->r && m1->c == m2->c && m2->transpose != nullptr) {
					if (m1->transpose != nullptr && *m1->transpose)
						error("Matrix is transposed by another operation", p);
					else
						error("Matrix is not transposed by a previous operation", p);
				}
			}
		}
		// default error message
		error(/*Tools::typeStr(t1) +*/ " should be " /*+ Tools.typeStr(t2)*/, p);
	}
}

void Compiler::addAssign(CodeValue v, CodeValue e, Position p) {
	if (v.d->isVar()) {
		//case CVar(vr, swiz):
		auto var = dynamic_cast<CVar*>(v.d);
		auto bits = swizBits(var->swiz, var->v->type);
		auto vp = props(var->v);
		// first write on a unknown var : assume it's a varying and reset its written bits
		if (var->v->kind == VUnknown) {
			var->v->kind = VVar;
			vp.write = 0;
		}
		switch (var->v->kind) {
		case VVar:
			if (!cur.vertex) error("You can't write a variable in fragment shader", v.p);
			// disable : conditional compilation might have if/else flow
			// 	if( vp.write & bits != 0  ) error("Multiple writes to the same variable are not allowed", v.p);
			vp.write |= bits;
			break;
		case VConst:
		case VParam:
			error("Constant values cannot be written", v.p);
			break;
		case VInput:
			error("Input values cannot be written", v.p);
			break;
		case VOut:
			if (!cur.vertex && vp.write != 0) error("You must use a single write for fragment shader output", v.p);
			vp.write |= bits;
			break;
		case VTmp:
			vp.write |= bits;
			break;
		case VTexture:
			error("You can't write to a texture", v.p);
			break;
		}
		if (var->swiz.size() > 0) {
			auto min = -1;
			for (auto s : var->swiz) {
				auto k = ::enumIndex(s);
				if( k <= min || (!allowAllWMasks && var->swiz.size() > 1 && k != min + 1)) error("Unsupported write mask", v.p);
				min = k;
			}
		}
	}
	else {
		error("Invalid assign", p);
	}
	Block block;
	block.v = new CodeValue(v);
	block.e = e;
	cur.exprs.push_back(block);
}

void Compiler::checkReadVar(Variable* v, std::vector<Comp> swiz, Position p, bool isCond) {
	auto vp = props(v);
	// first read on an unknown var, infer its type
	if (v->kind == VUnknown) {
		v->kind = VConst;
		vp.inferred = true;
	}
	switch (v->kind) {
	case VOut:
		error("Output cannot be read", p);
	case VVar:
		if (cur.vertex) error("You cannot read varying in vertex shader", p);
		vp.read = true;
		break;
	case VConst:
	case VParam:
		if (!isCond) {
			if (vp.readInShader != nullptr && *vp.readInShader != cur.vertex) error("You cannot read the same constant in both vertex and fragment shader", p);
			vp.readInShader = new bool(cur.vertex);
			if (!cur.vertex) v->index = 1; // mark as used in fragment shader
		}
		vp.read = true;
		break;
	case VTmp: {
		if (vp.write == 0) error("Variable '" + v->name + "' has not been initialized", p);
		auto bits = swizBits(swiz, v->type);
		if (vp.write && bits != bits) error("Some fields of '" + v->name + "' have not been initialized", p);
		vp.read = true;
		break;
	}
	case VInput:
		// allow reading in fragment shader : we will create a varying at runtime compile time
		vp.read = true;
		break;
	case VTexture:
		if (!allowTextureRead) error("You can't read from a texture", p);
		break;
	}
}

int Compiler::swizBits(std::vector<Comp> s, VarType* t) {
	if (s.size() == 0) return fullBits(t);
	int b = 0;
	for (auto x = s.begin(); x != s.end(); ++x)
		b |= 1 << ::enumIndex(*x);
	return b;
}

bool Compiler::tryUnify(VarType* t1, VarType* t2) {
	if (t1 == t2) return true;
	if (t1->isMatrix()) {
		//case TMatrix(r,c,t1):
		if (t2->isMatrix()) {
			auto m1 = dynamic_cast<TMatrix*>(t1);
			auto m2 = dynamic_cast<TMatrix*>(t2);
		//case TMatrix(r2, c2, t2):
			if (m1->r != m2->r || m1->c != m2->c) return false;
			if (m1->transpose != m2->transpose) {
				if (m1->transpose == nullptr)
					m1->transpose = m2->transpose;
				else if (m2->transpose == nullptr)
					m2->transpose = m1->transpose;
			}
			return *m1->transpose == *m2->transpose;
		}
	}
	else if (t1->isTexture()) {
		auto tex1 = dynamic_cast<TTexture*>(t1);
		auto tex2 = dynamic_cast<TTexture*>(t2);
		return tex2 != nullptr && tex1->cube == tex2->cube;
	}
	else if (t1->isFloat()) {
		return t2->isFloat();
	}
	else if (t1->isFloat2()) {
		return t2->isFloat2();
	}
	else if (t1->isFloat3()) {
		return t2->isFloat3();
	}
	else if (t1->isFloat4() || t1->isInt()) {
		return t2->isFloat4() || t2->isInt();
	}
	return false;
}

bool Compiler::isFloat(VarType* t) {
	return t->isFloat() || t->isFloat2() || t->isFloat3() || t->isFloat4() || t->isInt();
}

bool Compiler::isCompatible(VarType* t1, VarType* t2) {
	if (typeid(*t1) == typeid(*t2)) return true;
	else if (t1->isMatrix()) {
		if (t2->isMatrix()) {
			auto m1 = dynamic_cast<TMatrix*>(t1);
			auto m2 = dynamic_cast<TMatrix*>(t2);
			return m2->r == m1->r && m2->c == m1->c && (m1->transpose == nullptr || m2->transpose == nullptr || *m1->transpose == *m2->transpose);
		}	
	}
	else if (t1->isFloat3()) {
		return t2->isFloat3();
	}
	else if (t1->isFloat4() || t1->isInt()) {
		return t2->isFloat4() || t2->isInt();
	}
	return false;
}

CodeValue Compiler::makeOp(CodeOp op, ParsedValue ee1, ParsedValue ee2, Position p, bool isCond) {
	auto e1 = compileValue(new ParsedValue(ee1), new bool(false), new bool(isCond || ((op == CEq || op == CNeq) && ee2.v->isConst() && dynamic_cast<PConst*>(ee2.v)->c->isNull())));
	auto e2 = compileValue(new ParsedValue(ee2), new bool(false), new bool(isCond || ((op == CEq || op == CNeq) && e1.t->isNull())));

	// look for a valid operation as listed in "ops"
	auto types = ops[enumIndex(op)];
	Compiler::op first(nullptr, nullptr, nullptr);
	for (auto t = types.begin(); t != types.end(); ++t) {
		if (isCompatible(e1.t, t->p1) && isCompatible(e2.t, t->p2)) {
			if (first.p1 == nullptr && first.p2 == nullptr && first.r == nullptr) first = *t;
			if (tryUnify(e1.t, t->p1) && tryUnify(e2.t, t->p2)) {
				CodeValue value;
				auto cop = new COp(op, e1, e2);
				value.d = cop;
				value.t = t->r;
				value.p = p;
				return value;
			}
		}
	}
	// if we have an operation on a single scalar, let's map it on all floats
	if (e2.t->isFloat() && isFloat(e1.t))
		for (auto t = types.begin(); t != types.end(); ++t)
			if (isCompatible(e1.t, t->p1) && isCompatible(e1.t, t->p2)) {
				std::vector<Comp> swiz;
				for (int i = 0; i < Tools::floatSize(e1.t); ++i)
					swiz.push_back(X);
				CodeValue value;
				auto cop = new COp(op, e1, e1);
				CodeValue value2;
				auto cswiz = new CSwiz;
				cswiz->e = e2;
				cswiz->swiz = swiz;
				value2.d = cswiz;
				value2.t = e1.t;
				value2.p = e2.p;
				cop->e2 = value2;
				value.d = cop;
				value.t = e1.t;
				value.p = p;
				return value;
			}
	// ...or the other way around
	if (e1.t->isFloat() && isFloat(e2.t))
		for (auto t = types.begin(); t != types.end(); ++t)
			if (isCompatible(e2.t, t->p1) && isCompatible(e2.t, t->p2)) {
				std::vector<Comp> swiz;
				for (int i = 0; i < Tools::floatSize(e2.t); ++i)
					swiz.push_back(X);
				CodeValue value;
				auto cop = new COp(op, e1, e1);
				CodeValue value2;
				auto cswiz = new CSwiz;
				cswiz->e = e1;
				cswiz->swiz = swiz;
				value2.d = cswiz;
				value2.t = e2.t;
				value2.p = e1.p;
				cop->e2 = value2;
				value.d = cop;
				value.t = e2.t;
				value.p = p;
				return value;
			}
				
	// if we have a null check, infer a VParam
	if (e1.t->isNull() && (op == CEq || op == CNeq)) {
		auto tmp = e1;
		e1 = e2;
		e2 = tmp;
	}
	if (e2.t->isNull()) {
		if (e1.d->isVar()) {
			auto var = dynamic_cast<CVar*>(e1.d);
			if (var->swiz.size() == 0 && (var->v->kind == VParam || var->v->kind == VUnknown || (var->v->kind == VConst && props(var->v).inferred)) ) {
				var->v->kind = VParam;
				CodeValue value;
				auto cop = new COp(op, e1, e2);
				value.d = cop;
				value.t = new TBool;
				value.p = p;
				return value;
			}
		}
		error("Only constants can be compared to null", e1.p);
	}

	// special case for matrix multiply
	//switch( [op, e1.t, e2.t] ) {
	//case [CMul, TFloat4, TMatrix(4, 4, { t : false } )]:
	//	error("The right matrix must be transposed, please type explicitely as M44T", e2.p);
		
	// we have an error, so let's find the most appropriate override
	// in order to print the most meaningful error message
	if (first.p1 == nullptr && first.p2 == nullptr && first.r == nullptr)
		for (auto t : types)
			if (isCompatible(e1.t, t.p1)) {
				first = t;
				break;
			}
	if (first.p1 == nullptr && first.p2 == nullptr && first.r == nullptr)
		first = types[0];
	unify(e1.t, first.p1, e1.p);
	unify(e2.t, first.p2, e2.p);
	error("Where am I?", p);
	CodeValue value;
	value.d = nullptr;
	value.t = nullptr;
	value.p = p;
	return value;
}

CodeValue Compiler::makeUnop(CodeUnop op, ParsedValue ev, Position p, bool isCond) {
	auto e = compileValue(new ParsedValue(ev), new bool(false), new bool(isCond));
	auto rt = e.t;
	switch (op) {
	case CNot: {
		unify(e.t, new TBool, e.p);
		CodeValue value;
		auto cunop = new CUnop(op, e);
		value.d = cunop;
		value.t = new TBool;
		value.p = p;
		return value;
	}
	case CNorm:
		rt = new TFloat3;
		break;
	case CLen:
		rt = new TFloat;
		break;
	case CTrans:
		if (e.t->isMatrix()) {
			//TMatrix(r, c, t):
			auto m = dynamic_cast<TMatrix*>(e.t);
			// transpose-free ?
			if (m->transpose == nullptr) {
				m->transpose = new bool(true);
				e.p = p;
				return e;
			}
			CodeValue value;
			auto cunop = new CUnop(CTrans, e);
			value.d = cunop;
			auto matrix = new TMatrix;
			matrix->c = m->c;
			matrix->r = m->r;
			matrix->transpose = new bool(!*m->transpose);
			value.t = matrix;
			value.p = p;
			return value;
		}
	}
	if (!isFloat(e.t)) unify(e.t, new TFloat4, e.p); // force error
	CodeValue value;
	auto cunop = new CUnop(op, e);
	value.d = cunop;
	value.t = rt;
	value.p = p;
	return value;
}

float* Compiler::constValue(CodeValue v) {
	if (v.d->isConst()) {
		auto c = dynamic_cast<CConst*>(v.d);
		if (c->c->isFloat()) return new float(dynamic_cast<ConstFloat*>(c->c)->v);
		if (c->c->isInt()) return new float(static_cast<float>(dynamic_cast<ConstInt*>(c->c)->i));
	}
	return nullptr;
}

std::vector<Compiler::Operation> Compiler::initOps() {
	auto mat4 = new TMatrix;
	mat4->c = 4;
	mat4->r = 4;
	mat4->transpose = new bool(false);

	auto mat4_t = new TMatrix;
	mat4_t->c = 4;
	mat4_t->r = 4;
	mat4_t->transpose = new bool(true);

	auto mat3 = new TMatrix;
	mat3->c = 3;
	mat3->r = 3;
	mat3->transpose = new bool(false);

	auto mat3_t = new TMatrix;
	mat3_t->c = 3;
	mat3_t->r = 3;
	mat3_t->transpose = new bool(true);

	auto mat34_t = new TMatrix;
	mat34_t->c = 3;
	mat34_t->r = 4;
	mat34_t->transpose = new bool(true);

	std::vector<op> floats;
	floats.push_back(op(new TFloat, new TFloat, new TFloat));
	floats.push_back(op(new TFloat2, new TFloat2, new TFloat2));
	floats.push_back(op(new TFloat3, new TFloat3, new TFloat3));
	floats.push_back(op(new TFloat4, new TFloat4, new TFloat4));
	
	std::vector<Operation> ops;
	
	ops.push_back(Operation(CAdd, floats));
	ops.push_back(Operation(CSub, floats));
	ops.push_back(Operation(CDiv, floats));
	ops.push_back(Operation(CPow, floats));
	ops.push_back(Operation(CMod, floats));

	ops.push_back(Operation(CMin, floats));
	ops.push_back(Operation(CMax, floats));
	ops.push_back(Operation(CLt, floats));
	ops.push_back(Operation(CGte, floats));
	ops.push_back(Operation(CEq, floats));
	ops.push_back(Operation(CNeq, floats));
	ops.push_back(Operation(CLte, floats));
	ops.push_back(Operation(CGt, floats));

	std::vector<op> dotops;
	dotops.push_back(op(new TFloat4, new TFloat4, new TFloat));
	dotops.push_back(op(new TFloat3, new TFloat3, new TFloat));
	ops.push_back(Operation(CDot, dotops));

	std::vector<op> crossops;
	crossops.push_back(op(new TFloat3, new TFloat3, new TFloat3));
	ops.push_back(Operation(CCross, crossops));

	std::vector<op> logicops;
	logicops.push_back(op(new TBool, new TBool, new TBool));
	ops.push_back(Operation(CAnd, logicops));
	ops.push_back(Operation(COr, logicops));

	std::vector<op> noops;
	ops.push_back(Operation(CInterval, noops));

	floats.push_back(op(mat4_t, new TFloat4, new TFloat4));
	floats.push_back(op(mat34_t, new TFloat4, new TFloat3));
	floats.push_back(op(mat3_t, new TFloat3, new TFloat3));
	floats.push_back(op(mat4_t, new TFloat3, new TFloat3)); // only use the 3x4 part of the matrix
	floats.push_back(op(mat4, mat4_t, mat4));
	floats.push_back(op(mat3, mat3_t, mat3));
	floats.push_back(op(mat4_t, mat4, mat4_t));
	floats.push_back(op(mat3_t, mat3, mat3_t));
	ops.push_back(Operation(CMul, floats));

	return ops;
}
