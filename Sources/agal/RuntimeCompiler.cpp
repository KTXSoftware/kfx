#include "RuntimeCompiler.h"
#include "common/angleutils.h"
#include <algorithm>
#include <math.h>
#include <stdexcept>

using namespace agal;

namespace {
	std::string str(int i) {
		char buffer[20];
		snprintf(buffer, sizeof(buffer), "%d", i);
		return buffer;
	}

	int sortById(Variable* v1, Variable* v2) {
		return v1->id - v2->id;
	}

	int indexOf(std::vector<Variable*> variables, Variable* v) {
		for (unsigned i = 0; i < variables.size(); ++i) {
			if (variables[i] == v) return static_cast<int>(i);
		}
		return -1;
	}

	std::vector<Comp> splice(std::vector<Comp>& vec, int pos, int len) {
		std::vector<Comp> spliced;
		std::vector<Comp> remains;
		for (int i = 0; i < static_cast<int>(vec.size()); ++i) {
			if (i >= pos && i < pos + len) spliced.push_back(vec[i]);
			else remains.push_back(vec[i]);
		}
		vec.clear();
		vec.insert(vec.begin(), remains.begin(), remains.end());
		return spliced;
	}

	float floatValue(Const* c) {
		if (c->isNull()) return 0;
		if (c->isFloat()) return dynamic_cast<ConstFloat*>(c)->v;
		if (c->isInt()) return static_cast<float>(dynamic_cast<ConstInt*>(c)->i);
		throw std::runtime_error("assert");
	}
}

void RuntimeCompiler::error(std::string msg, Position p) {
	throw std::runtime_error(msg.c_str()); //, p);
}

Data RuntimeCompiler::compile(Data data) {
	usedVars.clear();
	constVars.clear();
	varProps.clear();
	objectVars.clear();
	defPos = data.shader.pos;
			
	std::map<std::string, Variable*> hVars;
	
	auto all = data.globals;
	for (auto d : data.shader.args) all.push_back(d);

	for (auto v : all)
		switch (v->kind) {
		case VConst:
		case VParam:
			hVars[v->name] = v;
			break;
		case VInput:
			props(v).global = true;
			break;
		}
	
	auto shader = compileCode(data.shader);
	auto vconst = constVars;
	constVars.clear();

	std::sort(usedVars.begin(), usedVars.end(), sortById);
		
	indexVars(shader, vconst);
	
	std::vector<Variable*> globals;
	for (auto v : usedVars) {
		if (!props(v).global)
			continue;
		globals.push_back(v);
	}
	
	Data ret;
	ret.globals = globals;
	ret.shader = shader;
	return ret;
}

Code RuntimeCompiler::compileCode(Code c) {
	isCond = false;
	isConst = false;
	cur.vertex = c.vertex;
	cur.args.clear();
	cur.consts.clear();
	cur.exprs.clear();
	cur.pos = c.pos;
	cur.tempSize = 0;
	for (auto e : c.exprs)
		compileAssign(e.v, &e.e);
	checkVars();
	return cur;
}

VarProps2& RuntimeCompiler::props(Variable* v) {
	if (varProps.find(v->id) == varProps.end()) {
		VarProps2 p;
		p.global = false;
		p.read = false;
		p.write = 0;
		p.newVar = nullptr;
		p.value = new ConstNull;
		p.isVertex = false;
		p.ref = nullptr;
		varProps[v->id] = p;
	}
	return varProps[v->id];
}


RuntimeCompiler::UsedSize RuntimeCompiler::calculateUsedSize(Variable* v, int* index) {
	v->index = *index;
	auto p = props(v);
	if (p.ref != nullptr) p.ref->index = v->index;
	
	if (v->type->isObject()) {
		auto object = dynamic_cast<TObject*>(v->type);
		//case TObject(fields):
		if (objectVars.find(v->id) == objectVars.end()) error("Error", -1); // not really used it seems ?
		auto o = objectVars[v->id];
		// only take into account the actual used registers
		int tot = 0;
		std::vector<TObject::Field> fl;
		for (auto f : object->fields) {
			if (o.fields.find(f.name) != o.fields.end()) {
				auto fv = o.fields[f.name];
				auto k = calculateUsedSize(fv, index);
				tot += k.size;
				TObject::Field field;
				field.name = f.name;
				field.t = k.v->type;
				fl.push_back(field);
			}
		}
		// mute the variable in usedVars with the one with the actual fields
		int index = indexOf(usedVars, v);
		if (index >= 0) {
			auto v2 = new Variable;
			v2->id = v->id;
			v2->index = v->index;
			v2->kind = v->kind;
			v2->name = v->name;
			v2->pos = v->pos;
			auto tob = new TObject;
			tob->fields = fl;
			v2->type = tob;
			v = v2;
			usedVars[index] = v;
		}
		UsedSize size;
		size.v = v;
		size.size = tot;
		return size;
	}
	else if (v->type->isArray()) {
		//case TArray(_, size)
		auto arr = dynamic_cast<TArray*>(v->type);
		if (objectVars.find(v->id) != objectVars.end()) {
			auto o = objectVars[v->id];
			int tot = 0;
			for (int i = 0; i < arr->size; ++i) {
				auto fv = o.fields[str(i)];
				auto k = calculateUsedSize(fv, index);
				tot += k.size;
			}
			UsedSize size;
			size.v = v;
			size.size = tot;
			return size;
		}
	}
	else {
		auto size = Tools::regSize(v->type);
		*index += size;
		UsedSize used;
		used.v = v;
		used.size = size;
		return used;
	}
	UsedSize used;
	used.v = nullptr;
	used.size = -1;
	return used;
}

namespace {
	int enumIndex(VarKind kind) {
		return (int)kind;
	}

	int enumIndex(Comp comp) {
		return (int)comp;
	}
}

void RuntimeCompiler::indexVars(Code c, std::vector<Variable*> constVars) {
	std::vector<int> indexes;
	for (int i = 0; i < 6; ++i) indexes.push_back(0);
			
	for (auto v : usedVars/*.copy()*/) {
		auto p = props(v);
		if (p.isVertex == c.vertex) {
			auto tkind = enumIndex(v->kind);
			auto inf = calculateUsedSize(v, new int(indexes[tkind]));
			auto v = inf.v;
			indexes[tkind] += inf.size;
			switch (v->kind) {
			case VConst:
			case VTexture:
				c.args.push_back(v);
				props(v).global = false; // remove from global list
				break;
			case VParam:
				error("assert", -1); // should have been translated to VConst
				break;
			case VInput:
			case VOut:
			case VVar:
			case VTmp:
				break;
			}
		}
	}
	// move consts at the end
	auto cdelta = indexes[enumIndex(VConst)];
	for (auto v : constVars) {
		if (v->kind != VConst) throw "assert";
		v->index += cdelta;
	}
		
	c.tempSize = indexes[enumIndex(VTmp)];
}

void RuntimeCompiler::compileAssign(CodeValue* v, CodeValue* e) {
	if (e->d->isIf()) {
		//case CIf(cond, eif, eelse):
		auto cif = dynamic_cast<CIf*>(e->d);
		if (isTrue(compileCond(cif->cond))) {
			for (auto e : cif->eif)
				compileAssign(e.v, new CodeValue(e.e));
		}
		else if (cif->eelse != nullptr) {
			for (auto e : *cif->eelse)
				compileAssign(e.v, new CodeValue(e.e));
		}
		return;
	}
	else if (e->d->isFor()) {
		//case CFor(vloop, it, exprs):
		auto cfor = dynamic_cast<CFor*>(e->d);
		if (cfor->it.d->isOp()) {
			//case COp(CInterval, _first, _max):
			auto cop = dynamic_cast<COp*>(cfor->it.d);
			auto _first = compileCond(cop->e1);
			auto _max = compileCond(cop->e2);
			if (_first->isInt() && _max->isInt()) {
				//switch( [compileCond(_first),compileCond(_max)] ) {
				//case [CInt(first), CInt(max)]:
				auto first = dynamic_cast<ConstInt*>(_first)->i;
				auto max = dynamic_cast<ConstInt*>(_max)->i;
				auto p = props(cfor->v);
				for (int i = first; i < max; ++i) {
					p.value = new ConstInt(i);
					p.newVar = nullptr;
					for (auto e : cfor->exprs)
						compileAssign(e.v, new CodeValue(e.e));
				}
			}
			else {
				error("assert", -1);
			}
		}
		else {
			auto vit = compileValue(new CodeValue(cfor->it));
			if (vit->d->isVar()) {
				//case CVar(v,_):
				auto cvar = dynamic_cast<CVar*>(vit->d);
				if (cvar->v->type->isArray()) {
					//case TArray(t, size):
					auto arr = dynamic_cast<TArray*>(cvar->v->type);
					auto v = newVar(cvar->v, cvar->v->pos);
					std::vector<Const*> values;
					if (arr->size == 0) {
						auto cond = compileCond(cfor->it);
						if (cond->isArray()) {
							auto arr2 = dynamic_cast<ConstArray*>(cond);
							values = arr2->v;
							arr->size = arr2->v.size();
						}
						else if (cond->isNull()) {
							arr->size = 0;
						}
						else {
							error("assert", -1);
						}
						v->type = new TArray(arr->t, arr->size);
					}
					if (objectVars.find(v->id) == objectVars.end()) {
						Object o;
						o.v = v;
						objectVars[v->id] = o;
					}
					auto obj = objectVars[v->id];
					auto p = props(cfor->v);
					for (int i = 0; i < arr->size; ++i) {
						auto vi = allocVar(std::string("$") + v->name + "#" + str(i), VConst, arr->t, e->p);
						vi->index = i;
						obj.fields[str(i)] = vi;
						p.newVar = vi->copy();
						p.value = props(vi).value = values[i];
						for (auto e : cfor->exprs)
							compileAssign(e.v, new CodeValue(e.e));
					}
					return;
				}
				else {
					error("assert", -1);
				}
			}
			else {
				error("assert", -1);
			}
		}
		return;
	}
	else if (e->d->isUnop()) {
		//case CUnop(op, _):
		auto unop = dynamic_cast<CUnop*>(e->d);
		if (unop->op == CKill) {
			addAssign(nullptr, *compileValue(e), e->p);
			return;
		}
	}
	if (v == nullptr)
		error("assert ", -1); //+Type.enumConstructor(e.d);
	auto v2 = compileValue(v, true);
	auto e2 = compileValue(e);
	addAssign(v2, *e2, cur.pos);
}

void RuntimeCompiler::checkVars() {
	for (auto v : usedVars) {
		auto p = props(v);
		switch (v->kind) {
		case VVar:
			if (p.write != fullBits(v->type) )
				error(std::string("Some components of variable '") + v->name + "' are not written by vertex shader", v->pos);
			else if( p.write != 15 && cur.vertex ) {
				// force the output write
				padWrite(v);
			}
			break;
		}
	}
}

CodeValue* RuntimeCompiler::compileValue(CodeValue* e, bool isTarget) {
	CodeValueDecl* d = NULL;
	if (e->d->isConst()) {
		d = e->d;
	}
	else if (e->d->isVar()) {
		//case CVar(v, swiz):
		auto cvar = dynamic_cast<CVar*>(e->d);
		auto v2 = newVar(cvar->v, e->p);
		auto p = props(v2);
		if (isCond)
			d = new CConst(p.value);
		else {
			if( isTarget )
				p.write |= swizBits(cvar->swiz, v2->type);
			else
				p.read = true;
			d = new CVar(v2, cvar->swiz);
		}
	}
	else if (e->d->isOp()) {
		//case COp(op, e1, e2):
		auto cop = dynamic_cast<COp*>(e->d);
		d = makeOp(cop->op, cop->e1, cop->e2);
	}
	else if (e->d->isTex()) {
		//case CTex(v, acc, mode):
		auto ctex = dynamic_cast<CTex*>(e->d);
		auto v = newVar(ctex->v, e->p); // texture
		props(v).read = true;
		auto acc = compileValue(&ctex->acc);
		std::vector<TexFlag*> flags;
		for (auto m : ctex->mode)
			if (m.f->isFlag()) {
				//case CTFlag(f):
				flags.push_back(dynamic_cast<CTFlag*>(m.f)->t);
			}
			else if (m.f->isParam()) {
				//case CTParam(t, v):
				auto param = dynamic_cast<CTParam*>(m.f);
				auto c = compileCond(param->value);
				switch (param->t) {
				case PWrap:
					if (isTrue(c)) flags.push_back(new TWrap);
					break;
				case PMipMap:
					if (!c->isNull()) {
						if (isTrue(c)) flags.push_back(new TMipMapLinear);
						else flags.push_back(new TMipMapNearest);
					}
					break;
				case PFilter:
					if (!c->isNull()) {
						if (isTrue(c)) flags.push_back(new TFilterLinear);
						else flags.push_back(new TFilterNearest);
					}
					break;
				case PLodBias:
					if (c->isNull()) { }
					else if (c->isInt()) {
						flags.push_back(new TLodBias(static_cast<float>(dynamic_cast<ConstInt*>(c)->i)));
					}
					else if (c->isFloat()) {
						flags.push_back(new TLodBias(dynamic_cast<ConstFloat*>(c)->v));
					}
					else {
						error("assert", -1);
					}
				case PSingle:
					if (isTrue(c)) flags.push_back(new TSingle);
				}
			}
		std::vector<TexMode> mode;
		for (auto f : flags) {
			TexMode tm;
			tm.f = new CTFlag(f);
			tm.p = e->p;
			mode.push_back(tm);
		}
		d = new CTex(v, *acc, mode);
	}
	else if (e->d->isCond()) {
		//case CCond(c, eif, eelse):
		auto ccond = dynamic_cast<CCond*>(e->d);
		if (isTrue(compileCond(ccond->cond)))
			return compileValue(&ccond->eif);
		return compileValue(&ccond->eelse);
	}
	else if (e->d->isVector()) {
		//case CVector(vals):
		auto cvec = dynamic_cast<CVector*>(e->d);
		return new CodeValue(compileVector(cvec->vals, e->p));
	}
	else if (e->d->isUnop()) {
		//case CUnop(op, e):
		auto unop = dynamic_cast<CUnop*>(e->d);
		makeUnop(unop->op, unop->e);
	}
	else if (e->d->isSwiz()) {
		//case CSwiz(v, swiz):
		auto cswiz = dynamic_cast<CSwiz*>(e->d);
		auto v = compileValue(&cswiz->e, isTarget);
		// build swizzling
		if (v->d->isVar()) {
			//case CVar(v, s2):
			auto cvar = dynamic_cast<CVar*>(v->d);
			std::vector<Comp> ns;
			if (cvar->swiz.size() == 0)
				ns = cswiz->swiz;
			else {
				// combine swizzlings
				for (auto s : cswiz->swiz)
					ns.push_back(cvar->swiz[enumIndex(s)]);
			}
			CodeValue value;
			auto cvar2 = new CVar(cvar->v, ns);
			value.d = cvar2;
			value.t = Tools::makeFloat(cswiz->swiz.size());
			value.p = e->p;
			return new CodeValue(value);
		}
		else {
			CodeValue value;
			auto swiz = new CSwiz;
			swiz->e = cswiz->e;
			swiz->swiz = cswiz->swiz;
			value.d = swiz;
			value.t = Tools::makeFloat(cswiz->swiz.size());
			value.p = e->p;
			return new CodeValue(value);
		}
	}
	else if (e->d->isRow()) {
		//case CRow(v, index):
		auto crow = dynamic_cast<CRow*>(e->d);
		auto v = compileValue(&crow->e1);
		auto index = compileValue(&crow->e2);
		if (v->d->isVar()) {
			//case CVar(v, swiz):
			auto cvar = dynamic_cast<CVar*>(v->d);
			if (cvar->swiz.size() == 0)
				if (cvar->v->type->isArray()) {
					//case TArray(t, _):
					auto array = dynamic_cast<TArray*>(cvar->v->type);
					auto v = newVar(cvar->v, e->p);
					props(v).read = true;
					CodeValue value;
					auto access = new CAccess;
					access->v = v;
					access->idx = *index;
					value.d = access;
					value.t = array->t;
					value.p = e->p;
					return new CodeValue(value);
				}
		}
		error("assert row " /*+ crow->e1*/, -1);
	}
	else if (e->d->isAccess()) {
		//case CAccess(v, idx):
		auto access = dynamic_cast<CAccess*>(e->d);
		auto v = newVar(access->v, e->p);
		props(v).read = true;
		auto idx = compileValue(&access->idx);
		CodeValue value;
		auto caccess = new CAccess;
		caccess->v = access->v;
		caccess->idx = access->idx;
		value.d = caccess;
		value.t = e->t;
		value.p = e->p;
		return new CodeValue(value);
	}
	else if (e->d->isSubBlock()) {
		//case CSubBlock(tmp, v):
		auto subblock = dynamic_cast<CSubBlock*>(e->d);
		auto exprs = cur.exprs;
		cur.exprs.clear();
		for (auto e : subblock->tmpExpr)
			compileAssign(e.v, &e.e);
		auto tmp = cur.exprs;
		cur.exprs = exprs;
		CodeValue value;
		auto csubblock = new CSubBlock;
		csubblock->tmpExpr = tmp;
		csubblock->v = *compileValue(&subblock->v);
		value.d = csubblock;
		value.t = e->t;
		value.p = e->p;
		return new CodeValue(value);
	}
	else if (e->d->isField()) {
		//case CField(v, f):
		auto field = dynamic_cast<CField*>(e->d);
		auto v = compileValue(&field->e, isTarget);
		if (v->d->isVar()) {
			//case CVar(v, _):
			auto var = dynamic_cast<CVar*>(v->d);
			if (objectVars.find(var->v->id) == objectVars.end()) {
				Object o;
				o.v = var->v;
				objectVars[var->v->id] = o;
			}
			auto obj = objectVars[var->v->id];
			if (obj.fields.find(field->f) == obj.fields.end()) {
				auto vv = allocVar(std::string("$") + var->v->name + "." + field->f, var->v->kind, e->t, e->p);
				obj.fields[field->f] = vv;
			}
			auto v2 = obj.fields[field->f];
			CodeValue value;
			auto cvar = new CVar(v2, std::vector<Comp>());
			value.d = cvar;
			value.t = v2->type;
			value.p = e->p;
			return new CodeValue(value);
		}
		else if (v->d->isConst() && dynamic_cast<CConst*>(v->d)->c->isNull()) {
			return v;
		}
		else if (v->d->isConst() && dynamic_cast<CConst*>(v->d)->c->isObject()) {
			//case CConst(CObject(fl)):
			auto object = dynamic_cast<ConstObject*>(dynamic_cast<CConst*>(v->d)->c);
			auto val = object->fields[field->f];
			VarType* ft = new TNull;
			if (v->t->isObject()) {
				//case TObject(fl):
				auto object2 = dynamic_cast<TObject*>(v->t);
				for (auto fi : object2->fields)
					if (fi.name == field->f) {
						ft = fi.t;
						break;
					}
			}
			if (v == nullptr) { //?
				CodeValue value;
				auto cconst = new CConst(new ConstNull);
				value.d = cconst;
				value.t = new TNull;
				value.p = e->p;
				return new CodeValue(value);
			}
			else {
				CodeValue value;
				auto cconst = new CConst(val);
				value.d = cconst;
				value.t = ft;
				value.p = e->p;
				return new CodeValue(value);
			}
		}
		else {
			error("assert", -1);
		}
	}
	else {
		error("assert ", -1);//+Type.enumConstructor(e.d);
	}
	// translate the constant to the corresponding variable value
	if (!isConst)
		if (d->isConst()) {
			std::vector<float> floats;
			floats.push_back(floatValue(dynamic_cast<CConst*>(d)->c));
			d = allocConst(floats, e->p).d;
		}
	CodeValue value;
	value.d = d;
	value.t = e->t;
	value.p = e->p;
	return new CodeValue(value);
}

bool RuntimeCompiler::isTrue(Const* c) {
	if (c->isNull()) return false;
	if (c->isInt()) return dynamic_cast<ConstInt*>(c)->i != 0;
	if (c->isFloat()) return dynamic_cast<ConstFloat*>(c)->v != 0;
	if (c->isBool()) return dynamic_cast<ConstBool*>(c)->b;
	if (c->isFloats() || c->isObject() || c->isArray()) return true;
	throw std::exception();
}

Const* RuntimeCompiler::compileCond(CodeValue v) {
	auto old = isCond;
	auto oldC = isConst;
	isCond = true;
	isConst = true;
	auto v2 = compileValue(new CodeValue(v));
	isCond = old;
	isConst = oldC;
	if (v2->d->isConst()) {
		auto cconst = dynamic_cast<CConst*>(v2->d);
		return cconst->c;
	}
	else {
		error("assert", -1);
		return nullptr;
	}
}

Variable* RuntimeCompiler::newVar(Variable* v, Position p_) {
	if (isCond) {
		if (v->kind != VParam)
			error("assert", -1);
		return v;
	}
	if (v->id < 0) return v;
	auto p = props(v);
	if (p.newVar == nullptr) {
		auto kind = v->kind;
		if (kind == VParam) kind = VConst;
		Variable* var = new Variable;
		var->id = v->id;
		var->kind = kind;
		var->index = 0;
		var->name = v->name;
		var->type = v->type;
		var->pos = v->pos;
		p.newVar = var;
		p.isVertex = cur.vertex;
		usedVars.push_back(p.newVar);
	}
	return p.newVar;
}

Variable* RuntimeCompiler::allocVar(std::string name, VarKind k, VarType* t, Position p) {
	int id = -(varId + 1);
	varId++;
	Variable* v = new Variable;
	v->id = id;
	v->name = name;
	v->kind = k;
	v->type = t;
	v->index = -1;
	v->pos = p;
	return v;
}

void RuntimeCompiler::addAssign(CodeValue* v, CodeValue e, Position p) {
	Block block;
	block.v = v;
	block.e = e;
	cur.exprs.push_back(block);
}

void RuntimeCompiler::padWrite(Variable* v) {
	//if (!config.padWrites)
	//	return;
	// if we already have a partial "mov" copy, we can simply extend the writing on other components
	for (auto e : cur.exprs) {
		if (e.v == nullptr) continue;
		if (e.v->d->isVar()) {
			//case CVar(vv, sv):
			auto cvar = dynamic_cast<CVar*>(e.v->d);
			if (v == cvar->v && isGoodSwiz(cvar->swiz)) {
				if (e.e.d->isVar()) {
					//case CVar(v2, sv2):
					auto cvar2 = dynamic_cast<CVar*>(e.e.d);
					// only allow "mov" extension if we are sure that the variable is padded with "1"
					if (cvar2->v->kind == VInput || cvar2->v->kind == VVar) {
						// remove swizzle on write
						Variable* vn = v->copy();
						props(v).ref = vn;
						vn->type = new TFloat4;
						e.v->d = new CVar(vn, std::vector<Comp>());
						// remove swizzle on read
						if (isGoodSwiz(cvar2->swiz)) {
							auto p = props(cvar2->v);
							auto vn2 = p.ref;
							// allow several writes
							if (vn2 == nullptr) {
								vn2 = cvar2->v->copy();
								vn2->type = new TFloat4;
								p.ref = vn2;
							}
							e.e.d = new CVar(vn2, std::vector<Comp>());
						} else
						// or pad swizzle on input var
							while (cvar2->swiz.size() < 4 )
								cvar2->swiz.push_back(X);
						// adjust types
						e.e.t = e.v->t = new TFloat4;
						return;
					}
				}
			}
		}
	}
	// store 1-values into remaining components
	std::vector<Comp> missing;
	std::vector<float> ones;
	for (int i = Tools::floatSize(v->type); i < 4; ++i) {
		missing.push_back((Comp)i);
		ones.push_back(1.f);
	}
	auto c = allocConst(ones, v->pos);
	CodeValue value;
	auto var = new CVar(v, missing);
	value.d = var;
	value.t = Tools::makeFloat(missing.size());
	value.p = v->pos;
	addAssign(new CodeValue(value), c, v->pos);
}

bool RuntimeCompiler::isGoodSwiz(std::vector<Comp> s) {
	if (s.size() == 0) return true;
	int cur = 0;
	for (auto x : s)
		if (enumIndex(x) != cur++ )
			return false;
	return true;
}

CodeValue RuntimeCompiler::allocConst(std::vector<float> cvals, Position p) {
	std::vector<Comp> swiz;
	swiz.push_back(X);
	swiz.push_back(Y);
	swiz.push_back(Z);
	swiz.push_back(W);
	std::vector<Comp> dup;
	std::vector<float> dvals;

	// de-dupe the input values
	for (unsigned i = 0; i < cvals.size(); ++i) {
		bool found = false;
		for (unsigned j = 0; j < dvals.size(); ++j) {
			if (cvals[i] == dvals[j]) {
				dup.push_back(swiz[j]);
				found = true;
				break;
			}
		}
		if (!found) {
			dup.push_back(swiz[dvals.size()]);
			dvals.push_back(cvals[i]);
		}
	}
		
	// find an already existing constant
	for (unsigned index = 0; index < cur.consts.size(); ++index) {
		auto c = cur.consts[index];
		std::vector<Comp> s;
		for (auto v : dvals) {
			for (unsigned i = 0; i < c.size(); ++i)
				if (c[i] == v) {
					s.push_back(swiz[i]);
					break;
				}
		}
		if (s.size() == dvals.size())
			return makeConst(index, mergeSwiz(s, dup), p);
	}

	// find an empty slot
	for (unsigned i = 0; i < cur.consts.size(); ++i) {
		auto c = cur.consts[i];
		if (c.size() + dvals.size() <= 4 ) {
			std::vector<Comp> s;
			for (auto v : dvals) {
				s.push_back(swiz[c.size()]);
				c.push_back(v);
			}
			return makeConst(i, mergeSwiz(s, dup), p);
		}
	}
	auto index = cur.consts.size();
	cur.consts.push_back(dvals);
	return makeConst(index, mergeSwiz(splice(swiz, 0, dvals.size()), dup), p);
}

CodeValue RuntimeCompiler::makeConst(int index, std::vector<Comp> swiz, Position p) {
	auto v = allocVar(std::string("$c") + str(index), VConst, new TFloat4, p);
	constVars.push_back(v);
	v->index = index;
	CodeValue value;
	auto cvar = new CVar(v, swiz);
	value.d = cvar;
	value.t = Tools::makeFloat(swiz.size());
	value.p = p;
	return value;
}
	
std::vector<Comp> RuntimeCompiler::mergeSwiz(std::vector<Comp> from, std::vector<Comp> to) {
	std::vector<Comp> out;
	for (auto s : to)
		out.push_back(from[enumIndex(s)]);
	return out;
}

int RuntimeCompiler::swizBits(std::vector<Comp> s, VarType* t) {
	if (s.size() == 0) return fullBits(t);
	int b = 0;
	for (auto x : s)
		b |= 1 << enumIndex(x);
	return b;
}

int RuntimeCompiler::fullBits(VarType* t) {
	return (1 << Tools::floatSize(t)) - 1;
}

namespace {
	template<class F> CConst* const1(Const* c1, Const* c2, F f) {
		return new CConst(new ConstFloat(static_cast<float>(f(floatValue(c1), floatValue(c2)))));
	}
}

CodeValueDecl* RuntimeCompiler::makeOp(CodeOp op, CodeValue e1_, CodeValue e2_) {
	CodeValue e1 = compileConstValue(e1_);
	CodeValue e2 = compileConstValue(e2_);
	
	Const* c1;
	if (e1.d->isConst()) c1 = dynamic_cast<CConst*>(e1.d)->c;
	else c1 = nullptr;

	Const* c2;
	if (e2.d->isConst()) c2 = dynamic_cast<CConst*>(e2.d)->c;
	else c2 = nullptr;

	if (c1 != nullptr && c2 != nullptr) {
		switch (op) {
		case CEq:
			return new CConst(new ConstBool(c1->equals(c2)));
		case CNeq:
			return new CConst(new ConstBool(!c1->equals(c2)));
		case CLt: {
			auto c = compare(c1, c2);
			if (c != nullptr)
				return new CConst(new ConstBool(c < 0));
			break;
		}
		case CGt: {
			auto c = compare(c1, c2);
			if (c != nullptr)
				return new CConst(new ConstBool(c > 0));
			break;
		}
		case CLte: {
			auto c = compare(c1, c2);
			if (c != nullptr)
				return new CConst(new ConstBool(c <= 0));
			break;
		}
		case CGte: {
			auto c = compare(c1, c2);
			if (c != nullptr)
				return new CConst(new ConstBool(c >= 0));
			break;
		}
		case COr:
			return new CConst(new ConstBool(isTrue(c1) || isTrue(c2)));
		case CAnd:
			return new CConst(new ConstBool(isTrue(c1) && isTrue(c2)));
		case CAdd: return const1(c1, c2, [](float a, float b) { return a + b; });//function(a, b) return a + b);
		case CSub: return const1(c1, c2, [](float a, float b) { return a - b; });
		case CMul:
		case CDot:
			return const1(c1, c2, [](float a, float b) { return a * b; });
		case CDiv: return const1(c1, c2, [](float a, float b) { return a / b; });
		case CPow: return const1(c1, c2, [](float a, float b) { return pow(a, b); });
		case CMod: return const1(c1, c2, [](float a, float b) { return (int)a % (int)b; });
		case CMin: return const1(c1, c2, [](float a, float b) { return a < b ? a : b; });
		case CMax: return const1(c1, c2, [](float a, float b) { return a > b ? a : b; });
		case CCross:
		case CInterval: throw "assert";
		}
	}
	// force const building
	e1 = compileValueForce(e1);
	e2 = compileValueForce(e2);
	return new COp(op, e1, e2);
}

CodeValue RuntimeCompiler::compileConstValue(CodeValue v) {
	if (v.d->isConst()) {
		return v;
	}
	else {
		auto old = isConst;
		isConst = true;
		auto v2 = compileValue(&v);
		isConst = old;
		return *v2;
	}
}

CodeValue RuntimeCompiler::compileValueForce(CodeValue v) {
	auto old = isConst;
	isConst = false;
	auto v2 = compileValue(&v);
	isConst = old;
	return *v2;
}

CodeValue RuntimeCompiler::compileVector(std::vector<CodeValue> values, Position p) {
	if (values.size() == 0 || values.size() > 4 )
		error("assert", -1);
	std::vector<float> consts;
	std::vector<CodeValue> exprs;
	for (unsigned i = 0; i < values.size(); ++i) {
		auto e = compileConstValue(values[i]);
		if (e.d->isConst()) {
			auto cconst = dynamic_cast<CConst*>(e.d);
			consts.push_back(floatValue(cconst->c));
		}
		else exprs[i] = e;
	}
	// all values are constants
	if (consts.size() == values.size())
		return allocConst(consts, p);
	// declare a new temporary
	auto v = allocVar("$tmp" + str(varProps.size()), VTmp, Tools::makeFloat(values.size()), p);
	usedVars.push_back(v);
	// assign expressions first
	auto old = cur.exprs;
	cur.exprs.clear();
	std::vector<Comp> write;
	for (unsigned i = 0; i < values.size(); ++i) {
		CodeValue* e = nullptr;
		if (i < exprs.size()) e = new CodeValue(exprs[i]);
		std::vector<Comp> xyzw;
		xyzw.push_back(X);
		xyzw.push_back(Y);
		xyzw.push_back(Z);
		xyzw.push_back(W);
		auto c = xyzw[i];
		if (e == nullptr) {
			write.push_back(c);
			continue;
		}
		CodeValue value;
		std::vector<Comp> swiz;
		swiz.push_back(c);
		auto cvar = new CVar(v, swiz);
		value.d = cvar;
		value.t = new TFloat;
		value.p = e->p;
		addAssign(new CodeValue(value), *e, p);
	}
	// assign constants if any
	if (write.size() > 0) {
		if (isUnsupportedWriteMask(write)) {
			for (unsigned i = 0; i < write.size(); ++i) {
				CodeValue value;
				std::vector<Comp> swiz;
				swiz.push_back(write[i]);
				auto cvar = new CVar(v, swiz);
				value.d = cvar;
				value.t = new TFloat;
				value.p = p;
				std::vector<float> floats;
				floats.push_back(consts[i]);
				addAssign(new CodeValue(value), allocConst(floats, p), p);
			}
		}
		else {
			CodeValue value;
			auto cvar = new CVar(v, write);
			value.d = cvar;
			value.t = Tools::makeFloat(write.size());
			value.p = p;
			addAssign(new CodeValue(value), allocConst(consts, p), p);
		}
	}
	// return temporary
	CodeValue ret;
	auto cvar = new CVar(v, std::vector<Comp>());
	ret.d = cvar;
	ret.t = v->type;
	ret.p = p;

	CodeValue sub;
	auto subblock = new CSubBlock;
	subblock->tmpExpr = cur.exprs;
	subblock->v = ret;
	sub.d = subblock;
	sub.t = ret.t;
	sub.p = p;

	cur.exprs = old;
	return sub;
}

namespace {
	template<class F> CConst* const2(Const* c, F f) {
		return new CConst(new ConstFloat(static_cast<float>(f(floatValue(c)))));
	}
}

CodeValueDecl* RuntimeCompiler::makeUnop(CodeUnop op, CodeValue e_) {
	auto e = compileConstValue(e_);
	if (e.d->isConst()) {
		auto constant = dynamic_cast<CConst*>(e.d);
		auto c = constant->c;
		switch( op ) {
		case CNorm:
		case CTrans:
		case CKill:
			break;
		case CInt: return const2(c, [](float x) { return int(x); });
		case CFrac: return const2(c, [](float x) { return (int)x % 1; });
		case CExp: return const2(c, [](float x) { return exp(x); });
		case CAbs: return const2(c, [](float x) { return fabs(x); });
		case CRsq: return const2(c, [](float x) { return 1 / sqrt(x); });
		case CRcp: return const2(c, [](float x) { return 1 / x; });
		case CLog: return const2(c, [](float x) { return log(x); });
		case CSqrt: return const2(c, [](float x) { return sqrt(x); });
		case CSin: return const2(c, [](float x) { return sin(x); });
		case CCos: return const2(c, [](float x) { return cos(x); });
		case CSat: return const2(c, [](float x) { return cos(x); });
		case CNeg: return const2(c, [](float x) { return -x; });
		case CLen: return const2(c, [](float x) { return x; });
		case CNot:
			return new CConst(new ConstBool(!isTrue(c)));
		}
	}
	e = compileValueForce(e);
	return new CUnop(op, e);
}

int* RuntimeCompiler::compare(Const* c1, Const* c2) {
	float f1, f2;
	
	if (c1->isFloat()) f1 = dynamic_cast<ConstFloat*>(c1)->v;
	else if (c1->isInt()) f1 = static_cast<float>(dynamic_cast<ConstInt*>(c1)->i);
	else return nullptr;

	if (c2->isFloat()) f2 = dynamic_cast<ConstFloat*>(c2)->v;
	else if (c2->isInt()) f2 = static_cast<float>(dynamic_cast<ConstInt*>(c2)->i);
	else return nullptr;
	
	return (f1 < f2) ? new int(-1) : ((f1 > f2) ? new int(1) : new int(0));
}

bool RuntimeCompiler::isUnsupportedWriteMask(std::vector<Comp> s) {
	return s.size() != 0 && s.size() > 1 && (s[0] != X || s[1] != Y || (s.size() > 2 && (s[2] != Z || (s.size() > 3 && s[3] != W))));
}
