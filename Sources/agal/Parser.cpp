#include "Parser.h"
#include "common/angleutils.h"
#include "compiler/debug.h"
#include "compiler/InfoSink.h"
#include "compiler/UnfoldShortCircuit.h"
#include "compiler/SearchSymbol.h"
#include "compiler/DetectDiscontinuity.h"
#include <map>

using namespace agal;

namespace {
	std::string str(int i) {
		char buffer[20];
		snprintf(buffer, sizeof(buffer), "%d", i);
		return buffer;
	}
}

ParsedHxsl Parser::parse(TIntermNode* e) {
	TIntermAggregate* aggregate = e->getAsAggregate();
	for (auto it = aggregate->getSequence().begin(); it != aggregate->getSequence().end(); ++it) {
		parseDecl(*it);
	}
	if (main == nullptr) error("Missing main function", e->getLine().first_line);
	allowReturn = false;
	ParsedCode s = buildShader(main);
	std::map<std::string, ParsedCode> help;
	allowReturn = true;
	for (auto it = helpers.begin(); it != helpers.end(); ++it) {
		help[it->first] = buildShader(it->second);
	}

	ParsedHxsl hxsl;
	hxsl.shader = s;
	hxsl.globals = globals;
	hxsl.pos = e->getLine().first_line;
	hxsl.helpers = help;
	return hxsl;
}

ParsedCode Parser::buildShader(TIntermAggregate* f) {
	cur.pos = f->getLine().first_line;
	cur.args.clear();
	cur.exprs.clear();
	Position pos = f->getLine().first_line;
	TIntermSequence& sequence = f->getSequence();
	TIntermSequence& arguments = sequence[0]->getAsAggregate()->getSequence();
	for (auto it = arguments.begin(); it != arguments.end(); ++it) {
		TIntermSymbol* symbol = (*it)->getAsSymbolNode();
		//if (symbol->getType().getStruct()) {
		//	addConstructor(symbol->getType(), scopedStruct(symbol->getType().getTypeName()), NULL);
		//}
		cur.args.push_back(allocVar(symbol->getQualifierString(), symbol->getTypePointer(), VUnknown, pos));
	}
	parseExpr(sequence[1]->getAsAggregate()->getSequence());
	return cur;
}

ParsedVar Parser::allocVar(const char* v, TType* t, VarKind k, Position p) {
	ParsedVar var;
	var.n = v;
	var.k = k;
	var.t = t == nullptr ? new TNull : getType(t, p);
	var.p = p;
	return var;
}

ParsedVar Parser::allocVarDecl(std::string v, TType* t, Position p) {
	VarKind kind = VUnknown;
	switch (t->getQualifier()) {
	case EvqTemporary:     // For temporaries (within a function), read/write
	case EvqGlobal:        // For globals read/write
		break;
	case EvqConst:         // User defined constants and non-output parameters in functions
		kind = VConst;
		break;
	case EvqAttribute:     // Readonly
		kind = VInput;
		break;
	case EvqVaryingIn:     // readonly, fragment shaders only
	case EvqVaryingOut:    // vertex shaders only  read/write
	case EvqInvariantVaryingIn:     // readonly, fragment shaders only
	case EvqInvariantVaryingOut:    // vertex shaders only  read/write
		kind = VVar;
		break;
	case EvqUniform:       // Readonly, vertex and fragment
		kind = VConst;
		break;

	// parameters
	case EvqIn:
	case EvqOut:
	case EvqInOut:
	case EvqConstReadOnly:

	// built-ins written by vertex shader
	case EvqPosition:
	case EvqPointSize:

	// built-ins read by fragment shader
	case EvqFragCoord:
	case EvqFrontFacing:
	case EvqPointCoord:

	// built-ins written by fragment shader
	case EvqFragColor:
	case EvqFragData:
	case EvqFragDepth:
		break;
	}
	ParsedVar var = allocVar(v.c_str(), t, kind, p);
	//VarKind
	return var;
}

void Parser::parseDecl(TIntermNode* e) {
	auto aggregate = e->getAsAggregate();
	switch (aggregate->getOp()) {
	case EOpDeclaration: {
		auto v = allocVarDecl(aggregate->getSequence()[0]->getAsSymbolNode()->getSymbol().c_str(), aggregate->getSequence()[0]->getAsTyped()->getTypePointer(), e->getLine().first_line);
		globals.push_back(v);
		return;
	}
	case EOpFunction:
		if (aggregate->getName() == "main(") main = aggregate;
		else {
			if (helpers.find(aggregate->getName().c_str()) != helpers.end()) error(std::string("Duplicate function '") + aggregate->getName().c_str() + "'", e->getLine().first_line);
			helpers[aggregate->getName().c_str()] = aggregate;
		}
		return;
	}
}

VarType* Parser::getType(TType* t, Position pos) {
	switch (t->getBasicType()) {
	case EbtVoid:
		return new TNull;
	case EbtFloat:
		if (t->isMatrix()) {
			auto m = new TMatrix;
			m->c = 4;
			m->r = 4;
			return m;
		}
		else {
			switch (t->getNominalSize()) {
			case 1:
				return new TFloat;
			case 2:
				return new TFloat2;
			case 3:
				return new TFloat3;
			case 4:
				return new TFloat4;
			default:
				error("Unknown type", pos);
			}
		}
	case EbtInt:
		return new TInt;
	case EbtBool:
		return new TBool;
	case EbtGuardSamplerBegin:  // non type:  see implementation of IsSampler()
	case EbtSampler2D:
	case EbtSamplerCube:
	case EbtSamplerExternalOES:  // Only valid if OES_EGL_image_external exists.
	case EbtSampler2DRect:       // Only valid if GL_ARB_texture_rectangle exists.
	case EbtGuardSamplerEnd:    // non type:  see implementation of IsSampler()
	case EbtStruct:
	case EbtAddress:            // should be deprecated??
	case EbtInvariant:
		break;
	}
	/*switch(t) {
	case TPath(p):
		if( p.params.length == 1 ) {
			switch( p.params[0] ) {
			case TPExpr(e):
				switch( e.expr ) {
				case EConst(c):
					switch( c ) {
					case CInt(i):
						p.params = [];
						var i = Std.parseInt(i);
						if( i > 0 )
							return TArray(getType(t,pos), i);
					default:
					}
				default:
				}
			case TPType(t) if( p.pack.length == 0 && p.name == "Array" && p.sub == null ):
				return TArray(getType(t, pos), 0); // 0 length is for runtime constant
			default:
			}
		}
		if( p.pack.length > 0 || p.sub != null || p.params.length > 0 )
			error("Unsupported type", pos);
		return switch( p.name ) {
		case "Bool": TBool;
		case "Float": TFloat;
		case "Float2": TFloat2;
		case "Float3": TFloat3;
		case "Float4": TFloat4;
		case "Matrix", "M44": TMatrix(4, 4, { t : null } );
		case "M33": TMatrix(3, 3, { t : null } );
		case "M34": TMatrix(3, 4, { t : null } );
		case "M43": TMatrix(4, 3, { t : null } );
		case "Texture": TTexture(false);
		case "CubeTexture": TTexture(true);
		case "Color", "Int": TInt;
		default:
			error("Unknown type '" + p.name + "'", pos);
		}
	
	}*/
	return new TFloat;
}

ParsedValue Parser::parseValue(TIntermTyped* e) {
	auto binary = e->getAsBinaryNode();
	auto symbol = e->getAsSymbolNode();
	auto aggregate = e->getAsAggregate();
	TIntermConstantUnion* constunion = e->getAsConstantUnion();
	if (binary != nullptr) {
		CodeOp op;
		switch (binary->getOp()) {
		case EOpAdd:
			op = CAdd;
			break;
		case EOpSub:
			op = CSub;
			break;
		case EOpMul:
			op = CMul;
			break;
		case EOpDiv:
			op = CDiv;
			break;
		case EOpLessThan:
			op = CLt;
			break;
		case EOpGreaterThan:
			op = CGt;
			break;
		case EOpLessThanEqual:
			op = CLte;
			break;
		case EOpGreaterThanEqual:
			op = CGte;
			break;
		case EOpEqual:
			op = CEq;
			break;
		case EOpNotEqual:
			op = CNeq;
			break;
		case EOpLogicalOr:
			op = COr;
			break;
		case EOpLogicalAnd:
			op = COr;
			break;
		case EOpMatrixTimesVector:
			op = CMul;
			break;
		case EOpVectorSwizzle: {
			auto field = new PField;
			field->e = parseValue(binary->getLeft());
			for (unsigned i = 0; i < binary->getRight()->getAsAggregate()->getSequence().size(); ++i) {
				int num = binary->getRight()->getAsAggregate()->getSequence()[i]->getAsConstantUnion()->getUnionArrayPointer()->getIConst();
				switch (num) {
				case 0:
					field->field += "x";
					break;
				case 1:
					field->field += "y";
					break;
				case 2:
					field->field += "z";
					break;
				case 3:
					field->field += "w";
					break;
				}
			}
			ParsedValue value;
			value.v = field;
			value.p = binary->getLine().first_line;
			return value;
		}
		//case OpMod: CMod;
		//case OpInterval: CInterval;
		default:
			error("Unsupported operation", e->getLine().first_line);
		}
		ParsedValue value;
		value.v = new POp(op, parseValue(binary->getLeft()), parseValue(binary->getRight()));
		value.p = e->getLine().first_line;
		return value;
	}
	else if (symbol != nullptr) {
		ParsedValue value;
		auto var = new PVar;
		var->v = symbol->getSymbol().c_str();
		value.v = var;
		value.p = e->getLine().first_line;
		return value;
	}
	else if (constunion != nullptr) {
		ParsedValue value;
		value.p = constunion->getLine().first_line;
		auto constVar = new PConst;
		switch (constunion->getType().getBasicType()) {
		case EbtFloat:	
			constVar->c = new ConstFloat(constunion->getUnionArrayPointer()->getFConst());
			break;
		default:
			error("Unknown type", constunion->getLine().first_line);
		}
		value.v = constVar;
		return value;
	}
	else if (aggregate != nullptr) {
		switch (aggregate->getOp()) {
		case EOpConstructVec4: {
			auto vector = new PVector;
			for (size_t i = 0; i < aggregate->getSequence().size(); ++i) {
				vector->el.push_back(parseValue(aggregate->getSequence()[i]->getAsTyped()));
			}
			ParsedValue value;
			value.v = vector;
			value.p = aggregate->getLine().first_line;
			return value;
		}
		default:
			error("Unknown aggregate", constunion->getLine().first_line);
		}
	}

	error("Unsupported value expression", e->getLine().first_line);
	ParsedValue value;
	value.v = nullptr;
	value.p = e->getLine().first_line;
	return value;

	/*switch (e.expr) {
	case EField(ef, s):
		return { v : PField(parseValue(ef),s), p : e.pos };
	case EConst(c):
		switch( c ) {
		case CIdent(i) #if !haxe3 , CType(i) #end:
			switch( i ) {
			case "null":
				return { v : PConst(CNull), p : e.pos };
			case "true":
				return { v : PConst(CBool(true)), p : e.pos };
			case "false":
				return { v : PConst(CBool(false)), p : e.pos };
			default:
				return { v : PVar(i), p : e.pos };
			}
		case CInt(v):
			return { v : PConst(CInt(Std.parseInt(v))), p : e.pos };
		case CFloat(f):
			return { v : PConst(CFloat(Std.parseFloat(f))), p : e.pos };
		default:
		}
	case EUnop(OpNeg, _, { expr : EConst(CInt(v)) } ):
		return { v : PConst(CInt(-Std.parseInt(v))), p : e.pos };
	case EUnop(op, _, e1):
		var op = switch( op ) {
		case OpNeg: CNeg;
		case OpNot: CNot;
		default: error("Unsupported operation", e.pos);
		}
		return { v : PUnop(op,parseValue(e1)), p : e.pos };
	case ECall(c, params):
		switch( c.expr ) {
		case EField(v, f):
			return makeCall(f, [v].concat(params), e.pos);
		case EConst(c):
			switch( c ) {
			case CIdent(i) #if !haxe3 , CType(i) #end:
				return makeCall(i, params, e.pos);
			default:
			}
		default:
		}
	case EArrayDecl(values):
		var vl = [];
		for( v in values )
			vl.push(parseValue(v));
		return { v : PVector(vl), p : e.pos };
	case EParenthesis(k):
		var v = parseValue(k);
		v.p = e.pos;
		return v;
	case EIf(ec, eif, eelse), ETernary(ec,eif,eelse):
		var vcond = parseValue(ec);
		var vif = parseValue(eif);
		if( eelse == null ) error("'if' needs an 'else'", e.pos);
		var velse = parseValue(eelse);
		return { v : PCond(vcond, vif, velse), p : e.pos };
	case EArray(e1, e2):
		var e1 = parseValue(e1);
		var e2 = parseValue(e2);
		return { v : PRow(e1, e2), p : e.pos };
	default:
	}
	*/
}

void Parser::parseExpr(TIntermSequence& e) {
	for (auto it = e.begin(); it != e.end(); ++it) {
		auto aggregate = (*it)->getAsAggregate();
		auto binop = (*it)->getAsBinaryNode();
		if (aggregate != nullptr) {
			switch (aggregate->getOp()) {
			case EOpFunctionCall: {
				ParsedExpr expr;
				expr.v = nullptr;
				ParsedValue value;
				value.v = nullptr;
				value.p = aggregate->getLine().first_line;
				std::vector<TIntermTyped*> args;
				for (auto it2 = aggregate->getSequence().begin(); it2 != aggregate->getSequence().end(); ++it2) {
					args.push_back((*it2)->getAsTyped());
				}
				expr.e = new ParsedValue(makeCall(aggregate->getName().c_str(), args, aggregate->getLine().first_line));
				expr.p = aggregate->getLine().first_line;
				cur.exprs.push_back(expr);
				break;
			}
			case EOpDeclaration: {
				PLocal* local = new PLocal;
				auto var = dynamic_cast<TIntermBinary*>(aggregate->getSequence()[0]);
				if (var->getOp() != EOpInitialize) error("Expected initialization.", var->getLine().first_line);
				local->v = allocVar(var->getLeft()->getAsSymbolNode()->getSymbol().c_str(), var->getLeft()->getTypePointer(), VTmp, var->getLine().first_line);
				
				ParsedExpr expr;
				ParsedValue* value = new ParsedValue;
				value->v = local;
				value->p = var->getLine().first_line;
				expr.v = value;
				expr.e = new ParsedValue(parseValue(var->getRight()));
				expr.p = var->getLine().first_line;
				cur.exprs.push_back(expr);
				break;
			}
			default:
				break;
			}
		}
		else if (binop != nullptr) {
			switch (binop->getOp()) {
			case EOpAssign:
				addAssign(parseValue(binop->getLeft()), parseValue(binop->getRight()), binop->getLine().first_line);
				break;
			case EOpAddAssign:
		
				break;
			case EOpSubAssign:
		
				break;
			case EOpMulAssign:
		
				break;
			case EOpVectorTimesScalarAssign:
		
				break;
			case EOpMatrixTimesScalarAssign:
		
				break;
			case EOpVectorTimesMatrixAssign:
	
				break;
			case EOpMatrixTimesMatrixAssign:
			//case OpAssignOp(op):
			//	addAssign(parseValue(e1), parseValue( { expr : EBinop(op, e1, e2), pos : e.pos } ), e.pos);
				break;
			default:
				error("Operation should have side-effects", binop->getLine().first_line);
			}
		}

			/*switch (e.expr) {
	case EBlock(el):
		auto eold = cur.exprs;
		auto old = allowReturn;
		auto last = el[el.length - 1];
		cur.exprs = [];
		for (e in el) {
			allowReturn = old && (e == last);
			parseExpr(e);
		}
		allowReturn = old;
		eold.push({ v : null, e : { v : PBlock(cur.exprs), p : e.pos }, p : e.pos });
		cur.exprs = eold;
	case EIf(cond, eif, eelse):
		var pcond = parseValue(cond);

		var eold = cur.exprs;
		cur.exprs = [];
		parseExpr(eif);
		var pif = { v : PBlock(cur.exprs), p : eif.pos };
			
		var pelse = null;
		if( eelse != null ) {
			cur.exprs = [];
			parseExpr(eelse);
			pelse = { v : PBlock(cur.exprs), p : eelse.pos };
		}
		cur.exprs = eold;
		cur.exprs.push( {v:null, e: { v:PIf(pcond, pif, pelse), p : e.pos }, p : e.pos} );
	case EFor(it, expr):
		var iter = null, vname = null;
		switch( it.expr ) {
		case EIn(v,it):
			switch( v.expr ) {
			case EConst(c):
				switch( c ) {
				case CIdent(i) #if !haxe3 , CType(i) #end: vname = i;
				default:
				}
			default:
			}
			iter = parseValue(it);
		default:
		}
		if( vname == null )
			error("For should be in the form for( x in it )", it.pos);

		var old = cur.exprs;
		cur.exprs = [];
		parseExpr(expr);
		var pexpr = { v : PBlock(cur.exprs), p : expr.pos };
		cur.exprs = old;
		cur.exprs.push( {v : null, e:{v:PFor(vname, iter, pexpr), p:e.pos}, p:e.pos } );
	case EReturn(r):
		if( r == null ) error("Return must return a value", e.pos);
		if( !allowReturn ) error("Return only allowed as final expression in helper methods", e.pos);
		var v = parseValue(r);
		cur.exprs.push( { v : null, e : { v : PReturn(v), p : e.pos }, p : e.pos } );
	default:
		error("Unsupported expression", e.pos);
	}*/
	}
}

ParsedValue Parser::makeUnop(CodeUnop op, ParsedValue e, Position p) {
	ParsedValue value;
	value.v = new PUnop(op, e);
	value.p = p;
	return value;
}

ParsedValue Parser::makeOp(CodeOp op, ParsedValue e1, ParsedValue e2, Position p) {
	ParsedValue value;
	value.v = new POp(op, e1, e2);
	value.p = p;
	return value;
}

ParsedValue Parser::makeCall(std::string n, std::vector<TIntermTyped*> params, Position p) {
	if (helpers.find(n) != helpers.end()) {
		std::vector<ParsedValue> vl;
		for (auto it = params.begin(); it != params.end(); ++it)
			vl.push_back(parseValue(*it));
		ParsedValue value;
		auto call = new PCall;
		call->n = n;
		call->vl = vl;
		value.v = call;
		value.p = p;
		return value;
	}

	// texture handling
	/*if( n == "get" && params.length >= 2 ) {
		var v = parseValue(params.shift());
		var v = switch( v.v ) {
		case PVar(v): v;
		default: error("get should only be used on a single texture variable", v.p);
		};
		var t = parseValue(params.shift());
		var flags = [];
		var idents = ["mm_no","mm_near","mm_linear","wrap","clamp","nearest","linear","single"];
		var values = [TMipMapDisable,TMipMapNearest,TMipMapLinear,TWrap,TClamp,TFilterNearest,TFilterLinear,TSingle];
		var targets = ["mipmap", "wrap", "filter", "lod"];
		var targetValues = [PMipMap, PWrap, PFilter, PLodBias];
		for( p in params ) {
			switch( p.expr ) {
			case EBinop(OpAssign, { expr : EConst(CIdent(sflag)), pos : fpos }, e2):
				var ip = Lambda.indexOf(targets, sflag);
				if( ip >= 0 ) {
					flags.push({ f : PTParam(targetValues[ip],parseValue(e2)), p : p.pos });
					continue;
				}
				error("Invalid parameter, should be "+targets.join("|"), fpos);
			case EConst(CIdent(sflag)):
				var ip = Lambda.indexOf(idents, sflag);
				if( ip >= 0 ) {
					flags.push({ f : PTFlag(values[ip]), p : p.pos });
					continue;
				}
			case ECall({ expr : EConst(CIdent("lod")) }, [{ expr : EConst(c) }]):
				switch( c ) {
				case CInt(v), CFloat(v):
					flags.push({ f : PTFlag(TLodBias(Std.parseFloat(v))), p : p.pos });
					continue;
				default:
				}
			default:
			}
			error("Invalid parameter, should be "+idents.join("|"), p.pos);
		}
		return { v : PTex(v, t, flags), p : p };
	}*/
	// build operation
	std::vector<ParsedValue> v;
	for (auto it = params.begin(); it != params.end(); ++it)
		v.push_back(parseValue(*it));
	
#define checkParams(k) if (params.size() < k) error(n + " requires " + str(k) + " parameters", p);

	if      (n == "inv" || n == "rcp")                       { checkParams(1); return makeUnop(CRcp, v[0], p);   }
	else if (n == "sqt" || n == "sqrt")                      { checkParams(1); return makeUnop(CSqrt, v[0], p);  }
	else if (n == "rsq" || n == "rsqrt")                     { checkParams(1); return makeUnop(CRsq, v[0], p);   }
	else if (n == "log")                                     { checkParams(1); return makeUnop(CLog, v[0], p);   }
	else if (n == "exp")                                     { checkParams(1); return makeUnop(CExp, v[0], p);   }
	else if (n == "len" || n == "length")                    { checkParams(1); return makeUnop(CLen, v[0], p);   }
	else if (n == "sin")                                     { checkParams(1); return makeUnop(CSin, v[0], p);   }
	else if (n == "cos")                                     { checkParams(1); return makeUnop(CCos, v[0], p);   }
	else if (n == "abs")                                     { checkParams(1); return makeUnop(CAbs, v[0], p);   }
	else if (n == "neg")                                     { checkParams(1); return makeUnop(CNeg, v[0], p);   }
	else if (n == "sat" || n == "saturate")                  { checkParams(1); return makeUnop(CSat, v[0], p);   }
	else if (n == "frc" || n == "frac")                      { checkParams(1); return makeUnop(CFrac, v[0], p);  }
	else if (n == "int")                                     { checkParams(1); return makeUnop(CInt,v[0], p);    }
	else if (n == "nrm" || n ==  "norm" || n == "normalize") { checkParams(1); return makeUnop(CNorm, v[0], p);  }
	else if (n == "trans" || n == "transpose")               { checkParams(1); return makeUnop(CTrans, v[0], p); }

	else if (n == "add")                                          { checkParams(2); return makeOp(CAdd, v[0], v[1], p);   }
	else if (n == "sub")                                          { checkParams(2); return makeOp(CSub, v[0], v[1], p);   }
	else if (n == "mul")                                          { checkParams(2); return makeOp(CMul, v[0], v[1], p);   }
	else if (n == "div")                                          { checkParams(2); return makeOp(CDiv, v[0], v[1], p);   }
	else if (n == "pow")                                          { checkParams(2); return makeOp(CPow, v[0], v[1], p);   }
	else if (n == "min")                                          { checkParams(2); return makeOp(CMin, v[0], v[1], p);   }
	else if (n == "max")                                          { checkParams(2); return makeOp(CMax, v[0], v[1], p);   }
	else if (n == "mod")                                          { checkParams(2); return makeOp(CMod, v[0], v[1], p);   }
	else if (n == "dp" || n == "dp3" || n == "dp4" || n == "dot") { checkParams(2); return makeOp(CDot, v[0], v[1], p);   }
	else if (n == "crs" || n == "cross")                          { checkParams(2); return makeOp(CCross, v[0], v[1], p); }

	else if (n == "lt" || n == "slt")  { checkParams(2); return makeOp(CLt, v[0], v[1], p);  }
	else if (n == "gte" || n == "sge") { checkParams(2); return makeOp(CGte, v[0], v[1], p); }
	else if (n == "gt" || n == "sgt")  { checkParams(2); return makeOp(CGt, v[0], v[1], p);  }
	else if (n == "lte" || n == "sle") { checkParams(2); return makeOp(CLte, v[0], v[1], p); }
	else if (n == "eq" || n == "seq")  { checkParams(2); return makeOp(CEq, v[0], v[1], p);  }
	else if (n == "neq" || n == "sne") { checkParams(2); return makeOp(CNeq, v[0], v[1], p); }
	else if (n == "or")                { checkParams(2); return makeOp(COr, v[0], v[1], p);  }
	else if (n == "and")               { checkParams(2); return makeOp(CAnd, v[0], v[1], p); }
	else if (n == "not")               { checkParams(1); return makeUnop(CNot, v[0], p);     }
	else {
		error(std::string("Unknown operation '") + n + "'", p);
		ParsedValue value;
		value.v = nullptr;
		value.p = p;
		return value;
	}
}
