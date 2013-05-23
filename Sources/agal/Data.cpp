#include "Data.h"

using namespace agal;

int Tools::regSize(VarType* t) {
	if (t->isMatrix()) {
		auto matrix = dynamic_cast<TMatrix*>(t);
		// assume matrix are always packed
		if (matrix->transpose == nullptr) return matrix->c < matrix->r ? matrix->c : matrix->r;
		else return matrix->transpose ? matrix->c : matrix->r;
	}
	else if (t->isArray()) {
		auto array = dynamic_cast<TArray*>(t);
		return regSize(array->t) * array->size;
	}
	else if (t->isObject()) {	
		auto object = dynamic_cast<TObject*>(t);
		int k = 0;
		for (auto it = object->fields.begin(); it != object->fields.end(); ++it)
			k += regSize(it->t);
		return k;
	}
	else return 1;
}

int Tools::floatSize(VarType* t) {
	if (t->isNull()) return 1;
	if (t->isBool()) return 1;
	if (t->isFloat()) return 1;
	if (t->isFloat2()) return 2;
	if (t->isFloat3()) return 3;
	if (t->isFloat4() || t->isInt()) return 4;
	if (t->isTexture()) return 0;
	if (t->isMatrix()) {
		auto matrix = dynamic_cast<TMatrix*>(t);
		return matrix->c * matrix->r;
	}
	if (t->isArray()) {
		auto array = dynamic_cast<TArray*>(t);
		int size = floatSize(array->t);
		if (size < 4) size = 4;
		return size * array->size;
	}
	if (t->isObject()) {
		return regSize(t) * 4;
	}
	return -1;
}

VarType* Tools::makeFloat(int i) {
	switch (i) {
		case 1:
			return new TFloat;
		case 2:
			return new TFloat2;
		case 3:
			return new TFloat3;
		case 4:
			return new TFloat4;
		default:
			return new TFloat4;
	};
}

int Tools::getMaxTextures() {
	return 8;
}

Tools::RegProp Tools::getProps(RegType r, bool fragment) {
	switch (r) {
	case RAttr:
		if (fragment) {
			return RegProp(false, false, 0);
		}
		else {
			return RegProp(true, false, 8);
		}
	case RConst:
		return RegProp(true, false, fragment ? 28 : 128);
	case RTemp:
		return RegProp(true, true, 8);
	case ROut:
		return RegProp(false, true, 1);
	case RVar:
		return RegProp(true, true, 8);
	//case RSampler: if( fragment ) { read : true, write : false, count : 8 } else { read : false, write : false, count : 0 };
	}
	return RegProp(false, false, 0);
}
