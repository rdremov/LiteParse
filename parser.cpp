//---------------------------------------------------------------------------
// Purpose:		Parse and calculate math formula expression
// Author:		Roman Dremov
// Date:		November 2016
// Usage:		parser [-acdhruvw] gitolite.log
//---------------------------------------------------------------------------

#include <stdlib.h>
#include <string.h>

// formula engine

namespace RVD_FORMULA
{
#define	FA

enum OP
{
	OP_NONE,
	OP_PLUS,
	OP_MINUS,
	OP_MULTIPLY,
	OP_DIVIDE,
	OP_UNARY_PLUS,
	OP_UNARY_MINUS,
};

enum OPDIR
{
	OPDIR_LR,
	OPDIR_RL,
};

enum TYPE
{
	TYPE_void,
	TYPE_int,
	TYPE_double,
	TYPE_string,
};

struct STR
{
	char*	data;
	int		len;
};

template<class T, class TL, class TR>
T	bin(TL l, TR r, char op)
{
	switch( op )
	{
	case OP_PLUS:
		return l + r;
	case OP_MINUS:
		return l - r;
	case OP_MULTIPLY:
		return l * r;
	case OP_DIVIDE:
		return l / r;
	}
	return 0;
}

template <class H, class T>
struct typelist
{
	typedef H head;
	typedef T tail;
};

class null_typelist {};

struct VAL
{
	int		type;
	union
	{
		int		ii;
		double	dd;
		STR		str;
	};

	VAL()
	{
		type = TYPE_void;
	}

	~VAL()
	{
		Cleanup();
	}

	void	Cleanup()
	{
		type = TYPE_void;
	}

	void	Set(int val)
	{
		if( type != TYPE_int )
			Cleanup();
		type = TYPE_int;
		ii = val;
	}

	void	Set(double val)
	{
		if( type != TYPE_double )
			Cleanup();
		type = TYPE_double;
		dd = val;
	}

	int		Bin(const VAL& left, const VAL& right, char op)
	{
		switch( left.type )
		{
		case TYPE_int:
			switch( right.type )
			{
			case TYPE_int:
				Set(bin<int, int, int>(left.ii, right.ii, op));
				break;
			case TYPE_double:
				Set(bin<double, int, double>(left.ii, right.dd, op));
				break;
			default:
				Cleanup();
				break;
			}
			break;
		case TYPE_double:
			switch( right.type )
			{
			case TYPE_int:
				Set(bin<double, double, int>(left.ii, right.ii, op));
				break;
			case TYPE_double:
				Set(bin<double, double, double>(left.ii, right.ii, op));
				break;
			default:
				Cleanup();
				break;
			}
			break;
		default:
			Cleanup();
			break;
		}

		return 0;
	}
};

struct NODE;

typedef int		(*PFNC)(NODE* p);

// http://en.cppreference.com/w/cpp/language/operator_precedence
struct	OPENTRY
{
	char	prec;	// precedence (3 is higher than 4 etc)
	char	dir;	// OPDIR
	char	count;	// operator node count (2 for binary etc)
};

// order according to enum OP
static OPENTRY l_ops[] =
{
	{0, 0, 0},
	{6, 0, 2},
	{6, 0, 2},
	{5, 0, 2},
	{5, 0, 2},
	{3, 1, 1},
	{3, 1, 1},
};

inline bool is_space(char cc)
{
	switch( cc )
	{
	case ' ':
	case '\t':
	case '\r':
	case '\n':
		return true;
	}
	return false;
}

inline bool is_digit(char cc)
{
	return cc >= '0' && cc <= '9';
}

inline bool is_exp(char cc)
{
	return 'e' == cc || 'E' == cc;
}

inline char	get_sign(char cc)
{
	if( '+' == cc )
		return 1;
	else if( '-' == cc )
		return -1;
	return 0;
}

int		atoi(__int64& v, const STR& str, int offset, bool bSign = false)
{
	v = 0;
	if( offset < str.len )
	{
		int ii = offset;
		char sign = 0;
		if( bSign && (sign = get_sign(str.data[ii])) )
			ii++;
		bool bDigits = false;
		for( ; ii<str.len; ii++)
		{
			if( !is_digit(str.data[ii]) )
				break;
			if( bDigits )
				v *= 10;
			v += str.data[ii] - '0';
			bDigits = true;
		}
		if( bDigits )
		{
			if( sign < 0 )
				v = -v;
			return ii - offset;
		}
	}
	return 0;
}

template<class V>
void	pow(V& v, int n)
{
	if( n > 0 )
	{
		while( n-- )
			v *= 10;
	}
	else if( n < 0 )
	{
		while( n++ )
			v /= 10;
	}
}

struct NODE
{
	NODE*	next;
	NODE*	child;
	char	op;
	VAL		val;
	
	NODE()
	{
		next = NULL;
		child = NULL;
		op = OP_NONE;
	}

	~NODE()
	{
		delete child;
		delete next;
	}

	int		Exec()
	{
		int ret = 0;

		if( child )
		{
			if( ret = child->Exec() )
				return ret;
		}

		if( op != OP_NONE )
		{
			if( ret = val.Bin(child->next->val, child->val, op) )
				return ret;
		}

		if( next )
		{
			if( ret = next->Exec() )
				return ret;
		}

		return ret;
	}
};

struct	PARSER
{
	const STR&	str;
	int			index;

	PARSER(const STR& strFormula)
		:	str(strFormula)
	{
	}

	NODE*	Parse()
	{
		NODE* pRoot = NULL;
		NODE* pOps = NULL;
		NODE* pTemp;

		// https://en.wikipedia.org/wiki/Shunting-yard_algorithm

		#define INSERT_NODE(_p, _head)	_p->next = _head, _head = _p
		#define REMOVE_NODE(_p, _head)	_p = _head, _head = _p->next
		#define MOVE_NODE				REMOVE_NODE(pTemp, pOps), INSERT_NODE(pTemp, pRoot)

		for(index=0; index<str.len; index++)
		{
			if( is_space(str.data[index]) )
				continue;

			OP op;
			if( Parse(op, pRoot != NULL) )
			{
				NODE* pOp = new NODE;
				pOp->op = op;
				while( pOps && l_ops[op].prec >= l_ops[pOps->op].prec )
					MOVE_NODE;
				INSERT_NODE(pOp, pOps);
				continue;
			}

			VAL val;
			if( Parse(val) )
			{
				NODE* pNum = new NODE;
				pNum->val = val;
				INSERT_NODE(pNum, pRoot);
				continue;
			}
		}

		while( pOps )
			MOVE_NODE;

		return pRoot;
	}

	bool	Parse(OP& op, bool bRoot)
	{
		op = OP_NONE;

		switch( str.data[index] )
		{
		case '+':
			op = bRoot ? OP_PLUS : OP_UNARY_PLUS;
			break;
		case '-':
			op = bRoot ? OP_MINUS : OP_UNARY_MINUS;
			break;
		case '*':
			op = OP_MULTIPLY;
			break;
		case '/':
			op = OP_DIVIDE;
			break;

		default:
			return false;
		}
		return true;
	}

	
	bool	Parse(VAL& val)
	{
		int offset = index;

		char sign = get_sign(str.data[offset]);
		if( sign )
			offset++;

		__int64 vInt;
		int cInt = atoi(vInt, str, offset);
		offset += cInt;

		__int64 vFrac;
		int cFrac = 0;
		bool bFloat = false;
		if( offset < str.len && '.' == str.data[offset] )
		{
			offset++;
			bFloat = true;
			cFrac = atoi(vFrac, str, offset);
			offset += cFrac;
		}

		if( cInt || cFrac )
		{
			__int64 vExp;
			int cExp = 0;
			if( offset < str.len && is_exp(str.data[offset]) )
			{
				offset++;
				cExp = atoi(vExp, str, offset, true);
				offset += cExp;
			}

			if( bFloat )
			{
				double vFloat = vInt;
				pow(vFloat, cFrac);
				vFloat += vFrac;
				pow(vFloat, vExp - cFrac);
				if( sign < 0 )
					vFloat = -vFloat;
				val.Set(vFloat);
				index = offset;
				return true;
			}
			else if( cInt )
			{
				pow(vInt, cExp);
				if( sign < 0 )
					vInt = -vInt;
				val.Set((int)vInt);
				index = offset;
				return true;
			}
		}

		return false;
	}

};

void		tree(NODE* parent)
{
	if( parent->op != OP_NONE )
	{
		NODE* p = parent;
		for(int ii=0; ii<l_ops[parent->op].count; ii++)
		{
			p = p->next;
			tree(p);
		}
		parent->child = parent->next;
		parent->next = p->next;
		p->next = NULL;
	}
}

NODE*		parse(const STR& str)
{
	PARSER parser(str);
	NODE* pRoot = parser.Parse();
	if( pRoot )
		tree(pRoot);
	return pRoot;
}

}	// namespace RVD_FORMULA

using namespace RVD_FORMULA;

int main(int argc, char* argv[])
{
	char szFormula[]="1.23e-3*2+3*4";
	STR strFormula = {szFormula, strlen(szFormula)};
	NODE* pNode = parse(strFormula);
	int nRet = pNode->Exec();
	delete pNode;
	return nRet;
}

