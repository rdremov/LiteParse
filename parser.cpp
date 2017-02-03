//---------------------------------------------------------------------------
// Purpose:		Parse and calculate math formula expression
// Author:		Roman Dremov
// Date:		November 2016
// Usage:		see void test(char* szFormula, const VAL& val)
//---------------------------------------------------------------------------

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>

namespace RVD_FORMULA
{

enum	ERROR
{
	E_NONE,
	E_UNKNOWN,
	E_COMMA,
	E_QUOTE,
	E_PARENTHESIS,
	E_VAL,
	E_OP,
	E_VAR,
	E_FUNC,
	E_ARG,
	E_OVERFLOW,
};

enum OP
{
	OP_NONE,
	OP_PLUS,
	OP_MINUS,
	OP_MULTIPLY,
	OP_DIVIDE,
	OP_UNARY_PLUS,
	OP_UNARY_MINUS,

	OP_PARENTHESIS = -100,
	OP_CONST,
	OP_VAR,
	OP_FUNC,
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
	TYPE_str,
};

inline bool	is_digit(char cc)
{
	return cc >= '0' && cc <= '9';
}

inline bool	is_letter(char cc)
{
	return (cc >= 'A' && cc <= 'Z') || (cc >= 'a' && cc <= 'z');
}

inline bool	is_space(char cc)
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

struct STR
{
	char*	data;
	int		len;

	void	Init()
	{
		data = NULL;
		len = 0;
	}

	void	Init(char* psz, int l = 0)
	{
		data = psz;
		len = (l > 0) ? l : strlen(psz);
	}

	void	Free()
	{
		if( data )
		{
			delete[] data;
			Init();
		}
	}

	void	Copy(const STR& str)
	{
		Free();
		if( str.len > 0 )
		{
			if( data = new char[str.len+1] )
			{
				data[str.len] = 0;
				if( str.data )
					memcpy(data, str.data, str.len);
				len = str.len;
			}
		}
	}

	bool	Equal(const STR& str) const
	{
		if( len != str.len )
			return false;
		if( !len )
			return true;
		return 0 == memcmp(data, str.data, len);
	}

	bool	Equal(const char* psz) const
	{
		for(int ii=0; ii<len; ii++)
		{
			if(data[ii] != *psz++ )
				return false;
		}
		return 0 == *psz;
	}
};

template<class T>
T	una(T t, char op)
{
	if( OP_UNARY_MINUS == op )
		return -t;
	return t;
}

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

struct VAL
{
	char		type;
	union
	{
		int		ii;
		double	dd;
		STR		str;
	};

	VAL()
	{
		Init();
	}

	VAL(VAL& val);	//{Transfer(val);}

	VAL(int val)
	{
		Init();
		Set(val);
	}
	
	VAL(double val)
	{
		Init();
		Set(val);
	}
	
	VAL(char* psz)
	{
		Init();
		STR val;
		val.Init(psz);
		Set(val);
	}

	~VAL()
	{
		Cleanup();
	}

	void	Init()
	{
		type = TYPE_void;
		str.Init();
	}

	void	Cleanup()
	{
		if( type == TYPE_str )
			str.Free();
		type = TYPE_void;
	}

	bool	Equal(const VAL& val) const
	{
		if( type != val.type )
			return false;
		switch( type )
		{
		case TYPE_int:
			return ii == val.ii;
		case TYPE_double:
			return dd == val.dd;
		case TYPE_str:
			return str.Equal(val.str);
		}
		return true;
	}

	void	Transfer(VAL& val)
	{
		memcpy(this, &val, sizeof(VAL));
		val.Init();
	}

	int		Unary(const VAL& val, char op)
	{
		Cleanup();
		switch( val.type )
		{
		case TYPE_int:
			Set(una(val.ii, op));
			break;
		case TYPE_double:
			Set(una(val.dd, op));
			break;
		}
		return 0;
	}

	int		Binary(const VAL& left, const VAL& right, char op)
	{
		Cleanup();
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
			}
			break;
		case TYPE_double:
			switch( right.type )
			{
			case TYPE_int:
				Set(bin<double, double, int>(left.dd, right.ii, op));
				break;
			case TYPE_double:
				Set(bin<double, double, double>(left.dd, right.dd, op));
				break;
			}
			break;
		}
		return 0;
	}

	void	Set(int val)
	{
		type = TYPE_int;
		ii = val;
	}

	void	Set(double val)
	{
		type = TYPE_double;
		dd = val;
	}

	void	Set(const STR& val)
	{
		type = TYPE_str;
		str.Copy(val);
	}

	double	Get()
	{
		switch( type )
		{
		case TYPE_int:
			return ii;
		case TYPE_double:
			return dd;
		}
		return 0;
	}
};

struct CONSTANT
{
	const char*	name;
	VAL			val;
};

#define Pi	3.14159265359
#define e	2.71828182846

#define CONSTENTRY(_v)		{#_v, _v}

static CONSTANT g_consts[] = 
{
	CONSTENTRY(Pi),
	CONSTENTRY(e),
};

int	find_const(const STR& str)
{
	for(int ii=0; ii<_countof(g_consts); ii++)
	{
		if( str.Equal(g_consts[ii].name) )
			return ii;
	}
	return -1;
}

struct Node;
typedef void (*PFNNode)(Node&);

struct FUNC
{
	const char*	name;
	PFNNode		pfn;
	char		nargs;
};

static void sin(Node& node);
static void min(Node& node);

#define FUNCENTRY(_fn, _nargs)		{#_fn, &_fn, _nargs}

static FUNC g_funcs[] = 
{
	FUNCENTRY(min, 2),
	FUNCENTRY(sin, 1),
};

int	find_func(const STR& str)
{
	for(int ii=0; ii<_countof(g_funcs); ii++)
	{
		if( str.Equal(g_funcs[ii].name) )
			return ii;
	}
	return -1;
}

// http://en.cppreference.com/w/cpp/language/operator_precedence
struct	OPEntry
{
	char	prec;	// precedence (3 is higher than 4 etc)
	char	dir;	// OPDIR
	char	count;	// operator node count (1 for unary, 2 for binary etc)
};

// order according to enum OP
static OPEntry l_ops[] =
{
	{0, 0, 0},		// OP_NONE
	{6, 0, 2},		// OP_PLUS
	{6, 0, 2},		// OP_MINUS
	{5, 0, 2},		// OP_MULTIPLY
	{5, 0, 2},		// OP_DIVIDE
	{3, 1, 1},		// OP_UNARY_PLUS
	{3, 1, 1},		// OP_UNARY_MINUS
};

struct Node
{
	Node*	_next;
	Node*	_child;
	char	_op;
	int		_index;
	VAL		_val;
	
	Node()
	{
		_next = NULL;
		_child = NULL;
		_op = OP_NONE;
		_index = 0;
	}

	~Node()
	{
		delete _child;
		delete _next;
	}

	VAL&	Val()
	{
		if( _op == OP_CONST && _index > 0 )
			return g_consts[_index - 1].val;
		return _val;
	}

	int		GetChildrenCount()
	{
		if( !_child )
			return 0;
		return _child->GetCount();
	}

	int		GetCount()
	{
		int nCount = 1;
		Node* next = _next;
		while( next )
		{
			nCount++;
			next = next->_next;
		}
		return nCount;
	}

	int		Test()
	{
		if( OP_FUNC == _op )
		{
			if( g_funcs[_index-1].nargs != GetChildrenCount() )
				return E_ARG;
		}
		return E_NONE;
	}

	int		Exec()
	{
		int ret = 0;

		if( _child )
		{
			if( ret = _child->Exec() )
				return ret;
		}

		if( _op > OP_NONE )
		{
			switch( l_ops[_op].count )
			{
			case 1:
				if( ret = Val().Unary(_child->Val(), _op) )
					return ret;
				break;
			case 2:
				if( ret = Val().Binary(_child->_next->Val(), _child->Val(), _op) )
					return ret;
				break;
			}
		}
		else if( _op < 0 )
		{
			switch( _op )
			{
			case OP_FUNC:
				g_funcs[_index-1].pfn(*this);
				break;
			case OP_VAR:
				break;
			case OP_PARENTHESIS:
				Val() = _child->Val();
				break;
			}
		}

		if( _next )
		{
			if( ret = _next->Exec() )
				return ret;
		}

		return ret;
	}

	void		MakeTree()
	{
		if( _op > OP_NONE )
		{
			Node* p = this;
			for(int ii=0; ii<l_ops[_op].count; ii++)
			{
				p = p->_next;
				p->MakeTree();
			}
			_child = _next;
			_next = p->_next;
			p->_next = NULL;
		}
	}
};

void sin(Node& node)
{
	node.Val().Set(::sin(node._child->Val().Get()));
}

double	min(double d1, double d2)
{
	return (d1 < d2) ? d1 : d2;
}

void min(Node& node)
{
	node.Val().Set(min(node._child->_next->Val().Get(), node._child->Val().Get()));
}

class	Parser
{
	enum PARSE
	{
		P_OPERATOR = 1,
		P_VALUE,
	};

public:
	Parser(const STR& strFormula)
		:	_str(strFormula), _index(0), _error(E_NONE)
	{
	}

	Node*	Parse(bool bParenthesis = false)
	{
		Node* pRoot = NULL;
		Node* pOps = NULL;
		Node* pTemp;
		int nLast = P_OPERATOR;

		// https://en.wikipedia.org/wiki/Shunting-yard_algorithm

		#define INSERT_NODE(_p, _head)	_p->_next = _head, _head = _p
		#define REMOVE_NODE(_p, _head)	_p = _head, _head = _p->_next
		#define MOVE_NODE				REMOVE_NODE(pTemp, pOps), INSERT_NODE(pTemp, pRoot)
		#define LAST_ERROR(_curr, _err)	if(nLast == _curr){_error = _err; break;} else nLast = _curr

		for( ; _index<_str.len; _index++)
		{
			if( _error )
				break;

			if( ParseSpace() )
				continue;

			if( ',' == _str.data[_index] )
			{
				LAST_ERROR(P_OPERATOR, E_COMMA);
				continue;
			}

			if(  ')' == _str.data[_index] )
			{
				bParenthesis = !bParenthesis;
				break;
			}

			if( '(' == _str.data[_index] )
			{
				_index++;
				LAST_ERROR(P_VALUE, E_PARENTHESIS);
				Node* pParent = new Node;
				pParent->_op = OP_PARENTHESIS;
				pParent->_child = Parse(true);
				INSERT_NODE(pParent, pRoot);
				continue;
			}

			OP op;
			if( ParseOperator(op, !pRoot) )
			{
				if( 1 == l_ops[op].count )
					nLast = 0;
				LAST_ERROR(P_OPERATOR, E_OP);
				Node* pOp = new Node;
				pOp->_op = op;
				while( pOps && l_ops[op].prec >= l_ops[pOps->_op].prec )
					MOVE_NODE;
				INSERT_NODE(pOp, pOps);
				continue;
			}

			VAL val;
			if( ParseVal(val) )
			{
				LAST_ERROR(P_VALUE, E_VAL);
				Node* pNum = new Node;
				pNum->_val.Transfer(val);
				INSERT_NODE(pNum, pRoot);
				continue;
			}

			STR name;
			if( ParseName(name) )
			{
				LAST_ERROR(P_VALUE, E_VAR);
				for( ; _index<_str.len-1; _index++)
				{
					if( !is_space(_str.data[_index]) )
						break;
				}		
				if( '(' == _str.data[_index] )
				{
					int nFunc = find_func(name);
					if( nFunc < 0 )
					{
						_error = E_FUNC;
						break;
					}
					_index++;
					Node* pFunc = new Node;
					pFunc->_op = OP_FUNC;
					pFunc->_index = nFunc + 1;
					pFunc->_child = Parse(true);
					if( _error = pFunc->Test() )
						break;
					INSERT_NODE(pFunc, pRoot);
					continue;
				}

				int nConst = find_const(name);
				if( nConst < 0 )
				{
					_error = E_VAR;
					break;
				}
				Node* pConst = new Node;
				pConst->_op = OP_CONST;
				pConst->_index = nConst + 1;
				INSERT_NODE(pConst, pRoot);
				continue;
			}

			_error = E_UNKNOWN;
			break;
		}

		if( bParenthesis )
			_error = E_PARENTHESIS;

		if( _error )
		{
			delete pRoot;
			return NULL;
		}

		while( pOps )
			MOVE_NODE;

		if( pRoot )
			pRoot->MakeTree();

		return pRoot;
	}

protected:
	bool	ParseSpace()
	{
		return is_space(_str.data[_index]);
	}
	
	bool	ParseOperator(OP& op, bool bFirst)
	{
		op = OP_NONE;
		switch( _str.data[_index] )
		{
		case '+':
			op = bFirst ? OP_UNARY_PLUS : OP_PLUS;
			break;
		case '-':
			op = bFirst ? OP_UNARY_MINUS : OP_MINUS;
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
	
	bool	ParseVal(VAL& val)
	{
		if( '"' == _str.data[_index] )
		{
			for(int ii = _index + 1; ii<_str.len; ii++)
			{
				if( '"' == _str.data[ii] )
				{
					STR str;
					str.Init(_str.data + _index + 1, ii - _index - 1);
					val.Set(str);
					_index = ii;
					return true;
				}
			}
			_error = E_QUOTE;
			return false;
		}
		char* pb = _str.data + _index;
		char* pe = NULL;
		double dd = strtod(pb, &pe);
		if( pe == pb )
			return false;
		//if( dd == HUGE_VAL || dd == -HUGE_VAL )
		//{
		//	_error = E_OVERFLOW;
		//	return false;
		//}
		_index += pe - pb - 1;
		bool bFloat = false;
		while( pb < pe )
		{
			if( '.' == *pb )
			{
				bFloat = true;
				break;
			}
			pb++;
		}
		if( bFloat )
			val.Set(dd);
		else
			val.Set((int)dd);
		return true;
	}

	bool	ParseName(STR& var)
	{
		int ii = _index;
		for( ; ii<_str.len; ii++)
		{
			char cc = _str.data[ii];
			if( is_letter(cc) || '_' == cc )
				continue;
			if( ii > _index && is_digit(cc) )
				continue;
			break;
		}
		if( ii == _index )
			return false;
		var.Init(_str.data + _index, ii - _index);
		_index = ii;
		return true;
	}

private:
	const STR&	_str;
	int			_index;
	int			_error;
};

}	// namespace RVD_FORMULA

using namespace RVD_FORMULA;

void test(char* szFormula, const VAL& val)
{
	STR strFormula;
	strFormula.Init(szFormula);
	Parser parser(strFormula);
	Node* pRoot = parser.Parse();
	if( pRoot )
	{
		int ret = pRoot->Exec();
		assert(!ret);
		assert(val.Equal(pRoot->Val()));
		delete pRoot;
	}
	else if( val.type != TYPE_void )
		assert(false);
}

#define TEST(_expr)		test(#_expr, VAL(_expr))

int main(int argc, char* argv[])
{
	TEST(-1-2);
	TEST(min(3,2.));
	TEST(sin(3.));
	TEST(Pi);
	TEST("junk");
	TEST(1+2*((3+4)*2-6));
	return 0;
}

