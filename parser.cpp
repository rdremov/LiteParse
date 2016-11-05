//---------------------------------------------------------------------------
// Purpose:		Parse and calculate math formula expression
// Author:		Roman Dremov
// Date:		November 2016
// Usage:		parser [-acdhruvw] gitolite.log
//---------------------------------------------------------------------------

#include <stdlib.h>
#include <string.h>
#include <assert.h>
// formula engine

namespace RVD_FORMULA
{

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

struct STR
{
	char*	data;
	int		len;

	void	Init()
	{
		data = NULL;
		len = 0;
	}

	void	Init(char* psz)
	{
		data = psz;
		len = strlen(psz);
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

	VAL(const VAL& val);	// empty to prevent compile and use

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
};

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
	{0, 0, 0},
	{6, 0, 2},
	{6, 0, 2},
	{5, 0, 2},
	{5, 0, 2},
	{3, 1, 1},
	{3, 1, 1},
};

struct Node
{
	Node*	_next;
	Node*	_child;
	char	_op;
	VAL		_val;
	
	Node()
	{
		_next = NULL;
		_child = NULL;
		_op = OP_NONE;
	}

	~Node()
	{
		delete _child;
		delete _next;
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
				if( ret = _val.Unary(_child->_val, _op) )
					return ret;
				break;
			case 2:
				if( ret = _val.Binary(_child->_next->_val, _child->_val, _op) )
					return ret;
				break;
			}
		}
		else if( _op == OP_PARENTHESIS )
			_val = _child->_val;

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

class	Parser
{
public:
	Parser(const STR& strFormula)
		:	_str(strFormula), _index(0)
	{
	}

	Node*	Parse()
	{
		Node* pRoot = NULL;
		Node* pOps = NULL;
		Node* pTemp;

		// https://en.wikipedia.org/wiki/Shunting-yard_algorithm

		#define INSERT_NODE(_p, _head)	_p->_next = _head, _head = _p
		#define REMOVE_NODE(_p, _head)	_p = _head, _head = _p->_next
		#define MOVE_NODE				REMOVE_NODE(pTemp, pOps), INSERT_NODE(pTemp, pRoot)

		for( ; _index<_str.len; _index++)
		{
			if( ParseSpace() )
				continue;

			if(  ')' == _str.data[_index] )
				break;

			if( '(' == _str.data[_index] )
			{
				_index++;
				Node* pParent = new Node;
				pParent->_op = OP_PARENTHESIS;
				pParent->_child = Parse();
				INSERT_NODE(pParent, pRoot);
				continue;
			}

			OP op;
			if( ParseOperator(op, !pRoot) )
			{
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
				Node* pNum = new Node;
				pNum->_val.Transfer(val);
				INSERT_NODE(pNum, pRoot);
				continue;
			}

			// error
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
		switch( _str.data[_index] )
		{
		case ' ':
		case '\t':
		case '\r':
		case '\n':
			return true;
		}
		return false;
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
					STR str = {_str.data + _index + 1, ii - _index - 1};
					val.Set(str);
					return true;
				}
			}
			return false;
		}
		char* pb = _str.data + _index;
		char* pe = NULL;
		double dd = strtod(pb, &pe);
		if( pe == pb )
			return false;
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

private:
	const STR&	_str;
	int			_index;
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
		assert(val.Equal(pRoot->_val));
		delete pRoot;
	}
	else if( val.type != TYPE_void )
		assert(false);
}

#define TEST(_expr)		test(#_expr, VAL(_expr))

int main(int argc, char* argv[])
{
	TEST("junk");
	TEST(1+2*((3+4)*2-6));
	return 0;
}

