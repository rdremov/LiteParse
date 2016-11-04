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
	TYPE_string,
};

struct STR
{
	char*	data;
	int		len;
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

		if( op > OP_NONE )
		{
			switch( l_ops[op].count )
			{
			case 1:
				if( ret = val.Unary(child->val, op) )
					return ret;
				break;
			case 2:
				if( ret = val.Binary(child->next->val, child->val, op) )
					return ret;
				break;
			}
		}
		else if( op == OP_PARENTHESIS )
			val = child->val;

		if( next )
		{
			if( ret = next->Exec() )
				return ret;
		}

		return ret;
	}

	void		MakeTree()
	{
		if( op > OP_NONE )
		{
			NODE* p = this;
			for(int ii=0; ii<l_ops[op].count; ii++)
			{
				p = p->next;
				p->MakeTree();
			}
			child = next;
			next = p->next;
			p->next = NULL;
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

	NODE*	Parse()
	{
		NODE* pRoot = NULL;
		NODE* pOps = NULL;
		NODE* pTemp;

		// https://en.wikipedia.org/wiki/Shunting-yard_algorithm

		#define INSERT_NODE(_p, _head)	_p->next = _head, _head = _p
		#define REMOVE_NODE(_p, _head)	_p = _head, _head = _p->next
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
				NODE* pParent = new NODE;
				pParent->op = OP_PARENTHESIS;
				pParent->child = Parse();
				INSERT_NODE(pParent, pRoot);
				continue;
			}

			OP op;
			if( ParseOperator(op, !pRoot) )
			{
				NODE* pOp = new NODE;
				pOp->op = op;
				while( pOps && l_ops[op].prec >= l_ops[pOps->op].prec )
					MOVE_NODE;
				INSERT_NODE(pOp, pOps);
				continue;
			}

			VAL val;
			if( ParseVal(val) )
			{
				NODE* pNum = new NODE;
				pNum->val = val;
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

int main(int argc, char* argv[])
{
	int ret = 0;
	char szFormula[]="1+2*((3+4)*2-6)";
	STR strFormula = {szFormula, strlen(szFormula)};
	NODE* pRoot = NULL;
	{
		Parser parser(strFormula);
		pRoot = parser.Parse();
	}
	if( pRoot )
	{
		ret = pRoot->Exec();
		delete pRoot;
	}
	return ret;
}

