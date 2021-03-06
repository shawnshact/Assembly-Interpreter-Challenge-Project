"""A Scheme interpreter and its read-eval-print loop."""

from scheme_builtins import *
from scheme_reader import *
from ucb import main, trace

##############
# Eval/Apply #
##############

def begin_eval(expr, env):
    if expr.second is nil:
        return scheme_eval(expr.first,env,True)
    else:
        scheme_eval(expr.first,env)
        return begin_eval(expr.second, env)

def quasi_eval(expr, env, tail=False):
    if not isinstance(expr, Pair):
        return expr
    else:
        if expr.first == 'unquote':
            expr, expr.first = expr.second, scheme_eval(expr.second.first, env,tail)
            if expr.second is nil:
                return expr.first
        if isinstance(expr.first, Pair):
            return Pair(quasi_eval(expr.first,env), quasi_eval(expr.second,env))
        return Pair(expr.first, quasi_eval(expr.second,env))

def scheme_eval(expr, env, tail=True): # Optional third argument is ignored
    """Evaluate Scheme expression EXPR in environment ENV.

    >>> expr = read_line('(+ 2 2)')
    >>> expr
    Pair('+', Pair(2, Pair(2, nil)))
    >>> scheme_eval(expr, create_global_frame())
    4
    """
    if not isinstance(expr, Pair):
        if isinstance(expr, str) and env.bindings.get(expr, None) == None:
            raise SchemeError("Unknown identifier: {0}".format(expr))
        return env.bindings.get(expr, expr)
    elif expr.first == "define":
        if expr.second is not nil and expr.second.second is not nil:
            if not isinstance(expr.second.first,Pair):
                symbol = expr.second.first
                val = expr.second.second.map(lambda param: scheme_eval(param, env)).first
                return env.define(symbol, val)
            else:
                symbol = expr.second.first.first
                formals = expr.second.first.second
                body = expr.second.second
                return env.define(symbol, LambdaProcedure(formals, body, env))
        else:
            raise SchemeError("define must contain at least 2 items.")
    elif expr.first == 'quote':
        return expr.second.first
    elif expr.first == 'quasiquote':
        return quasi_eval(expr.second.first, env)
    elif expr.first == 'begin':
        return begin_eval(expr.second, env)
    elif expr.first == 'lambda':
        return env.lambda_expr(expr.second)
    elif expr.first == 'and':
        return and_form(expr.second, env)
    elif expr.first == 'or':
        return or_form(expr.second, env)
    elif expr.first == 'if':
        return if_form(expr.second, env)
    elif expr.first == 'cond':
        return cond_form(expr.second, env)
    elif expr.first == 'let':
        return let_form(expr.second, env)
    elif expr.first == 'mu':
        return env.mu_expr(expr.second)
    elif expr.first == 'define-macro':
        return env.macro_expr(expr.second)
    else:
        eval_expr = expr.second.map(lambda param: scheme_eval(param, env))
        if not isinstance(expr.first, Pair):
            if env.bindings.get(expr.first, None) != None:
                return env.bindings[expr.first].apply(eval_expr, env)
            else:
                raise  SchemeError("Cannot call {0} as it's not a procedure".format(expr.first))
        else:
            if isinstance(expr.first.first, Pair) or expr.first.first == 'lambda' or expr.first.first == 'mu' or isinstance(env.bindings.get(expr.first.first,None), Procedure):
                return complete_apply(scheme_eval(expr.first,env), eval_expr,env) #add functionality here
            else:
                raise  SchemeError("Cannot call {0} as it's not a procedure".format(expr.first))

def scheme_apply(procedure, args, env):
    """Apply Scheme PROCEDURE to argument values ARGS (a Scheme list) in
    environment ENV."""
    check_procedure(procedure)
    if isinstance(procedure, Procedure):
        return procedure.apply(args, env)



################
# Environments #
################

class Frame:
    """An environment frame binds Scheme symbols to Scheme values."""

    def __init__(self, parent):
        """An empty frame with parent frame PARENT (which may be None)."""
        self.parent = parent
        if self.parent is None:
            self.bindings = {}
        else:
            self.bindings = parent.bindings.copy()

    def __repr__(self):
        if self.parent is None:
            return '<Global Frame>'
        s = sorted(['{0}: {1}'.format(k, v) for k, v in self.bindings.items()])
        return '<{{{0}}} -> {1}>'.format(', '.join(s), repr(self.parent))

    def define(self, symbol, value):
        """Define Scheme SYMBOL to have VALUE."""
        if isinstance(symbol, str):
            self.bindings[symbol] = value
        else:
            raise SchemeError("Invalid define form.")

        return symbol

    # BEGIN PROBLEM 2/3
    "*** YOUR CODE HERE ***"
    def lambda_expr(self, expr):
        if expr.second is not nil:
            return LambdaProcedure(expr.first, expr.second, self)
        else:
            raise SchemeError('{0} must contain at least 2 items.'.format(expr))

    def mu_expr(self,expr):
        if expr.second is not nil:
            return MuProcedure(expr.first, expr.second)
        else:
            raise SchemeError('{0} must contain at least 2 items.'.format(expr))

    def macro_expr(self,expr):
        if len(expr) < 2:
            raise SchemeError('define-macro must contain at least 2 items.')
        if not isinstance(expr.first, Pair) or scheme_numberp(expr.first.first):
            raise SchemeError("Improper form for define-macro.")
        name = expr.first.first
        formals = expr.first.second
        body = expr.second
        return self.define(name, MacroProcedure(formals,body))
    # END PROBLEM 2/3

##############
# Procedures #
##############

class Procedure:
    """The supertype of all Scheme procedures."""
    def make_call_frame(self, args, parent):
        self.env = Frame(parent)

def scheme_procedurep(x):
    return isinstance(x, Procedure)

class BuiltinProcedure(Procedure):
    """A Scheme procedure defined as a Python function."""

    def __init__(self, fn, use_env=False, name='builtin'):
        self.name = name
        self.fn = fn
        self.use_env = use_env

    def __str__(self):
        return '#[{0}]'.format(self.name)

    def apply(self, args, env):
        """Apply SELF to ARGS in ENV, where ARGS is a Scheme list.

        >>> env = create_global_frame()
        >>> plus = env.bindings['+']
        >>> twos = Pair(2, Pair(2, nil))
        >>> plus.apply(twos, env)
        4
        """
        # BEGIN PROBLEM 2
        "*** YOUR CODE HERE ***"
        args_list = []
        try:
            if args is nil:
                return self.fn()
            elif args.second is nil:
                if self.use_env:
                    return self.fn(args.first, env)
                return self.fn(args.first)
            elif args.second.second is nil:
                if self.use_env:
                    return self.fn(args.first, args.second.first, env)
                return self.fn(args.first, args.second.first)
            else:
                while args is not nil:
                    if isinstance(args.first, Pair):
                        args_list.extend(scheme_eval(args.first, env))
                    else:
                        args_list.append(args.first)
                    args = args.second
                return self.fn(*args_list)
        except:
            raise SchemeError("Cannot call {0} as it's not a procedure".format(args))

        # END PROBLEM 2



class LambdaProcedure(Procedure):
    """A procedure defined by a lambda expression or a define form."""

    def __init__(self, formals, body, env):
        """A procedure with formal parameter list FORMALS (a Scheme list),
        whose body is the Scheme list BODY, and whose parent environment
        starts with Frame ENV."""
        self.formals = formals
        self.body = body
        self.env = env

    def __str__(self):
        return str(Pair('lambda', Pair(self.formals, self.body)))

    def __repr__(self):
        return 'LambdaProcedure({0}, {1}, {2})'.format(
            repr(self.formals), repr(self.body), repr(self.env))

    def apply(self, args, env):
        """evaluates a lambda procedure on the arguments that have been
        passed in"""
        if len(args) > len(self.formals):
            raise SchemeError('Too many arguments to function call.')
        elif len(args) < len(self.formals):
            raise SchemeError('Too few arguments to function call.')
        else:
            self.make_call_frame(args, self.env)
            temp_formals = self.formals
            while args is not nil:
                self.env.define(temp_formals.first, args.first)
                args, temp_formals = args.second, temp_formals.second
            if len(self.body) <= 1:
                return scheme_eval(self.body.first, self.env,True)
            else:
                return begin_eval(self.body,self.env)

def add_builtins(frame, funcs_and_names):
    """Enter bindings in FUNCS_AND_NAMES into FRAME, an environment frame,
    as built-in procedures. Each item in FUNCS_AND_NAMES has the form
    (NAME, PYTHON-FUNCTION, INTERNAL-NAME)."""
    for name, fn, proc_name in funcs_and_names:
        frame.define(name, BuiltinProcedure(fn, name=proc_name))

#################
# Special Forms #
#################

"""
How you implement special forms is up to you. We recommend you encapsulate the
logic for each special form separately somehow, which you can do here.
"""
def and_form(args,env):
    if args is nil:
        return True
    while args.second is not nil:
        arg = scheme_eval(args.first, env)
        if arg is False:
            return arg
        args = args.second
    return scheme_eval(args.first,env,True)

def or_form(args,env):
    if args is nil:
        return False
    while args.second is not nil:
        arg = scheme_eval(args.first, env)
        if arg is not False:
            return arg
        args = args.second
    return scheme_eval(args.first,env,True)

def if_form(args,env):
    if len(args) == 1:
        raise SchemeError('if statement must contain at least 2 items.')
    else:
        if scheme_eval(args.first,env) is not False:
            return scheme_eval(args.second.first, env,True)
        else:
            if args.second.second is not nil:
                return scheme_eval(args.second.second.first, env,True)

def cond_form(args,env):
    while args is not nil:
        if not isinstance(args.first,Pair):
            raise SchemeError('{0} is not a valid list.'.format(args.first))
        cond_expr = args.first
        if cond_expr.first == 'else':
            bool_expr = True
        else:
            bool_expr = scheme_eval(cond_expr.first,env)
        if bool_expr is not False or cond_expr == 'else':
            if cond_expr.second is not nil:
                return begin_eval(cond_expr.second,env)
            else:
                return bool_expr
        args = args.second

def let_form(args, env):
    bindings = args.first
    body = args.second
    def extract_names_and_expr(args):
        names, expr = nil, nil
        while args is not nil:
            bind = args.first
            if len(bind) > 2:
                raise SchemeError('{0} must contain at most 2 items.'.format(bind))
            names, expr = Pair(bind.first, names), Pair(scheme_eval(bind.second.first,env), expr)
            args = args.second
        return names, expr

    names, expr = extract_names_and_expr(bindings)
    new_proc = LambdaProcedure(names, body, env)
    return scheme_eval(new_proc, env).apply(expr, env)


# Utility methods for checking the structure of Scheme programs

def check_form(expr, min, max=float('inf')):
    """Check EXPR is a proper list whose length is at least MIN and no more
    than MAX (default: no maximum). Raises a SchemeError if this is not the
    case.

    >>> check_form(read_line('(a b)'), 2)
    """
    if not scheme_listp(expr):
        raise SchemeError('badly formed expression: ' + repl_str(expr))
    length = len(expr)
    if length < min:
        raise SchemeError('too few operands in form')
    elif length > max:
        raise SchemeError('too many operands in form')

def check_formals(formals):
    """Check that FORMALS is a valid parameter list, a Scheme list of symbols
    in which each symbol is distinct. Raise a SchemeError if the list of
    formals is not a well-formed list of symbols or if any symbol is repeated.

    >>> check_formals(read_line('(a b c)'))
    """
    symbols = set()
    def check_and_add(symbol):
        if not scheme_symbolp(symbol):
            raise SchemeError('non-symbol: {0}'.format(symbol))
        if symbol in symbols:
            raise SchemeError('duplicate symbol: {0}'.format(symbol))
        symbols.add(symbol)

    while isinstance(formals, Pair):
        check_and_add(formals.first)
        formals = formals.second

    if formals != nil:
        check_and_add(formals)

def check_procedure(procedure):
    """Check that PROCEDURE is a valid Scheme procedure."""
    if not scheme_procedurep(procedure):
        raise SchemeError('{0} is not callable: {1}'.format(
            type(procedure).__name__.lower(), repl_str(procedure)))

#################
# Dynamic Scope #
#################

class MuProcedure(Procedure):
    """A procedure defined by a mu expression, which has dynamic scope.
     _________________
    < Scheme is cool! >
     -----------------
            \   ^__^
             \  (oo)\_______
                (__)\       )\/\
                    ||----w |
                    ||     ||
    """

    def __init__(self, formals, body):
        """A procedure with formal parameter list FORMALS (a Scheme list) and
        Scheme list BODY as its definition."""
        self.formals = formals
        self.body = body


    def __str__(self):
        return str(Pair('mu', Pair(self.formals, self.body)))

    def __repr__(self):
        return 'MuProcedure({0}, {1})'.format(
            repr(self.formals), repr(self.body))

    def apply(self, args, env):
        """evaluates a lambda procedure on the arguments that have been
        passed in"""
        if len(args) > len(self.formals):
            raise SchemeError('Too many arguments to function call.')
        elif len(args) < len(self.formals):
            raise SchemeError('Too few arguments to function call.')
        else:
            temp_formals = self.formals
            while args is not nil:
                env.define(temp_formals.first, args.first)
                args, temp_formals = args.second, temp_formals.second
            if len(self.body) <= 1:
                return scheme_eval(self.body.first, env,True)
            else:
                return scheme_eval(begin_eval(self.body,env),env)

class MacroProcedure(Procedure):
    def __init__(self, formals, body):
        """A procedure with formal parameter list FORMALS (a Scheme list) and
        Scheme list BODY as its definition."""
        self.formals = formals
        self.body = body


    def __str__(self):
        return str(Pair('#macro', Pair(self.formals, self.body)))

    def __repr__(self):
        return 'MacroProcedure({0}, {1})'.format(repr(self.formals), repr(self.body))

    def apply(self, args, env):
        """evaluates a macro procedure on the arguments that have been
        passed in"""
        expr = scheme_eval(self.body,env)
        print(expr)


##################
# Tail Recursion #
##################

# Make classes/functions for creating tail recursive programs here?
class Thunk:
    def __init__(self, expr, env):
        self.expr = expr
        self.env = env


def make_tail_eval(eval_func):
    def tail_eval(expr, env, tail=False):
        if tail and not scheme_symbolp(expr) and not scheme_atomp(expr) and expr is not None:
            return Thunk(expr, env)
        eval_expr = Thunk(expr, env)
        while isinstance(eval_expr,Thunk):
            eval_expr = eval_func(eval_expr.expr,eval_expr.env)
        return eval_expr
    return tail_eval

scheme_eval = make_tail_eval(scheme_eval)


def complete_apply(procedure, args, env):
    """Apply procedure to args in env; ensure the result is not a Thunk.
    Right now it just calls scheme_apply, but you will need to change this
    if you attempt the extra credit."""
    val = scheme_apply(procedure, args, env)
    # Add stuff here?
    if isinstance(val, Thunk):
        return scheme_eval(val.expr, val.env)
    return val
####################
# Extra Procedures #
####################

def scheme_map(fn, s, env):
    check_type(fn, scheme_procedurep, 0, 'map')
    check_type(s, scheme_listp, 1, 'map')
    return s.map(lambda x: complete_apply(fn, Pair(x, nil), env))

def scheme_filter(fn, s, env):
    check_type(fn, scheme_procedurep, 0, 'filter')
    check_type(s, scheme_listp, 1, 'filter')
    head, current = nil, nil
    while s is not nil:
        item, s = s.first, s.second
        if complete_apply(fn, Pair(item, nil), env):
            if head is nil:
                head = Pair(item, nil)
                current = head
            else:
                current.second = Pair(item, nil)
                current = current.second
    return head

def scheme_reduce(fn, s, env):
    check_type(fn, scheme_procedurep, 0, 'reduce')
    check_type(s, lambda x: x is not nil, 1, 'reduce')
    check_type(s, scheme_listp, 1, 'reduce')
    value, s = s.first, s.second
    while s is not nil:
        value = complete_apply(fn, scheme_list(value, s.first), env)
        s = s.second
    return value

################
# Input/Output #
################

def read_eval_print_loop(next_line, env, interactive=False, quiet=False,
                         startup=False, load_files=()):
    """Read and evaluate input until an end of file or keyboard interrupt."""
    if startup:
        for filename in load_files:
            scheme_load(filename, True, env)
    while True:
        try:
            src = next_line()
            while src.more_on_line:
                expression = scheme_read(src)
                result = scheme_eval(expression, env)
                if not quiet and result is not None:
                    print(repl_str(result))
        except (SchemeError, SyntaxError, ValueError, RuntimeError) as err:
            if (isinstance(err, RuntimeError) and
                'maximum recursion depth exceeded' not in getattr(err, 'args')[0]):
                raise
            elif isinstance(err, RuntimeError):
                print('Error: maximum recursion depth exceeded')
            else:
                print('Error:', err)
        except KeyboardInterrupt:  # <Control>-C
            if not startup:
                raise
            print()
            print('KeyboardInterrupt')
            if not interactive:
                return
        except EOFError:  # <Control>-D, etc.
            print()
            return

def scheme_load(*args):
    """Load a Scheme source file. ARGS should be of the form (SYM, ENV) or
    (SYM, QUIET, ENV). The file named SYM is loaded into environment ENV,
    with verbosity determined by QUIET (default true)."""
    if not (2 <= len(args) <= 3):
        expressions = args[:-1]
        raise SchemeError('"load" given incorrect number of arguments: '
                          '{0}'.format(len(expressions)))
    sym = args[0]
    quiet = args[1] if len(args) > 2 else True
    env = args[-1]
    if (scheme_stringp(sym)):
        sym = eval(sym)
    check_type(sym, scheme_symbolp, 0, 'load')
    with scheme_open(sym) as infile:
        lines = infile.readlines()
    args = (lines, None) if quiet else (lines,)
    def next_line():
        return buffer_lines(*args)

    read_eval_print_loop(next_line, env, quiet=quiet)

def scheme_open(filename):
    """If either FILENAME or FILENAME.scm is the name of a valid file,
    return a Python file opened to it. Otherwise, raise an error."""
    try:
        return open(filename)
    except IOError as exc:
        if filename.endswith('.scm'):
            raise SchemeError(str(exc))
    try:
        return open(filename + '.scm')
    except IOError as exc:
        raise SchemeError(str(exc))

def create_global_frame():
    """Initialize and return a single-frame environment with built-in names."""
    env = Frame(None)
    env.define('eval',
               BuiltinProcedure(scheme_eval, True, 'eval'))
    env.define('apply',
               BuiltinProcedure(complete_apply, True, 'apply'))
    env.define('load',
               BuiltinProcedure(scheme_load, True, 'load'))
    env.define('procedure?',
               BuiltinProcedure(scheme_procedurep, False, 'procedure?'))
    env.define('map',
               BuiltinProcedure(scheme_map, True, 'map'))
    env.define('filter',
               BuiltinProcedure(scheme_filter, True, 'filter'))
    env.define('reduce',
               BuiltinProcedure(scheme_reduce, True, 'reduce'))
    env.define('undefined', None)
    add_builtins(env, BUILTINS)
    return env

@main
def run(*argv):
    import argparse
    parser = argparse.ArgumentParser(description='CS 61A Scheme Interpreter')
    parser.add_argument('-load', '-i', action='store_true',
                       help='run file interactively')
    parser.add_argument('file', nargs='?',
                        type=argparse.FileType('r'), default=None,
                        help='Scheme file to run')
    args = parser.parse_args()

    next_line = buffer_input
    interactive = True
    load_files = []

    if args.file is not None:
        if args.load:
            load_files.append(getattr(args.file, 'name'))
        else:
            lines = args.file.readlines()
            def next_line():
                return buffer_lines(lines)
            interactive = False

    read_eval_print_loop(next_line, create_global_frame(), startup=True,
                         interactive=interactive, load_files=load_files)
    tscheme_exitonclick()
