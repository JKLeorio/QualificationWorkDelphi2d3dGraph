
import sys
from sympy import symbols, Eq, solve, sympify

def main():
    if len(sys.argv) < 3:
        print("Usage: solve_expr.py <expression> <variable>")
        return

    expr = sys.argv[1]
    var = sys.argv[2]

    x, y, z = symbols('x y z')

    try:
        parts = expr.split('=')
        if len(parts) != 2:
            print("Invalid equation format.")
            return

        lhs = sympify(parts[0])
        rhs = sympify(parts[1])
        equation = Eq(lhs, rhs)

        solution = solve(equation, symbols(var))
        if not solution:
            print("No solution found.")
        else:
            print(f"{var} = {solution[0]}")
    except Exception as e:
        print(f"Error: {e}")

if __name__ == '__main__':
    main()
