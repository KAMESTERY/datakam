package model

// https://rosettacode.org/wiki/Y_combinator#Go

// Func Function
type Func func([]string) []string

// FuncFunc Function Function
type FuncFunc func(Func) Func

// RecursiveFunc Recursive Function
type RecursiveFunc func(RecursiveFunc) Func

// Y Y combinator
func Y(f FuncFunc) Func {
	g := func(r RecursiveFunc) Func {
		return f(func(x []string) []string {
			return r(r)(x)
		})
	}
	return g(g)
}
