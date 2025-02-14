# Lepton-lang

> Former Saki-lang. No more MyGo. No more Ave Mujica. No more Saki. Getting tired. -- Feb 15, 2025. After watching Ave Mujica ep7. 

Lepton is a dependently-typed, pure functional language with algebraic subtyping and overloaded superposition types.  It prioritizes simplicity in design, using a Scala-inspired syntax while leveraging a type system grounded in Martin-Löf Type Theory. Lepton introduces novel features like constraint universes and superposition types, serving as a research platform for exploring advanced type systems and verified program synthesis.

```
def Eq(A: 'Type, a b: A): 'Type = ∀(P: A -> 'Type) -> P(a) -> P(b)
def refl(A: 'Type, a: A): A.Eq(a, a) = (P: A -> 'Type, pa: P(a)) => pa
def symm(A: 'Type, a b: A, eqab: A.Eq(a, b)): A.Eq(b, a) = eqab((b': A) => A.Eq(b', a), A.refl(a))

type ℕ = inductive { Zero; Succ(ℕ) }
def succ(n: ℕ): ℕ = ℕ::Succ(n)

operator binary (^=) left-assoc { looser-than (+) }
def (^=)(a b: ℕ): 'Type = ℕ.Eq(a, b)

def (+)(a b : ℕ): ℕ = match a {
    case ℕ::Zero => b
    case ℕ::Succ(a') => ℕ::Succ(a' + b)
}

def induction(
    P: ℕ -> 'Type, base: P(ℕ::Zero), 
    step: ∀(n: ℕ) -> P(n) -> P(n.succ), nat: ℕ,
): P(nat) = match nat {
    case ℕ::Zero => base
    case ℕ::Succ(n') => step(n', P.induction(base, step, n'))
}

def step(a b: ℕ, eqba: (b ^= a), P: ℕ -> 'Type, pa: P(a)): P(b) = ℕ.symm(b, a, eqba)(P, pa)

def theoremPlusZero: ∀(n: ℕ) -> (n + ℕ::Zero ^= n) = {
    ((n: ℕ) => (n + ℕ::Zero ^= n)).induction(ℕ.refl(ℕ::Zero),
        (n: ℕ, assumption: (n + ℕ::Zero ^= n)) => {
            let nPrimeEq = (n': ℕ) => (n'.succ ^= n.succ)
            step(n, n + ℕ::Zero, assumption, nPrimeEq, ℕ.refl(n.succ))
        }
    )
}
```

[Documents](https://lepton-lang.github.io/) | [Playground](https://lepton-lang.tech/) | [Online REPL](https://repl.lepton-lang.tech/)
