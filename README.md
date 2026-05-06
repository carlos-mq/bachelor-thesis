# bachelor-thesis
The repository for the implementation of my bachelor's thesis, titled "Interactive Tactic-Based Program Synthesis".

**Note**: The following documentation is an excerpt of the "Details of the Haskell Implementation" chapter of the thesis.

## Using the Implementation

To run the implementation, it is assumed that GHC and Cabal have been installed. After cloning the repository, simply run ``` cabal run``` inside it. For a detailed guide by example, see the Walkthrough at the end of the Introduction.

### List of Commands and Tactics

Inside the REPL, the user can enter one of the following commands:

- ``` let [defName] [type]``` **(command)**: creates a new top-level definition ```let ([defName] : [type]) := (- : [type])```.
- ``` letrec [defName] [type]``` **(command)**: creates a new top-level definition ```letrec ([defName] : [type]) := (- : [type])```.
- ``` jump [holeIndex]``` **(command)**: moves the focus to the hole with index ```[holeIndex]```.
- ``` exit``` **(command)**: gracefully exits the REPL.
- ``` intro [varName]``` **(tactic)**: if the current hole has type $\tau_0 \to \tau_1$, replaces it with the expression $\lambda ([varName] : \tau_0) \mapsto (- : \tau_1)$. This corresponds to the "Intro" synthesis rule.
- ``` cases``` **(tactic)**: if the current hole has type $\tau$, replaces it with the expression $\text{if} (- : \mathbb{B}) \text{then} (- : \tau) \text{else} (- : \tau)$. This corresponds to the "Cases" synthesis rule.
- ``` genApply``` **(tactic)**: if the current hole has type $\tau$, replaces it with the expression $(- : u \to \tau) (- : u)$, where $u$ is a fresh unification variable. This corresponds to the "Apply" synthesis rule.
- ``` varLocal [varName]``` **(tactic)**: if the current hole has type $\tau$, replaces it with the variable $\text{[varName]}$ from the local context $\Gamma$, as long as its type $\tau_0$ can be unified with $\tau$. This corresponds to the "VarLocal" synthesis rule.
- ``` varGlobal [varName]``` **(tactic)**: if the current hole has type $\tau$, replaces it with the variable $\text{[varName]}$ from the global context $Sigma$, as long as its type $\tau_0$, after all the type variables have been replaced with unification variables, can be unified with $\tau$ (thus behaving like a polymorphic function). This corresponds to the "VarGlobal" synthesis rule.
- ``` int [integer]``` **(tactic)**: if the current hole has type $\tau$ unifiable with $\mathbb{Z}$, replaces it with the integer literal $\text{[integer]}$. At the moment, only parsing of non-negative integers is supported. This corresponds to the "Int" synthesis rule.
- ``` bool [boolean]``` **(tactic)**: if the current hole has type $\tau$ unifiable with $\mathbb{B}$, replaces it with the boolean literal $\text{[boolean]} \in \{ \text{true}, \text{false} \}$. This corresponds to the "Bool" synthesis rule.
- ``` localApply [funcName]``` **(tactic)**: if the current hole has type $\tau$ and the function from the local context $\text{[funcName]}$ has type $\tau_1 \to \dots \to \tau_n \to \tau_0$, where $\tau_0$ is unifiable with $\tau$, replaces the hole with $\text{[funcName]} \quad (- : \tau_n) \quad \dots \quad (- : \tau_1)$, and also propagating the substitution resulting from the unification.
- ``` globalApply [funcName]``` **(tactic)**: if the current hole has type $tau$ and the function from the global context $\text{[funcName]}$ has type $\tau_1 \to \dots \to \tau_n \to \tau_0$ (first having replaced all its type variables with unification variables), where $\tau_0$ is unifiable with $\tau$, replaces the hole with $\text{[funcName]} \quad (- : \tau_n) \quad \dots \quad (- : \tau_1)$, and also propagating the substitution resulting from the unification.

For debugging purposes, we also have the following commands (use at your own risk):

- ``` groundContext```: prints the ground context.
- ``` getFocus```: prints the current focus (of the zipper; see the upcoming section on zippers for program navigation).
- ``` descendFocus```: descend the focus to the focus of the children.
- ``` ascendFocus```: ascend the focus to the parent.
- ``` goLeft```: move the focus to the immediate left sibling.
- ``` goRight```: move the focus to the immediate right sibling.

### Syntax for Types

To write types in the REPL:
- The primitive type $\mathbb{Z}$ is written ``` Int```.
- The primitive type $\mathbb{B}$ is written ``` Bool```.
- The list type $[t]$ is written ``` [t]```.
- The function type $\tau_1 \to \tau_2$ is written ``` t1 -> t2```, where ``` t1```, ``` t2``` represent $\tau_1, \tau_2$, respectively.
- The product type $\tau_1 \times \tau_2$ is written ``` t1 * t2```, where ``` t1```, ``` t2``` represent $\tau_1, \tau_2$, respectively. Has higher precedence than the function type.
- Parentheses can be written, if necessary.
For example, the type $(\text{inType} \to \text{outType}) \to [\text{inType}] \to [\text{outType}]$ would be written:
```
(inType -> outType) -> [inType] -> [outType]
```
and the type $a \times b \to b$ would be written:
```
a * b -> b
```