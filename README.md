# bachelor-thesis
The repository for the implementation of my bachelor's thesis, titled "Interactive Tactic-Based Program Synthesis".


## Bugs
3. Doing "varGlobal nil" on "[?7]" makes the next current hole computation not terminate.
4. In a recursive definition, the top-level definition isn't being recognized in the local context of holes inside it.