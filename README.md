
- [Implementing type-safe heterogeneous collections](https://discourse.haskell.org/t/implementing-type-safe-heterogeneous-collections/8605) on the Haskell Discourse.

- [Towards Scalable Dataframe Systems](https://arxiv.org/abs/2001.00888) paper.  

- Some relevant modules from `base`:
    - [`Data.Typeable`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Typeable.html) 
    - [`Type.Reflection`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Type-Reflection.html) that provides a more "powerful", type-indexed version of the `TypeRep`s from `Data.Typeable`, 
    - [`Data.Dynamic`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Dynamic.html) 
    - [`Data.Type.Equality`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Type-Equality.html) that provides [`testEquality`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Type-Equality.html#v:testEquality).

- ["A reflection on types" paper (2016)](https://www.seas.upenn.edu/~sweirich/papers/wadlerfest2016.pdf) that explains the theory behind [`Type.Reflection`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Type-Reflection.html). A [talk on the subject](https://www.youtube.com/watch?v=asdABzBUoGM) by Stephanie Weirich.