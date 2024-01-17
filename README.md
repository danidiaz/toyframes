
- [Implementing type-safe heterogeneous collections](https://discourse.haskell.org/t/implementing-type-safe-heterogeneous-collections/8605) on the Haskell Discourse.

- [Towards Scalable Dataframe Systems](https://arxiv.org/abs/2001.00888) paper.  

- Some relevant modules from `base`:
    - [`Data.Typeable`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Typeable.html) 
    - [`Type.Reflection`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Type-Reflection.html) that provides a more "powerful", type-indexed version of the `TypeRep`s from `Data.Typeable`, 
    - [`Data.Dynamic`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Dynamic.html) 
    - [`Data.Type.Equality`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Type-Equality.html) that provides [`testEquality`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Type-Equality.html#v:testEquality).
