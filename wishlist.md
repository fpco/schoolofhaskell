This is just a braindump wishlist:

Type Info Wishlist
------------------

* Explanations for builtin type syntax.  For example, explanations of
forall, (=>), tuples, lists, etc.  These could also be made into links
to relevant documentation, though that might bug advanced users (could
make this a setting).

  - Would also apply to highlighted expressions etc, could possibly
  also apply to code in the editor.

* Highlighting of AST nodes of the type on hover.  This lets the user
know the structure of the type's AST, and can aid in learning Haskell
syntax.  Unfortunately, this is not possible to do perfectly with our
current information, because we need fixities.  Even for advanced
users, this will be helpful for understanding types that involve infix
operators.

  - For expressions within the editor, the above is already given by type
  info.

* Allow type synonyms to be replaced by definitions and vice versa.

  - The equivalent in expressions would be inlining and abstraction
  refactorings.

I think the best implementation strategy for the items above is to
expose annotated ASTs from ide-backend, possibly involving
modifications of GHC itself.  Until then, we'll stick to the
haskell-src-exts / autocomplete map annotation of type info.
