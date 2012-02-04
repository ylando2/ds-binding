ds-binding
=========================

I love the way the arc language destruction binding work.
So I build one myself. I try two different approach. I thing that the
more functional is faster.

In this module there are three macros in two different versions.
The three macros are: ds-let, ds-set! and ds-define.

The destruction binding in the arc style work in the following way:
(ds-let argument lst body ...)
lst is a list and the argument are in a list structure in example: ((a) (b c))
If you want an argument to take the rest of the list you write: ( a b . rest )
If you want an optional argument you write ( a b ( o opt value))
where opt is the optional argument and value is the default value.

License
-------
ds-binding is released under the MIT license.
