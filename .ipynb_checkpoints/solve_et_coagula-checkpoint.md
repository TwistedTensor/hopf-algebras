# Solve et Coagula
### Mathematical Alchemy

While alchemy as a scientific persuit has been discredited, the rich symbolism
and imagery has proved useful in expressing complex ideas through metaphors.
One of the basic principals of alchemy is the duality between breaking something
into its elemental parts, and recombining those elements into something
new. This process is refered to as *Solve et Coagula* (latin for solve and coagulate).

There is a certain set of interrelated constructions in mathematics which I have been
interested in for a while.  What is particularly
intriguing to me is that they connect seemingly disparate mathematical concepts in
beautiful and surprising ways. One commonality is that they can all be thought of
intuitively as being analogous to the *Solve et Coagula* process. At first glance it
may seem (and rightly so) that the process of taking things apart and reassembling their
peices is so vague and general that it could apply to almost anything. However, the
examples I have in mind are quite specific and the way they connect with eachother
is quite deep and non-trivial.

As the easiest example I can think of, consider expanding a product of polynomials:
$$(a_0+a_1x+...+a_nx^n)(b_0+b_1x+...+b_mx^m)$$ 
Supposed we are asked, "What is the coefficient of $x^j$ in the product?"

While this is trivial, considering the process by which we compute the answer will
be instructive for what follows. The process is as follows:

- Consider the set of terms of the left factor and the set of terms from the right
factor:

$$L=\{a_0,a_1x,...,a_nx^n\},R=\{b_0,b_1x,...,b_mx^m\}$$

- Consider every possible product of an element of $L$ with an element of $R$ which
results in a term of order $j$:

$$\{a_0b_jx^j,a_1b_{j-1}x^j,...,a_jb_0x^j\}$$

- Sum these coefficients to get the desired result:

$$a_0b_j+a_1b_{j-1}+...+a_jb_0$$


We have broken each polynomial into its constituate parts (the *solve* step), then
we have recombined some of them to get the result (The *coagula* step). Note that the
recombination actually happens in two steps, first we take every combination
of elemental parts (monomials) that can combine to give us what we want, then we sum
over all such pairs. This idea of summing over every possible way to get a particular
result is the key to undertanding the ubiquity of this type of process. Some key
examples are:

- In **combinatorics**, in order to count the number of objects of a certain class
(e.g. partitions of an integer, subets of a finite set), we can decompose the object
into simpler parts. For each such decomposition, we count how many ways we can get
the first part, second part, ... and multiply them together to count the number of
objects with that specific decomposition. Then we sum over all decompositions. The
connection between this idea and the multiplication of polynomials is made explicit
in the theory of *generating functions*.

- In **probability theory**, to find the probability of some complex event occuring (say
rolling a pair of dice and getting 7) we can consider all of the ways to decompose it
into simpler events (rolling 1 die). The probability of a specific decomposition will be
the product of the probabilities of the simple events (rolling 4 on die 1 and 3 on die 2
has a probability of $\frac{1}{6}\cdot\frac{1}{6}=\frac{1}{36}$. Summing over all
decompositions (all values of die 1 and die 2 which add to 7) gives the probability of
the complex event. In the case of a discrete event space this is clearly a consequence
of the combinatorial result since we are counting the number of favorable outcomes
and the number of total outcomes. However, similar results hold true for continuous
distributions if we replace sums with integrals (this leads naturally to the following).


- Convolution
- Integral transforms
- Zeta function
- Bayesian statistics
- Statistical mechanics
- Quantum mechanics
- Group algebra
- Groupoid algebra
- Hopf algebra
- Special functions?
- Control theory?
- PDEs?
- Lie groups?
- Mathematical finance?

Categorical explanation in terms of adjoint functors (solve functor et coagula functor)?
Execise (POLY) from Chapter 3.1 of Toposes, Theories and Triples is as follows

*Let R be any commutative ring. For each set X, let T X be the set of polynomials
in a finite number of variables with the variables in X and coefficients from R.
Show that T is the functor part of a triple (µ is defined to “collect terms”).*

Considering $T$ as an endofunctor on sets, if $f$ is a set function $A\to B$, define
$Tf:TA\to TB$ as follows: $$f\big(\sum_{(ij..l)} r_{(ij..l)}a_0^ia_1^j\cdots
a_n^l\big) = \sum_{(ij..l)} r_{(ij..l)}f(a_0)^if(a_1)^j\cdots f(a_n)^l$$

$T^2A$ will be the set of polynomials $q=\sum_{(ij..l)} r_{(ij..l)}p_0^ip_1^j\cdots
p_n^l$ where each $p_i$ is an element of (the set) $TA$. As an element of $T^2A$ we
cannot
simplify such an expression since we are to regard each $p_i$ as a formal symbol.
In essence, we have applied the forgetful functor in order to regard $TA$ as a set.
If again consider each $p_i$ to be an element of the polynomial ring with variables
in $A$ and coefficients in $R$, then we can simplify $q$ by distributing and combining
like terms and we will get a new polynomial with variables in $A$ and coefficients in
$R$. This recombination gives us the NT $\mu:T^2\to T$.   
Since $R$ is commutative, we have that $\mu\circ(\mu T)=\mu\circ(T \mu)$.

Now, let $\eta$ be the NT defined by letting $\eta id_A:A\to R[[A]]$ be the map
which sends each element $a\in A$ to the monomial $a\in R[[A]]$. For any polynomial,
replacing each variable $a$ with the formal monomial $a$, the un-formalizing to again
view $a$ as a variable clearly does nothing. Thus $\mu\circ(\eta T)=\mu\circ(T\eta)
= id_T$

