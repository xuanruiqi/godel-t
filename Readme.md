# Godel's System T
A simple interpreter for Godel's System T that doesn't actually work yet. Mainly because of the lack 
of type checking contexts. But yes, you could do some primitive recursion in it.  

I'm also planning on adding some theorem proving (proofs of progress, preservation and totality) to 
this, using Idris's theorem proving features. But this will be mainly orthogonal with the actual 
interpreter.  

## Building
`idris build lamt.ipkg`

## Acknowledgements
I took some code (mainly in `Parse`) from Edwin Brady himself.

## Author
Xuanrui Qi

## License
Mozilla Public License 2.0
