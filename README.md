# Mr. Reds
REliable Dynamic Simulation for Meandering Rivers (just a name!)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3885611.svg)](https://doi.org/10.5281/zenodo.3885611)

## License
This project is licensed under the terms of the MIT license.  
[HERE](https://github.com/zht9947/Mr_Reds/blob/master/LICENSE.md) to see the full text.

Parts of I/O of this project are based on the XMDF library ([www.xmswiki.com/wiki/XMDF](https://www.xmswiki.com/wiki/XMDF)).  
Please see this file:  
&emsp;|-- \_\_Sources\\  
&emsp;&emsp;|-- MR\_MOD\_FILE\_MANIPULATIONS\\  
&emsp;&emsp;&emsp;|-- MR\_MOD\_FILE\_XMDF\_MANIPULATIONS\\  
&emsp;&emsp;&emsp;&emsp;|-- XMDF\\  
&emsp;&emsp;&emsp;&emsp;&emsp;|-- XMDF.f90  
for its license.

## Notes on compilation
+ You may need to compile your own version of the XMDF library.  
  + Download its source codes from [www.xmswiki.com/wiki/XMDF](https://www.xmswiki.com/wiki/XMDF) (v2.2 or later is required).
+ If you compile this project on a non-intel(R) processor, you may need to comment out all the compiler directives starting with "!DIR$" to disable relevant optimizations.
