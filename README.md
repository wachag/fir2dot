fir2dot

A small, "hacked together in one rainy morning" FIRRTL pass which generates a Graphviz description of the module hierarchy. 
Because I needed it, and going the YoSys way was not good enough for me.

Graphviz - AFAIK - can't create a good rectilinear layout, the resulting graph is not really nice.

Usage in sbt:

runMain Main design.fir

where design.fir is the generated FIRRTL for your Chisel hardware.
