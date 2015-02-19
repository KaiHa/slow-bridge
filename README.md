# slow-bridge
A small script to throttle the network traffic by building a bridge.

With `brctl` and `tc` (respectively `netem`) you have (on Linux) the 
tools at hand to simulate a slow laggy network.

This small script shows how they can be used for that. 
