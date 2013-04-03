My GHR is initialized to be 12'hFFF while my PHT entries are all 2'b00.
GHR will locate a prediction from the PHT and calculate a taken/not take path in
the decode state. The predicted instruction will enter in the Fetch state as the
branch goes to the decode state. So a NOP is inserted between the two.

Using this branch prediction primes takes 126911950 cycles.
