   Minimum period: 7.539ns (Maximum Frequency: 132.636MHz)
   Minimum input arrival time before clock: 8.540ns
   Maximum output required time after clock: 6.431ns
   Maximum combinational path delay: 6.951ns

  My design is based of the LC3-b datapath design in that it has seprate modules
  to run shift control, memory data decoding and pc control. There aren't any
  added modules that are more complicated than a mux or decoder. Most of the
  arithmetic instructions are handled by the ALU or shift modules. Register
  loads/stores are handled by the register files and 2 muxes. Memory stores and
  loads are either first run through an encoder or decoder in order to gain the
  correct byte information. PC control is manipulated by two muxes to the PC
  registers. The decoder outputs about 18 control signals to run this datapath.

  My critical path is somewhere in the means of a memory store or load. To store
  to memory requires signals from not only the ALU constructs but also the
  encoder. This is similar for a load, except it runs through a decoder as well
  as requires inputs to run through the register file.
