// This file contains ALU control logic.


package dinocpu.components


import chisel3._

import chisel3.util._


/**

 * The ALU control unit

 *

 * Input:  aluop        Specifying the type of instruction using ALU

 *                          . 0 for none of the below

 *                          . 1 for 64-bit R-type

 *                          . 2 for 64-bit I-type

 *                          . 3 for 32-bit R-type

 *                          . 4 for 32-bit I-type

 *                          . 5 for non-arithmetic instruction types that uses ALU (auipc/jal/jarl/Load/Store)

 * Input:  funct7       The most significant bits of the instruction.

 * Input:  funct3       The middle three bits of the instruction (12-14).

 *

 * Output: operation    What we want the ALU to do.

 *

 * For more information, see Section 4.4 and A.5 of Patterson and Hennessy.

 * This is loosely based on figure 4.12

 */

class ALUControl extends Module {

  val io = IO(new Bundle {

    val aluop     = Input(UInt(3.W))

    val funct7    = Input(UInt(7.W))

    val funct3    = Input(UInt(3.W))


    val operation = Output(UInt(5.W))

  })


  io.operation := "b11111".U // Invalid

  switch(io.aluop) {

    is(5.U) {


      io.operation := "b01100".U

      //io.operation := "b01101".U


      switch(io.funct3) {

        is("b011".U) {

          io.operation := "b01100".U

        }

      }

    }

    is(2.U) {

      switch(io.funct3) {

        is("b000".U) { //ADDI

          io.operation := "b01100".U

        }

        is("b010".U) { //SLTI

          io.operation := "b01010".U

        }

        is("b011".U) { //SLTUI

          io.operation := "b01011".U

        }

        is("b111".U) { //ANDI

          io.operation := "b00000".U

        }

        is("b110".U) { //ORI

          io.operation := "b00001".U

        }     

        is("b100".U) { //XORI

          io.operation := "b00010".U

        }                        

      }

      switch(io.funct7(6,1)) {

        is("b000000".U) {

          switch(io.funct3) {

            is("b001".U) { //SLLI

              io.operation := "b00110".U

            }

            is("b101".U) { //SRLI

              io.operation := "b01000".U

            }

          }

        }

        is("b010000".U) {

          switch(io.funct3) {

            is("b101".U) { // SRAI

              io.operation := "b00100".U

            }

          }

        }

      }

    }

    is(4.U) {

      switch(io.funct3) {

        is("b000".U) { //ADDIW

          io.operation := "b01101".U

        }

      }

      switch(io.funct7) {

        is("b0000000".U) {

          switch(io.funct3) {

            is("b001".U) { // SLLIW

              io.operation := "b00111".U

            }

            is("b101".U) { // SRLIW

              io.operation := "b01001".U

            }

          }

        }

        is("b0100000".U) {

          switch(io.funct3) {

            is("b101".U) { // SRAIW

              io.operation := "b00101".U

            }

          }

        }

      }

    }

    is(1.U) {

      switch(io.funct7) {

        is("b0000000".U) {

          switch(io.funct3) {

            is("b000".U) { //ADD

              io.operation := "b01100".U

            }

            is("b001".U) { //SLL

              io.operation := "b00110".U

            }

            is("b010".U) { //SLT

              io.operation := "b01010".U

            }

            is("b011".U) { //SLTU

              io.operation := "b01011".U

            }

            is("b100".U) { //XOR

              io.operation := "b00010".U

            }

            is("b101".U) { //SRL

              io.operation := "b01000".U

            }

            is("b110".U) { //OR

              io.operation := "b00001".U

            }

            is("b111".U) { //AND

              io.operation := "b00000".U

            }

          }

        }

        is("b0100000".U) {

          switch(io.funct3) {

            is("b000".U) { //SUB

              io.operation := "b01110".U

            }

            is("b101".U) { //SRA

              io.operation := "b00100".U

            }

          }

        }

        is("b0000001".U) {

          switch(io.funct3) {

            is("b000".U) { //MUL

              io.operation := "b10000".U

            }

            is("b001".U) { //MULH

              io.operation := "b10010".U

            }

            is("b010".U) { //MULHSU

              io.operation := "b11100".U

            }

            is("b011".U) { //MULHU

              io.operation := "b10011".U

            }

            is("b100".U) { //DIV

              io.operation := "b10100".U

            }

            is("b101".U) { //DIVU

              io.operation := "b10101".U

            }

            is("b110".U) { //REM

              io.operation := "b11000".U

            }

            is("b111".U) { //REMU

              io.operation := "b11001".U

            }

          }

        }

      }

    }

    is(3.U) {

      switch(io.funct7) {

        is("b0000000".U) {

          switch(io.funct3) {

            is("b000".U) { //ADDW

              io.operation := "b01101".U

            }

            is("b001".U) { //SLLW

              io.operation := "b00111".U

            }

            is("b101".U) { //SRLW

              io.operation := "b01001".U

            }

          }

        }

        is("b0100000".U) {

          switch(io.funct3) {

            is("b000".U) { //SUBW

              io.operation := "b01111".U

            }

            is("b101".U) { //SRAW

              io.operation := "b00101".U

            }

          }

        }

        is("b0000001".U) {

          switch(io.funct3) {

            is("b000".U) { //MULW

              io.operation := "b10001".U

            }

            is("b100".U) { //DIVW

              io.operation := "b10110".U

            }

            is("b101".U) { //DIVUW

              io.operation := "b10111".U

            }

            is("b110".U) { //REMW

              io.operation := "b11010".U

            }

            is("b111".U) { //REMUW

              io.operation := "b11011".U

            }

          }

        }

      }

    }

  }

}
