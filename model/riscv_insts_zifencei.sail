/*=======================================================================================*/
/*  This Sail RISC-V architecture model, comprising all files and                        */
/*  directories except where otherwise noted is subject the BSD                          */
/*  two-clause license in the LICENSE file.                                              */
/*                                                                                       */
/*  SPDX-License-Identifier: BSD-2-Clause                                                */
/*=======================================================================================*/

/* ****************************************************************** */
/* This file specifies the instructions in the 'Zifencei' extension.     */

/* ****************************************************************** */

function clause extensionEnabled(Ext_Zifencei) = true

union clause ast = FENCEI : (bits(17), bits(5))

mapping clause encdec = FENCEI(res1, res2) if extensionEnabled(Ext_Zifencei)
  <-> res1 @ 0b001 @ res2 @ 0b0001111 if extensionEnabled(Ext_Zifencei)

/* fence.i is a nop for the memory model */
function clause execute FENCEI(res1, res2) = { /* __barrier(Barrier_RISCV_i); */ RETIRE_SUCCESS }

// mapping clause assembly = FENCEI() <-> "fence.i"
