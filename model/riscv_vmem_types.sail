/*=======================================================================================*/
/*  This Sail RISC-V architecture model, comprising all files and                        */
/*  directories except where otherwise noted is subject the BSD                          */
/*  two-clause license in the LICENSE file.                                              */
/*                                                                                       */
/*  SPDX-License-Identifier: BSD-2-Clause                                                */
/*=======================================================================================*/

// Type constraint that checks if 'v is a valid virtual memory mode size.
// type is_sv_mode('v) -> Bool = 'v in {32, 39, 48, 57}

// Range of possible page levels depending on the virtual memory mode:
//
//   Sv32 = range(0, 1)
//   Sv39 = range(0, 2)
//   Sv48 = range(0, 3)
//   Sv57 = range(0, 4)
//
type level_range = range(0, 2)

// Size of a Page Table Entry, depending on the virtual memory mode.
type pte_bits = bits(64)

// Number of Physical Page Number bits, depending on the virtual memory mode.
type ppn_bits = bits(44)

// Number of Virtual Page Number bits, depending on the virtual memory mode.
type vpn_bits = bits(39 - pagesize_bits)


// Extensions for memory Accesstype.

type ext_access_type = unit

let Data  : ext_access_type = ()

let default_write_acc : ext_access_type = Data

val accessType_to_str : AccessType(ext_access_type) -> string
function accessType_to_str (a) =
  match a {
    Read(_)           => "R",
    Write(_)          => "W",
    ReadWrite(_, _)   => "RW",
    Execute()         => "X",
    Cache(CleanFlush) => "CACHE.CLEANFLUSH",
    Cache(Inval)      => "CACHE.INVAL",
    Cache(Zero)       => "CACHE.ZERO"
  }

overload to_str = {accessType_to_str}
