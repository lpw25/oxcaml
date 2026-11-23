(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(** Probe management for code emission *)

type probe = private
  { stack_offset : int;
    num_stack_slots : int Stack_class.Tbl.t;
    contains_calls : bool;
    probe_name : string;
    probe_enabled_at_init : bool;
    probe_handler_code_sym : string;
        (** Record frame info held in the corresponding mutable variables. *)
    probe_label : Label.t;
        (** Probe site, recorded in .note.stapsdt section for enabling and
            disabling the probes *)
    probe_insn : Linear.instruction
        (** For optimized probes, the Iprobe instruction, recorded at the probe
            site and used for emitting the notes and the wrapper code at the end
            of the compilation unit. For non-optimized probes, this will be a
            direct call instruction. *)
  }

val get_probes : unit -> probe list

(** Add a probe site. *)
val add_probe :
  stack_offset:int ->
  num_stack_slots:int Stack_class.Tbl.t ->
  contains_calls:bool ->
  probe_name:string ->
  probe_enabled_at_init:bool ->
  probe_handler_code_sym:string ->
  probe_label:Label.t ->
  probe_insn:Linear.instruction ->
  unit

(** Add a [%probe_point] site. [label] is the address recorded in its
    .note.stapsdt note. *)
val add_point : name:string -> label:Label.t -> dbg:Debuginfo.t -> unit

(** The local symbol of the stub that increments the counts array [counts], for
    use by [%probe_point] sites. *)
val find_or_add_point_stub : Cmm.symbol -> Asm_targets.Asm_symbol.t

(** The counts array and stub symbols of the stubs needed in this compilation
    unit. *)
val get_point_stubs : unit -> (Cmm.symbol * Asm_targets.Asm_symbol.t) list

(** Reset the probe semaphore registry *)
val reset : unit -> unit

(** Find or add a semaphore for a probe name. Returns the label string for the
    semaphore symbol.
    - [name]: probe name
    - [enabled_at_init]: whether the probe is enabled at initialization
    - [dbg]: debug info for error reporting

    Raises Emitaux.Error (Inconsistent_probe_init ...) if the same probe is used
    with different enabled_at_init values. *)
val find_or_add_semaphore : string -> bool option -> Debuginfo.t -> string

(** Emit probe notes and semaphores to the assembly output *)
val emit_probe_notes : add_def_symbol:(string -> unit) -> unit
