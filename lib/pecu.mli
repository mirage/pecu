type decoder
(** The types for decoders. *)

type src = [ `Manual | `Channel of in_channel | `String of string ]
(** The type for input sources. With a [`Manual] source the client must provide
   input with {!src}. *)

type decode = [ `Await | `End | `Data of string | `Line of string | `Malformed of string ]

val src: decoder -> Bytes.t -> int -> int -> unit
(** [src d s j l] provides [d] with [l] bytes to read, starting at [j] in [s].
   This byte range is read by calls to {!decode} with [d] until [`Await] is
   returned. To signal the end of input call the function [l = 0]. *)

val decoder: src -> decoder
(** [decoder src] is a decoder that inputs from [src]. *)

val decode: decoder -> decode
(** [decode d] is:
    {ul
    {- [`Await] if [d] has a [`Manual] input source and awaits
       for more input. The client must use {!src} to provide it.}
    {- [`End] if the end of input was reached.}
    {- [`Malformed bytes] if the [bytes] sequence is malformed according to the
       decoded quoted-printable encoding scheme. If you are interested in a
       best-effort decoding you can still continue to decode after an error
       until the decode synchronizes again on valid bytes.}
    {- [`Data data] if a [data] sequence value was decoded.}
    {- [`Line line] if a [line seauence value plus a line-break was decoded.]}}

    {b Note.} Repeated invocation always eventually returns [`End], even
    in case of errors. *)

val decoder_line: decoder -> int
(** [decoder_line d] is the line number of the last decoded (or malformed)
   output. See {!decoder} for details. *)

val decoder_column: decoder -> int
(** [decoder_column d] is the column number of the last decoded (or malformed)
   output. See {!decoder} for details. *)

val decoder_byte_count: decoder -> int
(** [decoder_byte_count d] is the number of characters already decoded on [d]
   (inclueded malformed ones). This is the last {!decode}'s end output offset
   counting from beginning of the stream. *)

val decoder_count: decoder -> int
(** [decoder_count d] is the number of characters already decoded on [d]
   (including malformed ones). See {!decoder} for details. *)

val decoder_src: decoder -> src
(** [decoder_src d] is [d]'s input source. *)

val decoder_dangerous: decoder -> bool
(** [decoder_dangerous d] returns [true] if encoded input does not respect the
   80-columns rule. In this case, internal buffers can grow automatically. If
   you are interested in a best-effort decoding you can still continue to decode
   even if [decoder_dangerous d] returns [true]. However, it could be an attack
   entry point in a server-context. *)
