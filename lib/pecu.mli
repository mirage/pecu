type decoder

type src = [ `Manual | `Channel of in_channel | `String of string ]
type decode = [ `Await | `End | `Data of string | `Line of string | `Malformed of string ]

val src: decoder -> Bytes.t -> int -> int -> unit
val decoder: src -> decoder
val decode: decoder -> decode

val decoder_line: decoder -> int
val decoder_column: decoder -> int
val decoder_byte_count: decoder -> int
val decoder_count: decoder -> int
val decoder_src: decoder -> src
val decoder_dangerous: decoder -> bool
