(**
   - [read_file rel_path] is the string contained in the file
   - found at the relative file path, [rel_path]
 **)
val read_file : string -> string

(**
   - [write_file rel_path to_write] writes [to_write]
   - into a file stored at [rel_path], creating it if
   - it does not exist.
**)
val write_file : string -> string -> unit
