## Tape (bi-directional byte tape)

A dynamically growing, size-capped, bi-directional byte tape for OCaml. Cells are bytes (masked to 8 bits). Designed for fast pointer movement and cell access, with optional left/right growth and efficient bulk transfers.

### Key features
- **Bi-directional growth**: Supports negative and positive logical positions via a bias offset.
- **Size cap**: Maximum size configurable; default is 32KB.
- **Byte semantics**: All writes are masked with `0xFF`.
- **Fast path**: Critical operations use inlining and unsafe array access after validation.
- **Bulk I/O**: Bounded and auto-growing blit operations with `int array` buffers.

### Creation
- `create ?bias_offset ?max_size initial_size`  
  - `bias_offset`: `Start | Middle` (default: `Middle`)  
  - `max_size`: default `32768` (32KB)  
  - `initial_size`: minimum effective size is `256`

### Pointer movement
- `move_exn t n` — Move pointer by `n` (can be negative); grows if needed.
- `scan_to_zero_exn t delta` — Scan in steps of `abs delta` in the sign of `delta` until a zero cell is found; performs a single final pointer move.

### Cell operations
- `get t` — Read current cell.
- `set t v` — Write masked value.
- `add t v` — Add masked value to current cell.
- `set_at_offset_exn t delta v` — Write at logical offset without moving.
- `add_at_offset_exn t delta v` — Add at logical offset without moving.
- `multransfer_exn t pairs` — If current cell is non-zero, zero it and add `source * c` to offsets `d` for each `(d, c)`.

### Bulk operations (int arrays)
- `blit_out_exn t dst len` — Copy `len` cells out starting at current position into `dst`.
- `blit_in_exn t src len` — Copy `len` cells from `src` into tape; no growth, errors on overflow.
- `blit_in_ensure_exn t src len` — Copy with automatic growth.

### Introspection
- `len t` — Current buffer length.  
- `bias t` — Current bias (physical offset for logical 0).  
- `logical_pos t` — Current logical pointer (can be negative).  
- `physical_pos t` — Current physical index in buffer.  
- `max_size t` — Configured size cap.

### Growth behavior
- Reallocates on pointer movement outside current bounds or when `blit_in_ensure_exn` needs space.
- Capacity increases exponentially: base ~1.5× current length, then doubles until sufficient, capped by `max_size`.
- Direction-aware:
  - Left expansion shifts existing data right to preserve right side positioning.
  - Right expansion preserves left side positioning.
  - Internal reallocation recenters bias for future bi-directional growth.

### Errors
All errors raise `TapeExn` with:
- `MaxAllocationReached of int * int`
- `BlitOutOfBounds of int * int * int`
- `BlitNegativeLength of int`

### Example
```ocaml
(* Create a middle-biased tape with 1KB initial size (min effective is 256) *)
let t = Tape.create 1024 in

Tape.move_exn t 10;
Tape.set t 65;        (* 'A' *)
let v = Tape.get t in

(* Bulk copy with auto growth *)
let src = [|1; 2; 3; 4; 5|] in
Tape.blit_in_ensure_exn t src 5;
```
