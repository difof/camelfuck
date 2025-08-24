# Brainfuck Tape

A high-performance, bi-directional tape implementation for the Brainfuck interpreter. This OCaml implementation provides a dynamically resizing memory tape with efficient pointer movement and memory management optimizations.

## What is a Brainfuck Tape?

In Brainfuck, the tape is the primary data structure - an array of memory cells that the program manipulates using a movable pointer. Traditional implementations use a fixed-size array, but this becomes problematic when programs need to move far left (negative indices) or grow beyond initial bounds.

The standard Brainfuck operations on the tape are:
- `>` and `<` - move the pointer right or left
- `+` and `-` - increment or decrement the current cell value
- `[` and `]` - loop while current cell is non-zero
- `.` and `,` - output and input current cell value

## Implementation Details

This implementation solves several key challenges with Brainfuck tape management:

### Bi-directional Growth

Unlike simple array based tapes, this implementation supports movement in both directions from the starting position. When the pointer moves beyond current bounds (either negative or positive), the tape automatically reallocates with space on both sides.

The `bias` system is central to this design:
- The tape maintains a logical position (`pos`) and a bias offset (`bias`)
- Physical buffer index = `bias + pos`
- When `pos` goes negative, we still have valid physical addresses

### Smart Reallocation Strategy

Memory grows intelligently based on access patterns:

```ocaml
let rec grow x = if x < needed then grow (x lsl 1) else x in
let new_len = grow (max 32 (t.len + (t.len lsr 1))) in
```

- Minimum allocation: 256 bytes
- Growth factor: 1.5x current size, rounded up to next power of 2
- Direction aware: growing left preserves right side data positioning
- Maximum size cap prevents runaway allocation

### Three Growth Scenarios

1. **Left expansion** (`new_index < 0`): Allocates extra space on the left, shifts existing data right in one blit operation
2. **Right expansion** (`new_index >= current_length`): Allocates space on the right, keeps existing data in place  
3. **Internal reallocation**: Centers the bias for future bi-directional growth

### Memory Safety

The implementation includes comprehensive bounds checking:
- `MaxAllocationReached` - caps memory usage to prevent system exhaustion
- `BlitOutOfBounds` - prevents buffer overruns during bulk operations
- `BlitNegativeLength` - validates copy operation parameters

All operations come in two variants:
- `_exn` versions that throw exceptions for error handling
- Safe versions that return `Result` types for functional error handling

### Performance Optimizations

Several design choices optimize for common Brainfuck execution patterns:

- **Inline functions**: Critical path operations like `get`, `set`, and `move` are inlined
- **Unsafe operations**: Internal buffer access uses `unsafe_get`/`unsafe_set` after bounds validation
- **Byte masking**: Cell values are automatically masked to 8-bit range (`v land 0xFF`)
- **Minimal allocation**: Only grows when absolutely necessary

### Bulk Operations

The tape supports efficient bulk data transfer:
- `blit_out` - extract data from tape to external buffer
- `blit_in` - copy external data into tape (with bounds checking)
- `blit_in_ensure` - copy external data with automatic growth

These operations are useful for implementing Brainfuck extensions or optimizing interpreter performance with block operations.

## Usage

```ocaml
(* create a tape with middle bias and 1KB initial size *)
let tape = Tape.create 1024 in

(* move around and manipulate data *)
Tape.move_exn tape 10;
Tape.set tape 65;  (* set to 'A' *)
let value = Tape.get tape in

(* move to negative positions *)
Tape.move_exn tape (-20);
Tape.set tape 66;  (* set to 'B' *)

(* bulk operations *)
let data = Bytes.of_string "Hello" in
Tape.blit_in_ensure_exn tape data 5;
```

## Configuration Options

- `bias_offset`: Choose `Start` or `Middle` based on program's memory growth patterns
- `max_size`: Set upper limit on tape size (default: 16KB)
- `initial_size`: Starting buffer size (minimum: 256 bytes)
