# Testing in Camelfuck

This directory contains unit tests for the Camelfuck project.

## Running Tests

To run all tests:
```bash
dune runtest
```

To run tests for a specific module:
```bash
dune exec test/tape/test_tape.exe
```

To run tests in watch mode (re-run on file changes):
```bash
dune runtest -w
```

## Adding New Tests

1. Create a new test file in the appropriate directory
2. Add a corresponding dune file or update the existing one
3. Use **Alcotest** for writing test cases
4. Ensure the library being tested is listed in the dune file's `libraries` field
