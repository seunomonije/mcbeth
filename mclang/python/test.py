# import mcl
# from mcl.ocaml_def_wrappers import *

import pyzx as zx
import sys
import json

def main():
  ocaml_output = None

  ## Reading from stdin
  for line in sys.stdin:
    ocaml_output = line

  loaded_program = json.loads(ocaml_output)

  graph = zx.Graph()
  for el in loaded_program:
    # Turns the dictionary into an iterable, 
    # and gets the first value.
    command = next(iter(el))

    if command == 'Prep':
      graph.add_vertex(qubit=el[command])

  circuit = zx.extract_circuit(graph.copy())
  print(circuit.to_qasm())
  


if __name__ == '__main__':
  main()


"""
print(mcl.approx_pi(1000))
print(mcl.print_from_ocaml(string='Hi! Did this pass of correctly?'))

program = Program()
program.add(Prep(1).serialize())
program.add(Prep(2).serialize())
program.add(Prep(3).serialize())

print(program)
print(mcl.build_program(cmd_type="Prep"))
"""