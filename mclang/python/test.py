# import mcl
# from mcl.ocaml_def_wrappers import *

import pyzx as zx
import sys
import json

class ZXBuilder:
  OPERATIONS_MAP = {
    'Prep': zx.graph.base.BaseGraph.add_vertex

  }

def main():
  ocaml_output = ''

  ## Reading from stdin
  for line in sys.stdin:
    # Just concatenate every line since
    # we're only expecting a single string
    ocaml_output += line

  print(ocaml_output)
  loaded_program = json.loads(ocaml_output)
  
  graph = zx.Graph()
  for el in loaded_program:
    # Turns the dictionary into an iterable, 
    # and gets the first value.
    command = next(iter(el))

    # For the time being, repeatedly convert
    # back and forth from graph to circuit
    # form when adding operations.
    if command == 'Prep':
      graph.add_vertex(qubit=el[command])

    if command == 'XCorrect':
      circuit = zx.extract_circuit(graph)
      circuit.add_gate('NOT', el[command])
      graph = circuit.to_graph()

    if command == 'Entangle':
      qubit1, qubit2 = el[command][0], el[command][1]
      circuit = zx.extract_circuit(graph)
      circuit.add_gate("CZ", qubit1, qubit2)
      graph = circuit.to_graph()


  circuit = zx.extract_circuit(graph.copy())
  print(graph)
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