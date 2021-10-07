# import mcl
# from mcl.ocaml_def_wrappers import *

import pyzx as zx
import sys
import json

class ZXBuilder:
  CMD_MAP = {
    'Prep': zx.gates.HAD,
    'XCorrect': zx.gates.NOT,
    'ZCorrect': zx.gates.Z,
    'Entangle': zx.gates.CZ,
  }

  def __init__(self, json_input: str) -> None:
    self.graph = zx.Graph()
    self._convert_json_to_zx_graph(json_input)
    return
  

  def to_qasm(self) -> str:
    circuit = self.to_circuit()
    return circuit.to_qasm()

  def to_circuit(self):
    return zx.extract_circuit(self.graph)

  def _convert_json_to_zx_graph(self, json_input: str) -> None:
    """Converts json input string into a ZX graph data structure.

      Args:
        json_input: the JSON string to be converted.
    """
    for el in json_input:
      # Turns the dictionary into an iterable, 
      # and gets the first value.
      command = next(iter(el))
      command_data = el[command]

      if command == 'Prep':
        # We know that the first and only element of the qubit list
        #  is the qubit index.
        self.graph.add_vertex(qubit=command_data['on_qubits'][0])

      op = self.CMD_MAP.get(command)
      # Overwrites the current graph each time it's ran.
      self.graph = self._add_op_to_graph(op(*command_data['on_qubits']), self.graph)

  def _add_op_to_graph(self, operation: zx.gates.Gate, graph: zx.Graph) -> zx.Graph:
    """Adds an operation to a PyZX graph by converting to
    circuit form, adding the operation to the circuit, then
    transitioning back to graph form.

    Args:
      operation: the operation to add to the graph.
      qubits: the qubit(s) on which the operation should be added.
              Note that for multi-qubit gates, the control gate(s)
              should be provided at the beginning of the list.
      graph: the graph to add the operation to.

    Returns:
      The overwritten graph with the operation added to it.
    """
    circuit = zx.extract_circuit(graph)
    circuit.add_gate(operation)
    returned_graph = circuit.to_graph()
    return returned_graph

def main():
  stdin = ''

  ## Reading from stdin
  for line in sys.stdin:
    # Just concatenate every line since
    # we're only expecting a single string
    stdin += line

  loaded_program = json.loads(stdin)
  print(loaded_program)

  zx_graph = ZXBuilder(loaded_program)
  qasm = zx_graph.to_qasm()
  print(qasm)

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