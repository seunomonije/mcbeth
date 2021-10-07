import cirq
import sys
import json

class CirqBuilder:
  CMD_MAP = {
    'Prep': cirq.H,
    'XCorrect': cirq.X,
    'ZCorrect': cirq.Z,
    'Entangle': cirq.CZ,
    'Measure': cirq.measure,
  }

  def to_qasm(self) -> str:
    return self.circuit.to_qasm()

  def to_circuit(self) -> cirq.Circuit:
    return self.circuit

  def __init__(self, json_input: str) -> None:
    self.circuit = cirq.Circuit()
    self.qubit_map = {}
    self._convert_json_to_cirq_circuit(json_input)

  def _convert_json_to_cirq_circuit(self, json_input: str) -> None:
    """Converts json input string into a Cirq circuit.

    Args:
      json_input: the JSON string to be converted.
    """

    for el in json_input:
      command = next(iter(el))
      command_data = el[command]

      if command == 'Prep':
        qubit_no = command_data['on_qubits'][0]
        cirq_qubit = cirq.LineQubit(qubit_no)
        self.qubit_map[qubit_no] = cirq_qubit
      
      
      op = self.CMD_MAP.get(command)
      cirq_qubits = self._get_qubits_of_command(command_data)
      self.circuit.append(op(*cirq_qubits))

  def _get_qubits_of_command(self, command_data) -> list:
    cirq_qubits = []
    for qubit in command_data['on_qubits']:
      cirq_qubit = self.qubit_map.get(qubit)
      cirq_qubits.append(cirq_qubit)
    return cirq_qubits

def main():
  stdin = ''

  ## Reading from stdin
  for line in sys.stdin:
    # Just concatenate every line since
    # we're only expecting a single string
    stdin += line

  loaded_program = json.loads(stdin)
  print(loaded_program)

  circuit = CirqBuilder(loaded_program)
  print(circuit.to_circuit())
  print(circuit.to_qasm())

if __name__ == '__main__':
  main()