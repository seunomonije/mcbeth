import cirq
import sys
import json
from typing import Union, Dict, Tuple, Iterable, Sequence

def _default_measurement_key_(qubits: Iterable[cirq.Qid]) -> str:
  return ','.join(str(q) for q in qubits)

class XBasisMeasurementGate(cirq.Gate):
  """Custopm measurement gate to measure in the X-basis
  with specified angle.
  """
  def __init__(self, theta, key=''):
    super(XBasisMeasurementGate, self)
    self.theta = theta
    self.key = key

  def _num_qubits_(self):
    return 1

  @property
  def key(self) -> str:
    return str(self.mkey)

  @key.setter
  def key(self, key: Union[str, cirq.value.MeasurementKey]) -> None:
    if isinstance(key, str):
      key = cirq.value.MeasurementKey(name=key)
    self.mkey = key

  def with_key(self, key: Union[str, cirq.value.MeasurementKey]) -> 'XBasisMeasurementGate':
    """Creates a XBasisMeasurement gate with a new key but otherwise identical."""
    if key == self.key:
        return self
    return XBasisMeasurementGate(self.theta, key=key)

  def _with_measurement_key_mapping_(self, key_map: Dict[str, str]) -> 'XBasisMeasurementGate':
    return self.with_key(cirq.protocols.with_measurement_key_mapping(self.mkey, key_map))

  def _is_measurement_(self) -> bool:
    return True

  def _measurement_key_name_(self) -> str:
    return self.key

  def _measurement_key_obj_(self) -> cirq.value.MeasurementKey:
    return self.mkey

  def _decompose_(self, qubits):
    # Note the qubits here is a tuple with one
    yield cirq.H(*qubits)
    yield cirq.Rx(rads=self.theta)(*qubits)
    yield cirq.MeasurementGate(1, self.mkey).on(*qubits)

  def _circuit_diagram_info_(self, args):
    return f"XM^{self.theta}"
  

class OpResolver:
  def _resolve_prep(
    qubit: cirq.LineQubit,
    **kwargs,
  ):
    return cirq.H(qubit)

  def _resolve_x_correction(
      qubits: 'list[cirq.LineQubit]', 
      **kwargs,
      ):
      signal_qubits = kwargs.get('signal_qubits')

      # We know there's only one qubit in the cirq_qubits list here,
      # so we can use the * operator to deconstruct safely.
      if signal_qubits:
        ops = []
        for signal in signal_qubits:
          ops.append(cirq.CX(signal, *qubits))
        return ops
      else: 
        return cirq.X(*qubits)

  def _resolve_z_correction(
    qubits: 'list[cirq.LineQubit]',
    **kwargs,
    ):
    signal_qubits = kwargs.get('signal_qubits')

    # We know there's only one qubit in the cirq_qubits list here,
    # so we can use the * operator to deconstruct safely.
    if signal_qubits:
      ops = []
      for signal in signal_qubits:
        ops.append(cirq.CZ(signal, *qubits))
      return ops
    else: 
      return cirq.Z(*qubits)

  def _resolve_entanglement(
    qubits: 'list[cirq.LineQubit]',
    **kwargs,
  ):
    return cirq.CZ(*qubits)

  def _resolve_measurement(
    qubits: 'list[cirq.LineQubit]',
    **kwargs,
    ):
    angle = kwargs.get('measurement_angle')
    signal_s_qubits = kwargs.get('signal_s_qubits')
    signal_t_qubits = kwargs.get('signal_t_qubits')

    # Here, we should be able to just apply CNOT or CZ with control signal_s/signal_t to 
    # the desired qubit
    ops = []
    if signal_s_qubits:
      for signal_qubit in signal_s_qubits:
        ops.append(cirq.CX(signal_qubit, *qubits))
    if signal_t_qubits:
      for signal_qubit in signal_t_qubits:
        ops.append(cirq.CZ(signal_qubit, *qubits))
    
    # Uses the custom XBasisMeasurementGate class to handle
    # measurement
    gate = XBasisMeasurementGate(angle)
    ops.append(gate)
      
    return ops

class CirqBuilder:
  def to_qasm(self) -> str:
    return self.circuit.to_qasm()

  def to_circuit(self) -> cirq.Circuit:
    return self.circuit

class StrictCirqBuilder(CirqBuilder):
  """Builds a Cirq circuit strictly from a provided MCL program. 

  StrictCirqBuilder "strictly" builds circuits by taking commands
  from an serialized MCL program and placing operators at the 
  corresponding timestamps. For circuits that don't include dependent
  measurements, StrictCirqBuilder returns an accurate gate-based
  translation of an MCL program. 
  
  For circuits that do include dependent measurements, however, 
  StrictCirqBuilder deconstructs all dependent measurements into
  a series of controlled gates, and measurement operators. In most
  cases, the measurement will occur before the controlled gates, and
  StrictCirqBuilder will incorrectly construct a circuit repesenting this.

  An example of this can be seen in the simple quantum teleportation example
  (found on page 15 of Danos' The Measurement Calculus)
  X_3^(s_2) M_2^(-B) E_23 X_2^(s_1) M_1^(-a) E_12

  This input provides the following strict output:
  1: ───H───@───M───@───────────────
            │       │
  2: ───H───@───────X───@───M───@───
                        │       │
  3: ───H───────────────@───────X───

  The ValidCirqBuilder class uses the output from StrictCirqBuilder
  to construct valid circuits via the deferred measurement principle. 

  """
  CMD_MAP = {
    'Prep': OpResolver._resolve_prep,
    'XCorrect': OpResolver._resolve_x_correction,
    'ZCorrect': OpResolver._resolve_z_correction,
    'Entangle': OpResolver._resolve_entanglement,
    'Measure': OpResolver._resolve_measurement,
  }

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

      # Gather information about each operation. Assumes command_data 
      # contains maximum amount of information, and leaves determining
      # what to actually build to the resolver functions. 
      #
      # All commands are required to have a list of qubits to act on. Every
      # other item is optional/command depenedent.
      on_qubits = self._get_qubits_from_command_ints(command_data['on_qubits'])
      
      signal_qubits = self._get_qubits_from_command_ints(command_data.get('signals', []))
      signal_s_qubits = self._get_qubits_from_command_ints(command_data.get('signal_s', []))
      signal_t_qubits = self._get_qubits_from_command_ints(command_data.get('signal_t', []))

      measurement_angle = command_data.get('angle', None)


      # Rather than having a giant if/else clause, we pass everything through
      # to the resolver and let the functions handle deciidng what to add to
      # the circuit.
      # on_qubits is overwritten here to tyoe cirq.LineQubit, which is an exception.
      if command == 'Prep':
        qubit_no = command_data['on_qubits'][0]
        on_qubits = cirq.LineQubit(qubit_no)
        self.qubit_map[qubit_no] = on_qubits

      resolver = self.CMD_MAP.get(command)
      ops = resolver(
        on_qubits, 
        signal_qubits=signal_qubits, # For X and Z corrections
        signal_s_qubits=signal_s_qubits, # For measurement
        signal_t_qubits=signal_t_qubits, # For measurement
        measurement_angle=measurement_angle # For measurement
        )
      print(ops)
      self.circuit.append(ops)

  def _get_qubits_from_command_ints(self, qubit_list) -> list:
    cirq_qubits: list[cirq.LineQubit] = []

    for qubit in qubit_list:
      cirq_qubit: cirq.LineQubit = self.qubit_map.get(qubit)
      cirq_qubits.append(cirq_qubit)

    return cirq_qubits if len(cirq_qubits) > 0 else None

class ValidCirqBuilder(CirqBuilder):
  """
  
  ValidCirqBuilder implements the deferred measurement principle.
  To expand on the example in StrictCirqBuilder, the strict circuit:
  1: ───H───@───M───@───────────────
            │       │
  2: ───H───@───────X───@───M───@───
                        │       │
  3: ───H───────────────@───────X───

  is converted to the following valid circuit:

  1: ───H───@───────@───M───────────────
            │       │
  2: ───H───@───────X───@───────@───M───
                        │       │
  3: ───H───────────────@───────X───────
  """
  def __init__(self, strict_cirq_circuit):
    self.circuit = strict_cirq_circuit

  def _remove_measurements_from_circuit(self):
    measurement_tuples = []
    measurement_operations = []

    for i, moment in enumerate(self.circuit):
      for operation in moment.operations:
        if isinstance(operation.gate, cirq.MeasurementGate):
          measurement_tuples.append((i, operation))
          measurement_operations.append(operation)
    
    self.circuit.batch_remove(measurement_tuples)
    return measurement_operations

  def rearrange_measurements(self):
    """Rearranges measurements by finding the location
    of each measurement operator in the circuit, removing it,
    and then appending it to the end.
    """

    measurement_operations = self._remove_measurements_from_circuit()
    # Store a copy of the stripped_circuit
    self.stripped_circuit = self.circuit.copy()
    self.circuit.append(measurement_operations)

  
def main():
  stdin = ''

  ## Reading from stdin
  for line in sys.stdin:
    # Just concatenate every line since
    # we're only expecting a single string
    stdin += line

  loaded_program = json.loads(stdin)
  print(loaded_program)

  strict_circuit = StrictCirqBuilder(loaded_program)
  print(strict_circuit.to_circuit())
  print(strict_circuit.to_qasm())

  # valid_circuit = ValidCirqBuilder(strict_circuit.to_circuit())
  # valid_circuit.rearrange_measurements()

  # print(valid_circuit.to_circuit())
  # print(valid_circuit.stripped_circuit)
  # print(valid_circuit.to_qasm())

if __name__ == '__main__':
  main()