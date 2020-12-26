#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>

const char FILENAME[] = "input.txt";
const char DELIMITER = ',';

struct Machine {
  unsigned int pc;
  int *program;
  int halted;
};

enum Addressing {
  POSITION,
  IMMEDIATE
};

struct Opcode {
  int operation;
  int addressing1;
  int addressing2;
  int addressing3;
};

struct ArithmeticOp {
  int lhs;
  int rhs;
  int out_ptr;
};

struct JumpOp {
  int condition;
  int target;
};

int count_occurences(char *input, int length, char search) {
  int occurences = 0;
  for (int i = 0; i < length; i++) {
    if (input[i] == search)
      occurences++;
  }

  return occurences;
}

int* parse_program(char *input) {
  int length = count_occurences(input, strlen(input), DELIMITER) + 1;
  int *values = (int *)malloc(sizeof(int) * length);
  if (values == NULL) {
      fprintf(stderr, "Could not allocate");
      exit(1);
  }

  char *token;
  for (int i = 0; i < length; i++) {
    if (i == 0) {
      token = strtok(input, &DELIMITER);
    } else {
      token = strtok(NULL, &DELIMITER);
    }

    if (token == NULL) {
      fprintf(stderr, "Verify your input file.");
      exit(1);
    }

    char *end;
    long value = strtol(token, &end, 10);
    values[i] = (int)value;
  }

  return values;
}

int fetch_pc(struct Machine *machine) {
  int value = machine->program[machine->pc];
  machine->pc++;
  return value;
}

// hahahahahaha fuck it's bad
struct Opcode fetch_opcode(struct Machine *machine) {
  int value = fetch_pc(machine);
  int operation = value - (value / 100) * 100;
  int addressing1 = (value - (value / 1000) * 1000 - operation) / 100;
  int addressing2 = (value - (value / 10000) * 10000 - operation - addressing1 * 100) / 1000;
  int addressing3 = 0;

  struct Opcode opcode = {operation, addressing1, addressing2, addressing3};
  return opcode;
}

struct ArithmeticOp arithmetic_op(struct Machine *machine, struct Opcode opcode) {
  int lhs = fetch_pc(machine);
  if (opcode.addressing1 == POSITION)
    lhs = machine->program[lhs];
  int rhs = fetch_pc(machine);
  if (opcode.addressing2 == POSITION)
    rhs = machine->program[rhs];
  int out_ptr = fetch_pc(machine);
  struct ArithmeticOp op = {lhs, rhs, out_ptr};

  return op;
}

void add(struct Machine *machine, struct Opcode opcode) {
  struct ArithmeticOp op = arithmetic_op(machine, opcode);
  int result = op.lhs + op.rhs;
  machine->program[op.out_ptr] = result;
}

void multiply(struct Machine *machine, struct Opcode opcode) {
  struct ArithmeticOp op = arithmetic_op(machine, opcode);
  int result = op.lhs * op.rhs;
  machine->program[op.out_ptr] = result;
}

int input_int() {
  int input;
  while (true) {
    printf("\nEnter an integer\n> ");
    int result =  scanf("%d", &input);
    if (result == EOF) {
      fprintf(stderr, "Error reading input");
      exit(1);
    } else if (result == 0) {
      while (fgetc(stdin) != '\n');
    } else {
      return input;
    }
  }
}

void input(struct Machine *machine) {
  int input = input_int();
  int address = fetch_pc(machine);
  machine->program[address] = input;
}

void output(struct Machine *machine, struct Opcode opcode) {
  int address = fetch_pc(machine);
  if (opcode.addressing1 == POSITION)
    address = machine->program[address];
  printf("INTCODE: %d\n", address);
}

struct JumpOp jump_op(struct Machine *machine, struct Opcode opcode) {
  int condition = fetch_pc(machine);
  if (opcode.addressing1 == POSITION)
    condition = machine->program[condition];
  int target = fetch_pc(machine);
  if (opcode.addressing2 == POSITION)
    target = machine->program[target];

  struct JumpOp op = {condition, target};
  return op;
}

void jumpnz(struct Machine *machine, struct Opcode opcode) {
  struct JumpOp op = jump_op(machine, opcode);
  if (op.condition != 0) {
    machine->pc = op.target;
  }
  char *result = op.condition != 0 ? "TAKEN" : "NOT TAKEN";
}

void jumpz(struct Machine *machine, struct Opcode opcode) {
  struct JumpOp op = jump_op(machine, opcode);
  if (op.condition == 0) {
    machine->pc = op.target;
  }
  char *result = op.condition == 0 ? "TAKEN" : "NOT TAKEN";
}

void less_than(struct Machine *machine, struct Opcode opcode) {
  struct ArithmeticOp op = arithmetic_op(machine, opcode);
  int result = op.lhs < op.rhs ? 1 : 0;
  machine->program[op.out_ptr] = result;
}

void equals(struct Machine *machine, struct Opcode opcode) {
  struct ArithmeticOp op = arithmetic_op(machine, opcode);
  int result = op.lhs == op.rhs ? 1 : 0;
  machine->program[op.out_ptr] = result;
}

void execute_opcode(struct Machine *machine, struct Opcode opcode) {
  switch (opcode.operation) {
  case 99: // halt
    machine->halted = true;
    break;
  case 1: // add
    add(machine, opcode);
    break;
  case 2: // mult
    multiply(machine, opcode);
    break;
  case 3: // input
    input(machine);
    break;
  case 4: // output
    output(machine, opcode);
    break;
  case 5: // jumpnz
    jumpnz(machine, opcode);
    break;
  case 6: // jumpz
    jumpz(machine, opcode);
    break;
  case 7: // less than
    less_than(machine, opcode);
    break;
  case 8: // equals
    equals(machine, opcode);
    break;
  }
}

int execute_program(struct Machine *machine) {
  while (!machine->halted) {
    struct Opcode opcode = fetch_opcode(machine);
    execute_opcode(machine, opcode);
  }

  return 0;
}

struct Machine create_machine(int *program) {
  struct Machine machine = {0, program, false};
  return machine;
}

int main() {
  FILE *file = fopen(FILENAME, "r");
  if (!file) {
    fprintf(stderr, "Could not open input file.");
    return 1;
  }

  char line [2600];
  if (fgets(line, sizeof line, file) == NULL) {
    fprintf(stderr, "Could not read input file.");
    return 1;
  }

  int* program = parse_program(line);
  struct Machine machine = create_machine(program);
  execute_program(&machine);

  return 0;
}

