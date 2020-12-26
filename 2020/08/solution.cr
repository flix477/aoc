module Instructions
  private INSTRUCTION_MAP = {
    "nop" => Instruction::NOP,
    "acc" => Instruction::ACC,
    "jmp" => Instruction::JMP,
  }

  def self.from_s(s)
    INSTRUCTION_MAP[s]
  end
end

enum Instruction
  NOP
  ACC
  JMP
end

struct Machine
  def initialize
    @acc = 0
    @pc = 0
  end

  def initialize(@acc, @pc)
  end

  def execute(instruction, argument): Machine
    case instruction
    when Instruction::ACC
      Machine.new(@acc + argument, @pc + 1)
    when Instruction::JMP
      Machine.new(@acc, @pc + argument)
    when Instruction::NOP
      Machine.new(@acc, @pc + 1)
    else
      raise "unreachable" # ugh come on where are my exhaustive checks
    end
  end
end

alias Program = Array(Tuple(Instruction, Int32))

class ProgramIterator
  include Iterator(Machine)

  def initialize(@program : Program, @last = Machine.new)
  end

  def next
    if @last.@pc >= @program.size
      stop
    else
      instruction, argument = @program[@last.@pc]
      @last = @last.execute(instruction, argument)
      @last
    end
  end

  def last_non_looping_state() : Tuple(Array(Int32), Machine?)
    visited = [@last.@pc]
    state = @last
    each do |machine|
      if visited.includes? machine.@pc
        return {visited, state}
      end
      visited.push(machine.@pc)
      state = machine
    end

    return {visited, nil}
  end

  def execute() : Machine
    state = @last
    each do |machine|
      state = machine
    end
    state
  end
end

def parse_instruction(line)
  mnemonic, argument = line.split
  {Instructions.from_s(mnemonic), argument.to_i}
end

def solve1(program)
  _, state = ProgramIterator.new(program).last_non_looping_state()
  state.try { |x| x.@acc }
end

def fix_program(program) : Program?
  visited, _ = ProgramIterator.new(program).last_non_looping_state()
  until visited.empty?
    pc = visited.pop
    instruction, argument = program[pc]
    if instruction != Instruction::ACC
      new_program = program.clone
      new_instruction = instruction == Instruction::JMP ? Instruction::NOP : Instruction::JMP
      new_program[pc] = {new_instruction, argument}
      _, state = ProgramIterator.new(new_program).last_non_looping_state()
      if !state
        return new_program
      end
    end
  end
end

def solve2(program)
  fix_program(program).try { |p| ProgramIterator.new(p).execute.@acc }
end

def solve(path)
  program = File.read_lines(path)
    .map { |line| parse_instruction line }

  {solve1(program), solve2(program)}
end


puts solve("input.txt")
