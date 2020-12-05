struct Password
  min::Int
  max::Int
  letter::Char
  word::AbstractString
end

function parsepassword(line::AbstractString)
    sep = findfirst(isequal('-'), line)
    min = parse(Int, line[1:sep - 1])
    sepend = findfirst(isequal(' '), line)
    max = parse(Int, line[sep + 1:sepend])
    letter = line[sepend + 1]
    word = line[sepend + 4:end]

    Password(min, max, letter, word)
end

function getinputs(path::AbstractString)
  open(path,"r") do f
    return map(parsepassword, eachline(f))
  end
end

function policy1(password::Password)
  count = 0
  for c in password.word
    if c == password.letter
      count += 1
    end
  end

  count >= password.min && count <= password.max
end

function policy2(password::Password)
  min = password.word[password.min]
  max = password.word[password.max]
  count = min == password.letter && max != password.letter || min != password.letter && max == password.letter
end

function solve(inputs::AbstractVector{Password}, policy::Function)
  length(filter(policy, inputs))
end

println(solve(getinputs("input.txt"), policy1))
println(solve(getinputs("input.txt"), policy2))
