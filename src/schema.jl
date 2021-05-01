@option struct Instruction
    name::String
    params::Maybe{Vector{Any}}
end

@option struct Header
    name::String
    n_qubits::Int
    memory_slots::Int
end

@option struct Config
    n_qubits::Int
    shots::Int
    memory_slots::Int
    seed::Int
    max_credits::Int
end

@option struct Payload
    header::Header
    config::Config
    instructions::Vector{Instruction}
end
