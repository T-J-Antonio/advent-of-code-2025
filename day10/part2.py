from pyscipopt import Model, quicksum

def parseMachine(line: str):
    components = line.split(" ")
    joltages = components[-1].split("\n")[0]
    buttons = components[1:-1]
    parsedButtons = []
    for button in buttons:
        trimmed = button[1:-1]
        ns = trimmed.split(",")
        parsedButtons.append([int(n) for n in ns])
    trimmed = joltages[1:-1]
    ns = trimmed.split(",")
    parsedJoltages = [int(n) for n in ns]
    return [parsedButtons, parsedJoltages]

file = open("input.txt")
machines = []
for line in file:
    machines.append(parseMachine(line))
file.close()

sols = []
for machine in machines:
    buttons = machine[0]
    joltages = machine[1]
    model = Model()
    vars = []
    for i in range(len(buttons)):
        vars.append(model.addVar(f"x{i}", lb=0, vtype="INTEGER"))
    for i in range(len(joltages)):
        affectedButtons = []
        for j in range(len(buttons)):
            if i in buttons[j]:
                affectedButtons.append(vars[j])
        model.addCons(quicksum(affectedButtons) == joltages[i])
    model.setObjective(quicksum(vars), sense="minimize")
    model.optimize()
    sol = model.getObjVal()
    sols.append(sol)

print(sum(sols))
