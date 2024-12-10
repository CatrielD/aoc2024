using Base: copy

# --- Day 6: Guard Gallivant ---
# 
# The Historians use their fancy device again, this time to whisk you all away to the North Pole prototype suit manufacturing lab... in the year 1518! It turns out that having direct access to history is very convenient for a group of historians.
# 
# You still have to be careful of time paradoxes, and so it will be important to avoid anyone from 1518 while The Historians search for the Chief. Unfortunately, a single guard is patrolling this part of the lab.
# 
# Maybe you can work out where the guard will go ahead of time so that The Historians can search safely?
# 
# You start by making a map (your puzzle input) of the situation. For example:
# 
# ....#.....
# .........#
# ..........
# ..#.......
# .......#..
# ..........
# .#..^.....
# ........#.
# #.........
# ......#...
# 
# The map shows the current position of the guard with ^ (to indicate the guard is currently facing up from the perspective of the map). Any obstructions - crates, desks, alchemical reactors, etc. - are shown as #.
# 
# Lab guards in 1518 follow a very strict patrol protocol which involves repeatedly following these steps:
# 
#     If there is something directly in front of you, turn right 90 degrees.
#     Otherwise, take a step forward.
# 
# Following the above protocol, the guard moves up several times until she reaches an obstacle (in this case, a pile of failed suit prototypes):
# 
# ....#.....
# ....^....#
# ..........
# ..#.......
# .......#..
# ..........
# .#........
# ........#.
# #.........
# ......#...
# 
# Because there is now an obstacle in front of the guard, she turns right before continuing straight in her new facing direction:
# 
# ....#.....
# ........>#
# ..........
# ..#.......
# .......#..
# ..........
# .#........
# ........#.
# #.........
# ......#...
# 
# Reaching another obstacle (a spool of several very long polymers), she turns right again and continues downward:
# 
# ....#.....
# .........#
# ..........
# ..#.......
# .......#..
# ..........
# .#......v.
# ........#.
# #.........
# ......#...
# 
# This process continues for a while, but the guard eventually leaves the mapped area (after walking past a tank of universal solvent):
# 
# ....#.....
# .........#
# ..........
# ..#.......
# .......#..
# ..........
# .#........
# ........#.
# #.........
# ......#v..
# 
# By predicting the guard's route, you can determine which specific positions in the lab will be in the patrol path. Including the guard's starting position, the positions visited by the guard before leaving the area are marked with an X:
# 
# ....#.....
# ....XXXXX#
# ....X...X.
# ..#.X...X.
# ..XXXXX#X.
# ..X.X.X.X.
# .#XXXXXXX.
# .XXXXXXX#.
# #XXXXXXX..
# ......#X..
# 
# In this example, the guard will visit 41 distinct positions on your map.
#
#Predict the path of the guard. How many distinct positions will the guard visit before leaving the mapped area?



# https://docs.julialang.org/en/v1/manual/types/#%22Value-types%22
# It's worth noting that it's extremely easy to mis-use parametric
# "value" types, including Val; in unfavorable cases, you can easily
# end up making the performance of your code much worse. In
# particular, you would never want to write actual code as illustrated
# above. For more information about the proper (and improper) uses of
# Val, please read the more extensive discussion in the performance
# tips.
# ... LOL, but is fun ... vOv

struct Thing{C} end
Thing(x) = Thing{x}()

abstract type Floor end
abstract type GuardLookingUp end
abstract type GuardLookingRight end
abstract type GuardLookingDown end
abstract type GuardLookingLeft end
abstract type Stuff end
abstract type Breadcrumb end
const GuardType = Union{Type{GuardLookingDown},
                        Type{GuardLookingLeft},
                        Type{GuardLookingRight},
                        Type{GuardLookingUp},
                        Nothing}
const LabMapElement = Union{Type{Floor},
                            GuardType,
                            Type{Stuff},
                            Type{Breadcrumb}}

abstract type Stop end
abstract type GoOn end

function Specialize(::Thing{'.'})
    Floor
end
function Specialize(::Thing{'#'})
    Stuff
end
function Specialize(::Thing{'X'})
    # there is no need for this one, but...
    Breadcrumb
end
function Specialize(::Thing{'^'})
    GuardLookingUp
end
function Specialize(::Thing{'v'})
    GuardLookingDown
end
function Specialize(::Thing{'<'})
    GuardLookingLeft
end
function Specialize(::Thing{'>'})
    GuardLookingRight
end

example="""....#.....
           .........#
           ..........
           ..#.......
           .......#..
           ..........
           .#..^.....
           ........#.
           #.........
           ......#...
           """

######################################################################
###                                                                ###
###         from string to a map and the other way arround         ###
###                                                                ###
######################################################################


const LabMap = Matrix{LabMapElement}

function parse_labmap(str::String)
    temp = [[(Specialize ∘ Thing)(c) for c in line] for line in split(str)]
    res = Matrix{LabMapElement}(undef, length(temp), length(temp[1]))
    for (i, row) in enumerate(temp)
        for (j, x) in enumerate(row)
            res[i,j] = x
        end
    end
    res
end

function Base.show(io::IO, ::Type{Floor})
    write(io, '.')
end
function Base.show(io::IO, ::Type{Stuff})
    write(io, '#')
end
function Base.show(io::IO, ::Type{GuardLookingDown})
    write(io, 'v')
end
function Base.show(io::IO, ::Type{GuardLookingUp})
    write(io, '^')
end
function Base.show(io::IO, ::Type{GuardLookingRight})
    write(io, '>')
end
function Base.show(io::IO, ::Type{GuardLookingLeft})
    write(io, '<')
end
function Base.show(io::IO, ::Type{Breadcrumb})
    write(io, 'X')
end

# this loops forever, why? show it shouldn't call itself
#function Base.show(io::IO, s::SimulatorState)
#    show(s.map)
#    write(io, "guard in: $s.guard_pos\n")
#end

######################################################################
###                                                                ###
###                              logic                             ###
###                                                                ###
######################################################################


function is_guard(::GuardType)
    true
end

function is_guard(::LabMapElement)
    false
end

function Base.copy(pos::Union{CartesianIndex{2}, Nothing})
    if pos == Nothing
        Nothing
    else
        CartesianIndex(pos[1], pos[2])
    end
end

mutable struct SimulatorState
    guard_pos :: Union{CartesianIndex{2}, Nothing}
    map :: LabMap
    SimulatorState(map::LabMap) = new(findfirst(is_guard, map), map)
    SimulatorState(s::String) = SimulatorState(parse_labmap(s))
    SimulatorState(s::SimulatorState) = new(copy(s.guard_pos), copy(s.map))
end

function coord_inside(coord::Nothing, ::Any)
    false
end

function coord_inside(coord::CartesianIndex{2}, m::LabMap)
    0 < coord[1] && 0 < coord[2] && coord[1] <= m.size[1] && coord[2] <= m.size[2]
end

function height(m::LabMap)
    size(m)[1]
end

function width(m::LabMap)
    size(m)[2]
end

function get_guard(s::SimulatorState)
    s.map[s.guard_pos]
end

function guard_next(Nothing, ::GuardLookingUp)
    Nothing
end

function guard_next(s::SimulatorState)
    #guard_next_for(s, get_guard(s))
    pos = s.guard_pos
    g = get_guard(s)
    next_coord = guard_advance(s)
    if ! coord_inside(next_coord, s.map)
        (coord = Nothing, guard = Nothing)
    else
        if s.map[next_coord] == Stuff
            (coord = pos, guard = guard_rotate(g))
        else
            (coord = next_coord, guard = g)
        end
    end
end

function guard_advance(s::SimulatorState)
    pos = s.guard_pos
    guard_advance_for(pos, get_guard(s))
end

function guard_advance_for(pos::CartesianIndex{2}, g::Type{GuardLookingUp})
    CartesianIndex(pos[1]-1, pos[2])
end

function guard_rotate(g::Type{GuardLookingUp})
    GuardLookingRight
end

function guard_advance_for(pos::CartesianIndex{2}, g::Type{GuardLookingDown})
    CartesianIndex(pos[1]+1, pos[2])
end

function guard_rotate(g::Type{GuardLookingDown})
    GuardLookingLeft
end

function guard_advance_for(pos::CartesianIndex{2}, g::Type{GuardLookingRight})
    CartesianIndex(pos[1], pos[2]+1)
end

function guard_rotate(g::Type{GuardLookingRight})
    GuardLookingDown
end

function guard_advance_for(pos::CartesianIndex{2}, g::Type{GuardLookingLeft})
    CartesianIndex(pos[1], pos[2]-1)
end

function guard_rotate(g::Type{GuardLookingLeft})
    GuardLookingUp
end

function step_simulation!(s::SimulatorState)
    prev_coord = s.guard_pos
    next_coord, next_guard = values(guard_next(s))
    if next_coord == Nothing
        s.map[prev_coord] = Breadcrumb
        Stop
    else
        s.map[prev_coord] = Breadcrumb
        s.map[next_coord] = next_guard
        s.guard_pos = next_coord
        GoOn
    end
end

function run_simulation_for_part1_from!(s::SimulatorState, max_steps = 10000)
    flag = GoOn
    count = 0
    while flag == GoOn && count < max_steps
        flag = step_simulation!(s)
        count += 1
    end
end

function count_breadcrumbs(m::LabMap)
    length(findall(x -> x == Breadcrumb, m))
end

# --- Part Two ---
# 
# While The Historians begin working around the guard's patrol route, you borrow their fancy device and step outside the lab. From the safety of a supply closet, you time travel through the last few months and record the nightly status of the lab's guard post on the walls of the closet.
# 
# Returning after what seems like only a few seconds to The Historians, they explain that the guard's patrol area is simply too large for them to safely search the lab without getting caught.
# 
# Fortunately, they are pretty sure that adding a single new obstruction won't cause a time paradox. They'd like to place the new obstruction in such a way that the guard will get stuck in a loop, making the rest of the lab safe to search.
# 
# To have the lowest chance of creating a time paradox, The Historians would like to know all of the possible positions for such an obstruction. The new obstruction can't be placed at the guard's starting position - the guard is there right now and would notice.
# 
# In the above example, there are only 6 different positions where a new obstruction would cause the guard to get stuck in a loop. The diagrams of these six situations use O to mark the new obstruction, | to show a position where the guard moves up/down, - to show a position where the guard moves left/right, and + to show a position where the guard moves both up/down and left/right.
# 
# Option one, put a printing press next to the guard's starting position:
# 
# ....#.....
# ....+---+#
# ....|...|.
# ..#.|...|.
# ....|..#|.
# ....|...|.
# .#.O^---+.
# ........#.
# #.........
# ......#...
# 
# Option two, put a stack of failed suit prototypes in the bottom right quadrant of the mapped area:
# 
# ....#.....
# ....+---+#
# ....|...|.
# ..#.|...|.
# ..+-+-+#|.
# ..|.|.|.|.
# .#+-^-+-+.
# ......O.#.
# #.........
# ......#...
# 
# Option three, put a crate of chimney-squeeze prototype fabric next to the standing desk in the bottom right quadrant:
#
# ....#.....
# ....+---+#
# ....|...|.
# ..#.|...|.
# ..+-+-+#|.
# ..|.|.|.|.
# .#+-^-+-+.
# .+----+O#.
# #+----+...
# ......#...
# 
# Option four, put an alchemical retroencabulator near the bottom left corner:
# 
# ....#.....
# ....+---+#
# ....|...|.
# ..#.|...|.
# ..+-+-+#|.
# ..|.|.|.|.
# .#+-^-+-+.
# ..|...|.#.
# #O+---+...
# ......#...
# 
# Option five, put the alchemical retroencabulator a bit to the right instead:
# 
# ....#.....
# ....+---+#
# ....|...|.
# ..#.|...|.
# ..+-+-+#|.
# ..|.|.|.|.
# .#+-^-+-+.
# ....|.|.#.
# #..O+-+...
# ......#...
# 
# Option six, put a tank of sovereign glue right next to the tank of universal solvent:
# 
# ....#.....
# ....+---+#
# ....|...|.
# ..#.|...|.
# ..+-+-+#|.
# ..|.|.|.|.
# .#+-^-+-+.
# .+----++#.
# #+----++..
# ......#O..
# 
# It doesn't really matter what you choose to use as an obstacle so long as you and The Historians can put it into position without the guard noticing. The important thing is having enough options that you can find one that minimizes time paradoxes, and in this example, there are 6 different positions you could choose.
# 
# You need to get the guard stuck in a loop by adding a single new obstruction. How many different positions could you choose for this obstruction?

function check_for_loops(s_::SimulatorState)
    s = SimulatorState(s_)
    guard_rotate(get_guard(s))
end

function run_simulation_for_part2_from!(s::SimulatorState)
    flag = GoOn
    count = 0
    loops = 0
    while flag == GoOn && count < max_steps
        flag = step_simulation!(s)
        loops += check_for_loop(copy(s))
        count += 1
    end
    return loops
end

function (@main)(args)
    file = args[1]
    input_str = read(file, String)
    s = SimulatorState(parse_labmap(input_str))
    run_simulation_for_part1_from!(s)
    println("visited positions: $(count_breadcrumbs(s.map))")

    run_simulation_for_part2_from!(SimulatorState(input_str))
    0
end

