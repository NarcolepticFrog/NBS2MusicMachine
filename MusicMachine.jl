"Path to keybind mod's macro folder."
const BASE_DIR = "MINECRAFT_HOME/liteconfig/common/macros/"

using DataStructures
import Base.Order.ForwardOrdering

type NBSHeader
    length::Int
    height::Int
    name::String
    author::String
    original_author::String
    description::String
    tempo::Int
    auto_saving::Bool
    auto_saving_duration::Int
    time_signature::Int
    minutes_spent::Int
    left_clicks::Int
    right_clicks::Int
    blocks_added::Int
    blocks_removed::Int
    schematic_name::String
end
NBSHeader() = NBSHeader(0,0,"","","","",0,false,0,0,0,0,0,0,0,"")

import Base.read
function read(io::IO, ::Type{NBSHeader})
    header = NBSHeader()
    header.length = read(io, Int16)
    header.height = read(io, Int16)
    header.name = read_string(io)
    header.author = read_string(io)
    header.original_author = read_string(io)
    header.description = read_string(io)
    header.tempo = read(io, Int16)
    header.auto_saving = read(io, Int8) == 1
    header.auto_saving_duration = read(io, Int8)
    header.time_signature = read(io, Int8)
    header.minutes_spent = read(io, Int32)
    header.left_clicks = read(io, Int32)
    header.right_clicks = read(io, Int32)
    header.blocks_added = read(io, Int32)
    header.blocks_removed = read(io, Int32)
    header.schematic_name = read_string(io)

    header
end

function read_string(io::IO)
    length = read(io, Int32)
    chars = Array{Char}(length)
    for i in 1:length
        chars[i] = read(io, Char)
    end
    String(chars)
end

@enum NBSInstrument nbs_piano=0 nbs_double_bass=1 nbs_bass_drum=2 nbs_snare_drum=3 nbs_click=4

type NBSNote
    position::Int
    layer::Int
    instrument::NBSInstrument
    key::Int
end

function read_notes(io::IO)
    notes = Array{NBSNote}(0)
    tick = -1
    while true
        t_jumps = read(io, Int16)
        if t_jumps == 0
            break
        end
        tick += t_jumps
        layer = -1
        while true
            l_jumps = read(io, Int16)
            if l_jumps == 0
                break
            end
            layer += l_jumps
            instrument = NBSInstrument(read(io, Int8))
            key = read(io, Int8)
            push!(notes, NBSNote(tick, layer, instrument, key))
        end
    end
    notes
end

type NBSSong
    header::NBSHeader
    notes::Vector{NBSNote}
end

function read(io::IO, ::Type{NBSSong})
    header = read(io, NBSHeader)
    notes = read_notes(io)
    NBSSong(header, notes)
end

function readNBS(path::String)
    fh = open(path, "r")
    song = read(fh, NBSSong)
    close(fh)
    song
end

const NBSSound = Tuple{NBSInstrument, Int}


@enum MCItemType mc_stackable mc_unstackable

##################
### ItemStream ###
##################

"Represents a sequence of stackable and non-stackable items."
type ItemList
    unstackable_positions::SortedSet{Int, ForwardOrdering}
    num_items::Int
end
ItemList() = ItemList(SortedSet{Int,ForwardOrdering}(), 0)

"Number of inventory slots used by the item list."
function num_stacks(il::ItemList)
    if il.num_items <= 0
        return 0
    end
    stacks = 0
    last_pos = 0
    for pos in il.unstackable_positions
        stacks += div((pos - last_pos - 1) + 63, 64) + 1
        last_pos = pos
    end
    stacks += div((il.num_items - last_pos - 1) + 63, 64)
    stacks
end

"""
Returns two item streams, one containing all items up to and including the
`ix`th item, and the second containing all items at positions following
`ix+skip`.
"""
function split_at(il::ItemList, ix; skip=2)
    before_positions = SortedSet{Int, ForwardOrdering}()
    after_positions = SortedSet{Int, ForwardOrdering}()
    for pos in il.unstackable_positions
        if pos <= ix
            push!(before_positions, pos)
        elseif pos > ix + skip
            push!(after_positions, pos - ix - skip)
        end
    end
    before = ItemList(before_positions, ix)
    after = ItemList(after_positions, il.num_items - ix - skip)
    (before, after)
end

"Represents the item list as a human readable string."
function show_items(il::ItemList)
    if length(il.unstackable_positions) == 0
        return "empty"
    end
    result = ""
    for (item_type, stack_size) in StackIterator(il)
        if item_type == mc_unstackable
            result *= "□ "
        elseif item_type == mc_stackable
            result *= "≡×$(stack_size) "
        end
    end
    result
end

"""
Given an `NBSSong`, returns a dictionary mapping `NBSSound`s to item streams
that have unstackable items in each position that song should play. Only notes
occuring on or after position `first_pos` and before or on position `last_pos`
are included in the `ItemStream`s. By setting `modulus` and `generator`, it is
equivalent to running the function on subsequnce of notes occuring every
`modulus` ticks, starting from `generator`.
"""
function get_item_lists(s::NBSSong; first_pos = 0, last_pos = Inf, modulus=1, generator=0)
    item_lists = Dict{NBSSound, ItemList}()
    for n in s.notes
        if n.position < first_pos || n.position % modulus != generator
            continue
        elseif  n.position > last_pos
            break
        end
        p = (n.instrument, n.key)
        if !haskey(item_lists, p)
            item_lists[p] = ItemList()
        end
        scaled_pos = div(n.position, modulus)
        push!(item_lists[p].unstackable_positions, scaled_pos+1)
        item_lists[p].num_items = max(item_lists[p].num_items, scaled_pos+1)
    end
    item_lists
end

#####################
### StackIterator ###
#####################

import Base:start, next, done, eltype, length

type StackIterator
    il::ItemList
end

start(si::StackIterator) = 1

function next(si::StackIterator, pos)
    if pos in si.il.unstackable_positions
        return ((mc_unstackable,1), pos+1)
    else
        next_stackable = searchsortedafter(si.il.unstackable_positions, pos)
        if next_stackable == pastendsemitoken(si.il.unstackable_positions)
            last_stackable = si.il.num_items
        else
            last_stackable = deref((si.il.unstackable_positions,next_stackable))-1
        end
        stack_size = min(64, last_stackable - pos + 1)
        return ((mc_stackable, stack_size), pos+stack_size)
    end
end

done(si::StackIterator, pos) = pos > si.il.num_items

eltyle(si::StackIterator) = Tuple{MCItemType, Int}

length(si::StackIterator) = num_stacks(si.il)

#########################################
### Splitting ItemStreams into Chests ###
#########################################

"Returns the last item index that will fit in a single chest."
function get_chest_prefix(il::ItemList, chest_size=27)::Int
    if il.num_items <= 0
        return 0
    elseif num_stacks(il) <= chest_size
        return il.num_items
    else
        return sum(s for (i,s) in take(StackIterator(il), chest_size))
    end
end

"""
Splits an `ItemList` into sub-lists that fit into a single chest. After each
split, `skip` (by default `skip=2`) items are omitted in the next chest (this is
to account for the fact that rotating chests in the current machine takes 8
redstone ticks).
"""
function split_item_list(il::ItemList, chest_size=27; skip=2)
    chests = Array{ItemList}(0)
    while num_stacks(il) > chest_size
        split_point = get_chest_prefix(il, chest_size)
        first_chest, il = split_at(il, split_point; skip=skip)
        push!(chests, first_chest)
    end
    push!(chests, il)
    chests
end

"""
Returns `true` if none of the `ItemStreams` have any unstackable items in positions
`ix+1`, ..., `ix+skip`.
"""
function safe_split(ils, ix; skip=2)
    for il in values(ils)
        for j in 1:skip
            if ix+j in il.unstackable_positions
                return false
            end
        end
    end
    return true
end

function split_cost(ils, ix; skip=2)
    cost = 0
    for il in values(ils)
        for j in 1:skip
            if ix+j in il.unstackable_positions
                cost += 1
            end
        end
    end
    cost
end

function split_item_lists(ils::Dict{NBSSound, ItemList}, chest_size=27)
    ils = copy(ils)
    chests = Dict{NBSSound, Vector{ItemList}}()
    for sound in keys(ils)
        chests[sound] = Array{ItemList}(0)
    end
    done = false
    while !done
        done = true
        split_point = typemax(Int)
        for (sound, il) in ils
            if num_stacks(il) > chest_size
                done = false
                il_split = get_chest_prefix(il, chest_size)
                split_point = min(split_point, il_split)
            end
        end

        if !done
            best_cost = Inf
            best_split = 0
            for split in split_point - 10:split_point
                cost = split_cost(ils, split)
                if cost <= best_cost
                    best_cost = cost
                    best_split = split
                end
            end
            if best_split != split_point
                println("Adjusted split saved $(split_cost(ils, split_point) - best_cost) notes!")
            end
            for (sound, il) in ils
                chest, ils[sound] = split_at(il, best_split)
                push!(chests[sound], chest)
            end
        end

    end
    chests
end

###########################################
### Utilities for Programming Minecarts ###
###########################################

function make_shulker_string(il::ItemList)
    st = 0
    slot = 0
    last_type = mc_stackable
    parts = ["give @p minecraft:white_shulker_box 1 0 {BlockEntityTag:{Items:["]
    for (item_type, stack_size) in StackIterator(il)
        if item_type == mc_stackable
            if last_type == mc_unstackable
                st += 1
            end
            push!(parts, "{Slot:$(slot),id:\"minecraft:wool\",Count:$(stack_size)b,Damage:$(st)},")
        elseif item_type == mc_unstackable
            push!(parts,"{Slot:$(slot),id:\"minecraft:stone_shovel\",Count:1b},")
        end
        last_type = item_type
        slot += 1
    end
    push!(parts, "]}}")
    string(parts...)
end

"Deletes the macro files for filling chests."
function clear_chest_macros()
    for i in 1:24
        chest_path = joinpath(BASE_DIR, "chest$i.txt")
        if isfile(chest_path)
            rm(chest_path)
        end
    end
end

"""
Creates a macro in `BASE_DIR` that clears the users inventory, then fills it with the
items in the `ItemList` `il`. The stackable items are different colors of wool, which
limits the number of distinct stackable item types to 16.
"""
function save_macro(name::String, il::ItemList)
    fh = open(joinpath(BASE_DIR, "$(name).txt"), "w")
    st = 0
    last_type = mc_stackable
    println(fh, "/clear")
    println(fh, "\$\${WAIT(2t)}\$\$")
    for i in 1:9
        println(fh, "/give @p diamond_sword")
    end
    for (item_type, stack_size) in StackIterator(il)
        if item_type == mc_stackable
            if last_type == mc_unstackable
                st += 1
            end
            println(fh, "/give @p wool $(stack_size) $(st)")
        elseif item_type == mc_unstackable
            println(fh, "/give @p stone_shovel")
        end
        last_type = item_type
    end
    close(fh)
end

"""
This function takes an `NBSSong` and a tick-length and interactively walks the user
through programming the music machine. It writes macros to `BASE_DIR` that can be used
for efficiently filling each column of chests, as well as for scheduling the chest
rotations.
"""
function interactive_song_program(song::NBSSong, last_pos = Inf)
    block_type = ["air", "wood", "stone", "sand", "glass"]
    for tick_offset in 0:3
        println("\n\nTICK OFFSET = $(tick_offset)\n\n")

        ils = get_item_lists(song; last_pos=last_pos, modulus=4, generator=tick_offset)
        sounds = sort(collect(keys(ils)))

        for i in 1:12:length(sounds)
            println("BANK $(div(i-1,12)):")

            bank_ils = Dict{NBSSound, ItemList}()
            for j in 1:min(i+11, length(sounds))
                bank_ils[sounds[j]] = ils[sounds[j]]
            end

            chests = split_item_lists(bank_ils)

            rotation_times = ItemList()
            push!(rotation_times.unstackable_positions, 1)
            pos = 1
            for c in first(values(chests))[1:end-1]
                pos += c.num_items+2
                push!(rotation_times.unstackable_positions, pos)
                rotation_times.num_items = pos
            end
            save_macro("rotation", rotation_times)

            for j in i:min(i+11, length(sounds))
                println(" SOUND: ", block_type[Int(sounds[j][1])+1],":",sounds[j][2]-33, " ")
            end

            for j in i:min(i+11, length(sounds))
                clear_chest_macros()
                println("Column $(j-i+1)")
                for (k,c) in enumerate(chests[sounds[j]])
                    println("  Chest $k: ", show_items(c))
                    save_macro("chest$k", c)
                end
                readline()
            end
        end

    end
end
