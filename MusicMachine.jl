using DataStructures
import Base.Order.ForwardOrdering

###################
### NBS Parsing ###
###################

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

"Reads a length-prefixed string."
function read_string(io::IO)
    length = read(io, Int32)
    chars = Array{Char}(length)
    for i in 1:length
        chars[i] = read(io, Char)
    end
    String(chars)
end

@enum NBSInstrument nbs_piano=0 nbs_double_bass=1 nbs_bass_drum=2 nbs_snare_drum=3 nbs_click=4 nbs_guitar=5 nbs_flute=6 nbs_bell=7 nbs_chime=8 nbs_xylophone=9

type NBSNote
    position::Int
    layer::Int
    instrument::NBSInstrument
    key::Int
end

"Reads the notes for an NBS song from `io`."
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

"An `NBSSound` is an `NBSIntrument` together with the `key` it is played at."
const NBSSound = Tuple{NBSInstrument, Int}

##################
### ItemStream ###
##################

@enum MCItemType mc_stackable mc_unstackable

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

eltype(si::StackIterator) = Tuple{MCItemType, Int}

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
Searches for a location to split the item list that will not skip any unstackable
items. `chest_size` is the inventory size, `skip` is the number of items skipped
on a split, and `max_backtrack` is the maximum number of items we are willing
to backtrack on.
"""
function get_best_split(il::ItemList, chest_size=27, skip=2, max_backtrack=20)
    "Computes the number of unstackable items skipped when splitting at `ix`."
    function split_cost(ix)
        cost = 0
        for i in ix+1:ix+skip
            if i in il.unstackable_positions
                cost += 1
            end
        end
        cost
    end
    chest_prefix = get_chest_prefix(il, chest_size)
    split_ix = chest_prefix
    cost = split_cost(split_ix)
    for ix in split_ix:-1:split_ix-max_backtrack
        ix_cost = split_cost(ix)
        if ix_cost < cost
            split_ix = ix
            cost = ix_cost
        end
    end
    return split_ix
end

"""
Splits an `ItemList` into sub-lists that fit into a single chest. After each
split, `skip` (by default `skip=2`) items are omitted in the next chest (this is
to account for the fact that rotating chests in the current machine takes 8
redstone ticks).
"""
function split_item_list(il::ItemList, chest_size=27; skip=2, max_backtrack=10)
    chests = Array{ItemList}(0)
    while num_stacks(il) > chest_size
        #split_point = get_chest_prefix(il, chest_size)
        split_point = get_best_split(il, chest_size, skip, max_backtrack)
        first_chest, il = split_at(il, split_point; skip=skip)
        push!(chests, first_chest)
    end
    push!(chests, il)
    chests
end

#############################################
### Utilities for Programming The Machine ###
#############################################

"""
Given an `ItemList` and coordinates `(x,y,z)`, returns a setblock minecraft
command that will create a chest
"""
function make_chest(il::ItemList, x, y, z)
    shulker_boxes = split_item_list(il)
    command = "setblock $(x) $(y) $(z) minecraft:chest 0 replace {Items:["
    for (ix,sb) in enumerate(shulker_boxes)
        command *= "{id:\"minecraft:white_shulker_box\",Slot:$(ix-1)b,Count:1b,tag:{BlockEntityTag:{Items:["
        st = 0
        slot = 0
        last_type = mc_stackable
        for (item_type, stack_size) in StackIterator(sb)
            if item_type == mc_stackable
                if last_type == mc_unstackable
                    st += 1
                end
                command *= "{Slot:$(slot),id:\"minecraft:wool\",Count:$(stack_size)b,Damage:$(st)},"
            elseif item_type == mc_unstackable
                command *= "{Slot:$(slot),id:\"minecraft:stone_shovel\",Count:1b},"
            end
            last_type = item_type
            slot += 1
        end
        command *= "]}}},"
    end
    command *= "]}"
    command
end

"Returns the input memory chest for the `ix`th module offset by `generator` ticks."
function get_input_chest_location(ix)
    (x,y,z) = get_noteblock_location(ix)
    (x-6, y+3, z+3)
end

"Returns the output memory chest for the `ix`th module offset by `generator` ticks."
function get_output_chest_location(ix, facing = [1,0,0])
    (x,y,z) = get_noteblock_location(ix)
    (x-6, y-3, z)
end

"Returns the note block for the `ix`th module offset by `generator` ticks."
function get_noteblock_location(ix)
    z = (ix-1)%10
    y = div(ix-1,10)
    (-926, 14+7y, -1004 + 4z)
end

"Returns the instrument block for the `ix`th module offset by `generator` ticks."
function get_instrument_location(ix)
    (x,y,z) = get_noteblock_location(ix)
    return (x,y-1,z)
end

function get_repeter_location(ix)
    (x,y,z) = get_noteblock_location(ix)
    return (x-9, y+1, z+2)
end

function make_mcfunction(song::NBSSong, outfile)
    instrument_blocks = [
        "minecraft:air",
        "minecraft:planks",
        "minecraft:stone",
        "minecraft:sand",
        "minecraft:glass",
        "minecraft:wool",
        "minecraft:clay",
        "minecraft:gold_block",
        "minecraft:packed_ice",
        "minecraft:bone_block"
    ]
    command = ""
    ix = 1
    for generator in 0:3
        item_lists = get_item_lists(song;modulus=4, generator=generator)
        for ((instrument, key), il) in item_lists
            # Place the instrument block
            (x,y,z) = get_instrument_location(ix)
            block_type = instrument_blocks[Int(instrument)+1]
            command *= "setblock $(x) $(y) $(z) $(block_type)\n"

            # Set the noteblock key
            (x,y,z) = get_noteblock_location(ix)
            command *= "setblock $(x) $(y) $(z) minecraft:air 0 replace\n"
            command *= "setblock $(x) $(y) $(z) minecraft:noteblock 0 replace {note:$(key-33)b}\n"

            # Set the input chest
            (x,y,z) = get_input_chest_location(ix)
            command *= "setblock $(x) $(y) $(z) minecraft:air 0 replace\n"
            command *= make_chest(il, x, y, z) * "\n"

            # Set output chest
            (x,y,z) = get_output_chest_location(ix)
            command *= "setblock $(x) $(y) $(z) minecraft:air 0 replace\n"
            command *= "setblock $(x) $(y) $(z) minecraft:chest 0 replace\n"

            # Set repeater delay
            (x,y,z) = get_repeter_location(ix)
            command *= "setblock $(x) $(y) $(z) minecraft:air 0 replace\n"
            command *= "setblock $(x) $(y) $(z) minecraft:unpowered_repeater $(1+4*generator) replace\n"

            ix += 1
        end
    end
    @show ix
    fh = open(outfile, "w")
    print(fh, command)
    close(fh)
    return command
end
