-record(entry, {
    time     :: {{non_neg_integer(), 1..12, 1..31}, {0..23, 0..59, 0..59}},
    tag      :: 'regular' | 'slacking' | 'excluded',
    text     :: iolist(),
    duration :: non_neg_integer(),
    first_of_day :: boolean()
}).
