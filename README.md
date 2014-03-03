
# input event

    type      :: message | data | message_waiting
    class     :: alert | me | sim | te
    alphabet  :: default | octet | ucs2
    pid       :: uint8()
    src       :: integer()
    dst       :: integer()
    anumber   :: msisdn()
    bnumber   :: msisdn()
    smsc      :: msisdn()
    reg_exp   :: string()


# output event

    rp        :: boolean()
    udhi      :: boolean()
    udh       :: ...
    srr       :: boolean()
    mref      :: uint8()
    vpf       :: none | relative | enhanced | absolute
    vp        :: none | {relative, Seconds::integer()} |
                 {absolute,datetime()}
    addr      :: msisdn()
    pid       :: uint8()
    dcs       :: integer()
    type      :: message | data | message_waiting
    class     :: alert | me | sim | te
    alphabet  :: default | octet | ucs2
    compression :: compressed | uncompressed
    store     :: store | discard
    wait_type :: voicemail | fax | email | other

