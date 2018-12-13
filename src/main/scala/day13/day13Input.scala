package day13

object day13Input {

  val input = """                /-------------------------------------------------\                                                    /---------------------------\
                | /--------------+---------------------------------------\         |                                                    |                           |
                | |          /---+-------->------------------------------+-->-----\|                                           /--------+---\                       |
                | |          |   |            /---------------------\    |        ||                                           |        |   |                       |
                | |          |   |            |                     |    |/-------++-------------------------------------------+--------+---+-----------------------+-\
                | |          |   |            |                     |/---++-------++-------------------------------------------+--------+---+-----------------------+\|
                | |          |   |       /----+---------------------++---++-------++----------------\                         /+--------+---+-------\               |||
                | |          |   |  /----+----+-------------\       ||   ||       ||                |                         ||        |   |       |               |||
                | |          |   |  |    |   /+--<----------+-\/----++---++-------++----------------+-------------------------++--------+---+-----\ |               |||
                | |          |   |  |    |   ||             | ||    ||   ||       ||                |               /---------++-------\|   |   /-+-+------------\  |||
                | |          |   |  |    |   ||        /----+-++----++---++-------++----\           |               |         ||       ||   |   | | |            |  |||
                | |          |   |  |    |   ||        |    | ||    ||   ||       ||    |           |               |         ||       ||   |   | | |            |  |||
                | |          |   |  |    |   ||        |    | ||    ||  /++-------++----+-----------+---------------+---------++-------++---+---+-+-+---\        |  |||
                | |          |   |  |    |   ||        |    | ||    ||  |||     /-++----+-------\   |               |         ||       ||   |   | | |   |        |  |||
                | |   /------+---+--+----+---++--------+-\  | ||    ||  |||     | ||    |       |   |               |         ||       ||   |   | | |   |        |  |||
                | |   |      |   |  |    |   ||        | |  | ||    |\--+++-----+-++----+-------+---+---------------+---------++-------++---+---+-+-+---+--------+--+/|
                | |   |      |   |  |    |   ||        | |  |/++----+---+++-----+-++--\ |       |   |               |         ||       ||   |   | | |   |        |  | |
                | |   |      |   |  |    |   \+--------+-+--++/|    |   |||     | ||  | |       |   |       /-------+\        ||       ||   |   | | |   v        |  | |
                | |   |      |   |  |    |    |        |/+--++-+----+---+++-----+-++--+-+-------+---+-------+-------++--------++-------++---+---+-+-+\  |        |  | |
                | |/--+------+---+--+----+--\ |    /---+++--++-+----+---+++-----+-++\ | |       |   |       |       ||        ||       ||   |   | | ||  |        |  | |
                | ||  |      |   |  |    |  | |    |   |||  || |    | /-+++-----+-+++-+-+-------+---+-------+\      ||        ||       ||   |   | | ||  |        |  | |
                | ||  |      |   |  |    |  | |    |   |||  || |    | | |||     | ||| | |       |   |  /----++------++--------++-------++---+---+-+-++--+---\    |  | |
                | ||  |      |   |  |    |  | |    |   |||  || |    | | |||     | ||| | |       |   |  |    ||      ||        |\-------++---/   | | ||  |   |    |  | |
                | ||  |      |   |  |    |  | |/---+---+++--++-+----+-+-+++-----+-+++-+-+-------+---+--+----++------++--------+--------++\      | | ||  |   |    |  | |
                | ||  |/-----+---+-<+----+--+-++---+---+++--++-+----+-+-+++----\| ||| | |  /----+---+--+----++------++--------+--------+++------+-+-++--+---+----+--+\|
                | ||  ||     |   |  |    |  | ||   |   |||  || |    | | |||    || ||| | |  |    |   |  |    ||      ||        |        |||      | | ||  |   |    |  |||
                | ||  \+-----+---+--+----+--+-++---+---++/  || |    | | |||    || ||| | |  |   /+---+--+----++------++--------+-\      |||      | | ||  |   |    |  |||
                | ||   |     |   |  |    |/-+-++---+---++---++-+-\  | | |||    || ||| | |  |   ||   |  |    ||      || /------+-+------+++----\ | | ||  |   |    |  |||
                | ||   |     |   |  |    || | ||   |   || /-++-+-+--+-+-+++----++-+++-+-+--+---++---+--+----++------++-+------+-+------+++----+-+-+-++--+---+\   |  |||
                | \+---+-----+---+--+----++-+-++---+---++-+-++-+-+--+-+-+/|    || ||| | |  |   ||   |  |    ||      || |      | |   /--+++----+-+-+-++--+---++---+\ |||
                |  \---+-----+--<+--+----++-/ ||   |   || | |\-+-+--+-+-+-+----++-+++-/ |  |   ||   |  |    ||      || |      | |   |  |||    | | | ||  |   ||   || |||
                |      |     |   |  |    ||   ||   |/--++-+-+--+-+--+-+-+-+----++-+++---+--+---++---+--+----++------++-+------+-+---+--+++---\| | | ||  |   ||   || |||
                |      |     |   |  |    ||   ||   ||  \+-+-+--+-+--+-+<+-+----++-+++---/  |   ||   |  |    ||/-----++-+------+-+---+-\|||   || | | ||/-+---++--\|| |||
                |      |     |   |  |    ||   ||  /++---+-+-+--+-+--+-+-+-+----++-+++------+---++--\|  |    \++-----+/ |      | |   | ||||   || | | ||| |   ||  ||| |||
                |      |    /+---+--+----++---++--+++---+-+-+--+-+--+-+-+-+----++-+++------+---++--++--+-----++--\  |  |      | |   | ||||   || | | ||| |   ||  ||| |||
                |      | /--++---+--+----++---++--+++---+-+-+--+\|  | | | |/---++-+++------+---++--++--+-----++-\|  |  |      | |   | ||||   || | | ||| |   ||  ||| |||
                |    /-+-+--++---+--+----++---++--+++---+-+-+--+++--+-+-+-++---++-+++------+---++--++-\|    /++-++--+--+------+-+\  | ||||   || | | ||| |   ||  ||| |||
                |    | | |  ||   |  |/---++---++--+++---+-+-+--+++--+-+-+-++---++-+++------+---++--++-++----+++-++--+--+------+-++--+-++++--\|| | | ||| |   ||  ||| |||
                |    | | |  ||/--+--++---++---++--+++---+-+-+--+++--+-+-+-++---++-+++------+---++--++-++----+++-++--+--+------+-++--+-++++\ ||| | | ||| |   ||  ||| |||
                |/---+-+-+--+++--+--++\  ||   ||  |||   | | |  ||| /+-+-+-++---++-+++------+---++--++-++----+++-++--+\ |/-----+-++--+\||||| ||| | | ||| |   ||  ||| |||
                ||   | | |  |||  |  |||  ||   ||  |||   |/+-+--+++-++-+-+-++---++-+++------+---++--++-++----+++-++--++-++-----+\||  ||||||| ||| | | ||| |   ||  ||| |||
                ||   | | |  |||  |  |||  ||   ||  |||   ||| |  ||| || | | ||   || |||      |   ||  || ||    |||/++--++-++-----++++--+++++++-+++-+-+-+++-+\  ||  ||| |||
                ||   | | |  |||  |  |||  ||  /++--+++--\||| |  ||| || | | ||   || |||      |   ||  || ||    ||||||  \+-++-----++++--+++/\++-+++-+-+-+++-++--++--+++-/||
                ||   | | |  |||  |  |||  ||  |||  |||  |||| |  ||| || | | ||   || |||      |   ||  || ||    ||||||   | ||     ||||  |||  || ||| | | ||| ||  ||  |||  ||
                ||   | | |  |||  |  |||  |\--+++--+++--++++-+--++/ || | | ||   || |||      |   ||  || ||    ||||||   | ||     ||||  |||  || ||| | | ||| ||  ||  |||  ||
                ||   | |/+--+++--+--+++--+---+++--+++--++++-+--++--++-+\| ||   || |||      |   ||  || |\----++++++---+-++-----++++--+++--++-+++-+-+-+++-++--/|  |||  ||
                ||   | |||  |||  |  |\+--+---+++--+++--++++-+--++--++-+++-++---++-+++------+---++--++-+-----++++++---+-++-----++++--+++--++-/||/+-+-+++-++---+--+++-\||
                ||   | |||  |||  |  | |/-+---+++--+++--++++-+--++--++-+++-++---++-+++------+---++--++-+\    ||||||   | ||     ||||  |||  ||  |||| | ||| ||   |  ||| |||
                ||   | ||| /+++--+--+-++-+---+++--+++--++++-+--++--++-+++-++---++-+++--\   |   ||  || ||    ||||||   | ||     ||||  \++--++--++++-+-+++-++---+--++/ |||
                ||/--+-+++-++++--+--+-++-+---+++--+++--++++-+--++--++-+++-++\  || |||  |   \---++--++-++----++++++---+-++-----++++---++--++--++++-+-+++-++---+--++--+/|
                ||| /+-+++-++++--+--+-++-+---+++--+++--++++-+--++--++-+++-+++--++-+++--+-------++-\|| ||    |||||| /-+-++-----++++---++-\||  |||| | ||| ||   |  ||  | |
                ||| || ||| ||||  |  | || |   \++--+++--/||| |  ||  || ||| |||  || |||  |       || ||| ||    |||||| |/+-++-----++++---++-+++--++++-+-+++\||   |  ||  | |
                ||| || ||| ||||  |  | || |    ||  \++---+++-+--++--++-+++-+++--++-+++--+-------++-+/| ||    |||||| ||| ||     ||||   || |||  |||| | ||||||   |  ||  | |
                ||| || ||| |\++--+--+-++-+----++---++---+++-+--++--++-+++-+++--++-+++--+-------++-+-+-++----+++++/ ||| ||     ||||   || |||  |||| | ||||||   |  ||  | |
                ||| || ||| | ||  |  | || |    ||   ||   ||| |  ||  ||/+++-+++--++-+++--+-------++-+-+-++----+++++--+++-++-\   ||||   || |||  |||| | ||||||   |  ||  | |
                ||| || ||| | ||  |  | || |    ||   ||   ||| |  \+--++++++-+++--++-+++--+-------++-+-+-++----+++++--+++-++-+---++++---++-+++--++++-/ ||||||   |  ||  | |
                ||| || ||| | ||  |  | ||/+----++---++---+++-+---+--++++++-+++--++-+++--+------\|| | | ||    ||||| /+++-++-+---++++-\ || |||  ||||   ||||||   |  ||  | |
                ||| || ||| | || /+--+-++++\   ||   ||  /+++-+---+--++++++-+++--++-+++--+------+++-+-+-++----+++++-++++-++-+---++++\| || |||  ||||   ||||||   |  ||  | |
                ||| || ||| | || || /+-+++++---++---++--++++-+---+--++++++-+++--++-+++--+------+++-+-+-++----+++++-++++-++-+-\ |||||| || |||  ||||   ||||||   |  ||  | |
                ||| || \++-+-++-++-++-+++++---++---++--++++-+---+--++++++-+++--/| |||  |      ||| | | ||    ||||| ||||/++-+-+-++++++-++\|||  ||||   ||||||   |  ||  | |
                ||| ||  || | || || || v||||   || /-++--++++-+---+--++++++-+++\  | |||  |      ||| | | ||    ||||| ||||||\-+-+-++++++-/|||||  ||||   ||||||   |  ||  | |
                ||| ||  || | || || || |||||   || | ||  |||| |   |  |||||| ||||  | |||  |      ||| | | ||    ||||| ||||||  | | ||||||  |||||  ||||   ||||||   |  ||  | |
                ||| ||  || | || || || |||||   || | ||  |||| |   |  |||||| ||||  | |||  |      |||/+-+-++----+++++-++++++--+-+-++++++--+++++--++++---++++++---+-\||  | |
                ||| ||  || | || || || |||||   || | \+--++++-+---+--++++++-++++--+-++/  |      ||||| | ||    ||||| ||||||/-+-+-++++++-\|||||  ||||   ||||||   | |||  | |
                ||| ||  || | || || || |||||   || |  |  |||| | /-+--++++++-++++--+-++---+------+++++-+-++----+++++-+++++++-+-+-++++++-++++++\ ||||   ||||||   | |||  | |
                ||| ||  || | || || || |||||   || |  |  |||| | | |  |||||| ||||  | ||   |      ||||| | ||    ||||| ||||||| | | |||||| ||||||| ||||   ||||||   | |||  | |
                ||| ||  || | ||/++-++-+++++---++-+--+--++++-+-+-+--++++++-++++--+-++\  |      ||||| | || /--+++++-+++++++-+\| |||||| ||||||| ||||   ||||||   | |||  | |
                ||| ||  || | ||||| ||/+++++---++-+--+--++++-+-+-+--++++++-++++\ | |||/-+------+++++-+-++-+--+++++-+++++++-+++-++++++-+++++++-++++---++++++---+\|||  | |
                ||| ||  || | ||||| ||||||||   || |  |  |||| | | |  |||||| ||||| | |||| |      ||||| | || |  ||||| |||^||| ||| \+++++-+++++++-++++---/|||||   |||||  | |
                ||| ||  || | ||||| ||||||||   |\-+--+--++++-+-+-+--++++++-+++++-+-++++-+------+++++-+-++-+--+++++-+++++++-+++--+++++-++++/|| ||\+----+++++---+++++--/ |
                ||| ||  || | ||||| ||||||||   |/-+--+--++++-+-+-+--++++++-+++++-+-++++-+------+++++-+-++-+--+++++-+++++++-+++--+++++-++++-++-++-+----+++++-\ |||||    |
                ||| ||  || | |||||/++++++++---++-+--+\ |||| | | |  |||||| ||||| | |||| |    /-+++++-+\|| |  ||||| ||||||| |||  ||||| |||| || || |    ||||| | |||||    |
                ||| ||  || | ||||||||||||||   || |  || |||| | | |  |||||| ||||| | |||| |    | ||||| |||| |  ||||| ||||||| |||  ||||| |||| || || |    ||||| | |||||    |
                ||| ||  || | ||||||||||||||   || |  || |||| | | |  |||||| ||||| | |||| |    | ||||| |||| |  ||||| |||||\+-+++--+++++-++++-++-+/ |    ||||| | |||||    |
                ||| ||  || | ||||||||||||||   || |  \+-++++-+-+-+--++++++-+++++-+-++++-+----+-+++++-++++-+--+++++-+++++-+-+++--+++++-++++-++-/  |    ||||| | |||||    |
                ||| ||  || | ||||\+++++++++---++-+---+-++++-+-+-+--++++++-+++++-+-+/|| |    | ||||| |||| |  ||||| ||||| | |||  ||||| |||| ||    |    ||||| | |||||    |
                ||| ||  || |/++++-+++++++++-\ || |   | |||| | | |  |||\++-+++++-+-+-++-+----+-+++++-++++-+--+/||| ||||| | |||  ||||| |||| ||    |    ||||| | |||||    |
                ||| ||  || |||||| ||||||||| | || |   | |||| | | |  ||| || ||||| | | || |    | ||||| |||| |  | ||| ||||| | |||  ||||| |||| ||    |    ||||| | |||||    |
                ||| ||  || |||||| ||||||||| | || |/--+-++++-+-+-+--+++-++-+++++-+-+-++-+----+-+++++-++++-+\ | ||| ||||| | |||  ||||| |||| ||    |    ||||| | |||||    |
                ||| ||  || |||||| ||||||\++-+-++-++--+-++++-+-+-+--+++-++-+++++-+-+-++-+----+-/|||| |||| || | ||| ||||| | |||  ||||| |||| ||    |    ||||| | |||||    |
                |||/++--++-++++++-++++++-++\| || ||  | |||| | | |  ||| v| ||||| | | || |    |  |||| |||| || | ||| ||||| | ||| /+++++-++++-++----+-\  ||||| | |||||    |
                ||||||  || ||\+++-++++++-++++-++-++--+-++++-+-+-+--+++-++-+++++-+-/ || |    |  |||| |||| || | ||| ||||| | ||| |||||| |||| ||    | |  ||||| | |||||    |
                ||||||  || || ||\-++++++-+/|| || ||  | |||| | | |  ||| || ||||| |   || |    |  |||| |||| || | ||| ||||\-+-+++-++++++-++/| ||    | |  ||||| | |||||    |
                ||||||  || || ||  ||\+++-+-++-++-++--+-++++-//+-+--+++-++-+++++-+---++-+----+--++++-++++-++-+-+++-++++--+-+++-++++++-++-+-++----+-+\ ||||| | |||||    |
                ||||||  || || ||  || ||| | || || ||  | ||||  || |  ||| || ||||| |   || |    |  |||| |||| || | ||| ||||  | ||| |||||| || | ||    \-++-+++++-+-++++/    |
                ||||||  || || ||  |\-+++-+-++-++-++--+-++++--++-+--+++-++-+++++-+---++-+----+--++++-++++-++-+-+++-++++--+-++/ |||||| || | ||      || ||||| | ||||     |
                ||||||  || || ||  |  ||| | || || ||  | |||\--++-+--+++-++-+++++-+---++-+----+--++++-++++-++-+-+++-++++--+-++--++++++-++-+-++------++-+++++-+-/|||     |
                ||||||  || || ||  |  ||| | || || ||  | |||   || |  ||| || ||||| |   || |    |  |||| |||| || | ||| ||||  | ||  |||||| || | ||      || ||||| |  |||     |
                ||||||  || |\-++--+--+++-+-+/ || ||  | |||   || |  ||| || ||||| |   || |    |  |||| |||| || | ||| ||||  | ||  |||||| || | ||      || ||||| |  |||     |
                ||||||  || |  ||  | /+++-+-+--++-++--+-+++---++-+--+++-++-+++++-+---++-+----+--++++\|||| || \-+++-++++--+-++--+++/|| || | ||      || ||||| |  |||     |
                ||||||  || |  ||  | |||| | |  || ||  | |||   || |  ||| || ||||| |   || |    |  ||||||||| ||   ||| ||||  | ||  ||| || || | ||      || ||||| |  |||     |
                ||||||  || | /++--+-++++-+-+--++-++--+-+++---++-+--+++-++-+++++-+---++-+----+--+++++++++-++---+++-++++--+-++--+++\|| || | ||      || ||||| |  |||     |
                ||||||  || | |||  | |||| | |  || ||  | |||   || |  ||| || ||||| |   || |    |  ||||||||| ||   ||| ||||/-+-++--++++++-++-+-++-----\|| ||||| |  |||     |
                ||||||  \+-+-+++--+-++++-+-+--++-++--+-+++---++-+--+++-/| \++++-+---++-+----+--+++++++++-++---+++-+++++-+-++--++++++-++-+-++-----+++-+++++-+--+++-----/
                ||||||   | | |||  | |||| | |  || ||  | |||   || |  \++--+--++++-+---++-+----+--+++++++++-++---+++-+++/| \-++--++++++-/| | ||     ||| ||||| |  |||
                ||||||   | | |||  | |||| | |  |\-++--+-+++---++-+---++--+--++++-+---++-+----+--+++++++++-++---+++-+++-+---++--++++++--+-+-++-----+++-+++++-/  |||
                ||||||   | | |||/-+-++++-+-+--+--++--+-+++---++-+---++--+--++++\|   || |    |  ||||||||| ||   ||| ||| |   ||  ||||||  | | ||     ||| |||||    |||
                ||||||   | | |||| | |||| | |  |  ||  | |||   || |   ||  |  ||||||   || |    |  ||||||||| ||   ||| ||| |   ||  ||||||  | | ||     ||| |||||    |||
                ||||||   | | |||| | |||| | |  |  ||  | |||   || |   ||  |  ||||||   || |    |  ||||||||| ||   ||| |\+-+---++--++++++--+-/ ||     ||| |||||    |||
                ||||||   | | |||| | |||| | |  \--++--+-+++---++-+---/|  |  ||||||   || |    |  ||||||||| ||   \++-+-+-+---++--++++++--/   ||     ||| |||||    |||
                ||||||   | | |||| | |||| | |     ||  | |||   || |   /+--+--++++++---++-+----+--+++++++++-++----++-+-+-+---++--++++++------++-----+++-+++++----+++----\
                ||||||   | | |||| | |||| | |     ||  | |||   || |   ||  |  \+++++---++-+----+--+++++++++-++----+/ \-+-+---++--+++++/      ||     ||| |||||    |||    |
                |||||| /-+-+-++++-+-++++-+-+-----++--+-+++---++-+---++--+-\ |||||   || |    |  ||||||||| ||    |    | |   ||/-+++++-------++-----+++-+++++\   |||    |
                |||||| | | | |||| | |||| | |     ||  | |||   || |   ||  | | ||||\---++-+----+--+/||||||| ||    |    | |   ||| |||||       ||     ||| ||||||   |||    |
                |||||| | | | |||| | |\++-+-+-----++--+-+++---++-+---++--+-+-++/|    || |    |  | ||||||| ||    |    | \---+++-+++++-------++-----/|| ||||||   |||    |
                |||||| | | | |||| | | || | |     ||  | |||   || |   |\--+-+-++-+----++-+----+--+-+++++++-++----+----+-----/|| |||||       ||      || ||||||   |||    |
                |||||| | | | |||| | | || | |     ||  | |||   || |   |   |/+-++-+----++-+-\  |  | |^||||| \+----+----+------/| |^|||       ||      || ||||||   |||    |
                |||||| | | | |||| | | || | |     ||  | |||   || |   |   ||| || |    || | |  |  | |||||||  |   /+----+-----\ | |||||       ||      || ||||||   |||    |
                |||||| | | | |||| | | || | |     ||  | |||   || |   |   ||| || |    || | ^  \--+-++++/||  |   ||    |  /--+-+-+++++--\    ||      || ||||||   |||    |
                |||||| | | | |||| | | || \-+-----++--+-+++---++-+---+---+++-++-+----++-+-+-----+-+++/ ||  |   ||  /-+--+--+-+-+++++--+----++\     || ||||||   |||    |
                |||||| | | | |||| | | ||   |     ||  | ||\---++-+---+---+++-++-+----++-+-+-----+-+++--++--+---++--+-+--+--+-+-+/|||  |    |||     || ||||||   |||    |
                |||||| | | \-++++-+-+-++---+-----++--+-++----++-+---+---+++-++-+----++-/ |     | |||  ||  |   ||  | |  |  | | | |||  |    |||     || |\++++---++/    |
                |||||| | |   |||| | | ||   |     ||  | |\----++-+---+---+++-++-+----++---+-----+-+++--++--+---++--+-+--+--+-+-+-+++--+----+++-----++-/ ||||   ||     |
                |||||| \-+---++++-+-+-++---+-----++--+-+-----++-+---+---++/ || |    ||   |     | |||  ||  |   ||  | |  |  | \-+-+++--+----+++-----++---+++/   ||     |
                ||||||   |   |||| | | ||   |     \+--+-+-----++-+---+---++--+/ |    ||   |     | |||  ||  |   ||  | |  |  |   | |||  |    |||     ||   |||    ||     |
                ||||||   |   |||| | | ||   |      |  | |     || |   |   ||  |  |    ||   |/----+-+++--++--+---++--+-+--+--+---+-+++--+----+++-----++---+++--\ ||     |
                ||||||   |   |||| | | ||   |      |  | |     || |   |   ||  |  |    |\---++----+-+++--++--+---++--+-+--+--+---+-+++--+----+++-----++---+++--+-/|     |
                |||||\---+---++++-+-+-++---+------+--+-+-----++-+---+---++--+--+----+----++----+-+++--/|  |   ||  | \--+--+---+-+++--+----+++-----++---/||  |  |     |
                |\+++----+---++++-+-+-/|   |      |  | |     || |   |   ||  |  |    |    ||    | |||   |  |   |\--+----+--+---+-+++--+----+++-----++----+/  |  |     |
                | |||    |   |||| | |  |   |      |  | |     || |   |   \+--+--+----+----++----+-+++---+--+---+---+----+--+---+-+++--+----+++-----++----/   |  |     |
                | |||    \---++++-+-+--+---+------+--+-+-----++-/   |    |  |  |    |    ||    | |||   |  |   |   |    \--+---+-+++--/  /-+++-----++----\   |  |     |
                | |||        |||| | |  |   |   /--+--+-+-----++---\ |    |  |  |    |    ||    | |||   |  |   |   \-------+---+-+++-----+-++/     ||    |   |  |     |
                | |||        |||| | |  |   |   |  |  | |     ||   | |    |  |  |    |    ||    \-+++---+--+---+-------<---+---+-/||     | ||      ||    |   |  |     |
                | ||\--------++++-+-+--+---+---+--+--+-+-----++---+-+----+--+--+----+----++------+/|   |  |   \-----------/   |  ||     | ||      ||    |   |  |     |
                | ||         |||| | |  |   |   |  |  | |     ||   | |    \--+--+----+----/|      | |   |  |                   |  ||   /-+-++------++----+\  |  |     |
                | ||         |||| | |  |   |   |  |  | |     ||   | |       |  |    |     |      | |   |  |                   |  ||   | | ||      ||    ||  |  |     |
                | ||         \+++-+-+--+---+---+--+--+-+-----++---+-+-------+--+----+-----+------+-+---+--+-------------------+--/|   | | ||      ||    ||  |  |     |
                | ||          ||| | |  \---+---+--+--+-+-----++---+-+-------+--+----+-----+------+-+---/  |                   |   |   | | ||      ||    ||  |  |     |
                | ||          ||\-+-+------+---+--+--+-+-----++---+-+-------+--/    |     |      | |      |                   |   |   | | ||      ||    ||  |  |     |
                | ||          ||  | |      |   |  |  | |     ||   | \-------+-------+-----+------+-+------+-------------------+---+---+-+-++------++----++--+--+-----/
                | ||          ||  | |     /+---+--+--+-+-----++---+--------\|       |     |   /--+-+------+-------------------+-\ |   | | ||      ||    ||  |  |
                | ||          ||  | \-----++---+--+--+-+-----++---+--------++-------+-----+---+--+-/      |                   \-+-+---+-+-++------/|    ||  |  |
                | \+----------++--+-------++---+--+--+-+-----++---+--------+/       |     |   |  |        |                     | |   | \-++-------+----/|  |  |
                |  |          \+--+-------++---+--+--+-+-----++---+--------+---->---+-----+---+--+--------+---------------------+-+---+---/|       |     |  |  |
                |  |           |  |       ||   |  |  |/+-----++---+------\ |        |     \---+--+--------+---------------------+-+---+----+-------+-----+--/  |
                |  |           \--+-------++---+--+--+++-----++---+------+-+--------/         |  |        |                     | |   |    |       |     |     |
                |  \--------------+-------+/   |  |  |||     ||   |      | |                  |  \--------+---------------------+-+---+----+-------+-----+-----/
                |            /----+-------+----+--+--+++-----++---+------+-+------------------+-----------+-------------------\ | |   |    |       |     |
                |            |    |       |    |  |  |||     ||   |      | |                  |           |                   | | |   \----+-------+-----/
                |            |    |       |    |  |  |||     ||   |      | |                  |           |                   | | |        |       |
                |            |    |       |    |  |  |||     |\---+------+-+------------------+-----------+-------------------+-+-+--------/       |
                |            |    |       \----+--+--+++-----+----+------+-/                  |           |                   | | |                |
                |            |    |            |  |  |||     \----+------+--------------------+-----------+-------------------+-+-+>---------------/
                |            |    |            |  |  |\+----------+------/                    |           |                   | | |
                |            |    |            |  \-<+-+----------+---------------------------+-----------/                   | | |
                |            |    |            |     | \----------+---------------------------+-------------------------------+-+-/
                |            |    \------------+-----/            |                           |                               | |
                |            |                 |                  |                           \-------------------------------+-/
                |            \-----------------+------------------+-----------------------------------------------------------/
                |                              \------------------/                                                                                                    """.stripMargin
}
