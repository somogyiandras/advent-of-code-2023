-- input = "\
-- \.....489............................152....503.........................180......200.........147.......13.......................239..........\
-- \......*.....186.48....681...732........*..................935.........*.....................*......................512............*..874....\
-- \..806.540......*.........*............249......904...358....*......957..867..863..........857.....264..............@....89=......97..*......\
-- \......................793........................*...=.....142...........*..*....%...................@......+...............36.......547....\
-- \...........=335..............906*634..932........201...157.............423..72..103.798.......732........161...............*................\
-- \..........................*..........*....662.................88....................$..............=........................439.126...269...\
-- \...905&...74.............795...128...872....*....*.....521.........../......806&.........456.....93..869.........168..............*.........\
-- \........../..=662.824.........+............863..47........-.700.....437..........316..........................96.*......348*632....138.$932.\
-- \........-..........*.......................................................465....*.........243......&........*...94..+..........*..........\
-- \..41...532.......511.....-......$........&....216.................................330...715..../...309........415.....388.....873.287.265...\
-- \....*...................338...496.......18......*............48........873................@.............493.................................\
-- \..849.............................136.#......787.........787..$....152..*........187..../.....738......*....................&85..960...749..\
-- \..............395.................*...478...........................*....860.......*...613./.........194........................*.....%.....\
-- \.....+799.........132..188........691..........689/...176..........274........569..840.....8....#........................451...71...........\
-- \..............93..&....*........@........#..................892..............................275.....862*180../.....355..*.............796..\
-- \.......400......*......676.....342.......930...389...925.......*...........580..3......395....................27....*....888..........#.....\
-- \.....%......205..888................60...........=..*.......540........742.*..........*.......858..................589.......679..490.......\
-- \...462..125*....................691..@.221=.........28...................*..31.615..827..............876...................-.........*762...\
-- \.............501............$..=...............*.........&.............842......@.......308................601...........546................\
-- \......428...*.............417.....760....25.....809......119.......................589.../.........610......../..%256...................858.\
-- \"
-- engine = loadEngine input 
-- symbols = searchSymbols engine validSymbol
-- adjacent = getAdjacent_to_Symbols engine symbols
-- marked = mapCells engine adjacent translate 
-- erased = eraseSymbols marked
-- parsedS = parseCells (cells erased)
