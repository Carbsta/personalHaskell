type Room = Matrix Tile
type Display = Matrix Char
type Matrix a = [Row a]
type Row a = [a]
type Tile = [Entity]
type Graphic = Char
data Point = Point Int Int deriving(Show)
data Object = Object { name :: String
                     , description :: String
                     } deriving(Show)
					 
data Character = Character { object :: Object
                           , hp :: Int
                           , position :: Point                         
                           } deriving(Show)
						   
data Orientation = Vertical | Horizontal deriving(Show)

data Entity = Player Character | Wall Object Orientation | Floor Object | Blank | Enemy Character deriving(Show)

main            :: IO ()
main            =  putStrLn (unlines (drawRoom (readRoom (testRoom))))

wall :: Object
wall =  Object "Wall" "A stone wall."

fl :: Object
fl = Object "Floor" "A stone floor tile."

player  :: Point -> Entity
player p =  Player (Character (Object "Steve" "An adventurer") 10 p)

emu  :: Point -> Entity
emu p =  Enemy (Character (Object "Emu" "A flightless bird") 2 p)

testRoom :: Display
testRoom = ["          "
           ,"  ------  "
           ,"  |....|  "
           ,"  |....|  "
           ,"  |....|  "
           ,"  ------  "
           ,"          "]
           
--addEntity :: Entity -> Room -> Room


drawRoom :: Room -> Display
drawRoom =  map (map draw)

readRoom :: Display -> Room
readRoom =  map (map readTile) 
             
draw :: Tile -> Graphic
draw =  graphics . head

graphics                      :: Entity -> Graphic
graphics (Player _)           = '@'
graphics (Wall _ Horizontal)  = '-'
graphics (Wall _ Vertical)    = '|'
graphics (Floor _)            = '.'
graphics (Blank)              = ' '
graphics (Enemy _)              = 'E'

readChar                      ::  Graphic -> Entity
readChar ' '                  =   Blank
readChar '-'                  =   Wall wall Horizontal
readChar '|'                  =   Wall wall Vertical
readChar '.'                  =   Floor fl

readTile                     ::  Graphic -> Tile
readTile c                    =  [readChar c]