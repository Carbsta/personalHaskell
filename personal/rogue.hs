import Data.Sequence (Seq, fromList, update, index)
import Data.Foldable (toList)


type Room = Matrix Tile
type Display = Matrix Char
type Matrix a = Seq (Row a)
type Row a = Seq a
type Tile = [Entity]
type Graphic = Char
data Point = Point Int Int deriving(Show)
data Object = Object { name :: String
                     , description :: String
                     , position :: Point
                     } deriving(Show)
           
data Character = Character { object :: Object
                           , hp :: Int                         
                           } deriving(Show)
               
data Orientation = Vertical | Horizontal deriving(Show)

data Entity = Player Character | Wall Object Orientation | Floor Object | Blank | Enemy Character deriving(Show)

wall :: Point -> Object
wall =  Object "Wall" "A stone wall."

fl :: Point -> Object
fl = Object "Floor" "A stone floor tile."

player  :: Point -> Entity
player p =  Player (Character (Object "Steve" "An adventurer" p) 10)

emu  :: Point -> Entity
emu p =  Enemy (Character (Object "Emu" "A flightless bird" p) 2)

testRoom :: Display
testRoom = fromList["          "
                   ,"  ------  "
                   ,"  |....|  "
                   ,"  |....|  "
                   ,"  |....|  "
                   ,"  ------  "
                   ,"          "]


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
graphics (Enemy _)            = 'E'

readChar                      ::  Graphic -> Entity
readChar ' '                  =   Blank
readChar '-'                  =   Wall wall Horizontal
readChar '|'                  =   Wall wall Vertical
readChar '.'                  =   Floor fl

readTile                     ::  Graphic -> Tile
readTile c                    =  [readChar c]