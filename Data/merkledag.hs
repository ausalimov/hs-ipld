import Data.Attoparsec.ByteString as A
import qualified Data.ByteString.Char8 as B
import Data.Word
import qualified Data.Map.Strict as M 
import Multihash


-- This represents the IPFS world
type IPFS_Simple = M.Map Multihash MDagNode

-- Note, you will never have two @links in the same level, 
-- since all nodes are deterministic as per the spec

data MDagNode = MDagNode {
	n_hash :: Multihash,
	n_vals :: M.Map B.ByteString B.ByteString
} deriving (Show)



----- Basic Interface -----

ipld_get_node :: IPFS_Simple -> B.ByteString -> Maybe B.ByteString
ipld_get_node w t = let b = (B.split '/' t)
                    in case follow_link w  of 
		                Just n -> resolve_link w n (tail b) 
		                Nothing -> Nothing


-- Given a node and a remaining path traversal, return the node at the end of the line
-- If the head of the traversal is not in the vals, return a failure (Nothing)
resolve_link :: IPFS_Simple -> MDagNode -> [B.ByteString] -> Maybe B.ByteString
resolve_link w n [b] = M.lookup b (n_vals n) -- last one won't be link
resolve_link w n (b:bs) = case M.lookup b (n_vals n) of	
	Just n' -> resolve_link w n' bs -- it's not a link
	Nothing -> case M.lookup "@link" (nvals n) of  -- it is a link! look for an @link
		Just l -> 

follow_link w b of 
		Nothing -> Nothing -- Hash doesn't exist! 
		Just new_node -> resolve_link w new_node bs



-- Looks up a hash. 
follow_link :: IPFS_Simple -> B.ByteString -> Maybe MDagNode
follow_link w b = case to_multihash b of 
	Just m -> M.lookup m w 
	Nothing -> Nothing





