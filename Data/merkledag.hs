import Data.Attoparsec.ByteString as A
import Data.ByteString as B
import Data.Word
import Data.Map.Strict as M 



-- This represents the IPFS world
type IPFS_Simple = M.Map B.ByteString MDagNode

-- Note, you will never have two @links in the same level, 
-- since all nodes are deterministic as per the spec
data MDagNode = MDagNode {
	n_hash :: B.ByteString,
	n_vals :: M.Map B.ByteString MDagNode
} deriving (Show)


----- Basic Interface -----

ipld_get_node :: IPFS_Simple -> B.ByteString -> Maybe MDagNode
ipld_get_node w t = resolve_link w (B.split '/' t) 


-- Given a node and a remaining path traversal, return the node at the end of the line
-- If the head of the traversal is not in the vals, return a failure (Nothing)
resolve_link :: IPFS_Simple -> MDagNode -> [B.ByteString] -> Maybe MDagNode
resolve_link w n [b] = M.lookup b (n_vals n)
resolve_link w n (b:bs) = case M.lookup b (n_vals n) of	
	Just n' -> resolve_link w n' bs
	Nothing -> case follow_link w b of 
		Nothing -> Nothing -- Hash doesn't exist! 
		Just new_node -> resolve_link w new_node bs



follow_link :: IPFS_Simple -> B.ByteString -> Maybe MDagNode
follow_link t b = M.lookup b t 





