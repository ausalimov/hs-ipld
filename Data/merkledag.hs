import Data.Attoparsec.ByteString as A
import qualified Data.ByteString.Char8 as B
import Data.Word
import qualified Data.Map.Strict as M 
import Multihash


-- This represents the IPFS world
type IPFS_Simple = M.Map Multihash MDagNode

-- Note, you will never have two @links in the same level, 
-- since all nodes are deterministic as per the spec

data SubOb = SubMap { s_map :: M.Map B.ByteString SubOb}
                 | SubVal { s_val :: B.ByteString} deriving (Show)


data MDagNode = MDagNode {
	n_hash :: Multihash,
	n_vals :: SubOb -- will always be the map variation thereof
} deriving (Show)



----- Basic Interface -----

ipld_get_subob :: IPFS_Simple -> B.ByteString -> Maybe SubOb
ipld_get_subob w t = let b = (B.split '/' t)
                    in case follow_link w (head b) of 
		                Just n -> traverse w n (tail b) 
		                Nothing -> Nothing

traverse :: IPFS_Simple -> MDagNode -> [B.ByteString] -> Maybe SubOb
traverse w node [] = Just (n_vals node)
traverse w node b = case (n_vals node) of 
	SubMap node_map -> case M.lookup (head b) node_map of 
		Just subob -> case subob of 
			SubVal _ -> Just subob
			SubMap _ -> resolve_in_node w subob (tail b)
		Nothing -> resolve_link w (n_vals node) (tail b)
				

resolve_in_node :: IPFS_Simple -> 
				   SubOb -> 
				   [B.ByteString] -> 
				   Maybe SubOb
resolve_in_node w s [] = Just s 
resolve_in_node w s b = case s of 
	SubVal _ -> Just s 
	SubMap m -> case M.lookup (head b) m of 
		Just subob -> resolve_in_node w subob (tail b) 
		Nothing -> case M.lookup "@link" node_map of 
			Nothing -> Nothing 
			Just l -> case l of 
				SubVal link -> case 
			

resolve_link :: IPFS_Simple -> SubOb -> [B.ByteString] -> Maybe SubOb
resolve_link w s b = case s of 
	SubMap node_map -> case M.lookup "@link" node_map of 
			Nothing -> Nothing 
			Just l -> case l of 
				SubVal link -> case link == (head b) of 
					False -> Nothing
					True -> case follow_link w link of 
						Just node' -> traverse w node' (tail b)
						Nothing -> Nothing
	

{-
resolve_link w n [b] = M.lookup b (n_vals n) -- last one won't be link
resolve_link w n (b:bs) = case M.lookup b (n_vals n) of	
	Just n' -> resolve_link w n' bs -- it's not a link
	Nothing -> case M.lookup "@link" (nvals n) of  -- it is a link! look for an @link
		Just l -> 

follow_link w b of 
		Nothing -> Nothing -- Hash doesn't exist! 
		Just new_node -> resolve_link w new_node bs
-}



-- Looks up a hash. 
follow_link :: IPFS_Simple -> B.ByteString -> Maybe MDagNode
follow_link w b = case to_multihash b of 
	Just m -> M.lookup m w 
	Nothing -> Nothing





