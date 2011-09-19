module Cbc.Ast.Node where
import Cbc.Ast.Dumpable
-- import java.io.PrintStream;

data Node = Node  {location :: String } deriving Show
instance Dumpable Node where
  dump = show
-- abstract public class Node implements Dumpable {
--     public Node() {
--     }

--     abstract public Location location();

--     public void dump() {
--         dump(System.out);
--     }

--     public void dump(PrintStream s) {
--         dump(new Dumper(s));
--     }

--     public void dump(Dumper d) {
--         d.printClass(this, location());
--         _dump(d);
--     }

--     abstract protected void _dump(Dumper d);
-- }
