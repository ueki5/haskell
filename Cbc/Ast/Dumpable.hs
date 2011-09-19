module Cbc.Ast.Dumpable where
-- package net.loveruby.cflat.ast;

class Dumpable a where
  dump :: a -> String
-- public interface Dumpable {
--     void dump(Dumper d);
-- }
