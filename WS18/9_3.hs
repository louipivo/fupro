class C f where
  comp :: f b c -> f a b -> f a c

  data T f g = T (f String Int) (g Bool)
