CREATE TABLE foo (
  bar text,
  baz numeric,
  bak numeric,
  CHECK (bak = baz)
);
