MODULE Gcd;

TYPE Point = RECORD X, Y: INTEGER END;

VAR x: INTEGER;
    p: Point;

PROCEDURE GCD(a, b: INTEGER) : INTEGER;
VAR t: INTEGER;
BEGIN
  IF b = 0 THEN
    RETURN a;
  END;
  WHILE b # 0 DO
    t := a MOD b;
    a := b;
    b := t;
  END;
  p.X := a;
  RETURN a;
END GCD;

END Gcd.
