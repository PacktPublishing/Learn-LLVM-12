MODULE Point;

TYPE Point = RECORD X, Y: INTEGER END;

VAR p: Point;

PROCEDURE AssignX(a: INTEGER);
BEGIN
  p.X := a;
END AssignX;

END Point.
