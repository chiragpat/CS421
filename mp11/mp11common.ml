type point = float * float

type draw_cmd = Pixel of point
              | Line of point * point
              | Oval of point * point * point (* three consecutive points of
                                                 enclosing rectangle *)
