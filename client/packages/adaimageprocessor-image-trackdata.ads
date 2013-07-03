--------------------------------------------------------------------------------
-- Package: Adaimageprocessor.Image.Trackdata
--
-- Purpose:
-- This package holds all the constant values concerning the position of the
-- track on the images to be processed
--
--------------------------------------------------------------------------------

package Adaimageprocessor.Image.Trackdata is



   -- DATA STRUCTURE BEGIN

   -----------------------------------------------------------------------------
   -- Types: Adaimageprocessor.Image.Trackdata
   --
   -- Point_Structure - a record for X and Y values of a pixel.
   -- Points - an unconstrained array of X and Y values of pixels.
   -- Points_Access - a pointer to <Points>
   -- Slice_Structure - a record for a single slice.
   -- Slices - an unconstrained array of Slices.
   -----------------------------------------------------------------------------
   type Point_Structure is
      record
         X : Integer;
         Y : Integer;
      end record;

   type Points        is array (Positive range <>) of aliased Point_Structure;
   type Points_Access is access constant Points;

   type Slice_Structure is
      record
         Point : Points_Access;
         Threshold_Lower : Integer;
         Threshold_Upper : Integer;
         Link_Slice_Other_Rail : Integer;
      end record;
   type Slices        is array (Positive range <>) of Slice_Structure;
   -- the code could be futher improved by putting the rails into an array (i.e.
   -- an array of "Slices") istead of explicitly naming them Left_Rail and
   -- Right_Rail. This would make addressing in the worker-tasks easier.


   -- DATA STRUCTURE END

   -- DATA BEGIN

   -----------------------------------------------------------------------------
   -- Variables: Adaimageprocessor.Image.Trackdata
   --
   -- Left_Rail - all data of the left rail on the images
   -- Right_Rail - all data of the right rail on the images
   -----------------------------------------------------------------------------
   Left_Rail : aliased constant Slices :=
     (
      1 => ( Point => ( new Points'(
                       (0,541),
                       (1,541),
                       (1,542),
                       (2,542),
                       (3,542),
                       (3,543),
                       (4,543),
                       (5,543),
                       (5,544),
                       (6,544),
                       (7,544),
                       (8,544),
                       (9,544)
                      )),
            Threshold_Lower => -1,
            Threshold_Upper => -1,
            Link_Slice_Other_Rail => -1),

      2 => ( Point => ( new Points'(
                       (1,540),
                       (2,540),
                       (2,541),
                       (3,541),
                       (4,541),
                       (4,542),
                       (5,542),
                       (6,542),
                       (6,543),
                       (7,543),
                       (8,543),
                       (9,543),
                       (10,543)
                      )),
            Threshold_Lower => -1,
            Threshold_Upper => -1,
            Link_Slice_Other_Rail => -1),

      3 => ( Point => ( new Points'(
                       (1,538),
                       (1,539),
                       (2,539),
                       (3,539),
                       (3,540),
                       (4,540),
                       (5,540),
                       (5,541),
                       (6,541),
                       (7,541),
                       (7,542),
                       (8,542),
                       (9,542),
                       (10,542)
                      )),
            Threshold_Lower => -1,
            Threshold_Upper => -1,
            Link_Slice_Other_Rail => -1),

      4 => ( Point => ( new Points'(
                       (2,537),
                       (2,538),
                       (3,538),
                       (4,538),
                       (4,539),
                       (5,539),
                       (6,539),
                       (6,540),
                       (7,540),
                       (8,540),
                       (8,541),
                       (9,541),
                       (10,541)
                      )),
            Threshold_Lower => -1,
            Threshold_Upper => -1,
            Link_Slice_Other_Rail => -1),

      5 => ( Point => ( new Points'(
                       (2,536),
                       (3,536),
                       (3,537),
                       (4,537),
                       (5,537),
                       (5,538),
                       (6,538),
                       (7,538),
                       (7,539),
                       (8,539),
                       (9,539),
                       (9,540),
                       (10,540),
                       (11,540)
                      )),
            Threshold_Lower => -1,
            Threshold_Upper => -1,
            Link_Slice_Other_Rail => -1),

      6 => ( Point => ( new Points'(
                       (3,535),
                       (4,535),
                       (4,536),
                       (5,536),
                       (6,536),
                       (6,537),
                       (7,537),
                       (8,537),
                       (8,538),
                       (9,538),
                       (10,538),
                       (10,539),
                       (11,539),
                       (12,539)
                      )),
            Threshold_Lower => -1,
            Threshold_Upper => -1,
            Link_Slice_Other_Rail => -1),

      7 => ( Point => ( new Points'(
                       (3,534),
                       (4,534),
                       (5,534),
                       (5,535),
                       (6,535),
                       (7,535),
                       (7,536),
                       (8,536),
                       (9,536),
                       (9,537),
                       (10,537),
                       (11,537),
                       (11,538),
                       (12,538)
                      )),
            Threshold_Lower => -1,
            Threshold_Upper => -1,
            Link_Slice_Other_Rail => -1),

      8 => ( Point => ( new Points'(
                       (4,533),
                       (5,533),
                       (6,533),
                       (6,534),
                       (7,534),
                       (8,534),
                       (8,535),
                       (9,535),
                       (10,535),
                       (10,536),
                       (11,536),
                       (12,536),
                       (12,537)
                      )),
            Threshold_Lower => -1,
            Threshold_Upper => -1,
            Link_Slice_Other_Rail => -1),

      9 => ( Point => ( new Points'(
                       (4,532),
                       (5,532),
                       (6,532),
                       (7,532),
                       (7,533),
                       (8,533),
                       (9,533),
                       (9,534),
                       (10,534),
                       (11,534),
                       (11,535),
                       (12,535),
                       (13,535)
                      )),
            Threshold_Lower => -1,
            Threshold_Upper => -1,
            Link_Slice_Other_Rail => -1),

      10 => ( Point => ( new Points'(
                        (5,531),
                        (6,531),
                        (7,531),
                        (8,531),
                        (8,532),
                        (9,532),
                        (10,532),
                        (10,533),
                        (11,533),
                        (12,533),
                        (12,534),
                        (13,534)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      11 => ( Point => ( new Points'(
                        (5,530),
                        (6,530),
                        (7,530),
                        (8,530),
                        (9,530),
                        (9,531),
                        (10,531),
                        (11,531),
                        (11,532),
                        (12,532),
                        (13,532),
                        (13,533),
                        (14,533)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      12 => ( Point => ( new Points'(
                        (6,529),
                        (7,529),
                        (8,529),
                        (9,529),
                        (10,529),
                        (10,530),
                        (11,530),
                        (12,530),
                        (12,531),
                        (13,531),
                        (14,531),
                        (14,532)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      13 => ( Point => ( new Points'(
                        (7,528),
                        (8,528),
                        (9,528),
                        (10,528),
                        (11,528),
                        (11,529),
                        (12,529),
                        (13,529),
                        (13,530),
                        (14,530),
                        (15,530),
                        (15,531)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      14 => ( Point => ( new Points'(
                        (7,527),
                        (8,527),
                        (9,527),
                        (10,527),
                        (11,527),
                        (12,527),
                        (12,528),
                        (13,528),
                        (14,528),
                        (14,529),
                        (15,529),
                        (16,529)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      15 => ( Point => ( new Points'(
                        (7,525),
                        (7,526),
                        (8,526),
                        (9,526),
                        (10,526),
                        (11,526),
                        (12,526),
                        (13,526),
                        (13,527),
                        (14,527),
                        (15,527),
                        (15,528),
                        (16,528)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      16 => ( Point => ( new Points'(
                        (7,524),
                        (8,524),
                        (8,525),
                        (9,525),
                        (10,525),
                        (11,525),
                        (12,525),
                        (13,525),
                        (14,525),
                        (14,526),
                        (15,526),
                        (16,526),
                        (16,527),
                        (17,527)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      17 => ( Point => ( new Points'(
                        (8,523),
                        (9,523),
                        (9,524),
                        (10,524),
                        (11,524),
                        (12,524),
                        (13,524),
                        (14,524),
                        (15,524),
                        (15,525),
                        (16,525),
                        (17,525),
                        (17,526)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      18 => ( Point => ( new Points'(
                        (9,522),
                        (10,522),
                        (10,523),
                        (11,523),
                        (12,523),
                        (13,523),
                        (14,523),
                        (15,523),
                        (16,523),
                        (16,524),
                        (17,524),
                        (18,524)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      19 => ( Point => ( new Points'(
                        (9,521),
                        (10,521),
                        (11,521),
                        (11,522),
                        (12,522),
                        (13,522),
                        (14,522),
                        (15,522),
                        (16,522),
                        (17,522),
                        (17,523),
                        (18,523)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      20 => ( Point => ( new Points'(
                        (10,520),
                        (11,520),
                        (12,520),
                        (12,521),
                        (13,521),
                        (14,521),
                        (15,521),
                        (16,521),
                        (17,521),
                        (18,521),
                        (18,522),
                        (19,522)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      21 => ( Point => ( new Points'(
                        (10,519),
                        (11,519),
                        (12,519),
                        (13,519),
                        (13,520),
                        (14,520),
                        (15,520),
                        (16,520),
                        (17,520),
                        (18,520),
                        (19,520),
                        (19,521)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      22 => ( Point => ( new Points'(
                        (10,518),
                        (11,518),
                        (12,518),
                        (13,518),
                        (14,518),
                        (14,519),
                        (15,519),
                        (16,519),
                        (17,519),
                        (18,519),
                        (19,519),
                        (20,519)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      23 => ( Point => ( new Points'(
                        (11,517),
                        (12,517),
                        (13,517),
                        (14,517),
                        (15,517),
                        (15,518),
                        (16,518),
                        (17,518),
                        (18,518),
                        (19,518),
                        (20,518)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      24 => ( Point => ( new Points'(
                        (11,516),
                        (12,516),
                        (13,516),
                        (14,516),
                        (15,516),
                        (16,516),
                        (16,517),
                        (17,517),
                        (18,517),
                        (19,517),
                        (20,517)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      25 => ( Point => ( new Points'(
                        (12,515),
                        (13,515),
                        (14,515),
                        (15,515),
                        (16,515),
                        (17,515),
                        (17,516),
                        (18,516),
                        (19,516)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      26 => ( Point => ( new Points'(
                        (13,514),
                        (14,514),
                        (15,514),
                        (16,514),
                        (17,514),
                        (18,514),
                        (18,515),
                        (19,515),
                        (20,515),
                        (21,515),
                        (21,516)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      27 => ( Point => ( new Points'(
                        (13,513),
                        (14,513),
                        (15,513),
                        (16,513),
                        (17,513),
                        (18,513),
                        (19,513),
                        (19,514),
                        (20,514),
                        (21,514),
                        (22,514),
                        (22,515)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      28 => ( Point => ( new Points'(
                        (14,512),
                        (15,512),
                        (16,512),
                        (17,512),
                        (18,512),
                        (19,512),
                        (20,512),
                        (20,513),
                        (21,513),
                        (22,513),
                        (23,513)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      29 => ( Point => ( new Points'(
                        (14,511),
                        (15,511),
                        (16,511),
                        (17,511),
                        (18,511),
                        (19,511),
                        (20,511),
                        (21,511),
                        (21,512),
                        (22,512),
                        (23,512)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      30 => ( Point => ( new Points'(
                        (14,510),
                        (15,510),
                        (16,510),
                        (17,510),
                        (18,510),
                        (19,510),
                        (20,510),
                        (21,510),
                        (22,510),
                        (22,511),
                        (23,511),
                        (24,511)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      31 => ( Point => ( new Points'(
                        (14,508),
                        (14,509),
                        (15,509),
                        (16,509),
                        (17,509),
                        (18,509),
                        (19,509),
                        (20,509),
                        (21,509),
                        (22,509),
                        (23,509),
                        (23,510),
                        (24,510)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      32 => ( Point => ( new Points'(
                        (14,507),
                        (15,507),
                        (15,508),
                        (16,508),
                        (17,508),
                        (18,508),
                        (19,508),
                        (20,508),
                        (21,508),
                        (22,508),
                        (23,508),
                        (24,508),
                        (24,509)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      33 => ( Point => ( new Points'(
                        (15,506),
                        (16,506),
                        (16,507),
                        (17,507),
                        (18,507),
                        (19,507),
                        (20,507),
                        (21,507),
                        (22,507),
                        (23,507),
                        (24,507),
                        (25,507)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      34 => ( Point => ( new Points'(
                        (16,505),
                        (17,505),
                        (17,506),
                        (18,506),
                        (19,506),
                        (20,506),
                        (21,506),
                        (22,506),
                        (23,506),
                        (24,506),
                        (25,506)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      35 => ( Point => ( new Points'(
                        (17,504),
                        (18,504),
                        (18,505),
                        (19,505),
                        (20,505),
                        (21,505),
                        (22,505),
                        (23,505),
                        (24,505),
                        (25,505),
                        (26,505)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      36 => ( Point => ( new Points'(
                        (17,503),
                        (18,503),
                        (19,503),
                        (19,504),
                        (20,504),
                        (21,504),
                        (22,504),
                        (23,504),
                        (24,504),
                        (25,504),
                        (26,504)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      37 => ( Point => ( new Points'(
                        (18,502),
                        (19,502),
                        (20,502),
                        (20,503),
                        (21,503),
                        (22,503),
                        (23,503),
                        (24,503),
                        (25,503),
                        (26,503)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      38 => ( Point => ( new Points'(
                        (18,501),
                        (19,501),
                        (20,501),
                        (21,501),
                        (21,502),
                        (22,502),
                        (23,502),
                        (24,502),
                        (25,502)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      39 => ( Point => ( new Points'(
                        (19,500),
                        (20,500),
                        (21,500),
                        (22,500),
                        (22,501),
                        (23,501),
                        (24,501),
                        (25,501),
                        (26,501),
                        (27,501)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      40 => ( Point => ( new Points'(
                        (19,499),
                        (20,499),
                        (21,499),
                        (22,499),
                        (23,499),
                        (23,500),
                        (24,500),
                        (25,500),
                        (26,500),
                        (27,500),
                        (28,500)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      41 => ( Point => ( new Points'(
                        (19,498),
                        (20,498),
                        (21,498),
                        (22,498),
                        (23,498),
                        (24,498),
                        (24,499),
                        (25,499),
                        (26,499),
                        (27,499),
                        (28,499),
                        (29,499)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      42 => ( Point => ( new Points'(
                        (20,496),
                        (20,497),
                        (21,497),
                        (22,497),
                        (23,497),
                        (24,497),
                        (25,497),
                        (25,498),
                        (26,498),
                        (27,498),
                        (28,498),
                        (29,498)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      43 => ( Point => ( new Points'(
                        (21,495),
                        (21,496),
                        (22,496),
                        (23,496),
                        (24,496),
                        (25,496),
                        (26,496),
                        (26,497),
                        (27,497),
                        (28,497),
                        (29,497),
                        (30,497)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      44 => ( Point => ( new Points'(
                        (22,495),
                        (23,495),
                        (24,495),
                        (25,495),
                        (26,495),
                        (27,495),
                        (27,496),
                        (28,496),
                        (29,496),
                        (30,496)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      45 => ( Point => ( new Points'(
                        (22,494),
                        (23,494),
                        (24,494),
                        (25,494),
                        (26,494),
                        (27,494),
                        (28,494),
                        (28,495),
                        (29,495),
                        (30,495),
                        (31,495)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      46 => ( Point => ( new Points'(
                        (22,492),
                        (22,493),
                        (23,493),
                        (24,493),
                        (25,493),
                        (26,493),
                        (27,493),
                        (28,493),
                        (29,493),
                        (29,494),
                        (30,494),
                        (31,494)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      47 => ( Point => ( new Points'(
                        (22,491),
                        (23,491),
                        (23,492),
                        (24,492),
                        (25,492),
                        (26,492),
                        (27,492),
                        (28,492),
                        (29,492),
                        (30,492),
                        (30,493),
                        (31,493)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      48 => ( Point => ( new Points'(
                        (23,490),
                        (24,490),
                        (24,491),
                        (25,491),
                        (26,491),
                        (27,491),
                        (28,491),
                        (29,491),
                        (30,491),
                        (31,491),
                        (31,492),
                        (32,492)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      49 => ( Point => ( new Points'(
                        (24,489),
                        (25,489),
                        (25,490),
                        (26,490),
                        (27,490),
                        (28,490),
                        (29,490),
                        (30,490),
                        (31,490),
                        (32,490),
                        (32,491)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      50 => ( Point => ( new Points'(
                        (24,488),
                        (25,488),
                        (26,488),
                        (26,489),
                        (27,489),
                        (28,489),
                        (29,489),
                        (30,489),
                        (31,489),
                        (32,489),
                        (33,489)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      51 => ( Point => ( new Points'(
                        (25,487),
                        (26,487),
                        (27,487),
                        (27,488),
                        (28,488),
                        (29,488),
                        (30,488),
                        (31,488),
                        (32,488),
                        (33,488),
                        (34,488)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      52 => ( Point => ( new Points'(
                        (25,486),
                        (26,486),
                        (27,486),
                        (28,486),
                        (28,487),
                        (29,487),
                        (30,487),
                        (31,487),
                        (32,487),
                        (33,487),
                        (34,487)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      53 => ( Point => ( new Points'(
                        (25,484),
                        (25,485),
                        (26,485),
                        (27,485),
                        (28,485),
                        (29,485),
                        (29,486),
                        (30,486),
                        (31,486),
                        (32,486),
                        (33,486),
                        (34,486)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      54 => ( Point => ( new Points'(
                        (26,483),
                        (26,484),
                        (27,484),
                        (28,484),
                        (29,484),
                        (30,484),
                        (30,485),
                        (31,485),
                        (32,485),
                        (33,485),
                        (34,485),
                        (35,485)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      55 => ( Point => ( new Points'(
                        (26,482),
                        (27,482),
                        (27,483),
                        (28,483),
                        (29,483),
                        (30,483),
                        (31,483),
                        (31,484),
                        (32,484),
                        (33,484),
                        (34,484),
                        (35,484)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      56 => ( Point => ( new Points'(
                        (27,481),
                        (28,481),
                        (28,482),
                        (29,482),
                        (30,482),
                        (31,482),
                        (32,482),
                        (32,483),
                        (33,483),
                        (34,483),
                        (35,483)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      57 => ( Point => ( new Points'(
                        (28,480),
                        (29,480),
                        (29,481),
                        (30,481),
                        (31,481),
                        (32,481),
                        (33,481),
                        (33,482),
                        (34,482),
                        (35,482),
                        (36,482)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      58 => ( Point => ( new Points'(
                        (28,479),
                        (29,479),
                        (30,479),
                        (30,480),
                        (31,480),
                        (32,480),
                        (33,480),
                        (34,480),
                        (34,481),
                        (35,481),
                        (36,481),
                        (37,481)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      59 => ( Point => ( new Points'(
                        (28,478),
                        (29,478),
                        (30,478),
                        (31,478),
                        (31,479),
                        (32,479),
                        (33,479),
                        (34,479),
                        (35,479),
                        (35,480),
                        (36,480),
                        (37,480)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      60 => ( Point => ( new Points'(
                        (29,477),
                        (30,477),
                        (31,477),
                        (32,477),
                        (32,478),
                        (33,478),
                        (34,478),
                        (35,478),
                        (36,478),
                        (36,479),
                        (37,479),
                        (38,479)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      61 => ( Point => ( new Points'(
                        (29,476),
                        (30,476),
                        (31,476),
                        (32,476),
                        (33,476),
                        (33,477),
                        (34,477),
                        (35,477),
                        (36,477),
                        (37,477),
                        (37,478),
                        (38,478)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      62 => ( Point => ( new Points'(
                        (30,475),
                        (31,475),
                        (32,475),
                        (33,475),
                        (34,475),
                        (34,476),
                        (35,476),
                        (36,476),
                        (37,476),
                        (38,476),
                        (38,477)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      63 => ( Point => ( new Points'(
                        (30,474),
                        (31,474),
                        (32,474),
                        (33,474),
                        (34,474),
                        (35,474),
                        (35,475),
                        (36,475),
                        (37,475),
                        (38,475),
                        (39,475)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      64 => ( Point => ( new Points'(
                        (30,472),
                        (30,473),
                        (31,473),
                        (32,473),
                        (33,473),
                        (34,473),
                        (35,473),
                        (36,473),
                        (36,474),
                        (37,474),
                        (38,474),
                        (39,474)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      65 => ( Point => ( new Points'(
                        (30,471),
                        (31,471),
                        (31,472),
                        (32,472),
                        (33,472),
                        (34,472),
                        (35,472),
                        (36,472),
                        (37,472),
                        (37,473),
                        (38,473),
                        (39,473),
                        (40,473)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      66 => ( Point => ( new Points'(
                        (31,470),
                        (32,470),
                        (32,471),
                        (33,471),
                        (34,471),
                        (35,471),
                        (36,471),
                        (37,471),
                        (38,471),
                        (38,472),
                        (39,472),
                        (40,472)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      67 => ( Point => ( new Points'(
                        (32,469),
                        (33,469),
                        (33,470),
                        (34,470),
                        (35,470),
                        (36,470),
                        (37,470),
                        (38,470),
                        (39,470),
                        (39,471),
                        (40,471),
                        (41,471)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      68 => ( Point => ( new Points'(
                        (33,468),
                        (34,468),
                        (34,469),
                        (35,469),
                        (36,469),
                        (37,469),
                        (38,469),
                        (39,469),
                        (40,469),
                        (40,470),
                        (41,470),
                        (42,470)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      69 => ( Point => ( new Points'(
                        (33,467),
                        (34,467),
                        (35,467),
                        (35,468),
                        (36,468),
                        (37,468),
                        (38,468),
                        (39,468),
                        (40,468),
                        (41,468),
                        (41,469),
                        (42,469)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      70 => ( Point => ( new Points'(
                        (34,466),
                        (35,466),
                        (36,466),
                        (36,467),
                        (37,467),
                        (38,467),
                        (39,467),
                        (40,467),
                        (41,467),
                        (42,467),
                        (42,468),
                        (43,468)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      71 => ( Point => ( new Points'(
                        (34,465),
                        (35,465),
                        (36,465),
                        (37,465),
                        (37,466),
                        (38,466),
                        (39,466),
                        (40,466),
                        (41,466),
                        (42,466),
                        (43,466),
                        (43,467)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      72 => ( Point => ( new Points'(
                        (34,463),
                        (34,464),
                        (35,464),
                        (36,464),
                        (37,464),
                        (38,464),
                        (38,465),
                        (39,465),
                        (40,465),
                        (41,465),
                        (42,465),
                        (43,465),
                        (44,465)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      73 => ( Point => ( new Points'(
                        (35,462),
                        (35,463),
                        (36,463),
                        (37,463),
                        (38,463),
                        (39,463),
                        (39,464),
                        (40,464),
                        (41,464),
                        (42,464),
                        (43,464),
                        (44,464)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      74 => ( Point => ( new Points'(
                        (35,461),
                        (36,461),
                        (36,462),
                        (37,462),
                        (38,462),
                        (39,462),
                        (40,462),
                        (40,463),
                        (41,463),
                        (42,463),
                        (43,463),
                        (44,463)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      75 => ( Point => ( new Points'(
                        (36,460),
                        (37,460),
                        (37,461),
                        (38,461),
                        (39,461),
                        (40,461),
                        (41,461),
                        (41,462),
                        (42,462),
                        (43,462),
                        (44,462),
                        (45,462)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      76 => ( Point => ( new Points'(
                        (37,459),
                        (38,459),
                        (38,460),
                        (39,460),
                        (40,460),
                        (41,460),
                        (42,460),
                        (42,461),
                        (43,461),
                        (44,461),
                        (45,461)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      77 => ( Point => ( new Points'(
                        (37,458),
                        (38,458),
                        (39,458),
                        (39,459),
                        (40,459),
                        (41,459),
                        (42,459),
                        (43,459),
                        (43,460),
                        (44,460),
                        (45,460),
                        (46,460)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      78 => ( Point => ( new Points'(
                        (38,457),
                        (39,457),
                        (40,457),
                        (40,458),
                        (41,458),
                        (42,458),
                        (43,458),
                        (44,458),
                        (44,459),
                        (45,459),
                        (46,459)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      79 => ( Point => ( new Points'(
                        (39,456),
                        (40,456),
                        (41,456),
                        (41,457),
                        (42,457),
                        (43,457),
                        (44,457),
                        (45,457),
                        (45,458),
                        (46,458),
                        (47,458)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      80 => ( Point => ( new Points'(
                        (39,455),
                        (40,455),
                        (41,455),
                        (42,455),
                        (42,456),
                        (43,456),
                        (44,456),
                        (45,456),
                        (46,456),
                        (46,457),
                        (47,457),
                        (48,457)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      81 => ( Point => ( new Points'(
                        (39,454),
                        (40,454),
                        (41,454),
                        (42,454),
                        (43,454),
                        (43,455),
                        (44,455),
                        (45,455),
                        (46,455),
                        (47,455),
                        (47,456),
                        (48,456),
                        (49,456)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      82 => ( Point => ( new Points'(
                        (40,453),
                        (41,453),
                        (42,453),
                        (43,453),
                        (44,453),
                        (44,454),
                        (45,454),
                        (46,454),
                        (47,454),
                        (48,454),
                        (48,455),
                        (49,455)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      83 => ( Point => ( new Points'(
                        (40,452),
                        (41,452),
                        (42,452),
                        (43,452),
                        (44,452),
                        (45,452),
                        (45,453),
                        (46,453),
                        (47,453),
                        (48,453),
                        (49,453),
                        (49,454),
                        (50,454)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      84 => ( Point => ( new Points'(
                        (40,451),
                        (41,451),
                        (42,451),
                        (43,451),
                        (44,451),
                        (45,451),
                        (46,451),
                        (46,452),
                        (47,452),
                        (48,452),
                        (49,452),
                        (50,452),
                        (50,453)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      85 => ( Point => ( new Points'(
                        (41,450),
                        (42,450),
                        (43,450),
                        (44,450),
                        (45,450),
                        (46,450),
                        (47,450),
                        (47,451),
                        (48,451),
                        (49,451),
                        (50,451)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      86 => ( Point => ( new Points'(
                        (42,449),
                        (43,449),
                        (44,449),
                        (45,449),
                        (46,449),
                        (47,449),
                        (48,449),
                        (48,450),
                        (49,450),
                        (50,450),
                        (51,450)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      87 => ( Point => ( new Points'(
                        (42,447),
                        (42,448),
                        (43,448),
                        (44,448),
                        (45,448),
                        (46,448),
                        (47,448),
                        (48,448),
                        (49,448),
                        (49,449),
                        (50,449),
                        (51,449)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      88 => ( Point => ( new Points'(
                        (43,446),
                        (43,447),
                        (44,447),
                        (45,447),
                        (46,447),
                        (47,447),
                        (48,447),
                        (49,447),
                        (50,447),
                        (50,448),
                        (51,448),
                        (52,448)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      89 => ( Point => ( new Points'(
                        (43,445),
                        (44,445),
                        (44,446),
                        (45,446),
                        (46,446),
                        (47,446),
                        (48,446),
                        (49,446),
                        (50,446),
                        (51,446),
                        (51,447),
                        (52,447)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      90 => ( Point => ( new Points'(
                        (44,444),
                        (45,444),
                        (45,445),
                        (46,445),
                        (47,445),
                        (48,445),
                        (49,445),
                        (50,445),
                        (51,445),
                        (52,445),
                        (52,446)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      91 => ( Point => ( new Points'(
                        (45,443),
                        (46,443),
                        (46,444),
                        (47,444),
                        (48,444),
                        (49,444),
                        (50,444),
                        (51,444),
                        (52,444),
                        (53,444)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      92 => ( Point => ( new Points'(
                        (45,442),
                        (46,442),
                        (47,442),
                        (47,443),
                        (48,443),
                        (49,443),
                        (50,443),
                        (51,443),
                        (52,443),
                        (53,443)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      93 => ( Point => ( new Points'(
                        (45,440),
                        (45,441),
                        (46,441),
                        (47,441),
                        (48,441),
                        (48,442),
                        (49,442),
                        (50,442),
                        (51,442),
                        (52,442),
                        (53,442),
                        (54,442)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      94 => ( Point => ( new Points'(
                        (46,439),
                        (46,440),
                        (47,440),
                        (48,440),
                        (49,440),
                        (49,441),
                        (50,441),
                        (51,441),
                        (52,441),
                        (53,441),
                        (54,441),
                        (55,441)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      95 => ( Point => ( new Points'(
                        (46,438),
                        (47,438),
                        (47,439),
                        (48,439),
                        (49,439),
                        (50,439),
                        (50,440),
                        (51,440),
                        (52,440),
                        (53,440),
                        (54,440),
                        (55,440)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      96 => ( Point => ( new Points'(
                        (47,437),
                        (48,437),
                        (48,438),
                        (49,438),
                        (50,438),
                        (51,438),
                        (51,439),
                        (52,439),
                        (53,439),
                        (54,439),
                        (55,439),
                        (56,439)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      97 => ( Point => ( new Points'(
                        (47,436),
                        (48,436),
                        (49,436),
                        (49,437),
                        (50,437),
                        (51,437),
                        (52,437),
                        (52,438),
                        (53,438),
                        (54,438),
                        (55,438),
                        (56,438)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      98 => ( Point => ( new Points'(
                        (48,435),
                        (49,435),
                        (50,435),
                        (50,436),
                        (51,436),
                        (52,436),
                        (53,436),
                        (53,437),
                        (54,437),
                        (55,437),
                        (56,437),
                        (57,437)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      99 => ( Point => ( new Points'(
                        (48,434),
                        (49,434),
                        (50,434),
                        (51,434),
                        (51,435),
                        (52,435),
                        (53,435),
                        (54,435),
                        (54,436),
                        (55,436),
                        (56,436),
                        (57,436)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      100 => ( Point => ( new Points'(
                         (49,433),
                         (50,433),
                         (51,433),
                         (52,433),
                         (52,434),
                         (53,434),
                         (54,434),
                         (55,434),
                         (55,435),
                         (56,435),
                         (57,435),
                         (58,435)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      101 => ( Point => ( new Points'(
                         (50,432),
                         (51,432),
                         (52,432),
                         (53,432),
                         (53,433),
                         (54,433),
                         (55,433),
                         (56,433),
                         (56,434),
                         (57,434),
                         (58,434),
                         (59,434)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      102 => ( Point => ( new Points'(
                         (50,431),
                         (51,431),
                         (52,431),
                         (53,431),
                         (54,431),
                         (54,432),
                         (55,432),
                         (56,432),
                         (57,432),
                         (57,433),
                         (58,433),
                         (59,433)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      103 => ( Point => ( new Points'(
                         (51,430),
                         (52,430),
                         (53,430),
                         (54,430),
                         (55,430),
                         (55,431),
                         (56,431),
                         (57,431),
                         (58,431),
                         (58,432),
                         (59,432),
                         (60,432)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      104 => ( Point => ( new Points'(
                         (51,429),
                         (52,429),
                         (53,429),
                         (54,429),
                         (55,429),
                         (56,429),
                         (56,430),
                         (57,430),
                         (58,430),
                         (59,430),
                         (59,431),
                         (60,431),
                         (61,431)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      105 => ( Point => ( new Points'(
                         (51,428),
                         (52,428),
                         (53,428),
                         (54,428),
                         (55,428),
                         (56,428),
                         (57,428),
                         (57,429),
                         (58,429),
                         (59,429),
                         (60,429),
                         (60,430),
                         (61,430)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      106 => ( Point => ( new Points'(
                         (52,427),
                         (53,427),
                         (54,427),
                         (55,427),
                         (56,427),
                         (57,427),
                         (58,427),
                         (58,428),
                         (59,428),
                         (60,428),
                         (61,428),
                         (61,429)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      107 => ( Point => ( new Points'(
                         (52,426),
                         (53,426),
                         (54,426),
                         (55,426),
                         (56,426),
                         (57,426),
                         (58,426),
                         (59,426),
                         (59,427),
                         (60,427),
                         (61,427),
                         (62,427)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      108 => ( Point => ( new Points'(
                         (53,424),
                         (53,425),
                         (54,425),
                         (55,425),
                         (56,425),
                         (57,425),
                         (58,425),
                         (59,425),
                         (60,425),
                         (60,426),
                         (61,426),
                         (62,426)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      109 => ( Point => ( new Points'(
                         (54,423),
                         (54,424),
                         (55,424),
                         (56,424),
                         (57,424),
                         (58,424),
                         (59,424),
                         (60,424),
                         (61,424),
                         (61,425),
                         (62,425),
                         (63,425)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      110 => ( Point => ( new Points'(
                         (54,422),
                         (55,422),
                         (55,423),
                         (56,423),
                         (57,423),
                         (58,423),
                         (59,423),
                         (60,423),
                         (61,423),
                         (62,423),
                         (62,424),
                         (63,424),
                         (64,424)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      111 => ( Point => ( new Points'(
                         (55,421),
                         (56,421),
                         (56,422),
                         (57,422),
                         (58,422),
                         (59,422),
                         (60,422),
                         (61,422),
                         (62,422),
                         (63,422),
                         (63,423),
                         (64,423)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      112 => ( Point => ( new Points'(
                         (55,420),
                         (56,420),
                         (57,420),
                         (57,421),
                         (58,421),
                         (59,421),
                         (60,421),
                         (61,421),
                         (62,421),
                         (63,421),
                         (64,421),
                         (64,422)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      113 => ( Point => ( new Points'(
                         (56,419),
                         (57,419),
                         (58,419),
                         (58,420),
                         (59,420),
                         (60,420),
                         (61,420),
                         (62,420),
                         (63,420),
                         (64,420),
                         (65,420)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      114 => ( Point => ( new Points'(
                         (56,418),
                         (57,418),
                         (58,418),
                         (59,418),
                         (59,419),
                         (60,419),
                         (61,419),
                         (62,419),
                         (63,419),
                         (64,419),
                         (65,419)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      115 => ( Point => ( new Points'(
                         (56,416),
                         (56,417),
                         (57,417),
                         (58,417),
                         (59,417),
                         (60,417),
                         (60,418),
                         (61,418),
                         (62,418),
                         (63,418),
                         (64,418),
                         (65,418),
                         (66,418)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      116 => ( Point => ( new Points'(
                         (57,415),
                         (57,416),
                         (58,416),
                         (59,416),
                         (60,416),
                         (61,416),
                         (61,417),
                         (62,417),
                         (63,417),
                         (64,417),
                         (65,417),
                         (66,417)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      117 => ( Point => ( new Points'(
                         (58,414),
                         (58,415),
                         (59,415),
                         (60,415),
                         (61,415),
                         (62,415),
                         (62,416),
                         (63,416),
                         (64,416),
                         (65,416),
                         (66,416)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      118 => ( Point => ( new Points'(
                         (58,413),
                         (59,413),
                         (59,414),
                         (60,414),
                         (61,414),
                         (62,414),
                         (63,414),
                         (63,415),
                         (64,415),
                         (65,415),
                         (66,415),
                         (67,415)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      119 => ( Point => ( new Points'(
                         (58,412),
                         (59,412),
                         (60,412),
                         (60,413),
                         (61,413),
                         (62,413),
                         (63,413),
                         (64,413),
                         (64,414),
                         (65,414),
                         (66,414),
                         (67,414)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      120 => ( Point => ( new Points'(
                         (59,411),
                         (60,411),
                         (61,411),
                         (61,412),
                         (62,412),
                         (63,412),
                         (64,412),
                         (65,412),
                         (65,413),
                         (66,413),
                         (67,413)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      121 => ( Point => ( new Points'(
                         (59,410),
                         (60,410),
                         (61,410),
                         (62,410),
                         (62,411),
                         (63,411),
                         (64,411),
                         (65,411),
                         (66,411),
                         (66,412),
                         (67,412),
                         (68,412)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      122 => ( Point => ( new Points'(
                         (60,409),
                         (61,409),
                         (62,409),
                         (63,409),
                         (63,410),
                         (64,410),
                         (65,410),
                         (66,410),
                         (67,410),
                         (67,411),
                         (68,411),
                         (69,411)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      123 => ( Point => ( new Points'(
                         (60,408),
                         (61,408),
                         (62,408),
                         (63,408),
                         (64,408),
                         (64,409),
                         (65,409),
                         (66,409),
                         (67,409),
                         (68,409),
                         (68,410),
                         (69,410)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      124 => ( Point => ( new Points'(
                         (61,407),
                         (62,407),
                         (63,407),
                         (64,407),
                         (65,407),
                         (65,408),
                         (66,408),
                         (67,408),
                         (68,408),
                         (69,408),
                         (69,409)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      125 => ( Point => ( new Points'(
                         (61,406),
                         (62,406),
                         (63,406),
                         (64,406),
                         (65,406),
                         (66,406),
                         (66,407),
                         (67,407),
                         (68,407),
                         (69,407),
                         (70,407)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      126 => ( Point => ( new Points'(
                         (62,405),
                         (63,405),
                         (64,405),
                         (65,405),
                         (66,405),
                         (67,405),
                         (67,406),
                         (68,406),
                         (69,406),
                         (70,406)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      127 => ( Point => ( new Points'(
                         (62,403),
                         (62,404),
                         (63,404),
                         (64,404),
                         (65,404),
                         (66,404),
                         (67,404),
                         (68,404),
                         (68,405),
                         (69,405),
                         (70,405),
                         (71,405)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      128 => ( Point => ( new Points'(
                         (63,402),
                         (63,403),
                         (64,403),
                         (65,403),
                         (66,403),
                         (67,403),
                         (68,403),
                         (69,403),
                         (69,404),
                         (70,404),
                         (71,404),
                         (72,404)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      129 => ( Point => ( new Points'(
                         (63,401),
                         (64,401),
                         (64,402),
                         (65,402),
                         (66,402),
                         (67,402),
                         (68,402),
                         (69,402),
                         (70,402),
                         (70,403),
                         (71,403),
                         (72,403),
                         (73,403)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      130 => ( Point => ( new Points'(
                         (64,400),
                         (65,400),
                         (65,401),
                         (66,401),
                         (67,401),
                         (68,401),
                         (69,401),
                         (70,401),
                         (71,401),
                         (71,402),
                         (72,402),
                         (73,402),
                         (74,402)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      131 => ( Point => ( new Points'(
                         (64,399),
                         (65,399),
                         (66,399),
                         (66,400),
                         (67,400),
                         (68,400),
                         (69,400),
                         (70,400),
                         (71,400),
                         (72,400),
                         (72,401),
                         (73,401),
                         (74,401)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      132 => ( Point => ( new Points'(
                         (65,398),
                         (66,398),
                         (67,398),
                         (67,399),
                         (68,399),
                         (69,399),
                         (70,399),
                         (71,399),
                         (72,399),
                         (73,399),
                         (73,400),
                         (74,400),
                         (75,400)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      133 => ( Point => ( new Points'(
                         (65,397),
                         (66,397),
                         (67,397),
                         (68,397),
                         (68,398),
                         (69,398),
                         (70,398),
                         (71,398),
                         (72,398),
                         (73,398),
                         (74,398),
                         (74,399),
                         (75,399)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      134 => ( Point => ( new Points'(
                         (66,396),
                         (67,396),
                         (68,396),
                         (69,396),
                         (69,397),
                         (70,397),
                         (71,397),
                         (72,397),
                         (73,397),
                         (74,397),
                         (75,397),
                         (75,398)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      135 => ( Point => ( new Points'(
                         (66,395),
                         (67,395),
                         (68,395),
                         (69,395),
                         (70,395),
                         (70,396),
                         (71,396),
                         (72,396),
                         (73,396),
                         (74,396),
                         (75,396),
                         (76,396)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      136 => ( Point => ( new Points'(
                         (67,394),
                         (68,394),
                         (69,394),
                         (70,394),
                         (71,394),
                         (71,395),
                         (72,395),
                         (73,395),
                         (74,395),
                         (75,395),
                         (76,395),
                         (77,395)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      137 => ( Point => ( new Points'(
                         (67,392),
                         (67,393),
                         (68,393),
                         (69,393),
                         (70,393),
                         (71,393),
                         (72,393),
                         (72,394),
                         (73,394),
                         (74,394),
                         (75,394),
                         (76,394),
                         (77,394)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      138 => ( Point => ( new Points'(
                         (68,391),
                         (68,392),
                         (69,392),
                         (70,392),
                         (71,392),
                         (72,392),
                         (73,392),
                         (73,393),
                         (74,393),
                         (75,393),
                         (76,393),
                         (77,393),
                         (78,393)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      139 => ( Point => ( new Points'(
                         (69,390),
                         (69,391),
                         (70,391),
                         (71,391),
                         (72,391),
                         (73,391),
                         (74,391),
                         (74,392),
                         (75,392),
                         (76,392),
                         (77,392),
                         (78,392),
                         (79,392)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      140 => ( Point => ( new Points'(
                         (69,389),
                         (70,389),
                         (70,390),
                         (71,390),
                         (72,390),
                         (73,390),
                         (74,390),
                         (75,390),
                         (75,391),
                         (76,391),
                         (77,391),
                         (78,391),
                         (79,391)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      141 => ( Point => ( new Points'(
                         (70,388),
                         (71,388),
                         (71,389),
                         (72,389),
                         (73,389),
                         (74,389),
                         (75,389),
                         (76,389),
                         (76,390),
                         (77,390),
                         (78,390),
                         (79,390),
                         (80,390)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      142 => ( Point => ( new Points'(
                         (71,387),
                         (72,387),
                         (72,388),
                         (73,388),
                         (74,388),
                         (75,388),
                         (76,388),
                         (77,388),
                         (77,389),
                         (78,389),
                         (79,389),
                         (80,389)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      143 => ( Point => ( new Points'(
                         (71,386),
                         (72,386),
                         (73,386),
                         (73,387),
                         (74,387),
                         (75,387),
                         (76,387),
                         (77,387),
                         (78,387),
                         (78,388),
                         (79,388),
                         (80,388),
                         (81,388)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      144 => ( Point => ( new Points'(
                         (71,385),
                         (72,385),
                         (73,385),
                         (74,385),
                         (74,386),
                         (75,386),
                         (76,386),
                         (77,386),
                         (78,386),
                         (79,386),
                         (79,387),
                         (80,387),
                         (81,387)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      145 => ( Point => ( new Points'(
                         (72,384),
                         (73,384),
                         (74,384),
                         (75,384),
                         (75,385),
                         (76,385),
                         (77,385),
                         (78,385),
                         (79,385),
                         (80,385),
                         (80,386),
                         (81,386),
                         (82,386)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      146 => ( Point => ( new Points'(
                         (73,383),
                         (74,383),
                         (75,383),
                         (76,383),
                         (76,384),
                         (77,384),
                         (78,384),
                         (79,384),
                         (80,384),
                         (81,384),
                         (81,385),
                         (82,385)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      147 => ( Point => ( new Points'(
                         (73,382),
                         (74,382),
                         (75,382),
                         (76,382),
                         (77,382),
                         (77,383),
                         (78,383),
                         (79,383),
                         (80,383),
                         (81,383),
                         (82,383),
                         (82,384)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      148 => ( Point => ( new Points'(
                         (74,381),
                         (75,381),
                         (76,381),
                         (77,381),
                         (78,381),
                         (78,382),
                         (79,382),
                         (80,382),
                         (81,382),
                         (82,382),
                         (83,382)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      149 => ( Point => ( new Points'(
                         (74,380),
                         (75,380),
                         (76,380),
                         (77,380),
                         (78,380),
                         (79,380),
                         (79,381),
                         (80,381),
                         (81,381),
                         (82,381),
                         (83,381)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      150 => ( Point => ( new Points'(
                         (74,379),
                         (75,379),
                         (76,379),
                         (77,379),
                         (78,379),
                         (79,379),
                         (80,379),
                         (80,380),
                         (81,380),
                         (82,380),
                         (83,380),
                         (84,380)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      151 => ( Point => ( new Points'(
                         (75,377),
                         (75,378),
                         (76,378),
                         (77,378),
                         (78,378),
                         (79,378),
                         (80,378),
                         (81,378),
                         (81,379),
                         (82,379),
                         (83,379),
                         (84,379),
                         (85,379)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      152 => ( Point => ( new Points'(
                         (76,376),
                         (76,377),
                         (77,377),
                         (78,377),
                         (79,377),
                         (80,377),
                         (81,377),
                         (82,377),
                         (82,378),
                         (83,378),
                         (84,378),
                         (85,378)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      153 => ( Point => ( new Points'(
                         (77,375),
                         (77,376),
                         (78,376),
                         (79,376),
                         (80,376),
                         (81,376),
                         (82,376),
                         (83,376),
                         (83,377),
                         (84,377),
                         (85,377),
                         (86,377)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      154 => ( Point => ( new Points'(
                         (77,374),
                         (78,374),
                         (78,375),
                         (79,375),
                         (80,375),
                         (81,375),
                         (82,375),
                         (83,375),
                         (84,375),
                         (84,376),
                         (85,376),
                         (86,376),
                         (87,376)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      155 => ( Point => ( new Points'(
                         (77,373),
                         (78,373),
                         (79,373),
                         (79,374),
                         (80,374),
                         (81,374),
                         (82,374),
                         (83,374),
                         (84,374),
                         (85,374),
                         (85,375),
                         (86,375),
                         (87,375)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      156 => ( Point => ( new Points'(
                         (78,372),
                         (79,372),
                         (80,372),
                         (80,373),
                         (81,373),
                         (82,373),
                         (83,373),
                         (84,373),
                         (85,373),
                         (86,373),
                         (86,374),
                         (87,374)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      157 => ( Point => ( new Points'(
                         (78,371),
                         (79,371),
                         (80,371),
                         (81,371),
                         (81,372),
                         (82,372),
                         (83,372),
                         (84,372),
                         (85,372),
                         (86,372),
                         (87,372),
                         (87,373)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      158 => ( Point => ( new Points'(
                         (79,370),
                         (80,370),
                         (81,370),
                         (82,370),
                         (82,371),
                         (83,371),
                         (84,371),
                         (85,371),
                         (86,371),
                         (87,371)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      159 => ( Point => ( new Points'(
                         (79,368),
                         (79,369),
                         (80,369),
                         (81,369),
                         (82,369),
                         (83,369),
                         (83,370),
                         (84,370),
                         (85,370),
                         (86,370),
                         (87,370)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      160 => ( Point => ( new Points'(
                         (80,367),
                         (80,368),
                         (81,368),
                         (82,368),
                         (83,368),
                         (84,368),
                         (84,369),
                         (85,369),
                         (86,369),
                         (87,369),
                         (88,369)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      161 => ( Point => ( new Points'(
                         (80,366),
                         (81,366),
                         (81,367),
                         (82,367),
                         (83,367),
                         (84,367),
                         (85,367),
                         (85,368),
                         (86,368),
                         (87,368),
                         (88,368)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      162 => ( Point => ( new Points'(
                         (81,365),
                         (82,365),
                         (82,366),
                         (83,366),
                         (84,366),
                         (85,366),
                         (86,366),
                         (86,367),
                         (87,367),
                         (88,367),
                         (89,367)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      163 => ( Point => ( new Points'(
                         (82,364),
                         (83,364),
                         (83,365),
                         (84,365),
                         (85,365),
                         (86,365),
                         (87,365),
                         (87,366),
                         (88,366),
                         (89,366)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      164 => ( Point => ( new Points'(
                         (82,363),
                         (83,363),
                         (84,363),
                         (84,364),
                         (85,364),
                         (86,364),
                         (87,364),
                         (88,364),
                         (88,365),
                         (89,365)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      165 => ( Point => ( new Points'(
                         (83,362),
                         (84,362),
                         (85,362),
                         (85,363),
                         (86,363),
                         (87,363),
                         (88,363),
                         (89,363),
                         (89,364),
                         (90,364)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      166 => ( Point => ( new Points'(
                         (83,360),
                         (83,361),
                         (84,361),
                         (85,361),
                         (86,361),
                         (86,362),
                         (87,362),
                         (88,362),
                         (89,362),
                         (90,362),
                         (90,363),
                         (91,363)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      167 => ( Point => ( new Points'(
                         (84,359),
                         (84,360),
                         (85,360),
                         (86,360),
                         (87,360),
                         (87,361),
                         (88,361),
                         (89,361),
                         (90,361),
                         (91,361),
                         (91,362)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      168 => ( Point => ( new Points'(
                         (85,358),
                         (85,359),
                         (86,359),
                         (87,359),
                         (88,359),
                         (88,360),
                         (89,360),
                         (90,360),
                         (91,360)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      169 => ( Point => ( new Points'(
                         (85,357),
                         (86,357),
                         (86,358),
                         (87,358),
                         (88,358),
                         (89,358),
                         (89,359),
                         (90,359),
                         (91,359),
                         (92,359)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      170 => ( Point => ( new Points'(
                         (86,356),
                         (87,356),
                         (87,357),
                         (88,357),
                         (89,357),
                         (90,357),
                         (90,358),
                         (91,358),
                         (92,358),
                         (93,358)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      171 => ( Point => ( new Points'(
                         (86,355),
                         (87,355),
                         (88,355),
                         (88,356),
                         (89,356),
                         (90,356),
                         (91,356),
                         (91,357),
                         (92,357),
                         (93,357)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      172 => ( Point => ( new Points'(
                         (86,354),
                         (87,354),
                         (88,354),
                         (89,354),
                         (89,355),
                         (90,355),
                         (91,355),
                         (92,355),
                         (92,356),
                         (93,356),
                         (94,356)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      173 => ( Point => ( new Points'(
                         (87,353),
                         (88,353),
                         (89,353),
                         (90,353),
                         (90,354),
                         (91,354),
                         (92,354),
                         (93,354),
                         (93,355),
                         (94,355)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      174 => ( Point => ( new Points'(
                         (87,352),
                         (88,352),
                         (89,352),
                         (90,352),
                         (91,352),
                         (91,353),
                         (92,353),
                         (93,353),
                         (94,353),
                         (94,354),
                         (95,354)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      175 => ( Point => ( new Points'(
                         (88,351),
                         (89,351),
                         (90,351),
                         (91,351),
                         (92,351),
                         (92,352),
                         (93,352),
                         (94,352),
                         (95,352),
                         (95,353)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      176 => ( Point => ( new Points'(
                         (89,350),
                         (90,350),
                         (91,350),
                         (92,350),
                         (93,350),
                         (93,351),
                         (94,351),
                         (95,351),
                         (96,351)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      177 => ( Point => ( new Points'(
                         (89,348),
                         (89,349),
                         (90,349),
                         (91,349),
                         (92,349),
                         (93,349),
                         (94,349),
                         (94,350),
                         (95,350),
                         (96,350),
                         (97,350)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      178 => ( Point => ( new Points'(
                         (90,347),
                         (90,348),
                         (91,348),
                         (92,348),
                         (93,348),
                         (94,348),
                         (95,348),
                         (95,349),
                         (96,349),
                         (97,349),
                         (98,349)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      179 => ( Point => ( new Points'(
                         (91,346),
                         (91,347),
                         (92,347),
                         (93,347),
                         (94,347),
                         (95,347),
                         (96,347),
                         (96,348),
                         (97,348),
                         (98,348)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      180 => ( Point => ( new Points'(
                         (91,345),
                         (92,345),
                         (92,346),
                         (93,346),
                         (94,346),
                         (95,346),
                         (96,346),
                         (97,346),
                         (97,347),
                         (98,347)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      181 => ( Point => ( new Points'(
                         (91,344),
                         (92,344),
                         (93,344),
                         (93,345),
                         (94,345),
                         (95,345),
                         (96,345),
                         (97,345),
                         (98,345),
                         (98,346),
                         (99,346)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      182 => ( Point => ( new Points'(
                         (92,343),
                         (93,343),
                         (94,343),
                         (94,344),
                         (95,344),
                         (96,344),
                         (97,344),
                         (98,344),
                         (99,344),
                         (99,345)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      183 => ( Point => ( new Points'(
                         (93,342),
                         (94,342),
                         (95,342),
                         (95,343),
                         (96,343),
                         (97,343),
                         (98,343),
                         (99,343),
                         (100,343)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      184 => ( Point => ( new Points'(
                         (93,340),
                         (93,341),
                         (94,341),
                         (95,341),
                         (96,341),
                         (96,342),
                         (97,342),
                         (98,342),
                         (99,342),
                         (100,342),
                         (101,342)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      185 => ( Point => ( new Points'(
                         (93,339),
                         (94,339),
                         (94,340),
                         (95,340),
                         (96,340),
                         (97,340),
                         (97,341),
                         (98,341),
                         (99,341),
                         (100,341),
                         (101,341)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      186 => ( Point => ( new Points'(
                         (94,338),
                         (95,338),
                         (95,339),
                         (96,339),
                         (97,339),
                         (98,339),
                         (98,340),
                         (99,340),
                         (100,340),
                         (101,340),
                         (102,340)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      187 => ( Point => ( new Points'(
                         (95,337),
                         (96,337),
                         (96,338),
                         (97,338),
                         (98,338),
                         (99,338),
                         (99,339),
                         (100,339),
                         (101,339),
                         (102,339)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      188 => ( Point => ( new Points'(
                         (95,336),
                         (96,336),
                         (97,336),
                         (97,337),
                         (98,337),
                         (99,337),
                         (100,337),
                         (100,338),
                         (101,338),
                         (102,338),
                         (103,338)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      189 => ( Point => ( new Points'(
                         (96,335),
                         (97,335),
                         (98,335),
                         (98,336),
                         (99,336),
                         (100,336),
                         (101,336),
                         (101,337),
                         (102,337),
                         (103,337)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      190 => ( Point => ( new Points'(
                         (96,334),
                         (97,334),
                         (98,334),
                         (99,334),
                         (99,335),
                         (100,335),
                         (101,335),
                         (102,335),
                         (102,336),
                         (103,336),
                         (104,336)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      191 => ( Point => ( new Points'(
                         (97,333),
                         (98,333),
                         (99,333),
                         (100,333),
                         (100,334),
                         (101,334),
                         (102,334),
                         (103,334),
                         (103,335),
                         (104,335),
                         (105,335)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      192 => ( Point => ( new Points'(
                         (97,332),
                         (98,332),
                         (99,332),
                         (100,332),
                         (101,332),
                         (101,333),
                         (102,333),
                         (103,333),
                         (104,333),
                         (104,334),
                         (105,334)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      193 => ( Point => ( new Points'(
                         (98,331),
                         (99,331),
                         (100,331),
                         (101,331),
                         (102,331),
                         (102,332),
                         (103,332),
                         (104,332),
                         (105,332),
                         (105,333)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      194 => ( Point => ( new Points'(
                         (98,330),
                         (99,330),
                         (100,330),
                         (101,330),
                         (102,330),
                         (103,330),
                         (103,331),
                         (104,331),
                         (105,331),
                         (106,331)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      195 => ( Point => ( new Points'(
                         (99,328),
                         (99,329),
                         (100,329),
                         (101,329),
                         (102,329),
                         (103,329),
                         (104,329),
                         (104,330),
                         (105,330),
                         (106,330),
                         (107,330)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      196 => ( Point => ( new Points'(
                         (100,327),
                         (100,328),
                         (101,328),
                         (102,328),
                         (103,328),
                         (104,328),
                         (105,328),
                         (105,329),
                         (106,329),
                         (107,329)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      197 => ( Point => ( new Points'(
                         (100,326),
                         (101,326),
                         (101,327),
                         (102,327),
                         (103,327),
                         (104,327),
                         (105,327),
                         (106,327),
                         (106,328),
                         (107,328)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      198 => ( Point => ( new Points'(
                         (101,325),
                         (102,325),
                         (102,326),
                         (103,326),
                         (104,326),
                         (105,326),
                         (106,326),
                         (107,326),
                         (107,327),
                         (108,327)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      199 => ( Point => ( new Points'(
                         (101,324),
                         (102,324),
                         (103,324),
                         (103,325),
                         (104,325),
                         (105,325),
                         (106,325),
                         (107,325),
                         (108,325),
                         (108,326),
                         (109,326)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      200 => ( Point => ( new Points'(
                         (102,323),
                         (103,323),
                         (104,323),
                         (104,324),
                         (105,324),
                         (106,324),
                         (107,324),
                         (108,324),
                         (109,324),
                         (109,325),
                         (110,325)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      201 => ( Point => ( new Points'(
                         (102,322),
                         (103,322),
                         (104,322),
                         (105,322),
                         (105,323),
                         (106,323),
                         (107,323),
                         (108,323),
                         (109,323),
                         (110,323),
                         (110,324)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      202 => ( Point => ( new Points'(
                         (103,320),
                         (103,321),
                         (104,321),
                         (105,321),
                         (106,321),
                         (106,322),
                         (107,322),
                         (108,322),
                         (109,322),
                         (110,322),
                         (111,322)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      203 => ( Point => ( new Points'(
                         (103,319),
                         (104,319),
                         (104,320),
                         (105,320),
                         (106,320),
                         (107,320),
                         (107,321),
                         (108,321),
                         (109,321),
                         (110,321),
                         (111,321)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      204 => ( Point => ( new Points'(
                         (104,318),
                         (105,318),
                         (105,319),
                         (106,319),
                         (107,319),
                         (108,319),
                         (108,320),
                         (109,320),
                         (110,320),
                         (111,320)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      205 => ( Point => ( new Points'(
                         (105,317),
                         (106,317),
                         (106,318),
                         (107,318),
                         (108,318),
                         (109,318),
                         (109,319),
                         (110,319),
                         (111,319),
                         (112,319)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      206 => ( Point => ( new Points'(
                         (105,316),
                         (106,316),
                         (107,316),
                         (107,317),
                         (108,317),
                         (109,317),
                         (110,317),
                         (110,318),
                         (111,318),
                         (112,318)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      207 => ( Point => ( new Points'(
                         (105,315),
                         (106,315),
                         (107,315),
                         (108,315),
                         (108,316),
                         (109,316),
                         (110,316),
                         (111,316),
                         (111,317),
                         (112,317),
                         (113,317)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      208 => ( Point => ( new Points'(
                         (106,314),
                         (107,314),
                         (108,314),
                         (109,315),
                         (109,315),
                         (110,315),
                         (111,315),
                         (112,315),
                         (112,316),
                         (113,316)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      209 => ( Point => ( new Points'(
                         (106,313),
                         (107,313),
                         (108,313),
                         (109,313),
                         (109,314),
                         (110,314),
                         (111,314),
                         (112,314),
                         (113,314),
                         (113,315),
                         (114,315)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      210 => ( Point => ( new Points'(
                         (107,312),
                         (108,312),
                         (109,312),
                         (110,312),
                         (110,313),
                         (111,313),
                         (112,313),
                         (113,313),
                         (114,313),
                         (114,314)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      211 => ( Point => ( new Points'(
                         (108,311),
                         (109,311),
                         (110,311),
                         (111,311),
                         (111,312),
                         (112,312),
                         (113,312),
                         (114,312)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      212 => ( Point => ( new Points'(
                         (108,310),
                         (109,310),
                         (110,310),
                         (111,310),
                         (112,310),
                         (112,311),
                         (113,311),
                         (114,311),
                         (115,311)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      213 => ( Point => ( new Points'(
                         (109,308),
                         (109,309),
                         (110,309),
                         (111,309),
                         (112,309),
                         (113,309),
                         (113,310),
                         (114,310),
                         (115,310),
                         (116,310)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      214 => ( Point => ( new Points'(
                         (110,307),
                         (110,308),
                         (111,308),
                         (112,308),
                         (113,308),
                         (114,308),
                         (114,309),
                         (115,309),
                         (116,309)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      215 => ( Point => ( new Points'(
                         (110,306),
                         (111,306),
                         (111,307),
                         (112,307),
                         (113,307),
                         (114,307),
                         (115,307),
                         (115,308),
                         (116,308),
                         (117,308)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      216 => ( Point => ( new Points'(
                         (110,305),
                         (111,305),
                         (112,305),
                         (112,306),
                         (113,306),
                         (114,306),
                         (115,306),
                         (116,306),
                         (116,307),
                         (117,307)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      217 => ( Point => ( new Points'(
                         (111,304),
                         (112,304),
                         (113,304),
                         (113,305),
                         (114,305),
                         (115,305),
                         (116,305),
                         (117,305),
                         (117,306),
                         (118,306)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      218 => ( Point => ( new Points'(
                         (111,303),
                         (112,303),
                         (113,303),
                         (114,303),
                         (114,304),
                         (115,304),
                         (116,304),
                         (117,304),
                         (118,304),
                         (118,305),
                         (119,305)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      219 => ( Point => ( new Points'(
                         (112,302),
                         (113,302),
                         (114,302),
                         (115,302),
                         (115,303),
                         (116,303),
                         (117,303),
                         (118,303),
                         (119,303),
                         (119,304)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      220 => ( Point => ( new Points'(
                         (113,301),
                         (114,301),
                         (115,301),
                         (116,301),
                         (116,302),
                         (117,302),
                         (118,302),
                         (119,302),
                         (120,302)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      221 => ( Point => ( new Points'(
                         (113,300),
                         (114,300),
                         (115,300),
                         (116,300),
                         (117,300),
                         (117,301),
                         (118,301),
                         (119,301),
                         (120,301),
                         (121,301)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      222 => ( Point => ( new Points'(
                         (114,298),
                         (114,299),
                         (115,299),
                         (116,299),
                         (117,299),
                         (118,299),
                         (118,300),
                         (119,300),
                         (120,300),
                         (121,300)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      223 => ( Point => ( new Points'(
                         (114,297),
                         (115,297),
                         (115,298),
                         (116,298),
                         (117,298),
                         (118,298),
                         (119,298),
                         (119,299),
                         (120,299),
                         (121,299),
                         (122,299)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      224 => ( Point => ( new Points'(
                         (115,296),
                         (116,296),
                         (116,297),
                         (117,297),
                         (118,297),
                         (119,297),
                         (120,297),
                         (120,298),
                         (121,298),
                         (122,298)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      225 => ( Point => ( new Points'(
                         (116,295),
                         (117,295),
                         (117,296),
                         (118,296),
                         (119,296),
                         (120,296),
                         (121,296),
                         (121,297),
                         (122,297),
                         (123,297)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      226 => ( Point => ( new Points'(
                         (116,294),
                         (117,294),
                         (118,294),
                         (118,295),
                         (119,295),
                         (120,295),
                         (121,295),
                         (122,295),
                         (122,296),
                         (123,296)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      227 => ( Point => ( new Points'(
                         (117,293),
                         (118,293),
                         (119,293),
                         (119,294),
                         (120,294),
                         (121,294),
                         (122,294),
                         (123,294),
                         (123,295)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      228 => ( Point => ( new Points'(
                         (117,292),
                         (118,292),
                         (119,292),
                         (120,292),
                         (120,293),
                         (121,293),
                         (122,293),
                         (123,293),
                         (124,293)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      229 => ( Point => ( new Points'(
                         (117,291),
                         (118,291),
                         (119,291),
                         (120,291),
                         (121,291),
                         (121,292),
                         (122,292),
                         (123,292),
                         (124,292),
                         (125,292)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      230 => ( Point => ( new Points'(
                         (118,290),
                         (119,290),
                         (120,290),
                         (121,290),
                         (122,290),
                         (122,291),
                         (123,291),
                         (124,291),
                         (125,291)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      231 => ( Point => ( new Points'(
                         (119,289),
                         (120,289),
                         (121,289),
                         (122,289),
                         (123,289),
                         (123,290),
                         (124,290),
                         (125,290)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      232 => ( Point => ( new Points'(
                         (119,287),
                         (119,288),
                         (120,288),
                         (121,288),
                         (122,288),
                         (123,288),
                         (124,288),
                         (124,289),
                         (125,289),
                         (126,289)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      233 => ( Point => ( new Points'(
                         (120,286),
                         (120,287),
                         (121,287),
                         (122,287),
                         (123,287),
                         (124,287),
                         (125,287),
                         (125,288),
                         (126,288)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      234 => ( Point => ( new Points'(
                         (121,285),
                         (121,286),
                         (122,286),
                         (123,286),
                         (124,286),
                         (125,286),
                         (126,286),
                         (126,287),
                         (127,287)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      235 => ( Point => ( new Points'(
                         (121,284),
                         (122,284),
                         (122,285),
                         (123,285),
                         (124,285),
                         (125,285),
                         (126,285),
                         (127,285),
                         (128,285)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      236 => ( Point => ( new Points'(
                         (122,283),
                         (123,283),
                         (123,284),
                         (124,284),
                         (125,284),
                         (126,284),
                         (127,284),
                         (128,284)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      237 => ( Point => ( new Points'(
                         (123,282),
                         (124,282),
                         (124,283),
                         (125,283),
                         (126,283),
                         (127,283),
                         (128,283),
                         (129,283)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      238 => ( Point => ( new Points'(
                         (123,281),
                         (124,281),
                         (125,281),
                         (125,282),
                         (126,282),
                         (127,282),
                         (128,282),
                         (129,282)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      239 => ( Point => ( new Points'(
                         (123,279),
                         (123,280),
                         (124,280),
                         (125,280),
                         (126,280),
                         (126,281),
                         (127,281),
                         (128,281),
                         (129,281),
                         (130,281)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      240 => ( Point => ( new Points'(
                         (124,278),
                         (124,279),
                         (125,279),
                         (126,279),
                         (127,279),
                         (127,280),
                         (128,280),
                         (129,280),
                         (130,280)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      241 => ( Point => ( new Points'(
                         (124,277),
                         (125,277),
                         (125,278),
                         (126,278),
                         (127,278),
                         (128,278),
                         (128,279),
                         (129,279),
                         (130,279),
                         (131,279)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      242 => ( Point => ( new Points'(
                         (125,276),
                         (126,276),
                         (126,277),
                         (127,277),
                         (128,277),
                         (129,277),
                         (129,278),
                         (130,278),
                         (131,278)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      243 => ( Point => ( new Points'(
                         (126,275),
                         (127,275),
                         (127,276),
                         (128,276),
                         (129,276),
                         (130,276),
                         (130,277),
                         (131,277),
                         (132,277)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      244 => ( Point => ( new Points'(
                         (126,274),
                         (127,274),
                         (128,274),
                         (128,275),
                         (129,275),
                         (130,275),
                         (131,275),
                         (131,276),
                         (132,276)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      245 => ( Point => ( new Points'(
                         (127,273),
                         (128,273),
                         (129,273),
                         (129,274),
                         (130,274),
                         (131,274),
                         (132,274),
                         (132,275)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      246 => ( Point => ( new Points'(
                         (127,272),
                         (128,272),
                         (129,272),
                         (130,272),
                         (130,273),
                         (131,273),
                         (132,273),
                         (133,273)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      247 => ( Point => ( new Points'(
                         (127,271),
                         (128,271),
                         (129,271),
                         (130,271),
                         (131,271),
                         (131,272),
                         (132,272),
                         (133,272)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      248 => ( Point => ( new Points'(
                         (128,269),
                         (128,270),
                         (129,270),
                         (130,270),
                         (131,270),
                         (132,270),
                         (132,271),
                         (133,271),
                         (134,271)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      249 => ( Point => ( new Points'(
                         (129,268),
                         (129,269),
                         (130,269),
                         (131,269),
                         (132,269),
                         (133,269),
                         (133,270),
                         (134,270),
                         (135,270)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      250 => ( Point => ( new Points'(
                         (130,267),
                         (130,268),
                         (131,268),
                         (132,268),
                         (133,268),
                         (134,268),
                         (134,269),
                         (135,269)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      251 => ( Point => ( new Points'(
                         (130,266),
                         (131,266),
                         (131,267),
                         (132,267),
                         (133,267),
                         (134,267),
                         (135,267),
                         (135,268),
                         (136,268)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      252 => ( Point => ( new Points'(
                         (131,265),
                         (132,265),
                         (132,266),
                         (133,266),
                         (134,266),
                         (135,266),
                         (136,266),
                         (136,267)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      253 => ( Point => ( new Points'(
                         (131,264),
                         (132,264),
                         (133,264),
                         (133,265),
                         (134,265),
                         (135,265),
                         (136,265),
                         (137,265)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      254 => ( Point => ( new Points'(
                         (132,262),
                         (132,263),
                         (133,263),
                         (134,263),
                         (134,264),
                         (135,264),
                         (136,264),
                         (137,264)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      255 => ( Point => ( new Points'(
                         (133,261),
                         (133,262),
                         (134,262),
                         (135,262),
                         (135,263),
                         (136,263),
                         (137,263),
                         (138,263)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      256 => ( Point => ( new Points'(
                         (134,260),
                         (134,261),
                         (135,261),
                         (136,261),
                         (136,262),
                         (137,262),
                         (138,262),
                         (139,262)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      257 => ( Point => ( new Points'(
                         (134,259),
                         (135,259),
                         (135,260),
                         (136,260),
                         (137,260),
                         (137,261),
                         (138,261),
                         (139,261)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      258 => ( Point => ( new Points'(
                         (134,258),
                         (135,258),
                         (136,258),
                         (136,259),
                         (137,259),
                         (138,259),
                         (138,260),
                         (139,260)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      259 => ( Point => ( new Points'(
                         (134,257),
                         (135,257),
                         (136,257),
                         (137,257),
                         (137,258),
                         (138,258),
                         (139,258),
                         (139,259),
                         (140,259)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      260 => ( Point => ( new Points'(
                         (135,256),
                         (136,256),
                         (137,256),
                         (138,256),
                         (138,257),
                         (139,257),
                         (140,257),
                         (140,258),
                         (141,258)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      261 => ( Point => ( new Points'(
                         (136,255),
                         (137,255),
                         (138,255),
                         (139,255),
                         (139,256),
                         (140,256),
                         (141,256),
                         (141,257)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      262 => ( Point => ( new Points'(
                         (136,254),
                         (137,254),
                         (138,254),
                         (139,254),
                         (140,254),
                         (140,255),
                         (141,255),
                         (142,255)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      263 => ( Point => ( new Points'(
                         (137,253),
                         (138,253),
                         (139,253),
                         (140,253),
                         (141,253),
                         (141,254),
                         (142,254)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      264 => ( Point => ( new Points'(
                         (137,251),
                         (137,252),
                         (138,252),
                         (139,252),
                         (140,252),
                         (141,252),
                         (142,252),
                         (142,253),
                         (143,253)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      265 => ( Point => ( new Points'(
                         (138,250),
                         (138,251),
                         (139,251),
                         (140,251),
                         (141,251),
                         (142,251),
                         (143,251),
                         (143,252),
                         (144,252)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      266 => ( Point => ( new Points'(
                         (138,249),
                         (139,249),
                         (139,250),
                         (140,250),
                         (141,250),
                         (142,250),
                         (143,250),
                         (144,250),
                         (144,251)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      267 => ( Point => ( new Points'(
                         (139,248),
                         (140,248),
                         (140,249),
                         (141,249),
                         (142,249),
                         (143,249),
                         (144,249),
                         (145,249)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      268 => ( Point => ( new Points'(
                         (139,247),
                         (140,247),
                         (141,247),
                         (141,248),
                         (142,248),
                         (143,248),
                         (144,248),
                         (145,248),
                         (146,248)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      269 => ( Point => ( new Points'(
                         (140,246),
                         (141,246),
                         (142,246),
                         (142,247),
                         (143,247),
                         (144,247),
                         (145,247),
                         (146,247)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      270 => ( Point => ( new Points'(
                         (141,245),
                         (142,245),
                         (143,245),
                         (143,246),
                         (144,246),
                         (145,246),
                         (146,246),
                         (147,246)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      271 => ( Point => ( new Points'(
                         (141,244),
                         (142,244),
                         (143,244),
                         (144,244),
                         (144,245),
                         (145,245),
                         (146,245),
                         (147,245),
                         (148,245)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      272 => ( Point => ( new Points'(
                         (142,243),
                         (143,243),
                         (144,243),
                         (145,243),
                         (145,244),
                         (146,244),
                         (147,244),
                         (148,244)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      273 => ( Point => ( new Points'(
                         (143,242),
                         (144,242),
                         (145,242),
                         (146,242),
                         (146,243),
                         (147,243),
                         (148,243),
                         (149,243)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      274 => ( Point => ( new Points'(
                         (143,240),
                         (143,241),
                         (144,241),
                         (145,241),
                         (146,241),
                         (147,241),
                         (147,242),
                         (148,242),
                         (149,242)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      275 => ( Point => ( new Points'(
                         (144,239),
                         (144,240),
                         (145,240),
                         (146,240),
                         (147,240),
                         (148,240),
                         (148,241),
                         (149,241),
                         (150,241)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      276 => ( Point => ( new Points'(
                         (144,238),
                         (145,238),
                         (145,239),
                         (146,239),
                         (147,239),
                         (148,239),
                         (149,239),
                         (149,240),
                         (150,240)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      277 => ( Point => ( new Points'(
                         (145,237),
                         (146,237),
                         (146,238),
                         (147,238),
                         (148,238),
                         (149,238),
                         (150,238),
                         (150,239),
                         (151,239)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      278 => ( Point => ( new Points'(
                         (145,236),
                         (146,236),
                         (147,236),
                         (147,237),
                         (148,237),
                         (149,237),
                         (150,237),
                         (151,237),
                         (151,238),
                         (152,238)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      279 => ( Point => ( new Points'(
                         (146,235),
                         (147,235),
                         (148,235),
                         (148,236),
                         (149,236),
                         (150,236),
                         (151,236),
                         (152,236),
                         (152,237)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      280 => ( Point => ( new Points'(
                         (146,234),
                         (147,234),
                         (148,234),
                         (149,234),
                         (149,235),
                         (150,235),
                         (151,235),
                         (152,235),
                         (153,235)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      281 => ( Point => ( new Points'(
                         (147,233),
                         (148,233),
                         (149,233),
                         (150,233),
                         (150,234),
                         (151,234),
                         (152,234),
                         (153,234)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      282 => ( Point => ( new Points'(
                         (148,232),
                         (149,232),
                         (150,232),
                         (151,232),
                         (151,233),
                         (152,233),
                         (153,233),
                         (154,233)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      283 => ( Point => ( new Points'(
                         (148,231),
                         (149,231),
                         (150,231),
                         (151,231),
                         (152,231),
                         (152,232),
                         (153,232),
                         (154,232)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      284 => ( Point => ( new Points'(
                         (149,229),
                         (149,230),
                         (150,230),
                         (151,230),
                         (152,230),
                         (153,230),
                         (153,231),
                         (154,231),
                         (155,231)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      285 => ( Point => ( new Points'(
                         (149,228),
                         (150,228),
                         (150,229),
                         (151,229),
                         (152,229),
                         (153,229),
                         (154,229),
                         (154,230),
                         (155,230)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      286 => ( Point => ( new Points'(
                         (150,227),
                         (151,227),
                         (151,228),
                         (152,228),
                         (153,228),
                         (154,228),
                         (155,228),
                         (155,229),
                         (156,229)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      287 => ( Point => ( new Points'(
                         (151,226),
                         (152,226),
                         (152,227),
                         (153,227),
                         (154,227),
                         (155,227),
                         (156,227),
                         (156,228)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      288 => ( Point => ( new Points'(
                         (151,225),
                         (152,225),
                         (153,225),
                         (153,226),
                         (154,226),
                         (155,226),
                         (156,226),
                         (157,226)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      289 => ( Point => ( new Points'(
                         (151,224),
                         (152,224),
                         (153,224),
                         (154,224),
                         (154,225),
                         (155,225),
                         (156,225),
                         (157,225)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      290 => ( Point => ( new Points'(
                         (152,223),
                         (153,223),
                         (154,223),
                         (155,223),
                         (155,224),
                         (156,224),
                         (157,224),
                         (158,224)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      291 => ( Point => ( new Points'(
                         (153,222),
                         (154,222),
                         (155,222),
                         (156,222),
                         (156,223),
                         (157,223),
                         (158,223),
                         (159,223)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      292 => ( Point => ( new Points'(
                         (153,221),
                         (154,221),
                         (155,221),
                         (156,221),
                         (157,221),
                         (157,222),
                         (158,222),
                         (159,222)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      293 => ( Point => ( new Points'(
                         (154,219),
                         (154,220),
                         (155,220),
                         (156,220),
                         (157,220),
                         (158,220),
                         (158,221),
                         (159,221)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      294 => ( Point => ( new Points'(
                         (155,218),
                         (155,219),
                         (156,219),
                         (157,219),
                         (158,219),
                         (159,219),
                         (159,220),
                         (160,220)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      295 => ( Point => ( new Points'(
                         (155,217),
                         (156,217),
                         (156,218),
                         (157,218),
                         (158,218),
                         (159,218),
                         (160,218),
                         (160,219)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      296 => ( Point => ( new Points'(
                         (155,216),
                         (156,216),
                         (157,216),
                         (157,217),
                         (158,217),
                         (159,217),
                         (160,217),
                         (161,217)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      297 => ( Point => ( new Points'(
                         (156,215),
                         (157,215),
                         (158,215),
                         (158,216),
                         (159,216),
                         (160,216),
                         (161,216)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      298 => ( Point => ( new Points'(
                         (157,214),
                         (158,214),
                         (159,214),
                         (159,215),
                         (160,215),
                         (161,215),
                         (162,215)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      299 => ( Point => ( new Points'(
                         (157,213),
                         (158,213),
                         (159,213),
                         (160,213),
                         (160,214),
                         (161,214),
                         (162,214),
                         (163,214)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      300 => ( Point => ( new Points'(
                         (158,212),
                         (159,212),
                         (160,212),
                         (161,212),
                         (161,213),
                         (162,213),
                         (163,213),
                         (164,213)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      301 => ( Point => ( new Points'(
                         (159,211),
                         (160,211),
                         (161,211),
                         (162,211),
                         (162,212),
                         (163,212),
                         (164,212),
                         (165,212)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      302 => ( Point => ( new Points'(
                         (159,209),
                         (159,210),
                         (160,210),
                         (161,210),
                         (162,210),
                         (163,210),
                         (163,211),
                         (164,211),
                         (165,211)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      303 => ( Point => ( new Points'(
                         (160,208),
                         (160,209),
                         (161,209),
                         (162,209),
                         (163,209),
                         (164,209),
                         (164,210),
                         (165,210)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      304 => ( Point => ( new Points'(
                         (160,207),
                         (161,207),
                         (161,208),
                         (162,208),
                         (163,208),
                         (164,208),
                         (165,208),
                         (165,209)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      305 => ( Point => ( new Points'(
                         (161,206),
                         (162,206),
                         (162,207),
                         (163,207),
                         (164,207),
                         (165,207),
                         (166,207)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      306 => ( Point => ( new Points'(
                         (162,205),
                         (163,205),
                         (163,206),
                         (164,206),
                         (165,206),
                         (166,206),
                         (167,206)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      307 => ( Point => ( new Points'(
                         (162,203),
                         (162,204),
                         (163,204),
                         (164,204),
                         (164,205),
                         (165,205),
                         (166,205),
                         (167,205)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      308 => ( Point => ( new Points'(
                         (163,202),
                         (163,203),
                         (164,203),
                         (165,203),
                         (165,204),
                         (166,204),
                         (167,204),
                         (168,204)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      309 => ( Point => ( new Points'(
                         (163,201),
                         (164,201),
                         (164,202),
                         (165,202),
                         (166,202),
                         (166,203),
                         (167,203),
                         (168,203)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      310 => ( Point => ( new Points'(
                         (164,200),
                         (165,200),
                         (165,201),
                         (166,201),
                         (167,201),
                         (167,202),
                         (168,202),
                         (169,202)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      311 => ( Point => ( new Points'(
                         (164,199),
                         (165,199),
                         (166,199),
                         (166,200),
                         (167,200),
                         (168,200),
                         (168,201),
                         (169,201),
                         (170,201)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      312 => ( Point => ( new Points'(
                         (165,198),
                         (166,198),
                         (167,198),
                         (167,199),
                         (168,199),
                         (169,199),
                         (169,200),
                         (170,200)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      313 => ( Point => ( new Points'(
                         (165,197),
                         (166,197),
                         (167,197),
                         (168,197),
                         (168,198),
                         (169,198),
                         (170,198),
                         (170,199),
                         (171,199)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      314 => ( Point => ( new Points'(
                         (166,196),
                         (167,196),
                         (168,196),
                         (169,196),
                         (169,197),
                         (170,197),
                         (171,197),
                         (171,198),
                         (172,198)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      315 => ( Point => ( new Points'(
                         (167,195),
                         (168,195),
                         (169,195),
                         (170,195),
                         (170,196),
                         (171,196),
                         (172,196),
                         (172,197)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      316 => ( Point => ( new Points'(
                         (167,194),
                         (168,194),
                         (169,194),
                         (170,194),
                         (171,194),
                         (171,195),
                         (172,195),
                         (173,195)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      317 => ( Point => ( new Points'(
                         (168,193),
                         (169,193),
                         (170,193),
                         (171,193),
                         (172,193),
                         (172,194),
                         (173,194)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      318 => ( Point => ( new Points'(
                         (168,192),
                         (169,192),
                         (170,192),
                         (171,192),
                         (172,192),
                         (173,192),
                         (173,193),
                         (174,193)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      319 => ( Point => ( new Points'(
                         (169,191),
                         (170,191),
                         (171,191),
                         (172,191),
                         (173,191),
                         (174,191),
                         (174,192)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      320 => ( Point => ( new Points'(
                         (170,189),
                         (170,190),
                         (171,190),
                         (172,190),
                         (173,190),
                         (174,190),
                         (175,190)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      321 => ( Point => ( new Points'(
                         (170,188),
                         (171,188),
                         (171,189),
                         (172,189),
                         (173,189),
                         (174,189),
                         (175,189),
                         (176,189)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      322 => ( Point => ( new Points'(
                         (171,187),
                         (172,187),
                         (172,188),
                         (173,188),
                         (174,188),
                         (175,188),
                         (176,188)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      323 => ( Point => ( new Points'(
                         (172,186),
                         (173,186),
                         (173,187),
                         (174,187),
                         (175,187),
                         (176,187),
                         (177,187)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      324 => ( Point => ( new Points'(
                         (172,185),
                         (173,185),
                         (174,185),
                         (174,186),
                         (175,186),
                         (176,186),
                         (177,186),
                         (178,186)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      325 => ( Point => ( new Points'(
                         (172,184),
                         (173,184),
                         (174,184),
                         (175,184),
                         (175,185),
                         (176,185),
                         (177,185),
                         (178,185)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      326 => ( Point => ( new Points'(
                         (173,183),
                         (174,183),
                         (175,183),
                         (176,183),
                         (176,184),
                         (177,184),
                         (178,184)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      327 => ( Point => ( new Points'(
                         (174,182),
                         (175,182),
                         (176,182),
                         (177,182),
                         (177,183),
                         (178,183)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      328 => ( Point => ( new Points'(
                         (174,180),
                         (174,181),
                         (175,181),
                         (176,181),
                         (177,181),
                         (178,181),
                         (178,182)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      329 => ( Point => ( new Points'(
                         (175,179),
                         (175,180),
                         (176,180),
                         (177,180),
                         (178,180),
                         (179,180)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      330 => ( Point => ( new Points'(
                         (176,178),
                         (176,179),
                         (177,179),
                         (178,179),
                         (179,179),
                         (180,179)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      331 => ( Point => ( new Points'(
                         (177,177),
                         (177,178),
                         (178,178),
                         (179,178),
                         (180,178),
                         (181,178)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      332 => ( Point => ( new Points'(
                         (177,176),
                         (178,176),
                         (178,177),
                         (179,177),
                         (180,177),
                         (181,177),
                         (182,177)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      333 => ( Point => ( new Points'(
                         (177,175),
                         (178,175),
                         (179,175),
                         (179,176),
                         (180,176),
                         (180,176),
                         (181,176),
                         (182,176)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      334 => ( Point => ( new Points'(
                         (178,174),
                         (179,174),
                         (180,174),
                         (180,175),
                         (181,175),
                         (182,175),
                         (183,175)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      335 => ( Point => ( new Points'(
                         (178,173),
                         (179,173),
                         (180,173),
                         (181,173),
                         (181,174),
                         (182,174),
                         (183,174),
                         (184,174)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      336 => ( Point => ( new Points'(
                         (179,172),
                         (180,172),
                         (181,172),
                         (182,172),
                         (182,173),
                         (183,173),
                         (184,173)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      337 => ( Point => ( new Points'(
                         (180,171),
                         (181,171),
                         (182,171),
                         (183,171),
                         (183,172),
                         (184,172),
                         (185,172)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      338 => ( Point => ( new Points'(
                         (180,169),
                         (180,170),
                         (181,170),
                         (182,170),
                         (183,170),
                         (184,170),
                         (184,171),
                         (185,171)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      339 => ( Point => ( new Points'(
                         (181,168),
                         (181,169),
                         (182,169),
                         (183,169),
                         (184,169),
                         (185,169),
                         (185,170)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      340 => ( Point => ( new Points'(
                         (181,167),
                         (182,167),
                         (182,168),
                         (183,168),
                         (184,168),
                         (185,168),
                         (186,168)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      341 => ( Point => ( new Points'(
                         (182,166),
                         (183,166),
                         (183,167),
                         (184,167),
                         (185,167),
                         (186,167),
                         (187,167)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      342 => ( Point => ( new Points'(
                         (182,165),
                         (183,165),
                         (184,165),
                         (184,166),
                         (185,166),
                         (186,166),
                         (187,166),
                         (188,166)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      343 => ( Point => ( new Points'(
                         (183,164),
                         (184,164),
                         (185,164),
                         (185,165),
                         (186,165),
                         (187,165),
                         (188,165)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      344 => ( Point => ( new Points'(
                         (184,163),
                         (185,163),
                         (186,163),
                         (186,164),
                         (187,164),
                         (188,164),
                         (189,164)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      345 => ( Point => ( new Points'(
                         (184,161),
                         (184,162),
                         (185,162),
                         (186,162),
                         (187,162),
                         (187,163),
                         (188,163),
                         (189,163),
                         (190,163)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      346 => ( Point => ( new Points'(
                         (185,160),
                         (185,161),
                         (186,161),
                         (187,161),
                         (188,161),
                         (188,162),
                         (189,162),
                         (190,162)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      347 => ( Point => ( new Points'(
                         (186,159),
                         (186,160),
                         (187,160),
                         (188,160),
                         (189,160),
                         (189,161),
                         (190,161),
                         (191,161)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      348 => ( Point => ( new Points'(
                         (186,158),
                         (187,158),
                         (187,159),
                         (188,159),
                         (189,159),
                         (190,159),
                         (190,160),
                         (191,160)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      349 => ( Point => ( new Points'(
                         (187,157),
                         (188,157),
                         (188,158),
                         (189,158),
                         (190,158),
                         (191,158),
                         (191,159)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      350 => ( Point => ( new Points'(
                         (188,156),
                         (189,156),
                         (189,157),
                         (190,157),
                         (191,157),
                         (192,157)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      351 => ( Point => ( new Points'(
                         (188,155),
                         (189,155),
                         (190,155),
                         (190,156),
                         (191,156),
                         (192,156),
                         (193,156)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      352 => ( Point => ( new Points'(
                         (188,154),
                         (189,154),
                         (190,154),
                         (191,154),
                         (191,155),
                         (192,155),
                         (193,155)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      353 => ( Point => ( new Points'(
                         (189,153),
                         (190,153),
                         (191,153),
                         (192,153),
                         (192,154),
                         (193,154),
                         (194,154)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      354 => ( Point => ( new Points'(
                         (189,152),
                         (190,152),
                         (191,152),
                         (192,152),
                         (193,152),
                         (193,153),
                         (194,153),
                         (195,153)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      355 => ( Point => ( new Points'(
                         (190,151),
                         (191,151),
                         (192,151),
                         (193,151),
                         (194,151),
                         (194,152),
                         (195,152)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      356 => ( Point => ( new Points'(
                         (191,150),
                         (192,150),
                         (193,150),
                         (194,150),
                         (195,150),
                         (195,151),
                         (196,151)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      357 => ( Point => ( new Points'(
                         (191,149),
                         (192,149),
                         (193,149),
                         (194,149),
                         (195,149),
                         (196,149),
                         (196,150)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      358 => ( Point => ( new Points'(
                         (192,148),
                         (193,148),
                         (194,148),
                         (195,148),
                         (196,148),
                         (197,148)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      359 => ( Point => ( new Points'(
                         (192,146),
                         (192,147),
                         (193,147),
                         (194,147),
                         (195,147),
                         (196,147),
                         (197,147)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      360 => ( Point => ( new Points'(
                         (193,145),
                         (193,146),
                         (194,146),
                         (195,146),
                         (196,146),
                         (197,146),
                         (198,146)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      361 => ( Point => ( new Points'(
                         (194,144),
                         (194,145),
                         (195,145),
                         (196,145),
                         (197,145),
                         (198,145),
                         (199,145)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      362 => ( Point => ( new Points'(
                         (194,143),
                         (195,143),
                         (195,144),
                         (196,144),
                         (197,144),
                         (198,144),
                         (199,144),
                         (200,144)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      363 => ( Point => ( new Points'(
                         (195,142),
                         (196,142),
                         (196,143),
                         (197,143),
                         (198,143),
                         (199,143),
                         (200,143)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      364 => ( Point => ( new Points'(
                         (196,141),
                         (197,141),
                         (197,142),
                         (198,142),
                         (199,142),
                         (200,142)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      365 => ( Point => ( new Points'(
                         (196,140),
                         (197,140),
                         (198,140),
                         (198,141),
                         (199,141),
                         (200,141),
                         (201,141)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      366 => ( Point => ( new Points'(
                         (196,139),
                         (197,139),
                         (198,139),
                         (199,139),
                         (199,140),
                         (200,140),
                         (201,140)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      367 => ( Point => ( new Points'(
                         (197,137),
                         (197,138),
                         (198,138),
                         (199,138),
                         (200,138),
                         (200,139),
                         (201,139),
                         (202,139)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      368 => ( Point => ( new Points'(
                         (198,136),
                         (198,137),
                         (199,137),
                         (200,137),
                         (201,137),
                         (201,138),
                         (202,138),
                         (203,138)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      369 => ( Point => ( new Points'(
                         (199,135),
                         (199,136),
                         (200,136),
                         (201,136),
                         (202,136),
                         (202,137),
                         (203,137)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      370 => ( Point => ( new Points'(
                         (199,134),
                         (200,134),
                         (200,135),
                         (201,135),
                         (202,135),
                         (203,135),
                         (203,136),
                         (204,136)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      371 => ( Point => ( new Points'(
                         (200,132),
                         (200,133),
                         (201,133),
                         (201,134),
                         (202,134),
                         (203,134),
                         (204,134),
                         (204,135)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      372 => ( Point => ( new Points'(
                         (200,131),
                         (201,131),
                         (201,132),
                         (202,132),
                         (202,133),
                         (203,133),
                         (204,133),
                         (205,133)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      373 => ( Point => ( new Points'(
                         (201,130),
                         (202,130),
                         (202,131),
                         (203,131),
                         (203,132),
                         (204,132),
                         (205,132)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      374 => ( Point => ( new Points'(
                         (202,129),
                         (203,129),
                         (203,130),
                         (204,130),
                         (204,131),
                         (205,131),
                         (206,131)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      375 => ( Point => ( new Points'(
                         (202,128),
                         (203,128),
                         (204,128),
                         (204,129),
                         (205,129),
                         (205,130),
                         (206,130)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      376 => ( Point => ( new Points'(
                         (203,127),
                         (204,127),
                         (205,127),
                         (205,128),
                         (206,128),
                         (206,129),
                         (207,129)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      377 => ( Point => ( new Points'(
                         (203,126),
                         (204,126),
                         (205,126),
                         (206,126),
                         (206,127),
                         (207,127),
                         (207,128),
                         (208,128)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      378 => ( Point => ( new Points'(
                         (204,125),
                         (205,125),
                         (206,125),
                         (207,125),
                         (207,126),
                         (208,126),
                         (208,127)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      379 => ( Point => ( new Points'(
                         (204,124),
                         (205,124),
                         (206,124),
                         (207,124),
                         (208,124),
                         (208,125),
                         (209,125)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      380 => ( Point => ( new Points'(
                         (205,123),
                         (206,123),
                         (207,123),
                         (208,123),
                         (209,123),
                         (209,124),
                         (210,124)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      381 => ( Point => ( new Points'(
                         (206,121),
                         (206,122),
                         (207,122),
                         (208,122),
                         (209,122),
                         (210,122),
                         (210,123),
                         (211,123)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      382 => ( Point => ( new Points'(
                         (206,120),
                         (207,120),
                         (207,121),
                         (208,121),
                         (209,121),
                         (210,121),
                         (211,121),
                         (211,122)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      383 => ( Point => ( new Points'(
                         (207,119),
                         (208,119),
                         (208,120),
                         (209,120),
                         (210,120),
                         (211,120),
                         (212,120),
                         (212,121)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      384 => ( Point => ( new Points'(
                         (208,118),
                         (209,118),
                         (209,119),
                         (210,119),
                         (211,119),
                         (212,119)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      385 => ( Point => ( new Points'(
                         (208,117),
                         (209,117),
                         (210,117),
                         (210,118),
                         (211,118),
                         (212,118),
                         (213,118)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      386 => ( Point => ( new Points'(
                         (209,116),
                         (210,116),
                         (211,116),
                         (211,117),
                         (212,117),
                         (213,117)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      387 => ( Point => ( new Points'(
                         (209,115),
                         (210,115),
                         (211,115),
                         (212,115),
                         (212,116),
                         (213,116),
                         (214,116)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      388 => ( Point => ( new Points'(
                         (210,113),
                         (210,114),
                         (211,114),
                         (212,114),
                         (213,114),
                         (213,115),
                         (214,115)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      389 => ( Point => ( new Points'(
                         (211,112),
                         (211,113),
                         (212,113),
                         (213,113),
                         (214,113),
                         (214,114),
                         (215,114)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      390 => ( Point => ( new Points'(
                         (212,111),
                         (212,112),
                         (213,112),
                         (214,112),
                         (215,112),
                         (215,113)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      391 => ( Point => ( new Points'(
                         (212,110),
                         (213,110),
                         (213,111),
                         (214,111),
                         (215,111),
                         (216,111)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      392 => ( Point => ( new Points'(
                         (213,109),
                         (214,109),
                         (214,110),
                         (215,110),
                         (216,110),
                         (217,110)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      393 => ( Point => ( new Points'(
                         (213,108),
                         (214,108),
                         (215,108),
                         (215,109),
                         (216,109),
                         (217,109)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      394 => ( Point => ( new Points'(
                         (214,106),
                         (214,107),
                         (215,107),
                         (216,107),
                         (216,108),
                         (217,108),
                         (218,108)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      395 => ( Point => ( new Points'(
                         (214,105),
                         (215,105),
                         (215,106),
                         (216,106),
                         (217,106),
                         (217,107),
                         (218,107),
                         (219,107)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      396 => ( Point => ( new Points'(
                         (215,104),
                         (216,104),
                         (216,105),
                         (217,105),
                         (218,105),
                         (218,106),
                         (219,106)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      397 => ( Point => ( new Points'(
                         (216,103),
                         (217,103),
                         (217,104),
                         (218,104),
                         (219,104),
                         (219,105),
                         (220,105)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      398 => ( Point => ( new Points'(
                         (217,101),
                         (217,102),
                         (218,102),
                         (218,103),
                         (219,103),
                         (220,103),
                         (220,104),
                         (221,104)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      399 => ( Point => ( new Points'(
                         (217,100),
                         (218,100),
                         (218,101),
                         (219,101),
                         (219,102),
                         (220,102),
                         (221,102),
                         (221,103)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      400 => ( Point => ( new Points'(
                         (218,99),
                         (219,99),
                         (219,100),
                         (220,100),
                         (220,101),
                         (221,101),
                         (222,101)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      401 => ( Point => ( new Points'(
                         (218,98),
                         (219,98),
                         (220,98),
                         (220,99),
                         (221,99),
                         (221,100),
                         (222,100),
                         (223,100)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      402 => ( Point => ( new Points'(
                         (219,97),
                         (220,97),
                         (221,97),
                         (221,98),
                         (222,98),
                         (222,99),
                         (223,99)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      403 => ( Point => ( new Points'(
                         (220,95),
                         (220,96),
                         (221,96),
                         (222,96),
                         (222,97),
                         (223,97),
                         (223,98),
                         (224,98)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      404 => ( Point => ( new Points'(
                         (221,94),
                         (221,95),
                         (222,95),
                         (223,95),
                         (223,96),
                         (224,96),
                         (224,97)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      405 => ( Point => ( new Points'(
                         (221,93),
                         (222,93),
                         (222,94),
                         (223,94),
                         (224,94),
                         (224,95),
                         (225,95)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      406 => ( Point => ( new Points'(
                         (222,91),
                         (222,92),
                         (223,92),
                         (223,93),
                         (224,93),
                         (225,93),
                         (225,94),
                         (226,94)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      407 => ( Point => ( new Points'(
                         (223,90),
                         (223,91),
                         (224,91),
                         (224,92),
                         (225,92),
                         (226,92),
                         (226,93),
                         (227,93)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      408 => ( Point => ( new Points'(
                         (224,89),
                         (224,90),
                         (225,90),
                         (225,91),
                         (226,91),
                         (227,91),
                         (227,92)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      409 => ( Point => ( new Points'(
                         (224,88),
                         (225,88),
                         (225,89),
                         (226,89),
                         (226,90),
                         (227,90),
                         (228,90)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      410 => ( Point => ( new Points'(
                         (225,87),
                         (226,87),
                         (226,88),
                         (227,88),
                         (227,89),
                         (228,89)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      411 => ( Point => ( new Points'(
                         (225,86),
                         (226,86),
                         (227,86),
                         (227,87),
                         (228,87),
                         (228,88),
                         (229,88)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      412 => ( Point => ( new Points'(
                         (226,84),
                         (226,85),
                         (227,85),
                         (228,85),
                         (228,86),
                         (229,86),
                         (229,87)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      413 => ( Point => ( new Points'(
                         (227,82),
                         (227,83),
                         (227,84),
                         (228,84),
                         (229,84),
                         (229,85),
                         (230,85)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      414 => ( Point => ( new Points'(
                         (228,82),
                         (228,83),
                         (229,83),
                         (230,83),
                         (230,84)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      415 => ( Point => ( new Points'(
                         (228,81),
                         (229,81),
                         (229,82),
                         (230,82),
                         (231,82)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      416 => ( Point => ( new Points'(
                         (229,79),
                         (229,80),
                         (230,80),
                         (230,81),
                         (231,81),
                         (232,81)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      417 => ( Point => ( new Points'(
                         (230,78),
                         (230,79),
                         (231,79),
                         (231,80),
                         (232,80),
                         (233,80)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      418 => ( Point => ( new Points'(
                         (230,77),
                         (231,77),
                         (231,78),
                         (232,78),
                         (232,79),
                         (233,79),
                         (234,79)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      419 => ( Point => ( new Points'(
                         (231,76),
                         (232,76),
                         (232,77),
                         (233,77),
                         (233,78),
                         (234,78)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      420 => ( Point => ( new Points'(
                         (231,75),
                         (232,75),
                         (233,75),
                         (233,76),
                         (234,76),
                         (234,77),
                         (235,77)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      421 => ( Point => ( new Points'(
                         (232,73),
                         (232,74),
                         (233,74),
                         (234,74),
                         (234,75),
                         (235,75),
                         (235,76)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      422 => ( Point => ( new Points'(
                         (233,72),
                         (233,73),
                         (234,73),
                         (235,73),
                         (235,74),
                         (236,74)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      423 => ( Point => ( new Points'(
                         (234,71),
                         (234,72),
                         (235,72),
                         (236,72),
                         (236,73),
                         (237,73)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      424 => ( Point => ( new Points'(
                         (234,70),
                         (235,70),
                         (235,71),
                         (236,71),
                         (237,71),
                         (237,72),
                         (238,72)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      425 => ( Point => ( new Points'(
                         (235,69),
                         (236,69),
                         (236,70),
                         (237,70),
                         (238,70),
                         (238,71)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      426 => ( Point => ( new Points'(
                         (235,68),
                         (236,68),
                         (237,68),
                         (237,69),
                         (238,69),
                         (239,69)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      427 => ( Point => ( new Points'(
                         (236,66),
                         (236,67),
                         (237,67),
                         (238,67),
                         (238,68),
                         (239,68)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      428 => ( Point => ( new Points'(
                         (237,65),
                         (237,66),
                         (238,66),
                         (239,66),
                         (239,67),
                         (240,67)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      429 => ( Point => ( new Points'(
                         (238,64),
                         (238,65),
                         (239,65),
                         (240,65),
                         (240,66),
                         (241,66)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      430 => ( Point => ( new Points'(
                         (238,63),
                         (239,63),
                         (239,64),
                         (240,64),
                         (241,64),
                         (241,65),
                         (242,65)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      431 => ( Point => ( new Points'(
                         (239,61),
                         (239,62),
                         (240,62),
                         (240,63),
                         (241,63),
                         (242,63),
                         (242,64),
                         (243,64)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      432 => ( Point => ( new Points'(
                         (240,61),
                         (241,61),
                         (241,62),
                         (242,62),
                         (243,62),
                         (243,63)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      433 => ( Point => ( new Points'(
                         (240,60),
                         (241,60),
                         (242,60),
                         (242,61),
                         (243,61),
                         (244,61),
                         (244,62)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      434 => ( Point => ( new Points'(
                         (241,58),
                         (241,59),
                         (242,59),
                         (243,59),
                         (243,60),
                         (244,60),
                         (245,60)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      435 => ( Point => ( new Points'(
                         (242,57),
                         (242,58),
                         (243,58),
                         (244,58),
                         (244,59),
                         (245,59)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      436 => ( Point => ( new Points'(
                         (242,56),
                         (243,56),
                         (243,57),
                         (244,57),
                         (245,57),
                         (245,58),
                         (246,58)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      437 => ( Point => ( new Points'(
                         (243,54),
                         (243,55),
                         (244,55),
                         (244,56),
                         (245,56),
                         (246,56),
                         (246,57),
                         (247,57)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      438 => ( Point => ( new Points'(
                         (244,53),
                         (244,54),
                         (245,54),
                         (245,55),
                         (246,55),
                         (247,55),
                         (247,56)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      439 => ( Point => ( new Points'(
                         (244,52),
                         (245,52),
                         (245,53),
                         (246,53),
                         (246,54),
                         (247,54),
                         (248,54)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      440 => ( Point => ( new Points'(
                         (245,50),
                         (245,51),
                         (246,51),
                         (246,52),
                         (247,52),
                         (247,53),
                         (248,53),
                         (249,53)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      441 => ( Point => ( new Points'(
                         (246,49),
                         (246,50),
                         (247,50),
                         (247,51),
                         (248,51),
                         (248,52),
                         (249,52)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      442 => ( Point => ( new Points'(
                         (247,48),
                         (247,49),
                         (248,49),
                         (248,50),
                         (249,50),
                         (249,51)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      443 => ( Point => ( new Points'(
                         (247,47),
                         (248,47),
                         (248,48),
                         (249,48),
                         (249,49),
                         (250,49)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      444 => ( Point => ( new Points'(
                         (248,46),
                         (249,46),
                         (249,47),
                         (250,47),
                         (250,48),
                         (251,48)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      445 => ( Point => ( new Points'(
                         (248,45),
                         (249,45),
                         (250,45),
                         (250,46),
                         (251,46),
                         (251,47),
                         (252,47)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      446 => ( Point => ( new Points'(
                         (249,44),
                         (250,44),
                         (251,44),
                         (251,45),
                         (252,45),
                         (252,46)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      447 => ( Point => ( new Points'(
                         (250,43),
                         (251,43),
                         (252,43),
                         (252,44),
                         (253,44)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      448 => ( Point => ( new Points'(
                         (250,42),
                         (251,42),
                         (252,42),
                         (253,42),
                         (253,43),
                         (254,43)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      449 => ( Point => ( new Points'(
                         (251,41),
                         (252,41),
                         (253,41),
                         (254,41),
                         (254,42),
                         (255,42)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      450 => ( Point => ( new Points'(
                         (251,40),
                         (252,40),
                         (253,40),
                         (254,40),
                         (255,40),
                         (255,41)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      451 => ( Point => ( new Points'(
                         (252,38),
                         (252,39),
                         (253,39),
                         (254,39),
                         (255,39),
                         (256,39),
                         (256,40)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      452 => ( Point => ( new Points'(
                         (253,37),
                         (253,38),
                         (254,38),
                         (255,38),
                         (256,38),
                         (257,38)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      453 => ( Point => ( new Points'(
                         (254,36),
                         (254,37),
                         (255,37),
                         (256,37),
                         (257,37),
                         (258,37)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      454 => ( Point => ( new Points'(
                         (254,35),
                         (255,35),
                         (255,36),
                         (256,36),
                         (257,36),
                         (258,36),
                         (259,36)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      455 => ( Point => ( new Points'(
                         (255,34),
                         (256,34),
                         (256,35),
                         (257,35),
                         (258,35),
                         (259,35)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      456 => ( Point => ( new Points'(
                         (255,33),
                         (256,33),
                         (257,33),
                         (257,34),
                         (258,34),
                         (259,34),
                         (260,34)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      457 => ( Point => ( new Points'(
                         (255,32),
                         (256,32),
                         (257,32),
                         (258,32),
                         (258,33),
                         (259,33),
                         (260,33)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      458 => ( Point => ( new Points'(
                         (256,30),
                         (256,31),
                         (257,31),
                         (258,31),
                         (259,31),
                         (259,32),
                         (260,32)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      459 => ( Point => ( new Points'(
                         (257,29),
                         (257,30),
                         (258,30),
                         (259,30),
                         (260,30),
                         (260,31),
                         (261,31)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      460 => ( Point => ( new Points'(
                         (258,28),
                         (258,29),
                         (259,29),
                         (260,29),
                         (261,29),
                         (261,30),
                         (262,30)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      461 => ( Point => ( new Points'(
                         (259,27),
                         (259,28),
                         (260,28),
                         (261,28),
                         (262,28),
                         (262,29),
                         (263,29)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      462 => ( Point => ( new Points'(
                         (259,26),
                         (260,26),
                         (260,27),
                         (261,27),
                         (262,27),
                         (263,27),
                         (263,28),
                         (264,28)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      463 => ( Point => ( new Points'(
                         (260,24),
                         (260,25),
                         (261,25),
                         (261,26),
                         (262,26),
                         (263,26),
                         (264,26)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      464 => ( Point => ( new Points'(
                         (261,23),
                         (261,24),
                         (262,24),
                         (262,25),
                         (263,25),
                         (264,25),
                         (265,25)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      465 => ( Point => ( new Points'(
                         (262,22),
                         (262,23),
                         (263,23),
                         (263,24),
                         (264,24),
                         (265,24)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      466 => ( Point => ( new Points'(
                         (262,21),
                         (263,21),
                         (263,22),
                         (264,22),
                         (264,23),
                         (265,23),
                         (266,23)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      467 => ( Point => ( new Points'(
                         (262,20),
                         (263,20),
                         (264,20),
                         (264,21),
                         (265,21),
                         (265,22),
                         (266,22),
                         (267,22)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      468 => ( Point => ( new Points'(
                         (263,19),
                         (264,19),
                         (265,19),
                         (265,20),
                         (266,20),
                         (266,21),
                         (267,21)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      469 => ( Point => ( new Points'(
                         (264,18),
                         (265,18),
                         (266,18),
                         (266,19),
                         (267,19),
                         (267,20),
                         (268,20)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      470 => ( Point => ( new Points'(
                         (264,17),
                         (265,17),
                         (266,17),
                         (267,17),
                         (267,18),
                         (268,18),
                         (268,19),
                         (269,19)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      471 => ( Point => ( new Points'(
                         (265,16),
                         (266,16),
                         (267,16),
                         (268,16),
                         (268,17),
                         (269,17),
                         (269,18)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      472 => ( Point => ( new Points'(
                         (265,15),
                         (266,15),
                         (267,15),
                         (268,15),
                         (269,15),
                         (269,16),
                         (270,16)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      473 => ( Point => ( new Points'(
                         (266,14),
                         (267,14),
                         (268,14),
                         (269,14),
                         (270,14),
                         (270,15)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      474 => ( Point => ( new Points'(
                         (266,13),
                         (267,13),
                         (268,13),
                         (269,13),
                         (270,13),
                         (271,13)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      475 => ( Point => ( new Points'(
                         (267,12),
                         (268,12),
                         (269,12),
                         (270,12),
                         (271,12)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      476 => ( Point => ( new Points'(
                         (268,10),
                         (268,11),
                         (269,11),
                         (270,11),
                         (271,11),
                         (272,11)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      477 => ( Point => ( new Points'(
                         (268,9),
                         (269,9),
                         (269,10),
                         (270,10),
                         (271,10),
                         (272,10),
                         (273,10)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      478 => ( Point => ( new Points'(
                         (269,8),
                         (270,8),
                         (270,9),
                         (271,9),
                         (272,9),
                         (273,9)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      479 => ( Point => ( new Points'(
                         (269,7),
                         (270,7),
                         (271,7),
                         (271,8),
                         (272,8),
                         (273,8)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      480 => ( Point => ( new Points'(
                         (270,6),
                         (271,6),
                         (272,6),
                         (272,7),
                         (273,7),
                         (274,7)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      481 => ( Point => ( new Points'(
                         (270,5),
                         (271,5),
                         (272,5),
                         (273,5),
                         (273,6),
                         (274,6),
                         (275,6)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      482 => ( Point => ( new Points'(
                         (271,4),
                         (272,4),
                         (273,4),
                         (274,4),
                         (274,5),
                         (275,5)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      483 => ( Point => ( new Points'(
                         (272,3),
                         (273,3),
                         (274,3),
                         (275,3),
                         (275,4)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      484 => ( Point => ( new Points'(
                         (272,2),
                         (273,2),
                         (274,2),
                         (275,2),
                         (276,2)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1)
     );

   Right_Rail : aliased constant Slices :=
     (
      1 => ( Point => ( new Points'(
                       (205,723),
                       (206,723),
                       (207,723),
                       (208,723),
                       (209,723),
                       (210,723),
                       (211,723),
                       (212,723),
                       (213,723),
                       (214,723),
                       (215,723),
                       (216,723)
                      )),
            Threshold_Lower => -1,
            Threshold_Upper => -1,
            Link_Slice_Other_Rail => -1),

      2 => ( Point => ( new Points'(
                       (206,721),
                       (206,722),
                       (207,722),
                       (208,722),
                       (209,722),
                       (210,722),
                       (211,722),
                       (212,722),
                       (213,722),
                       (214,722),
                       (215,722),
                       (216,722)
                      )),
            Threshold_Lower => -1,
            Threshold_Upper => -1,
            Link_Slice_Other_Rail => -1),

      3 => ( Point => ( new Points'(
                       (206,720),
                       (207,720),
                       (207,721),
                       (208,721),
                       (209,721),
                       (210,721),
                       (211,721),
                       (212,721),
                       (213,721),
                       (214,721),
                       (215,721),
                       (216,721)
                      )),
            Threshold_Lower => -1,
            Threshold_Upper => -1,
            Link_Slice_Other_Rail => -1),

      4 => ( Point => ( new Points'(
                       (207,719),
                       (208,719),
                       (208,720),
                       (209,720),
                       (210,720),
                       (211,720),
                       (212,720),
                       (213,720),
                       (214,720),
                       (215,720),
                       (216,720),
                       (217,720)
                      )),
            Threshold_Lower => -1,
            Threshold_Upper => -1,
            Link_Slice_Other_Rail => -1),

      5 => ( Point => ( new Points'(
                       (207,718),
                       (208,718),
                       (209,718),
                       (209,719),
                       (210,719),
                       (211,719),
                       (212,719),
                       (213,719),
                       (214,719),
                       (215,719),
                       (216,719),
                       (217,719)
                      )),
            Threshold_Lower => -1,
            Threshold_Upper => -1,
            Link_Slice_Other_Rail => -1),

      6 => ( Point => ( new Points'(
                       (207,717),
                       (208,717),
                       (209,717),
                       (210,717),
                       (210,718),
                       (211,718),
                       (212,718),
                       (213,718),
                       (214,718),
                       (215,718),
                       (216,718),
                       (217,718)
                      )),
            Threshold_Lower => -1,
            Threshold_Upper => -1,
            Link_Slice_Other_Rail => -1),

      7 => ( Point => ( new Points'(
                       (207,716),
                       (208,716),
                       (209,716),
                       (210,716),
                       (211,716),
                       (211,717),
                       (212,717),
                       (213,717),
                       (214,717),
                       (215,717),
                       (216,717),
                       (217,717)
                      )),
            Threshold_Lower => -1,
            Threshold_Upper => -1,
            Link_Slice_Other_Rail => -1),

      8 => ( Point => ( new Points'(
                       (207,715),
                       (208,715),
                       (209,715),
                       (210,715),
                       (211,715),
                       (212,715),
                       (212,716),
                       (213,716),
                       (214,716),
                       (215,716),
                       (216,716),
                       (217,716),
                       (218,716)
                      )),
            Threshold_Lower => -1,
            Threshold_Upper => -1,
            Link_Slice_Other_Rail => -1),

      9 => ( Point => ( new Points'(
                       (207,714),
                       (208,714),
                       (209,714),
                       (210,714),
                       (211,714),
                       (212,714),
                       (213,714),
                       (213,715),
                       (214,715),
                       (215,715),
                       (216,715),
                       (217,715),
                       (218,715)
                      )),
            Threshold_Lower => -1,
            Threshold_Upper => -1,
            Link_Slice_Other_Rail => -1),

      10 => ( Point => ( new Points'(
                        (207,713),
                        (208,713),
                        (209,713),
                        (210,713),
                        (211,713),
                        (212,713),
                        (213,713),
                        (214,713),
                        (214,714),
                        (215,714),
                        (216,714),
                        (217,714),
                        (218,714)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      11 => ( Point => ( new Points'(
                        (207,712),
                        (208,712),
                        (209,712),
                        (210,712),
                        (211,712),
                        (212,712),
                        (213,712),
                        (214,712),
                        (215,712),
                        (215,713),
                        (216,713),
                        (217,713),
                        (218,713)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      12 => ( Point => ( new Points'(
                        (208,711),
                        (209,711),
                        (210,711),
                        (211,711),
                        (212,711),
                        (213,711),
                        (214,711),
                        (215,711),
                        (216,711),
                        (216,712),
                        (217,712),
                        (218,712)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      13 => ( Point => ( new Points'(
                        (208,710),
                        (209,710),
                        (210,710),
                        (211,710),
                        (212,710),
                        (213,710),
                        (214,710),
                        (215,710),
                        (216,710),
                        (217,710),
                        (217,711),
                        (218,711)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      14 => ( Point => ( new Points'(
                        (208,709),
                        (209,709),
                        (210,709),
                        (211,709),
                        (212,709),
                        (213,709),
                        (214,709),
                        (215,709),
                        (216,709),
                        (217,709),
                        (218,709),
                        (218,710)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      15 => ( Point => ( new Points'(
                        (208,708),
                        (209,708),
                        (210,708),
                        (211,708),
                        (212,708),
                        (213,708),
                        (214,708),
                        (215,708),
                        (216,708),
                        (217,708),
                        (218,708)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      16 => ( Point => ( new Points'(
                        (208,706),
                        (208,707),
                        (209,707),
                        (210,707),
                        (211,707),
                        (212,707),
                        (213,707),
                        (214,707),
                        (215,707),
                        (216,707),
                        (217,707),
                        (218,707),
                        (219,707)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      17 => ( Point => ( new Points'(
                        (208,705),
                        (209,705),
                        (209,706),
                        (210,706),
                        (211,706),
                        (212,706),
                        (213,706),
                        (214,706),
                        (215,706),
                        (216,706),
                        (217,706),
                        (218,706),
                        (219,706)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      18 => ( Point => ( new Points'(
                        (209,704),
                        (210,704),
                        (210,705),
                        (211,705),
                        (212,705),
                        (213,705),
                        (214,705),
                        (215,705),
                        (216,705),
                        (217,705),
                        (218,705),
                        (219,705)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      19 => ( Point => ( new Points'(
                        (209,703),
                        (210,703),
                        (211,703),
                        (211,704),
                        (212,704),
                        (213,704),
                        (214,704),
                        (215,704),
                        (216,704),
                        (217,704),
                        (218,704),
                        (219,704)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      20 => ( Point => ( new Points'(
                        (209,702),
                        (210,702),
                        (211,702),
                        (212,702),
                        (212,703),
                        (213,703),
                        (214,703),
                        (215,703),
                        (216,703),
                        (217,703),
                        (218,703),
                        (219,703)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      21 => ( Point => ( new Points'(
                        (210,701),
                        (211,701),
                        (212,701),
                        (213,701),
                        (213,702),
                        (214,702),
                        (215,702),
                        (216,702),
                        (217,702),
                        (218,702),
                        (219,702),
                        (220,702)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      22 => ( Point => ( new Points'(
                        (210,700),
                        (211,700),
                        (212,700),
                        (213,700),
                        (214,700),
                        (214,701),
                        (215,701),
                        (216,701),
                        (217,701),
                        (218,701),
                        (219,701),
                        (220,701)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      23 => ( Point => ( new Points'(
                        (210,699),
                        (211,699),
                        (212,699),
                        (213,699),
                        (214,699),
                        (215,699),
                        (215,700),
                        (216,700),
                        (217,700),
                        (218,700),
                        (219,700),
                        (220,700)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      24 => ( Point => ( new Points'(
                        (210,698),
                        (211,698),
                        (212,698),
                        (213,698),
                        (214,698),
                        (215,698),
                        (216,698),
                        (216,699),
                        (217,699),
                        (218,699),
                        (219,699),
                        (220,699)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      25 => ( Point => ( new Points'(
                        (210,697),
                        (211,697),
                        (212,697),
                        (213,697),
                        (214,697),
                        (215,697),
                        (216,697),
                        (217,697),
                        (217,698),
                        (218,698),
                        (219,698),
                        (220,698)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      26 => ( Point => ( new Points'(
                        (210,696),
                        (211,696),
                        (212,696),
                        (213,696),
                        (214,696),
                        (215,696),
                        (216,696),
                        (217,696),
                        (218,696),
                        (218,697),
                        (219,697),
                        (220,697)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      27 => ( Point => ( new Points'(
                        (211,695),
                        (212,695),
                        (213,695),
                        (214,695),
                        (215,695),
                        (216,695),
                        (217,695),
                        (218,695),
                        (219,695),
                        (219,696),
                        (220,696),
                        (221,696)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      28 => ( Point => ( new Points'(
                        (211,694),
                        (212,694),
                        (213,694),
                        (214,694),
                        (215,694),
                        (216,694),
                        (217,694),
                        (218,694),
                        (219,694),
                        (220,694),
                        (220,695),
                        (221,695)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      29 => ( Point => ( new Points'(
                        (211,693),
                        (212,693),
                        (213,693),
                        (214,693),
                        (215,693),
                        (216,693),
                        (217,693),
                        (218,693),
                        (219,693),
                        (220,693),
                        (221,693),
                        (221,694)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      30 => ( Point => ( new Points'(
                        (211,691),
                        (211,692),
                        (212,692),
                        (213,692),
                        (214,692),
                        (215,692),
                        (216,692),
                        (217,692),
                        (218,692),
                        (219,692),
                        (220,692),
                        (221,692)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      31 => ( Point => ( new Points'(
                        (211,690),
                        (212,690),
                        (212,691),
                        (213,691),
                        (214,691),
                        (215,691),
                        (216,691),
                        (217,691),
                        (218,691),
                        (219,691),
                        (220,691),
                        (221,691)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      32 => ( Point => ( new Points'(
                        (212,689),
                        (213,689),
                        (213,690),
                        (214,690),
                        (215,690),
                        (216,690),
                        (217,690),
                        (218,690),
                        (219,690),
                        (220,690),
                        (221,690)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      33 => ( Point => ( new Points'(
                        (212,688),
                        (213,688),
                        (214,688),
                        (214,689),
                        (215,689),
                        (216,689),
                        (217,689),
                        (218,689),
                        (219,689),
                        (220,689),
                        (221,689)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      34 => ( Point => ( new Points'(
                        (212,687),
                        (213,687),
                        (214,687),
                        (215,687),
                        (215,688),
                        (216,688),
                        (217,688),
                        (218,688),
                        (219,688),
                        (220,688),
                        (221,688)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      35 => ( Point => ( new Points'(
                        (212,686),
                        (213,686),
                        (214,686),
                        (215,686),
                        (216,686),
                        (216,687),
                        (217,687),
                        (218,687),
                        (219,687),
                        (220,687),
                        (221,687)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      36 => ( Point => ( new Points'(
                        (212,685),
                        (213,685),
                        (214,685),
                        (215,685),
                        (216,685),
                        (217,685),
                        (217,686),
                        (218,686),
                        (219,686),
                        (220,686),
                        (221,686),
                        (222,686)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      37 => ( Point => ( new Points'(
                        (212,684),
                        (213,684),
                        (214,684),
                        (215,684),
                        (216,684),
                        (217,684),
                        (218,684),
                        (218,685),
                        (219,685),
                        (220,685),
                        (221,685),
                        (222,685)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      38 => ( Point => ( new Points'(
                        (213,683),
                        (214,683),
                        (215,683),
                        (216,683),
                        (217,683),
                        (218,683),
                        (219,683),
                        (219,684),
                        (220,684),
                        (221,684),
                        (222,684)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      39 => ( Point => ( new Points'(
                        (213,682),
                        (214,682),
                        (215,682),
                        (216,682),
                        (217,682),
                        (218,682),
                        (219,682),
                        (220,682),
                        (220,683),
                        (221,683),
                        (222,683),
                        (223,683)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      40 => ( Point => ( new Points'(
                        (213,681),
                        (214,681),
                        (215,681),
                        (216,681),
                        (217,681),
                        (218,681),
                        (219,681),
                        (220,681),
                        (221,681),
                        (221,682),
                        (222,682),
                        (223,682)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      41 => ( Point => ( new Points'(
                        (213,680),
                        (214,680),
                        (215,680),
                        (216,680),
                        (217,680),
                        (218,680),
                        (219,680),
                        (220,680),
                        (221,680),
                        (222,680),
                        (222,681),
                        (223,681)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      42 => ( Point => ( new Points'(
                        (213,679),
                        (214,679),
                        (215,679),
                        (216,679),
                        (217,679),
                        (218,679),
                        (219,679),
                        (220,679),
                        (221,679),
                        (222,679),
                        (223,679),
                        (223,680)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      43 => ( Point => ( new Points'(
                        (213,678),
                        (214,678),
                        (215,678),
                        (216,678),
                        (217,678),
                        (218,678),
                        (219,678),
                        (220,678),
                        (221,678),
                        (222,678),
                        (223,678),
                        (224,678)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      44 => ( Point => ( new Points'(
                        (213,676),
                        (213,677),
                        (214,677),
                        (215,677),
                        (216,677),
                        (217,677),
                        (218,677),
                        (219,677),
                        (220,677),
                        (221,677),
                        (222,677),
                        (223,677),
                        (224,677)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      45 => ( Point => ( new Points'(
                        (213,675),
                        (214,675),
                        (214,676),
                        (215,676),
                        (216,676),
                        (217,676),
                        (218,676),
                        (219,676),
                        (220,676),
                        (221,676),
                        (222,676),
                        (223,676),
                        (224,676)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      46 => ( Point => ( new Points'(
                        (213,674),
                        (214,674),
                        (215,674),
                        (215,675),
                        (216,675),
                        (217,675),
                        (218,675),
                        (219,675),
                        (220,675),
                        (221,675),
                        (222,675),
                        (223,675),
                        (224,675)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      47 => ( Point => ( new Points'(
                        (213,673),
                        (214,673),
                        (215,673),
                        (216,673),
                        (216,674),
                        (217,674),
                        (218,674),
                        (219,674),
                        (220,674),
                        (221,674),
                        (222,674),
                        (223,674),
                        (224,674)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      48 => ( Point => ( new Points'(
                        (214,672),
                        (215,672),
                        (216,672),
                        (217,672),
                        (217,673),
                        (218,673),
                        (219,673),
                        (220,673),
                        (221,673),
                        (222,673),
                        (223,673),
                        (224,673)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      49 => ( Point => ( new Points'(
                        (214,671),
                        (215,671),
                        (216,671),
                        (217,671),
                        (218,671),
                        (218,672),
                        (219,672),
                        (220,672),
                        (221,672),
                        (222,672),
                        (223,672),
                        (224,672)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      50 => ( Point => ( new Points'(
                        (214,670),
                        (215,670),
                        (216,670),
                        (217,670),
                        (218,670),
                        (219,670),
                        (219,671),
                        (220,671),
                        (221,671),
                        (222,671),
                        (223,671),
                        (224,671)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      51 => ( Point => ( new Points'(
                        (214,669),
                        (215,669),
                        (216,669),
                        (217,669),
                        (218,669),
                        (219,669),
                        (220,669),
                        (220,670),
                        (221,670),
                        (222,670),
                        (223,670),
                        (224,670)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      52 => ( Point => ( new Points'(
                        (215,668),
                        (216,668),
                        (217,668),
                        (218,668),
                        (219,668),
                        (220,668),
                        (221,668),
                        (221,669),
                        (222,669),
                        (223,669),
                        (224,669),
                        (225,669)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      53 => ( Point => ( new Points'(
                        (215,667),
                        (216,667),
                        (217,667),
                        (218,667),
                        (219,667),
                        (220,667),
                        (221,667),
                        (222,667),
                        (222,668),
                        (223,668),
                        (224,668),
                        (225,668)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      54 => ( Point => ( new Points'(
                        (215,666),
                        (216,666),
                        (217,666),
                        (218,666),
                        (219,666),
                        (220,666),
                        (221,666),
                        (222,666),
                        (223,666),
                        (223,667),
                        (224,667),
                        (225,667)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      55 => ( Point => ( new Points'(
                        (215,665),
                        (216,665),
                        (217,665),
                        (218,665),
                        (219,665),
                        (220,665),
                        (221,665),
                        (222,665),
                        (223,665),
                        (224,665),
                        (224,666),
                        (225,666)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      56 => ( Point => ( new Points'(
                        (215,664),
                        (216,664),
                        (217,664),
                        (218,664),
                        (219,664),
                        (220,664),
                        (221,664),
                        (222,664),
                        (223,664),
                        (224,664),
                        (225,664),
                        (225,665)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      57 => ( Point => ( new Points'(
                        (215,662),
                        (215,663),
                        (216,663),
                        (217,663),
                        (218,663),
                        (219,663),
                        (220,663),
                        (221,663),
                        (222,663),
                        (223,663),
                        (224,663),
                        (225,663)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      58 => ( Point => ( new Points'(
                        (215,661),
                        (216,661),
                        (216,662),
                        (217,662),
                        (218,662),
                        (219,662),
                        (220,662),
                        (221,662),
                        (222,662),
                        (223,662),
                        (224,662),
                        (225,662)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      59 => ( Point => ( new Points'(
                        (215,660),
                        (216,660),
                        (217,660),
                        (217,661),
                        (218,661),
                        (219,661),
                        (220,661),
                        (221,661),
                        (222,661),
                        (223,661),
                        (224,661),
                        (225,661)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      60 => ( Point => ( new Points'(
                        (216,659),
                        (217,659),
                        (218,659),
                        (218,660),
                        (219,660),
                        (220,660),
                        (221,660),
                        (222,660),
                        (223,660),
                        (224,660),
                        (225,660)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      61 => ( Point => ( new Points'(
                        (216,658),
                        (217,658),
                        (218,658),
                        (219,658),
                        (219,659),
                        (220,659),
                        (221,659),
                        (222,659),
                        (223,659),
                        (224,659),
                        (225,659)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      62 => ( Point => ( new Points'(
                        (216,657),
                        (217,657),
                        (218,657),
                        (219,657),
                        (220,657),
                        (220,658),
                        (221,658),
                        (222,658),
                        (223,658),
                        (224,658),
                        (225,658)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      63 => ( Point => ( new Points'(
                        (216,656),
                        (217,656),
                        (218,656),
                        (219,656),
                        (220,656),
                        (221,656),
                        (221,657),
                        (222,657),
                        (223,657),
                        (224,657),
                        (225,657)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      64 => ( Point => ( new Points'(
                        (216,655),
                        (217,655),
                        (218,655),
                        (219,655),
                        (220,655),
                        (221,655),
                        (222,655),
                        (222,656),
                        (223,656),
                        (224,656),
                        (225,656),
                        (226,656)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      65 => ( Point => ( new Points'(
                        (216,654),
                        (217,654),
                        (218,654),
                        (219,654),
                        (220,654),
                        (221,654),
                        (222,654),
                        (223,654),
                        (223,655),
                        (224,655),
                        (225,655),
                        (226,655)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      66 => ( Point => ( new Points'(
                        (216,653),
                        (217,653),
                        (218,653),
                        (219,653),
                        (220,653),
                        (221,653),
                        (222,653),
                        (223,653),
                        (224,653),
                        (224,654),
                        (225,654),
                        (226,654)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      67 => ( Point => ( new Points'(
                        (216,652),
                        (217,652),
                        (218,652),
                        (219,652),
                        (220,652),
                        (221,652),
                        (222,652),
                        (223,652),
                        (224,652),
                        (225,652),
                        (225,653),
                        (226,653)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      68 => ( Point => ( new Points'(
                        (216,650),
                        (216,651),
                        (217,651),
                        (218,651),
                        (219,651),
                        (220,651),
                        (221,651),
                        (222,651),
                        (223,651),
                        (224,651),
                        (225,651),
                        (226,651),
                        (226,652)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      69 => ( Point => ( new Points'(
                        (216,649),
                        (217,649),
                        (217,650),
                        (218,650),
                        (219,650),
                        (220,650),
                        (221,650),
                        (222,650),
                        (223,650),
                        (224,650),
                        (225,650),
                        (226,650)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      70 => ( Point => ( new Points'(
                        (217,648),
                        (218,648),
                        (218,649),
                        (219,649),
                        (220,649),
                        (221,649),
                        (222,649),
                        (223,649),
                        (224,649),
                        (225,649),
                        (226,649)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      71 => ( Point => ( new Points'(
                        (217,647),
                        (218,647),
                        (219,647),
                        (219,648),
                        (220,648),
                        (221,648),
                        (222,648),
                        (223,648),
                        (224,648),
                        (225,648),
                        (226,648)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      72 => ( Point => ( new Points'(
                        (217,646),
                        (218,646),
                        (219,646),
                        (220,646),
                        (220,647),
                        (221,647),
                        (222,647),
                        (223,647),
                        (224,647),
                        (225,647),
                        (226,647)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      73 => ( Point => ( new Points'(
                        (218,645),
                        (219,645),
                        (220,645),
                        (221,645),
                        (221,646),
                        (222,646),
                        (223,646),
                        (224,646),
                        (225,646),
                        (226,646)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      74 => ( Point => ( new Points'(
                        (218,644),
                        (219,644),
                        (220,644),
                        (221,644),
                        (222,644),
                        (222,645),
                        (223,645),
                        (224,645),
                        (225,645),
                        (226,645)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      75 => ( Point => ( new Points'(
                        (218,643),
                        (219,643),
                        (220,643),
                        (221,643),
                        (222,643),
                        (223,643),
                        (223,644),
                        (224,644),
                        (225,644),
                        (226,644)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      76 => ( Point => ( new Points'(
                        (219,642),
                        (220,642),
                        (221,642),
                        (222,642),
                        (223,642),
                        (224,642),
                        (224,643),
                        (225,643),
                        (226,643)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      77 => ( Point => ( new Points'(
                        (219,641),
                        (220,641),
                        (221,641),
                        (222,641),
                        (223,641),
                        (224,641),
                        (225,641),
                        (225,642),
                        (226,642)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      78 => ( Point => ( new Points'(
                        (219,640),
                        (220,640),
                        (221,640),
                        (222,640),
                        (223,640),
                        (224,640),
                        (225,640),
                        (226,640),
                        (226,641)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      79 => ( Point => ( new Points'(
                        (219,639),
                        (220,639),
                        (221,639),
                        (222,639),
                        (223,639),
                        (224,639),
                        (225,639),
                        (226,639),
                        (227,639)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      80 => ( Point => ( new Points'(
                        (219,637),
                        (219,638),
                        (220,638),
                        (221,638),
                        (222,638),
                        (223,638),
                        (224,638),
                        (225,638),
                        (226,638),
                        (227,638)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      81 => ( Point => ( new Points'(
                        (219,636),
                        (220,636),
                        (220,637),
                        (221,637),
                        (222,637),
                        (223,637),
                        (224,637),
                        (225,637),
                        (226,637),
                        (227,637)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      82 => ( Point => ( new Points'(
                        (219,635),
                        (220,635),
                        (221,635),
                        (221,636),
                        (222,636),
                        (223,636),
                        (224,636),
                        (225,636),
                        (226,636),
                        (227,636)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      83 => ( Point => ( new Points'(
                        (219,634),
                        (220,634),
                        (221,634),
                        (222,634),
                        (222,635),
                        (223,635),
                        (224,635),
                        (225,635),
                        (226,635),
                        (227,635),
                        (228,635)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      84 => ( Point => ( new Points'(
                        (219,633),
                        (220,633),
                        (221,633),
                        (222,633),
                        (223,633),
                        (223,634),
                        (224,634),
                        (225,634),
                        (226,634),
                        (227,634),
                        (228,634)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      85 => ( Point => ( new Points'(
                        (219,632),
                        (220,632),
                        (221,632),
                        (222,632),
                        (223,632),
                        (224,632),
                        (224,633),
                        (225,633),
                        (226,633),
                        (227,633),
                        (228,633)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      86 => ( Point => ( new Points'(
                        (219,631),
                        (220,631),
                        (221,631),
                        (222,631),
                        (223,631),
                        (224,631),
                        (225,631),
                        (225,632),
                        (226,632),
                        (227,632),
                        (228,632)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      87 => ( Point => ( new Points'(
                        (219,630),
                        (220,630),
                        (221,630),
                        (222,630),
                        (223,630),
                        (224,630),
                        (225,630),
                        (226,630),
                        (226,631),
                        (227,631),
                        (228,631),
                        (229,631)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      88 => ( Point => ( new Points'(
                        (220,629),
                        (221,629),
                        (222,629),
                        (223,629),
                        (224,629),
                        (225,629),
                        (226,629),
                        (227,629),
                        (227,630),
                        (228,630),
                        (229,630)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      89 => ( Point => ( new Points'(
                        (220,628),
                        (221,628),
                        (222,628),
                        (223,628),
                        (224,628),
                        (225,628),
                        (226,628),
                        (227,628),
                        (228,628),
                        (228,629),
                        (229,629)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      90 => ( Point => ( new Points'(
                        (220,627),
                        (221,627),
                        (222,627),
                        (223,627),
                        (224,627),
                        (225,627),
                        (226,627),
                        (227,627),
                        (228,627),
                        (229,627),
                        (229,628)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      91 => ( Point => ( new Points'(
                        (220,626),
                        (221,626),
                        (222,626),
                        (223,626),
                        (224,626),
                        (225,626),
                        (226,626),
                        (227,626),
                        (228,626),
                        (229,626),
                        (230,626)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      92 => ( Point => ( new Points'(
                        (220,624),
                        (220,625),
                        (221,625),
                        (222,625),
                        (223,625),
                        (224,625),
                        (225,625),
                        (226,625),
                        (227,625),
                        (228,625),
                        (229,625),
                        (230,625)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      93 => ( Point => ( new Points'(
                        (221,623),
                        (221,624),
                        (222,624),
                        (223,624),
                        (224,624),
                        (225,624),
                        (226,624),
                        (227,624),
                        (228,624),
                        (229,624),
                        (230,624)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      94 => ( Point => ( new Points'(
                        (221,622),
                        (222,622),
                        (222,623),
                        (223,623),
                        (224,623),
                        (225,623),
                        (226,623),
                        (227,623),
                        (228,623),
                        (229,623),
                        (230,623)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      95 => ( Point => ( new Points'(
                        (221,621),
                        (222,621),
                        (223,621),
                        (223,622),
                        (224,622),
                        (225,622),
                        (226,622),
                        (227,622),
                        (228,622),
                        (229,622),
                        (230,622)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      96 => ( Point => ( new Points'(
                        (221,620),
                        (222,620),
                        (223,620),
                        (224,620),
                        (224,621),
                        (225,621),
                        (226,621),
                        (227,621),
                        (228,621),
                        (229,621),
                        (230,621)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      97 => ( Point => ( new Points'(
                        (222,619),
                        (223,619),
                        (224,619),
                        (225,619),
                        (225,620),
                        (226,620),
                        (227,620),
                        (228,620),
                        (229,620),
                        (230,620),
                        (231,620)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      98 => ( Point => ( new Points'(
                        (222,618),
                        (223,618),
                        (224,618),
                        (225,618),
                        (226,618),
                        (226,619),
                        (227,619),
                        (228,619),
                        (229,619),
                        (230,619),
                        (231,619)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      99 => ( Point => ( new Points'(
                        (222,617),
                        (223,617),
                        (224,617),
                        (225,617),
                        (226,617),
                        (227,617),
                        (227,618),
                        (228,618),
                        (229,618),
                        (230,618),
                        (231,618)
                       )),
             Threshold_Lower => -1,
             Threshold_Upper => -1,
             Link_Slice_Other_Rail => -1),

      100 => ( Point => ( new Points'(
                         (222,616),
                         (223,616),
                         (224,616),
                         (225,616),
                         (226,616),
                         (227,616),
                         (228,616),
                         (228,617),
                         (229,617),
                         (230,617),
                         (231,617)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      101 => ( Point => ( new Points'(
                         (222,615),
                         (223,615),
                         (224,615),
                         (225,615),
                         (226,615),
                         (227,615),
                         (228,615),
                         (229,615),
                         (229,616),
                         (230,616),
                         (231,616)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      102 => ( Point => ( new Points'(
                         (223,614),
                         (224,614),
                         (225,614),
                         (226,614),
                         (227,614),
                         (228,614),
                         (229,614),
                         (230,614),
                         (230,615),
                         (231,615),
                         (232,615)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      103 => ( Point => ( new Points'(
                         (223,613),
                         (224,613),
                         (225,613),
                         (226,613),
                         (227,613),
                         (228,613),
                         (229,613),
                         (230,613),
                         (231,613),
                         (231,614),
                         (232,614)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      104 => ( Point => ( new Points'(
                         (223,611),
                         (223,612),
                         (224,612),
                         (225,612),
                         (226,612),
                         (227,612),
                         (228,612),
                         (229,612),
                         (230,612),
                         (231,612),
                         (232,612),
                         (232,613)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      105 => ( Point => ( new Points'(
                         (223,610),
                         (224,610),
                         (224,611),
                         (225,611),
                         (226,611),
                         (227,611),
                         (228,611),
                         (229,611),
                         (230,611),
                         (231,611),
                         (232,611)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      106 => ( Point => ( new Points'(
                         (224,609),
                         (225,609),
                         (225,610),
                         (226,610),
                         (227,610),
                         (228,610),
                         (229,610),
                         (230,610),
                         (231,610),
                         (232,610)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      107 => ( Point => ( new Points'(
                         (224,608),
                         (225,608),
                         (226,608),
                         (226,609),
                         (227,609),
                         (228,609),
                         (229,609),
                         (230,609),
                         (231,609),
                         (232,609)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      108 => ( Point => ( new Points'(
                         (224,607),
                         (225,607),
                         (226,607),
                         (227,607),
                         (227,608),
                         (228,608),
                         (229,608),
                         (230,608),
                         (231,608),
                         (232,608),
                         (233,608)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      109 => ( Point => ( new Points'(
                         (224,606),
                         (225,606),
                         (226,606),
                         (227,606),
                         (228,606),
                         (228,607),
                         (229,607),
                         (230,607),
                         (231,607),
                         (232,607),
                         (233,607)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      110 => ( Point => ( new Points'(
                         (224,605),
                         (225,605),
                         (226,605),
                         (227,605),
                         (228,605),
                         (229,605),
                         (229,606),
                         (230,606),
                         (231,606),
                         (232,606),
                         (233,606)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      111 => ( Point => ( new Points'(
                         (224,604),
                         (225,604),
                         (226,604),
                         (227,604),
                         (228,604),
                         (229,604),
                         (230,604),
                         (230,605),
                         (231,605),
                         (232,605),
                         (233,605)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      112 => ( Point => ( new Points'(
                         (225,603),
                         (226,603),
                         (227,603),
                         (228,603),
                         (229,603),
                         (230,603),
                         (231,603),
                         (231,604),
                         (232,604),
                         (233,604)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      113 => ( Point => ( new Points'(
                         (225,602),
                         (226,602),
                         (227,602),
                         (228,602),
                         (229,602),
                         (230,602),
                         (231,602),
                         (232,602),
                         (232,603),
                         (233,603)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      114 => ( Point => ( new Points'(
                         (225,600),
                         (225,601),
                         (226,601),
                         (227,601),
                         (228,601),
                         (229,601),
                         (230,601),
                         (231,601),
                         (232,601),
                         (233,601),
                         (233,602)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      115 => ( Point => ( new Points'(
                         (225,599),
                         (226,599),
                         (226,600),
                         (227,600),
                         (228,600),
                         (229,600),
                         (230,600),
                         (231,600),
                         (232,600),
                         (233,600),
                         (234,600)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      116 => ( Point => ( new Points'(
                         (226,598),
                         (227,598),
                         (227,599),
                         (228,599),
                         (229,599),
                         (230,599),
                         (231,599),
                         (232,599),
                         (233,599),
                         (234,599)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      117 => ( Point => ( new Points'(
                         (226,597),
                         (227,597),
                         (228,597),
                         (228,598),
                         (229,598),
                         (230,598),
                         (231,598),
                         (232,598),
                         (233,598),
                         (234,598)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      118 => ( Point => ( new Points'(
                         (226,596),
                         (227,596),
                         (228,596),
                         (229,596),
                         (229,597),
                         (230,597),
                         (231,597),
                         (232,597),
                         (233,597),
                         (234,597)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      119 => ( Point => ( new Points'(
                         (226,595),
                         (227,595),
                         (228,595),
                         (229,595),
                         (230,595),
                         (230,596),
                         (231,596),
                         (232,596),
                         (233,596),
                         (234,596)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      120 => ( Point => ( new Points'(
                         (226,594),
                         (227,594),
                         (228,594),
                         (229,594),
                         (230,594),
                         (231,594),
                         (231,595),
                         (232,595),
                         (233,595),
                         (234,595)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      121 => ( Point => ( new Points'(
                         (226,593),
                         (227,593),
                         (228,593),
                         (229,593),
                         (230,593),
                         (231,593),
                         (232,593),
                         (232,594),
                         (233,594),
                         (234,594)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      122 => ( Point => ( new Points'(
                         (226,592),
                         (227,592),
                         (228,592),
                         (229,592),
                         (230,592),
                         (231,592),
                         (232,592),
                         (233,592),
                         (233,593),
                         (234,593)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      123 => ( Point => ( new Points'(
                         (226,591),
                         (227,591),
                         (228,591),
                         (229,591),
                         (230,591),
                         (231,591),
                         (232,591),
                         (233,591),
                         (234,591),
                         (234,592)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      124 => ( Point => ( new Points'(
                         (226,589),
                         (226,590),
                         (227,590),
                         (228,590),
                         (229,590),
                         (230,590),
                         (231,590),
                         (232,590),
                         (233,590),
                         (234,590)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      125 => ( Point => ( new Points'(
                         (226,588),
                         (227,588),
                         (227,589),
                         (228,589),
                         (229,589),
                         (230,589),
                         (231,589),
                         (232,589),
                         (233,589),
                         (234,589)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      126 => ( Point => ( new Points'(
                         (226,587),
                         (227,587),
                         (228,587),
                         (228,588),
                         (229,588),
                         (230,588),
                         (231,588),
                         (232,588),
                         (233,588),
                         (234,588)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      127 => ( Point => ( new Points'(
                         (226,586),
                         (227,586),
                         (228,586),
                         (229,586),
                         (229,587),
                         (230,587),
                         (231,587),
                         (232,587),
                         (233,587),
                         (234,587),
                         (235,587)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      128 => ( Point => ( new Points'(
                         (227,585),
                         (228,585),
                         (229,585),
                         (230,585),
                         (230,586),
                         (231,586),
                         (232,586),
                         (233,586),
                         (234,586),
                         (235,586)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      129 => ( Point => ( new Points'(
                         (227,584),
                         (228,584),
                         (229,584),
                         (230,584),
                         (231,584),
                         (231,585),
                         (232,585),
                         (233,585),
                         (234,585),
                         (235,585)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      130 => ( Point => ( new Points'(
                         (227,583),
                         (228,583),
                         (229,583),
                         (230,583),
                         (231,583),
                         (232,583),
                         (232,584),
                         (233,584),
                         (234,584),
                         (235,584)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      131 => ( Point => ( new Points'(
                         (227,582),
                         (228,582),
                         (229,582),
                         (230,582),
                         (231,582),
                         (232,582),
                         (233,582),
                         (233,583),
                         (234,583),
                         (235,583)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      132 => ( Point => ( new Points'(
                         (227,580),
                         (227,581),
                         (228,581),
                         (229,581),
                         (230,581),
                         (231,581),
                         (232,581),
                         (233,581),
                         (234,581),
                         (234,582),
                         (235,582)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      133 => ( Point => ( new Points'(
                         (228,579),
                         (228,580),
                         (229,580),
                         (230,580),
                         (231,580),
                         (232,580),
                         (233,580),
                         (234,580),
                         (235,580),
                         (235,581)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      134 => ( Point => ( new Points'(
                         (228,578),
                         (229,578),
                         (229,579),
                         (230,579),
                         (231,579),
                         (232,579),
                         (233,579),
                         (234,579),
                         (235,579),
                         (236,579)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      135 => ( Point => ( new Points'(
                         (228,577),
                         (229,577),
                         (230,577),
                         (230,578),
                         (231,578),
                         (232,578),
                         (233,578),
                         (234,578),
                         (235,578),
                         (236,578)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      136 => ( Point => ( new Points'(
                         (228,576),
                         (229,576),
                         (230,576),
                         (231,576),
                         (231,577),
                         (232,577),
                         (233,577),
                         (234,577),
                         (235,577),
                         (236,577)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      137 => ( Point => ( new Points'(
                         (229,575),
                         (230,575),
                         (231,575),
                         (232,575),
                         (232,576),
                         (233,576),
                         (234,576),
                         (235,576),
                         (236,576)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      138 => ( Point => ( new Points'(
                         (229,574),
                         (230,574),
                         (231,574),
                         (232,574),
                         (233,574),
                         (233,575),
                         (234,575),
                         (235,575),
                         (236,575)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      139 => ( Point => ( new Points'(
                         (229,573),
                         (230,573),
                         (231,573),
                         (232,573),
                         (233,573),
                         (234,573),
                         (234,574),
                         (235,574),
                         (236,574),
                         (237,574)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      140 => ( Point => ( new Points'(
                         (229,572),
                         (230,572),
                         (231,572),
                         (232,572),
                         (233,572),
                         (234,572),
                         (235,572),
                         (235,573),
                         (236,573),
                         (237,573)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      141 => ( Point => ( new Points'(
                         (229,571),
                         (230,571),
                         (231,571),
                         (232,571),
                         (233,571),
                         (234,571),
                         (235,571),
                         (236,571),
                         (236,572),
                         (237,572)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      142 => ( Point => ( new Points'(
                         (229,570),
                         (230,570),
                         (231,570),
                         (232,570),
                         (233,570),
                         (234,570),
                         (235,570),
                         (236,570),
                         (237,570),
                         (237,571)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      143 => ( Point => ( new Points'(
                         (229,568),
                         (229,569),
                         (230,569),
                         (231,569),
                         (232,569),
                         (233,569),
                         (234,569),
                         (235,569),
                         (236,569),
                         (237,569)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      144 => ( Point => ( new Points'(
                         (230,567),
                         (230,568),
                         (231,568),
                         (232,568),
                         (233,568),
                         (234,568),
                         (235,568),
                         (236,568),
                         (237,568)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      145 => ( Point => ( new Points'(
                         (230,566),
                         (231,566),
                         (231,567),
                         (232,567),
                         (233,567),
                         (234,567),
                         (235,567),
                         (236,567),
                         (237,567),
                         (238,567)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      146 => ( Point => ( new Points'(
                         (230,565),
                         (231,565),
                         (232,565),
                         (232,566),
                         (233,566),
                         (234,566),
                         (235,566),
                         (236,566),
                         (237,566),
                         (238,566)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      147 => ( Point => ( new Points'(
                         (230,564),
                         (231,564),
                         (232,564),
                         (233,564),
                         (233,565),
                         (234,565),
                         (235,565),
                         (236,565),
                         (237,565),
                         (238,565),
                         (239,565)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      148 => ( Point => ( new Points'(
                         (230,563),
                         (231,563),
                         (232,563),
                         (233,563),
                         (234,563),
                         (234,564),
                         (235,564),
                         (236,564),
                         (237,564),
                         (238,564),
                         (239,564)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      149 => ( Point => ( new Points'(
                         (231,562),
                         (232,562),
                         (233,562),
                         (234,562),
                         (235,562),
                         (235,563),
                         (236,563),
                         (237,563),
                         (238,563),
                         (239,563)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      150 => ( Point => ( new Points'(
                         (231,561),
                         (232,561),
                         (233,561),
                         (234,561),
                         (235,561),
                         (236,561),
                         (236,562),
                         (237,562),
                         (238,562),
                         (239,562),
                         (240,562)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      151 => ( Point => ( new Points'(
                         (231,560),
                         (232,560),
                         (233,560),
                         (234,560),
                         (235,560),
                         (236,560),
                         (237,560),
                         (237,561),
                         (238,561),
                         (239,561),
                         (240,561)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      152 => ( Point => ( new Points'(
                         (231,559),
                         (232,559),
                         (233,559),
                         (234,559),
                         (235,559),
                         (236,559),
                         (237,559),
                         (238,559),
                         (238,560),
                         (239,560),
                         (240,560)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      153 => ( Point => ( new Points'(
                         (231,558),
                         (232,558),
                         (233,558),
                         (234,558),
                         (235,558),
                         (236,558),
                         (237,558),
                         (238,558),
                         (239,558),
                         (239,559),
                         (240,559)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      154 => ( Point => ( new Points'(
                         (231,556),
                         (231,557),
                         (232,557),
                         (233,557),
                         (234,557),
                         (235,557),
                         (236,557),
                         (237,557),
                         (238,557),
                         (239,557),
                         (240,557),
                         (240,558)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      155 => ( Point => ( new Points'(
                         (232,555),
                         (232,556),
                         (233,556),
                         (234,556),
                         (235,556),
                         (236,556),
                         (237,556),
                         (238,556),
                         (239,556),
                         (240,556),
                         (241,556)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      156 => ( Point => ( new Points'(
                         (232,554),
                         (233,554),
                         (233,555),
                         (234,555),
                         (235,555),
                         (236,555),
                         (237,555),
                         (238,555),
                         (239,555),
                         (240,555),
                         (241,555)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      157 => ( Point => ( new Points'(
                         (232,553),
                         (233,553),
                         (234,553),
                         (234,554),
                         (235,554),
                         (236,554),
                         (237,554),
                         (238,554),
                         (239,554),
                         (240,554),
                         (241,554),
                         (242,554)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      158 => ( Point => ( new Points'(
                         (232,552),
                         (233,552),
                         (234,552),
                         (235,552),
                         (235,553),
                         (236,553),
                         (237,553),
                         (238,553),
                         (239,553),
                         (240,553),
                         (241,553),
                         (242,553)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      159 => ( Point => ( new Points'(
                         (232,551),
                         (233,551),
                         (234,551),
                         (235,551),
                         (236,551),
                         (236,552),
                         (237,552),
                         (238,552),
                         (239,552),
                         (240,552),
                         (241,552),
                         (242,552)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      160 => ( Point => ( new Points'(
                         (232,550),
                         (233,550),
                         (234,550),
                         (235,550),
                         (236,550),
                         (237,550),
                         (237,551),
                         (238,551),
                         (239,551),
                         (240,551),
                         (241,551),
                         (242,551)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      161 => ( Point => ( new Points'(
                         (232,549),
                         (233,549),
                         (234,549),
                         (235,549),
                         (236,549),
                         (237,549),
                         (238,549),
                         (238,550),
                         (239,550),
                         (240,550),
                         (241,550),
                         (242,550),
                         (243,550)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      162 => ( Point => ( new Points'(
                         (233,548),
                         (234,548),
                         (235,548),
                         (236,548),
                         (237,548),
                         (238,548),
                         (239,548),
                         (239,549),
                         (240,549),
                         (241,549),
                         (242,549),
                         (243,549)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      163 => ( Point => ( new Points'(
                         (233,547),
                         (234,547),
                         (235,547),
                         (236,547),
                         (237,547),
                         (238,547),
                         (239,547),
                         (240,547),
                         (240,548),
                         (241,548),
                         (242,548),
                         (243,548)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      164 => ( Point => ( new Points'(
                         (233,546),
                         (234,546),
                         (235,546),
                         (236,546),
                         (237,546),
                         (238,546),
                         (239,546),
                         (240,546),
                         (241,546),
                         (241,547),
                         (242,547),
                         (243,547)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      165 => ( Point => ( new Points'(
                         (233,545),
                         (234,545),
                         (235,545),
                         (236,545),
                         (237,545),
                         (238,545),
                         (239,545),
                         (240,545),
                         (241,545),
                         (242,545),
                         (242,546),
                         (243,546)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      166 => ( Point => ( new Points'(
                         (234,544),
                         (235,544),
                         (236,544),
                         (237,544),
                         (238,544),
                         (239,544),
                         (240,544),
                         (241,544),
                         (242,544),
                         (243,544),
                         (243,545)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      167 => ( Point => ( new Points'(
                         (234,542),
                         (234,543),
                         (235,543),
                         (236,543),
                         (237,543),
                         (238,543),
                         (239,543),
                         (240,543),
                         (241,543),
                         (242,543),
                         (243,543)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      168 => ( Point => ( new Points'(
                         (234,541),
                         (235,541),
                         (235,542),
                         (236,542),
                         (237,542),
                         (238,542),
                         (239,542),
                         (240,542),
                         (241,542),
                         (242,542),
                         (243,542)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      169 => ( Point => ( new Points'(
                         (234,540),
                         (235,540),
                         (236,540),
                         (236,541),
                         (237,541),
                         (238,541),
                         (239,541),
                         (240,541),
                         (241,541),
                         (242,541),
                         (243,541),
                         (244,541)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      170 => ( Point => ( new Points'(
                         (235,539),
                         (236,539),
                         (237,539),
                         (237,540),
                         (238,540),
                         (239,540),
                         (240,540),
                         (241,540),
                         (242,540),
                         (243,540),
                         (244,540)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      171 => ( Point => ( new Points'(
                         (235,538),
                         (236,538),
                         (237,538),
                         (238,538),
                         (238,539),
                         (239,539),
                         (240,539),
                         (241,539),
                         (242,539),
                         (243,539),
                         (244,539)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      172 => ( Point => ( new Points'(
                         (235,537),
                         (236,537),
                         (237,537),
                         (238,537),
                         (239,537),
                         (239,538),
                         (240,538),
                         (241,538),
                         (242,538),
                         (243,538),
                         (244,538)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      173 => ( Point => ( new Points'(
                         (235,536),
                         (236,536),
                         (237,536),
                         (238,536),
                         (239,536),
                         (240,536),
                         (240,537),
                         (241,537),
                         (242,537),
                         (243,537),
                         (244,537)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      174 => ( Point => ( new Points'(
                         (235,535),
                         (236,535),
                         (237,535),
                         (238,535),
                         (239,535),
                         (240,535),
                         (241,535),
                         (241,536),
                         (242,536),
                         (243,536),
                         (244,536)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      175 => ( Point => ( new Points'(
                         (235,534),
                         (236,534),
                         (237,534),
                         (238,534),
                         (239,534),
                         (240,534),
                         (241,534),
                         (242,534),
                         (242,535),
                         (243,535),
                         (244,535)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      176 => ( Point => ( new Points'(
                         (236,533),
                         (237,533),
                         (238,533),
                         (239,533),
                         (240,533),
                         (241,533),
                         (242,533),
                         (243,533),
                         (243,534),
                         (244,534)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      177 => ( Point => ( new Points'(
                         (236,532),
                         (237,532),
                         (238,532),
                         (239,532),
                         (240,532),
                         (241,532),
                         (242,532),
                         (243,532),
                         (244,532),
                         (244,533)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      178 => ( Point => ( new Points'(
                         (236,530),
                         (236,531),
                         (237,531),
                         (238,531),
                         (239,531),
                         (240,531),
                         (241,531),
                         (242,531),
                         (243,531),
                         (244,531),
                         (245,531)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      179 => ( Point => ( new Points'(
                         (236,529),
                         (237,529),
                         (237,530),
                         (238,530),
                         (239,530),
                         (240,530),
                         (241,530),
                         (242,530),
                         (243,530),
                         (244,530),
                         (245,530)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      180 => ( Point => ( new Points'(
                         (236,528),
                         (237,528),
                         (238,528),
                         (238,529),
                         (239,529),
                         (240,529),
                         (241,529),
                         (242,529),
                         (243,529),
                         (244,529),
                         (245,529)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      181 => ( Point => ( new Points'(
                         (236,527),
                         (237,527),
                         (238,527),
                         (239,527),
                         (239,528),
                         (240,528),
                         (241,528),
                         (242,528),
                         (243,528),
                         (244,528),
                         (245,528)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      182 => ( Point => ( new Points'(
                         (236,526),
                         (237,526),
                         (238,526),
                         (239,526),
                         (240,526),
                         (240,527),
                         (241,527),
                         (242,527),
                         (243,527),
                         (244,527),
                         (245,527)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      183 => ( Point => ( new Points'(
                         (237,525),
                         (238,525),
                         (239,525),
                         (240,525),
                         (241,525),
                         (241,526),
                         (242,526),
                         (243,526),
                         (244,526),
                         (245,526)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      184 => ( Point => ( new Points'(
                         (237,524),
                         (238,524),
                         (239,524),
                         (240,524),
                         (241,524),
                         (242,524),
                         (242,525),
                         (243,525),
                         (244,525),
                         (245,525),
                         (246,525)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      185 => ( Point => ( new Points'(
                         (237,523),
                         (238,523),
                         (239,523),
                         (240,523),
                         (241,523),
                         (242,523),
                         (243,523),
                         (243,524),
                         (244,524),
                         (245,524),
                         (246,524)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      186 => ( Point => ( new Points'(
                         (237,522),
                         (238,522),
                         (239,522),
                         (240,522),
                         (241,522),
                         (242,522),
                         (243,522),
                         (244,522),
                         (244,523),
                         (245,523),
                         (246,523)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      187 => ( Point => ( new Points'(
                         (237,521),
                         (238,521),
                         (239,521),
                         (240,521),
                         (241,521),
                         (242,521),
                         (243,521),
                         (244,521),
                         (245,521),
                         (245,522),
                         (246,522)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      188 => ( Point => ( new Points'(
                         (238,520),
                         (239,520),
                         (240,520),
                         (241,520),
                         (242,520),
                         (243,520),
                         (244,520),
                         (245,520),
                         (246,520),
                         (246,521)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      189 => ( Point => ( new Points'(
                         (238,519),
                         (239,519),
                         (240,519),
                         (241,519),
                         (242,519),
                         (243,519),
                         (244,519),
                         (245,519),
                         (246,519)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      190 => ( Point => ( new Points'(
                         (238,517),
                         (238,518),
                         (239,518),
                         (240,518),
                         (241,518),
                         (242,518),
                         (243,518),
                         (244,518),
                         (245,518),
                         (246,518)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      191 => ( Point => ( new Points'(
                         (238,516),
                         (239,516),
                         (239,517),
                         (240,517),
                         (241,517),
                         (242,517),
                         (243,517),
                         (244,517),
                         (245,517),
                         (246,517)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      192 => ( Point => ( new Points'(
                         (238,515),
                         (239,515),
                         (240,515),
                         (240,516),
                         (241,516),
                         (242,516),
                         (243,516),
                         (244,516),
                         (245,516),
                         (246,516),
                         (247,516)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      193 => ( Point => ( new Points'(
                         (238,514),
                         (239,514),
                         (240,514),
                         (241,514),
                         (241,515),
                         (242,515),
                         (243,515),
                         (244,515),
                         (245,515),
                         (246,515),
                         (247,515)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      194 => ( Point => ( new Points'(
                         (239,513),
                         (240,513),
                         (241,513),
                         (242,513),
                         (242,514),
                         (243,514),
                         (244,514),
                         (245,514),
                         (246,514),
                         (247,514)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      195 => ( Point => ( new Points'(
                         (239,512),
                         (240,512),
                         (241,512),
                         (242,512),
                         (243,512),
                         (243,513),
                         (244,513),
                         (245,513),
                         (246,513),
                         (247,513)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      196 => ( Point => ( new Points'(
                         (239,511),
                         (240,511),
                         (241,511),
                         (242,511),
                         (243,511),
                         (244,511),
                         (244,512),
                         (245,512),
                         (246,512),
                         (247,512)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      197 => ( Point => ( new Points'(
                         (239,510),
                         (240,510),
                         (241,510),
                         (242,510),
                         (243,510),
                         (244,510),
                         (245,510),
                         (245,511),
                         (246,511),
                         (247,511)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      198 => ( Point => ( new Points'(
                         (239,509),
                         (240,509),
                         (241,509),
                         (242,509),
                         (243,509),
                         (244,509),
                         (245,509),
                         (246,509),
                         (246,510),
                         (247,510),
                         (248,510)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      199 => ( Point => ( new Points'(
                         (239,508),
                         (240,508),
                         (241,508),
                         (242,508),
                         (243,508),
                         (244,508),
                         (245,508),
                         (246,508),
                         (247,508),
                         (247,509),
                         (248,509)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      200 => ( Point => ( new Points'(
                         (239,506),
                         (239,507),
                         (240,507),
                         (241,507),
                         (242,507),
                         (243,507),
                         (244,507),
                         (245,507),
                         (246,507),
                         (247,507),
                         (248,507),
                         (248,508)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      201 => ( Point => ( new Points'(
                         (240,505),
                         (240,506),
                         (241,506),
                         (242,506),
                         (243,506),
                         (244,506),
                         (245,506),
                         (246,506),
                         (247,506),
                         (248,506)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      202 => ( Point => ( new Points'(
                         (240,504),
                         (241,504),
                         (241,505),
                         (242,505),
                         (243,505),
                         (244,505),
                         (245,505),
                         (246,505),
                         (247,505),
                         (248,505)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      203 => ( Point => ( new Points'(
                         (240,503),
                         (241,503),
                         (242,503),
                         (242,504),
                         (243,504),
                         (244,504),
                         (245,504),
                         (246,504),
                         (247,504),
                         (248,504)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      204 => ( Point => ( new Points'(
                         (240,502),
                         (241,502),
                         (242,502),
                         (243,502),
                         (243,503),
                         (244,503),
                         (245,503),
                         (246,503),
                         (247,503),
                         (248,503)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      205 => ( Point => ( new Points'(
                         (240,501),
                         (241,501),
                         (242,501),
                         (243,501),
                         (244,501),
                         (244,502),
                         (245,502),
                         (246,502),
                         (247,502),
                         (248,502),
                         (249,502)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      206 => ( Point => ( new Points'(
                         (241,500),
                         (242,500),
                         (243,500),
                         (244,500),
                         (245,500),
                         (245,501),
                         (246,501),
                         (247,501),
                         (248,501),
                         (249,501)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      207 => ( Point => ( new Points'(
                         (241,499),
                         (242,499),
                         (243,499),
                         (244,499),
                         (245,499),
                         (246,499),
                         (246,500),
                         (247,500),
                         (248,500),
                         (249,500)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      208 => ( Point => ( new Points'(
                         (241,498),
                         (242,498),
                         (243,498),
                         (244,498),
                         (245,498),
                         (246,498),
                         (247,498),
                         (247,499),
                         (248,499),
                         (249,499)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      209 => ( Point => ( new Points'(
                         (241,497),
                         (242,497),
                         (243,497),
                         (244,497),
                         (245,497),
                         (246,497),
                         (247,497),
                         (248,497),
                         (248,498),
                         (249,498)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      210 => ( Point => ( new Points'(
                         (242,496),
                         (243,496),
                         (244,496),
                         (245,496),
                         (246,496),
                         (247,496),
                         (248,496),
                         (249,496),
                         (249,497),
                         (250,497)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      211 => ( Point => ( new Points'(
                         (242,495),
                         (243,495),
                         (244,495),
                         (245,495),
                         (246,495),
                         (247,495),
                         (248,495),
                         (249,495),
                         (250,495),
                         (250,496)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      212 => ( Point => ( new Points'(
                         (242,494),
                         (243,494),
                         (244,494),
                         (245,494),
                         (246,494),
                         (247,494),
                         (248,494),
                         (249,494),
                         (250,494)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      213 => ( Point => ( new Points'(
                         (242,493),
                         (243,493),
                         (244,493),
                         (245,493),
                         (246,493),
                         (247,493),
                         (248,493),
                         (249,493),
                         (250,493)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      214 => ( Point => ( new Points'(
                         (242,491),
                         (242,492),
                         (243,492),
                         (244,492),
                         (245,492),
                         (246,492),
                         (247,492),
                         (248,492),
                         (249,492),
                         (250,492)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      215 => ( Point => ( new Points'(
                         (242,490),
                         (243,490),
                         (243,491),
                         (244,491),
                         (245,491),
                         (246,491),
                         (247,491),
                         (248,491),
                         (249,491),
                         (250,491)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      216 => ( Point => ( new Points'(
                         (243,489),
                         (244,489),
                         (244,490),
                         (245,490),
                         (246,490),
                         (247,490),
                         (248,490),
                         (249,490),
                         (250,490)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      217 => ( Point => ( new Points'(
                         (243,488),
                         (244,488),
                         (245,488),
                         (245,489),
                         (246,489),
                         (247,489),
                         (248,489),
                         (249,489),
                         (250,489)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      218 => ( Point => ( new Points'(
                         (243,487),
                         (244,487),
                         (245,487),
                         (246,487),
                         (246,488),
                         (247,488),
                         (248,488),
                         (249,488),
                         (250,488)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      219 => ( Point => ( new Points'(
                         (243,486),
                         (244,486),
                         (245,486),
                         (246,486),
                         (247,486),
                         (247,487),
                         (248,487),
                         (249,487),
                         (250,487),
                         (251,487)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      220 => ( Point => ( new Points'(
                         (243,485),
                         (244,485),
                         (245,485),
                         (246,485),
                         (247,485),
                         (248,485),
                         (248,486),
                         (249,486),
                         (250,486),
                         (251,486)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      221 => ( Point => ( new Points'(
                         (244,484),
                         (245,484),
                         (246,484),
                         (247,484),
                         (248,484),
                         (249,484),
                         (249,485),
                         (250,485),
                         (251,485)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      222 => ( Point => ( new Points'(
                         (244,483),
                         (245,483),
                         (246,483),
                         (247,483),
                         (248,483),
                         (249,483),
                         (250,483),
                         (250,484),
                         (251,484)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      223 => ( Point => ( new Points'(
                         (244,482),
                         (245,482),
                         (246,482),
                         (247,482),
                         (248,482),
                         (249,482),
                         (250,482),
                         (251,482),
                         (251,483)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      224 => ( Point => ( new Points'(
                         (244,480),
                         (244,481),
                         (245,481),
                         (246,481),
                         (247,481),
                         (248,481),
                         (249,481),
                         (250,481),
                         (251,481)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      225 => ( Point => ( new Points'(
                         (244,479),
                         (245,479),
                         (245,480),
                         (246,480),
                         (247,480),
                         (248,480),
                         (249,480),
                         (250,480),
                         (251,480)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      226 => ( Point => ( new Points'(
                         (244,478),
                         (245,478),
                         (246,478),
                         (246,479),
                         (247,479),
                         (248,479),
                         (249,479),
                         (250,479),
                         (251,479)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      227 => ( Point => ( new Points'(
                         (244,477),
                         (245,477),
                         (246,477),
                         (247,477),
                         (247,478),
                         (248,478),
                         (249,478),
                         (250,478),
                         (251,478),
                         (252,478)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      228 => ( Point => ( new Points'(
                         (245,476),
                         (246,476),
                         (247,476),
                         (248,476),
                         (248,477),
                         (249,477),
                         (250,477),
                         (251,477),
                         (252,477)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      229 => ( Point => ( new Points'(
                         (245,475),
                         (246,475),
                         (247,475),
                         (248,475),
                         (249,475),
                         (249,476),
                         (250,476),
                         (251,476),
                         (252,476)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      230 => ( Point => ( new Points'(
                         (245,474),
                         (246,474),
                         (247,474),
                         (248,474),
                         (249,474),
                         (250,474),
                         (250,475),
                         (251,475),
                         (252,475)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      231 => ( Point => ( new Points'(
                         (245,473),
                         (246,473),
                         (247,473),
                         (248,473),
                         (249,473),
                         (250,473),
                         (251,473),
                         (251,474),
                         (252,474)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      232 => ( Point => ( new Points'(
                         (245,472),
                         (246,472),
                         (247,472),
                         (248,472),
                         (249,472),
                         (250,472),
                         (251,472),
                         (252,472),
                         (252,473)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      233 => ( Point => ( new Points'(
                         (245,470),
                         (245,471),
                         (246,471),
                         (247,471),
                         (248,471),
                         (249,471),
                         (250,471),
                         (251,471),
                         (252,471),
                         (253,471)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      234 => ( Point => ( new Points'(
                         (246,469),
                         (246,470),
                         (247,470),
                         (248,470),
                         (249,470),
                         (250,470),
                         (251,470),
                         (252,470),
                         (253,470)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      235 => ( Point => ( new Points'(
                         (246,468),
                         (247,468),
                         (247,469),
                         (248,469),
                         (249,469),
                         (250,469),
                         (251,469),
                         (252,469),
                         (253,469)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      236 => ( Point => ( new Points'(
                         (246,467),
                         (247,467),
                         (248,467),
                         (248,468),
                         (249,468),
                         (250,468),
                         (251,468),
                         (252,468),
                         (253,468),
                         (254,468)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      237 => ( Point => ( new Points'(
                         (246,466),
                         (247,466),
                         (248,466),
                         (249,466),
                         (249,467),
                         (250,467),
                         (251,467),
                         (252,467),
                         (253,467),
                         (254,467)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      238 => ( Point => ( new Points'(
                         (246,465),
                         (247,465),
                         (248,465),
                         (249,465),
                         (250,465),
                         (250,466),
                         (251,466),
                         (252,466),
                         (253,466),
                         (254,466)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      239 => ( Point => ( new Points'(
                         (247,464),
                         (248,464),
                         (249,464),
                         (250,464),
                         (251,464),
                         (251,465),
                         (252,465),
                         (253,465),
                         (254,465)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      240 => ( Point => ( new Points'(
                         (247,463),
                         (248,463),
                         (249,463),
                         (250,463),
                         (251,463),
                         (252,463),
                         (252,464),
                         (253,464),
                         (254,464),
                         (255,464)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      241 => ( Point => ( new Points'(
                         (247,462),
                         (248,462),
                         (249,462),
                         (250,462),
                         (251,462),
                         (252,462),
                         (253,462),
                         (253,463),
                         (254,463),
                         (255,463)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      242 => ( Point => ( new Points'(
                         (247,461),
                         (248,461),
                         (249,461),
                         (250,461),
                         (251,461),
                         (252,461),
                         (253,461),
                         (254,461),
                         (254,462),
                         (255,462)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      243 => ( Point => ( new Points'(
                         (247,460),
                         (248,460),
                         (249,460),
                         (250,460),
                         (251,460),
                         (252,460),
                         (253,460),
                         (254,460),
                         (255,460),
                         (255,461)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      244 => ( Point => ( new Points'(
                         (248,459),
                         (249,459),
                         (250,459),
                         (251,459),
                         (252,459),
                         (253,459),
                         (254,459),
                         (255,459)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      245 => ( Point => ( new Points'(
                         (248,457),
                         (248,458),
                         (249,458),
                         (250,458),
                         (251,458),
                         (252,458),
                         (253,458),
                         (254,458),
                         (255,458)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      246 => ( Point => ( new Points'(
                         (249,456),
                         (249,457),
                         (250,457),
                         (251,457),
                         (252,457),
                         (253,457),
                         (254,457),
                         (255,457)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      247 => ( Point => ( new Points'(
                         (249,455),
                         (250,455),
                         (250,456),
                         (251,456),
                         (252,456),
                         (253,456),
                         (254,456),
                         (255,456)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      248 => ( Point => ( new Points'(
                         (249,454),
                         (250,454),
                         (251,454),
                         (251,455),
                         (252,455),
                         (253,455),
                         (254,455),
                         (255,455)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      249 => ( Point => ( new Points'(
                         (249,453),
                         (250,453),
                         (251,453),
                         (252,453),
                         (252,454),
                         (253,454),
                         (254,454),
                         (255,454)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      250 => ( Point => ( new Points'(
                         (249,452),
                         (250,452),
                         (251,452),
                         (252,452),
                         (253,452),
                         (253,453),
                         (254,453),
                         (255,453)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      251 => ( Point => ( new Points'(
                         (249,451),
                         (250,451),
                         (251,451),
                         (252,451),
                         (253,451),
                         (254,451),
                         (254,452),
                         (255,452)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      252 => ( Point => ( new Points'(
                         (249,450),
                         (250,450),
                         (251,450),
                         (252,450),
                         (253,450),
                         (254,450),
                         (255,450),
                         (255,451),
                         (256,451)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      253 => ( Point => ( new Points'(
                         (249,448),
                         (249,449),
                         (250,449),
                         (251,449),
                         (252,449),
                         (253,449),
                         (254,449),
                         (255,449),
                         (256,449),
                         (256,450)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      254 => ( Point => ( new Points'(
                         (249,447),
                         (250,447),
                         (250,448),
                         (251,448),
                         (252,448),
                         (253,448),
                         (254,448),
                         (255,448),
                         (256,448)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      255 => ( Point => ( new Points'(
                         (249,446),
                         (250,446),
                         (251,446),
                         (251,447),
                         (252,447),
                         (253,447),
                         (254,447),
                         (255,447),
                         (256,447)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      256 => ( Point => ( new Points'(
                         (250,445),
                         (251,445),
                         (252,445),
                         (252,446),
                         (253,446),
                         (254,446),
                         (255,446),
                         (256,446)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      257 => ( Point => ( new Points'(
                         (250,444),
                         (251,444),
                         (252,444),
                         (253,444),
                         (253,445),
                         (254,445),
                         (255,445),
                         (256,445),
                         (257,445)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      258 => ( Point => ( new Points'(
                         (250,443),
                         (251,443),
                         (252,443),
                         (253,443),
                         (254,443),
                         (254,444),
                         (255,444),
                         (256,444),
                         (257,444)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      259 => ( Point => ( new Points'(
                         (251,442),
                         (252,442),
                         (253,442),
                         (254,442),
                         (255,442),
                         (255,443),
                         (256,443),
                         (257,443)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      260 => ( Point => ( new Points'(
                         (251,441),
                         (252,441),
                         (253,441),
                         (254,441),
                         (255,441),
                         (256,441),
                         (256,442),
                         (257,442)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      261 => ( Point => ( new Points'(
                         (251,440),
                         (252,440),
                         (253,440),
                         (254,440),
                         (255,440),
                         (256,440),
                         (257,440),
                         (257,441)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      262 => ( Point => ( new Points'(
                         (251,439),
                         (252,439),
                         (253,439),
                         (254,439),
                         (255,439),
                         (256,439),
                         (257,439),
                         (258,439)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      263 => ( Point => ( new Points'(
                         (251,437),
                         (251,438),
                         (252,438),
                         (253,438),
                         (254,438),
                         (255,438),
                         (256,438),
                         (257,438),
                         (258,438)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      264 => ( Point => ( new Points'(
                         (252,436),
                         (252,437),
                         (253,437),
                         (254,437),
                         (255,437),
                         (256,437),
                         (257,437),
                         (258,437)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      265 => ( Point => ( new Points'(
                         (252,435),
                         (253,435),
                         (253,436),
                         (254,436),
                         (255,436),
                         (256,436),
                         (257,436),
                         (258,436)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      266 => ( Point => ( new Points'(
                         (252,434),
                         (253,434),
                         (254,434),
                         (254,435),
                         (255,435),
                         (256,435),
                         (257,435),
                         (258,435)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      267 => ( Point => ( new Points'(
                         (252,433),
                         (253,433),
                         (254,433),
                         (255,433),
                         (255,434),
                         (256,434),
                         (257,434),
                         (258,434)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      268 => ( Point => ( new Points'(
                         (252,432),
                         (253,432),
                         (254,432),
                         (255,432),
                         (256,432),
                         (256,433),
                         (257,433),
                         (258,433)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      269 => ( Point => ( new Points'(
                         (252,431),
                         (253,431),
                         (254,431),
                         (255,431),
                         (256,431),
                         (257,431),
                         (257,432),
                         (258,432),
                         (259,432)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      270 => ( Point => ( new Points'(
                         (252,430),
                         (253,430),
                         (254,430),
                         (255,430),
                         (256,430),
                         (257,430),
                         (258,430),
                         (258,431),
                         (259,431)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      271 => ( Point => ( new Points'(
                         (252,429),
                         (253,429),
                         (254,429),
                         (255,429),
                         (256,429),
                         (257,429),
                         (258,429),
                         (259,429),
                         (259,430)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      272 => ( Point => ( new Points'(
                         (252,427),
                         (252,428),
                         (253,428),
                         (254,428),
                         (255,428),
                         (256,428),
                         (257,428),
                         (258,428),
                         (259,428)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      273 => ( Point => ( new Points'(
                         (252,426),
                         (253,426),
                         (253,427),
                         (254,427),
                         (255,427),
                         (256,427),
                         (257,427),
                         (258,427),
                         (259,427)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      274 => ( Point => ( new Points'(
                         (252,425),
                         (253,425),
                         (254,425),
                         (254,426),
                         (255,426),
                         (256,426),
                         (257,426),
                         (258,426),
                         (259,426)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      275 => ( Point => ( new Points'(
                         (253,424),
                         (254,424),
                         (255,424),
                         (255,425),
                         (256,425),
                         (257,425),
                         (258,425),
                         (259,425),
                         (260,425)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      276 => ( Point => ( new Points'(
                         (253,423),
                         (254,423),
                         (255,423),
                         (256,423),
                         (256,424),
                         (257,424),
                         (258,424),
                         (259,424),
                         (260,424)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      277 => ( Point => ( new Points'(
                         (253,422),
                         (254,422),
                         (255,422),
                         (256,422),
                         (257,422),
                         (257,423),
                         (258,423),
                         (259,423),
                         (260,423)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      278 => ( Point => ( new Points'(
                         (254,421),
                         (255,421),
                         (256,421),
                         (257,421),
                         (258,421),
                         (258,422),
                         (259,422),
                         (260,422)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      279 => ( Point => ( new Points'(
                         (254,420),
                         (255,420),
                         (256,420),
                         (257,420),
                         (258,420),
                         (259,420),
                         (259,421),
                         (260,421)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      280 => ( Point => ( new Points'(
                         (254,419),
                         (255,419),
                         (256,419),
                         (257,419),
                         (258,419),
                         (259,419),
                         (260,419),
                         (260,420)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      281 => ( Point => ( new Points'(
                         (254,418),
                         (255,418),
                         (256,418),
                         (257,418),
                         (258,418),
                         (259,418),
                         (260,418),
                         (261,418)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      282 => ( Point => ( new Points'(
                         (254,416),
                         (254,417),
                         (255,417),
                         (256,417),
                         (257,417),
                         (258,417),
                         (259,417),
                         (260,417),
                         (261,417)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      283 => ( Point => ( new Points'(
                         (254,415),
                         (255,415),
                         (255,416),
                         (256,416),
                         (257,416),
                         (258,416),
                         (259,416),
                         (260,416),
                         (261,416)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      284 => ( Point => ( new Points'(
                         (255,414),
                         (256,414),
                         (256,415),
                         (257,415),
                         (258,415),
                         (259,415),
                         (260,415),
                         (261,415)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      285 => ( Point => ( new Points'(
                         (255,413),
                         (256,413),
                         (257,413),
                         (257,414),
                         (258,414),
                         (259,414),
                         (260,414),
                         (261,414)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      286 => ( Point => ( new Points'(
                         (255,412),
                         (256,412),
                         (257,412),
                         (258,412),
                         (258,413),
                         (259,413),
                         (260,413),
                         (261,413),
                         (262,413)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      287 => ( Point => ( new Points'(
                         (256,411),
                         (257,411),
                         (258,411),
                         (259,411),
                         (259,412),
                         (260,412),
                         (261,412),
                         (262,412)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      288 => ( Point => ( new Points'(
                         (256,410),
                         (257,410),
                         (258,410),
                         (259,410),
                         (260,410),
                         (260,411),
                         (261,411),
                         (262,411)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      289 => ( Point => ( new Points'(
                         (256,409),
                         (257,409),
                         (258,409),
                         (259,409),
                         (260,409),
                         (261,409),
                         (261,410),
                         (262,410)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      290 => ( Point => ( new Points'(
                         (256,408),
                         (257,408),
                         (258,408),
                         (259,408),
                         (260,408),
                         (261,408),
                         (262,408),
                         (262,409)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      291 => ( Point => ( new Points'(
                         (256,406),
                         (256,407),
                         (257,407),
                         (258,407),
                         (259,407),
                         (260,407),
                         (261,407),
                         (262,407)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      292 => ( Point => ( new Points'(
                         (256,405),
                         (257,405),
                         (257,406),
                         (258,406),
                         (259,406),
                         (260,406),
                         (261,406),
                         (262,406),
                         (263,406)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      293 => ( Point => ( new Points'(
                         (256,404),
                         (257,404),
                         (258,404),
                         (258,405),
                         (259,405),
                         (260,405),
                         (261,405),
                         (262,405),
                         (263,405)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      294 => ( Point => ( new Points'(
                         (256,403),
                         (257,403),
                         (258,403),
                         (259,403),
                         (259,404),
                         (260,404),
                         (261,404),
                         (262,404),
                         (263,404)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      295 => ( Point => ( new Points'(
                         (257,402),
                         (258,402),
                         (259,402),
                         (260,402),
                         (260,403),
                         (261,403),
                         (262,403),
                         (263,403)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      296 => ( Point => ( new Points'(
                         (257,401),
                         (258,401),
                         (259,401),
                         (260,401),
                         (261,401),
                         (261,402),
                         (262,402),
                         (263,402)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      297 => ( Point => ( new Points'(
                         (257,400),
                         (258,400),
                         (259,400),
                         (260,400),
                         (261,400),
                         (262,400),
                         (262,401),
                         (263,401)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      298 => ( Point => ( new Points'(
                         (257,399),
                         (258,399),
                         (259,399),
                         (260,399),
                         (261,399),
                         (262,399),
                         (263,399),
                         (263,400)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      299 => ( Point => ( new Points'(
                         (257,397),
                         (257,398),
                         (258,398),
                         (259,398),
                         (260,398),
                         (261,398),
                         (262,398),
                         (263,398)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      300 => ( Point => ( new Points'(
                         (257,396),
                         (258,396),
                         (258,397),
                         (259,397),
                         (260,397),
                         (261,397),
                         (262,397),
                         (263,397),
                         (264,397)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      301 => ( Point => ( new Points'(
                         (258,395),
                         (259,395),
                         (259,396),
                         (260,396),
                         (261,396),
                         (262,396),
                         (263,396),
                         (264,396)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      302 => ( Point => ( new Points'(
                         (258,394),
                         (259,394),
                         (260,394),
                         (260,395),
                         (261,395),
                         (262,395),
                         (263,395),
                         (264,395)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      303 => ( Point => ( new Points'(
                         (258,393),
                         (259,393),
                         (260,393),
                         (261,393),
                         (261,394),
                         (262,394),
                         (263,394),
                         (264,394)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      304 => ( Point => ( new Points'(
                         (258,392),
                         (259,392),
                         (260,392),
                         (261,392),
                         (262,392),
                         (262,393),
                         (263,393),
                         (264,393)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      305 => ( Point => ( new Points'(
                         (258,391),
                         (259,391),
                         (260,391),
                         (261,391),
                         (262,391),
                         (263,391),
                         (263,392),
                         (264,392)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      306 => ( Point => ( new Points'(
                         (258,390),
                         (259,390),
                         (260,390),
                         (261,390),
                         (262,390),
                         (263,390),
                         (264,390),
                         (264,391)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      307 => ( Point => ( new Points'(
                         (258,388),
                         (258,389),
                         (259,389),
                         (260,389),
                         (261,389),
                         (262,389),
                         (263,389),
                         (264,389)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      308 => ( Point => ( new Points'(
                         (259,387),
                         (259,388),
                         (260,388),
                         (261,388),
                         (262,388),
                         (263,388),
                         (264,388),
                         (265,388)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      309 => ( Point => ( new Points'(
                         (259,386),
                         (260,386),
                         (260,387),
                         (261,387),
                         (262,387),
                         (263,387),
                         (264,387),
                         (265,387)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      310 => ( Point => ( new Points'(
                         (259,385),
                         (260,385),
                         (261,386),
                         (261,386),
                         (262,386),
                         (263,386),
                         (264,386),
                         (265,386)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      311 => ( Point => ( new Points'(
                         (260,384),
                         (261,384),
                         (261,385),
                         (262,385),
                         (263,385),
                         (264,385),
                         (265,385)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      312 => ( Point => ( new Points'(
                         (260,383),
                         (261,383),
                         (262,383),
                         (262,384),
                         (263,384),
                         (264,384),
                         (265,384)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      313 => ( Point => ( new Points'(
                         (260,382),
                         (261,382),
                         (262,382),
                         (263,382),
                         (263,383),
                         (264,383),
                         (265,383),
                         (266,383)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      314 => ( Point => ( new Points'(
                         (261,381),
                         (262,381),
                         (263,381),
                         (264,381),
                         (264,382),
                         (265,382),
                         (266,382)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      315 => ( Point => ( new Points'(
                         (261,380),
                         (262,380),
                         (263,380),
                         (264,380),
                         (265,380),
                         (265,381),
                         (266,381)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      316 => ( Point => ( new Points'(
                         (261,379),
                         (262,379),
                         (263,379),
                         (264,379),
                         (265,379),
                         (266,379),
                         (266,380),
                         (267,380)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      317 => ( Point => ( new Points'(
                         (261,378),
                         (262,378),
                         (263,378),
                         (264,378),
                         (265,378),
                         (266,378),
                         (267,378),
                         (267,379)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      318 => ( Point => ( new Points'(
                         (261,376),
                         (261,377),
                         (262,377),
                         (263,377),
                         (264,377),
                         (265,377),
                         (266,377),
                         (267,377)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      319 => ( Point => ( new Points'(
                         (261,375),
                         (262,375),
                         (262,376),
                         (263,376),
                         (264,376),
                         (265,376),
                         (266,376),
                         (267,376)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      320 => ( Point => ( new Points'(
                         (261,374),
                         (262,374),
                         (263,374),
                         (263,375),
                         (264,375),
                         (265,375),
                         (266,375),
                         (267,375)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      321 => ( Point => ( new Points'(
                         (261,373),
                         (262,373),
                         (263,373),
                         (264,373),
                         (264,374),
                         (265,374),
                         (266,374),
                         (267,374)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      322 => ( Point => ( new Points'(
                         (262,372),
                         (263,372),
                         (264,372),
                         (265,372),
                         (265,373),
                         (266,373),
                         (267,373),
                         (268,373)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      323 => ( Point => ( new Points'(
                         (262,371),
                         (263,371),
                         (264,371),
                         (265,371),
                         (266,371),
                         (266,372),
                         (267,372),
                         (268,372)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      324 => ( Point => ( new Points'(
                         (262,370),
                         (263,370),
                         (264,370),
                         (265,370),
                         (266,370),
                         (267,370),
                         (267,371),
                         (268,371)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      325 => ( Point => ( new Points'(
                         (262,369),
                         (263,369),
                         (264,369),
                         (265,369),
                         (266,369),
                         (267,369),
                         (268,369),
                         (268,370)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      326 => ( Point => ( new Points'(
                         (262,367),
                         (262,368),
                         (263,368),
                         (264,368),
                         (265,368),
                         (266,368),
                         (267,368),
                         (268,368)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      327 => ( Point => ( new Points'(
                         (262,366),
                         (263,366),
                         (263,367),
                         (264,367),
                         (265,367),
                         (266,367),
                         (267,367),
                         (268,367)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      328 => ( Point => ( new Points'(
                         (262,365),
                         (263,365),
                         (264,365),
                         (264,366),
                         (265,366),
                         (266,366),
                         (267,366),
                         (268,366),
                         (269,366)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      329 => ( Point => ( new Points'(
                         (263,364),
                         (264,364),
                         (265,364),
                         (265,365),
                         (266,365),
                         (267,365),
                         (268,365),
                         (269,365)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      330 => ( Point => ( new Points'(
                         (263,363),
                         (264,363),
                         (265,363),
                         (266,363),
                         (266,364),
                         (267,364),
                         (268,364),
                         (269,364)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      331 => ( Point => ( new Points'(
                         (263,362),
                         (264,362),
                         (265,362),
                         (266,362),
                         (267,362),
                         (267,363),
                         (268,363),
                         (269,363)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      332 => ( Point => ( new Points'(
                         (263,361),
                         (264,361),
                         (265,361),
                         (266,361),
                         (267,361),
                         (268,361),
                         (268,362),
                         (269,362)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      333 => ( Point => ( new Points'(
                         (264,360),
                         (265,360),
                         (266,360),
                         (267,360),
                         (268,360),
                         (269,360),
                         (269,361)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      334 => ( Point => ( new Points'(
                         (264,359),
                         (265,359),
                         (266,359),
                         (267,359),
                         (268,359),
                         (269,359),
                         (270,359)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      335 => ( Point => ( new Points'(
                         (264,357),
                         (264,358),
                         (265,358),
                         (266,358),
                         (267,358),
                         (268,358),
                         (269,358),
                         (270,358)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      336 => ( Point => ( new Points'(
                         (264,356),
                         (265,356),
                         (265,357),
                         (266,357),
                         (267,357),
                         (268,357),
                         (269,357),
                         (270,357)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      337 => ( Point => ( new Points'(
                         (264,355),
                         (265,355),
                         (266,355),
                         (266,356),
                         (267,356),
                         (268,356),
                         (269,356),
                         (270,356)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      338 => ( Point => ( new Points'(
                         (264,354),
                         (265,354),
                         (266,354),
                         (267,354),
                         (267,355),
                         (268,355),
                         (269,355),
                         (270,355)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      339 => ( Point => ( new Points'(
                         (265,353),
                         (266,353),
                         (267,353),
                         (268,353),
                         (268,354),
                         (269,354),
                         (270,354)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      340 => ( Point => ( new Points'(
                         (265,352),
                         (266,352),
                         (267,352),
                         (268,352),
                         (269,352),
                         (269,353),
                         (270,353)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      341 => ( Point => ( new Points'(
                         (265,351),
                         (266,351),
                         (267,351),
                         (268,351),
                         (269,351),
                         (270,351),
                         (270,352),
                         (271,352)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      342 => ( Point => ( new Points'(
                         (265,350),
                         (266,350),
                         (267,350),
                         (268,350),
                         (269,350),
                         (270,350),
                         (271,350),
                         (271,351)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      343 => ( Point => ( new Points'(
                         (265,348),
                         (265,349),
                         (266,349),
                         (267,349),
                         (268,349),
                         (269,349),
                         (270,349),
                         (271,349)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      344 => ( Point => ( new Points'(
                         (265,347),
                         (266,347),
                         (266,348),
                         (267,348),
                         (268,348),
                         (269,348),
                         (270,348),
                         (271,348)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      345 => ( Point => ( new Points'(
                         (265,346),
                         (266,346),
                         (267,346),
                         (267,347),
                         (268,347),
                         (269,347),
                         (270,347),
                         (271,347)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      346 => ( Point => ( new Points'(
                         (265,345),
                         (266,345),
                         (267,345),
                         (268,345),
                         (268,346),
                         (269,346),
                         (270,346),
                         (271,346)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      347 => ( Point => ( new Points'(
                         (266,344),
                         (267,344),
                         (268,344),
                         (269,344),
                         (269,345),
                         (270,345),
                         (271,345),
                         (272,345)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      348 => ( Point => ( new Points'(
                         (266,343),
                         (267,343),
                         (268,343),
                         (269,343),
                         (270,343),
                         (270,344),
                         (271,344),
                         (272,344)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      349 => ( Point => ( new Points'(
                         (266,342),
                         (267,342),
                         (268,342),
                         (269,342),
                         (270,342),
                         (271,342),
                         (271,343),
                         (272,343),
                         (273,343)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      350 => ( Point => ( new Points'(
                         (266,341),
                         (267,341),
                         (268,341),
                         (269,341),
                         (270,341),
                         (271,341),
                         (272,341),
                         (272,342),
                         (273,342)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      351 => ( Point => ( new Points'(
                         (267,340),
                         (268,340),
                         (269,340),
                         (270,340),
                         (271,340),
                         (272,340),
                         (273,340),
                         (273,341)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      352 => ( Point => ( new Points'(
                         (267,338),
                         (267,339),
                         (268,339),
                         (269,339),
                         (270,339),
                         (271,339),
                         (272,339),
                         (273,339)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      353 => ( Point => ( new Points'(
                         (267,337),
                         (268,337),
                         (268,338),
                         (269,338),
                         (270,338),
                         (271,338),
                         (272,338),
                         (273,338),
                         (274,338)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      354 => ( Point => ( new Points'(
                         (267,336),
                         (268,336),
                         (269,336),
                         (269,337),
                         (270,337),
                         (271,337),
                         (272,337),
                         (273,337),
                         (274,337)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      355 => ( Point => ( new Points'(
                         (268,335),
                         (269,335),
                         (270,335),
                         (270,336),
                         (271,336),
                         (272,336),
                         (273,336),
                         (274,336)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      356 => ( Point => ( new Points'(
                         (268,334),
                         (269,334),
                         (270,334),
                         (271,334),
                         (271,335),
                         (272,335),
                         (273,335),
                         (274,335)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      357 => ( Point => ( new Points'(
                         (268,333),
                         (269,333),
                         (270,333),
                         (271,333),
                         (272,333),
                         (272,334),
                         (273,334),
                         (274,334)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      358 => ( Point => ( new Points'(
                         (268,332),
                         (269,332),
                         (270,332),
                         (271,332),
                         (272,332),
                         (273,332),
                         (273,333),
                         (274,333)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      359 => ( Point => ( new Points'(
                         (268,331),
                         (269,331),
                         (270,331),
                         (271,331),
                         (272,331),
                         (273,331),
                         (274,331),
                         (274,332)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      360 => ( Point => ( new Points'(
                         (268,329),
                         (268,330),
                         (269,330),
                         (270,330),
                         (271,330),
                         (272,330),
                         (273,330),
                         (274,330)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      361 => ( Point => ( new Points'(
                         (268,328),
                         (269,328),
                         (269,329),
                         (270,329),
                         (271,329),
                         (272,329),
                         (273,329),
                         (274,329)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      362 => ( Point => ( new Points'(
                         (269,327),
                         (270,327),
                         (270,328),
                         (271,328),
                         (272,328),
                         (273,328),
                         (274,328),
                         (275,328)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      363 => ( Point => ( new Points'(
                         (269,326),
                         (270,326),
                         (271,326),
                         (271,327),
                         (272,327),
                         (273,327),
                         (274,327),
                         (275,327)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      364 => ( Point => ( new Points'(
                         (269,325),
                         (270,325),
                         (271,325),
                         (272,325),
                         (272,326),
                         (273,326),
                         (274,326),
                         (275,326)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      365 => ( Point => ( new Points'(
                         (269,324),
                         (270,324),
                         (271,324),
                         (272,324),
                         (273,324),
                         (273,325),
                         (274,325),
                         (275,325)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      366 => ( Point => ( new Points'(
                         (269,323),
                         (270,323),
                         (271,323),
                         (272,323),
                         (273,323),
                         (274,323),
                         (274,324),
                         (275,324)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      367 => ( Point => ( new Points'(
                         (269,322),
                         (270,322),
                         (271,322),
                         (272,322),
                         (273,322),
                         (274,322),
                         (275,322),
                         (275,323)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      368 => ( Point => ( new Points'(
                         (270,320),
                         (270,321),
                         (271,321),
                         (272,321),
                         (273,321),
                         (274,321),
                         (275,321),
                         (276,321)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      369 => ( Point => ( new Points'(
                         (270,319),
                         (271,319),
                         (271,320),
                         (272,320),
                         (273,320),
                         (274,320),
                         (275,320),
                         (276,320)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      370 => ( Point => ( new Points'(
                         (270,318),
                         (271,318),
                         (272,318),
                         (272,319),
                         (273,319),
                         (274,319),
                         (275,319),
                         (276,319)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      371 => ( Point => ( new Points'(
                         (270,317),
                         (271,317),
                         (272,317),
                         (273,317),
                         (273,318),
                         (274,318),
                         (275,318),
                         (276,318)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      372 => ( Point => ( new Points'(
                         (270,316),
                         (271,316),
                         (272,316),
                         (273,316),
                         (274,316),
                         (274,317),
                         (275,317),
                         (276,317)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      373 => ( Point => ( new Points'(
                         (271,315),
                         (272,315),
                         (273,315),
                         (274,315),
                         (275,315),
                         (275,316),
                         (276,316)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      374 => ( Point => ( new Points'(
                         (271,314),
                         (272,314),
                         (273,314),
                         (274,314),
                         (275,314),
                         (276,314),
                         (276,315)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      375 => ( Point => ( new Points'(
                         (271,312),
                         (271,313),
                         (272,313),
                         (273,313),
                         (274,313),
                         (275,313),
                         (276,313)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      376 => ( Point => ( new Points'(
                         (272,311),
                         (272,312),
                         (273,312),
                         (274,312),
                         (275,312),
                         (276,312),
                         (277,312)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      377 => ( Point => ( new Points'(
                         (272,310),
                         (273,310),
                         (273,311),
                         (274,311),
                         (275,311),
                         (276,311),
                         (277,311)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      378 => ( Point => ( new Points'(
                         (272,309),
                         (273,309),
                         (274,309),
                         (274,310),
                         (275,310),
                         (276,310),
                         (277,310)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      379 => ( Point => ( new Points'(
                         (272,308),
                         (273,308),
                         (274,308),
                         (275,308),
                         (275,309),
                         (276,309),
                         (277,309)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      380 => ( Point => ( new Points'(
                         (272,307),
                         (273,307),
                         (274,307),
                         (275,307),
                         (276,307),
                         (276,308),
                         (277,308)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      381 => ( Point => ( new Points'(
                         (272,306),
                         (273,306),
                         (274,306),
                         (275,306),
                         (276,306),
                         (277,306),
                         (277,307)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      382 => ( Point => ( new Points'(
                         (272,305),
                         (273,305),
                         (274,305),
                         (275,305),
                         (276,305),
                         (277,305),
                         (278,305)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      383 => ( Point => ( new Points'(
                         (272,303),
                         (272,304),
                         (273,304),
                         (274,304),
                         (275,304),
                         (276,304),
                         (277,304),
                         (278,304)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      384 => ( Point => ( new Points'(
                         (273,302),
                         (273,303),
                         (274,303),
                         (275,303),
                         (276,303),
                         (277,303),
                         (278,303)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      385 => ( Point => ( new Points'(
                         (273,301),
                         (274,301),
                         (274,302),
                         (275,302),
                         (276,302),
                         (277,302),
                         (278,302)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      386 => ( Point => ( new Points'(
                         (273,300),
                         (274,300),
                         (275,300),
                         (275,301),
                         (276,301),
                         (277,301),
                         (278,301)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      387 => ( Point => ( new Points'(
                         (273,299),
                         (274,299),
                         (275,299),
                         (276,299),
                         (276,300),
                         (277,300),
                         (278,300)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      388 => ( Point => ( new Points'(
                         (274,298),
                         (275,298),
                         (276,298),
                         (277,298),
                         (277,299),
                         (278,299)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      389 => ( Point => ( new Points'(
                         (274,297),
                         (275,297),
                         (276,297),
                         (277,297),
                         (278,297),
                         (278,298)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      390 => ( Point => ( new Points'(
                         (274,295),
                         (274,296),
                         (275,296),
                         (276,296),
                         (277,296),
                         (278,296),
                         (279,296)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      391 => ( Point => ( new Points'(
                         (274,294),
                         (275,294),
                         (275,295),
                         (276,295),
                         (277,295),
                         (278,295),
                         (279,295)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      392 => ( Point => ( new Points'(
                         (274,293),
                         (275,293),
                         (276,293),
                         (276,294),
                         (277,294),
                         (278,294),
                         (279,294)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      393 => ( Point => ( new Points'(
                         (275,292),
                         (276,292),
                         (277,292),
                         (277,293),
                         (278,293),
                         (279,293),
                         (280,293)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      394 => ( Point => ( new Points'(
                         (275,291),
                         (276,291),
                         (277,291),
                         (278,291),
                         (278,292),
                         (279,292),
                         (280,292)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      395 => ( Point => ( new Points'(
                         (275,290),
                         (276,290),
                         (277,290),
                         (278,290),
                         (279,290),
                         (279,291),
                         (280,291)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      396 => ( Point => ( new Points'(
                         (275,289),
                         (276,289),
                         (277,289),
                         (278,289),
                         (279,289),
                         (280,289),
                         (280,290)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      397 => ( Point => ( new Points'(
                         (275,288),
                         (276,288),
                         (277,288),
                         (278,288),
                         (279,288),
                         (280,288)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      398 => ( Point => ( new Points'(
                         (275,286),
                         (275,287),
                         (276,287),
                         (277,287),
                         (278,287),
                         (279,287),
                         (280,287)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      399 => ( Point => ( new Points'(
                         (275,285),
                         (276,285),
                         (276,286),
                         (277,286),
                         (278,286),
                         (279,286),
                         (280,286)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      400 => ( Point => ( new Points'(
                         (276,284),
                         (277,284),
                         (277,285),
                         (278,285),
                         (279,285),
                         (280,285),
                         (281,285)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      401 => ( Point => ( new Points'(
                         (276,283),
                         (277,283),
                         (278,283),
                         (278,284),
                         (279,284),
                         (280,284),
                         (281,284)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      402 => ( Point => ( new Points'(
                         (276,282),
                         (277,282),
                         (278,282),
                         (279,282),
                         (279,283),
                         (280,283),
                         (281,283)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      403 => ( Point => ( new Points'(
                         (276,281),
                         (277,281),
                         (278,281),
                         (279,281),
                         (280,281),
                         (280,282),
                         (281,282)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      404 => ( Point => ( new Points'(
                         (276,280),
                         (277,280),
                         (278,280),
                         (279,280),
                         (280,280),
                         (281,280),
                         (281,281)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      405 => ( Point => ( new Points'(
                         (276,279),
                         (277,279),
                         (278,279),
                         (279,279),
                         (280,279),
                         (281,279),
                         (282,279)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      406 => ( Point => ( new Points'(
                         (277,278),
                         (278,278),
                         (279,278),
                         (280,278),
                         (281,278),
                         (282,278)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      407 => ( Point => ( new Points'(
                         (277,276),
                         (277,277),
                         (278,277),
                         (279,277),
                         (280,277),
                         (281,277),
                         (282,277)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      408 => ( Point => ( new Points'(
                         (277,275),
                         (278,275),
                         (278,276),
                         (279,276),
                         (280,276),
                         (281,276),
                         (282,276)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      409 => ( Point => ( new Points'(
                         (277,274),
                         (278,274),
                         (279,274),
                         (279,275),
                         (280,275),
                         (281,275),
                         (282,275)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      410 => ( Point => ( new Points'(
                         (278,273),
                         (279,273),
                         (280,273),
                         (280,274),
                         (281,274),
                         (282,274),
                         (283,274)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      411 => ( Point => ( new Points'(
                         (278,272),
                         (279,272),
                         (280,272),
                         (281,272),
                         (281,273),
                         (282,273),
                         (283,273)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      412 => ( Point => ( new Points'(
                         (278,271),
                         (279,271),
                         (280,271),
                         (281,271),
                         (282,271),
                         (282,272),
                         (283,272)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      413 => ( Point => ( new Points'(
                         (278,270),
                         (279,270),
                         (280,270),
                         (281,270),
                         (282,270),
                         (283,270),
                         (283,271)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      414 => ( Point => ( new Points'(
                         (278,269),
                         (279,269),
                         (280,269),
                         (281,269),
                         (282,269),
                         (283,269)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      415 => ( Point => ( new Points'(
                         (278,267),
                         (278,268),
                         (279,268),
                         (280,268),
                         (281,268),
                         (282,268),
                         (283,268)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      416 => ( Point => ( new Points'(
                         (279,266),
                         (279,267),
                         (280,267),
                         (281,267),
                         (282,267),
                         (283,267),
                         (284,267)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      417 => ( Point => ( new Points'(
                         (279,265),
                         (280,265),
                         (280,266),
                         (281,266),
                         (282,266),
                         (283,266),
                         (284,266)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      418 => ( Point => ( new Points'(
                         (279,264),
                         (280,264),
                         (281,264),
                         (281,265),
                         (282,265),
                         (283,265),
                         (284,265)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      419 => ( Point => ( new Points'(
                         (279,263),
                         (280,263),
                         (281,263),
                         (282,263),
                         (282,264),
                         (283,264),
                         (284,264)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      420 => ( Point => ( new Points'(
                         (279,262),
                         (280,262),
                         (281,262),
                         (282,262),
                         (283,262),
                         (283,263),
                         (284,263)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      421 => ( Point => ( new Points'(
                         (280,261),
                         (281,261),
                         (282,261),
                         (283,261),
                         (284,261),
                         (284,262),
                         (285,262)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      422 => ( Point => ( new Points'(
                         (280,260),
                         (281,260),
                         (282,260),
                         (283,260),
                         (284,260),
                         (285,260),
                         (285,261)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      423 => ( Point => ( new Points'(
                         (280,259),
                         (281,259),
                         (282,259),
                         (283,259),
                         (284,259),
                         (285,259)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      424 => ( Point => ( new Points'(
                         (280,257),
                         (280,258),
                         (281,258),
                         (282,258),
                         (283,258),
                         (284,258),
                         (285,258)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      425 => ( Point => ( new Points'(
                         (281,256),
                         (281,257),
                         (282,257),
                         (283,257),
                         (284,257),
                         (285,257)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      426 => ( Point => ( new Points'(
                         (281,255),
                         (282,255),
                         (282,256),
                         (283,256),
                         (284,256),
                         (285,256),
                         (286,256)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      427 => ( Point => ( new Points'(
                         (281,254),
                         (282,254),
                         (283,254),
                         (283,255),
                         (284,255),
                         (285,255),
                         (286,255)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      428 => ( Point => ( new Points'(
                         (281,253),
                         (282,253),
                         (283,253),
                         (284,253),
                         (284,254),
                         (285,254),
                         (286,254)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      429 => ( Point => ( new Points'(
                         (281,252),
                         (282,252),
                         (283,252),
                         (284,252),
                         (285,252),
                         (285,253),
                         (286,253)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      430 => ( Point => ( new Points'(
                         (281,251),
                         (282,251),
                         (283,251),
                         (284,251),
                         (285,251),
                         (286,251),
                         (286,252)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      431 => ( Point => ( new Points'(
                         (281,250),
                         (282,250),
                         (283,250),
                         (284,250),
                         (285,250),
                         (286,250)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      432 => ( Point => ( new Points'(
                         (282,248),
                         (282,249),
                         (283,249),
                         (284,249),
                         (285,249),
                         (286,249),
                         (287,249)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      433 => ( Point => ( new Points'(
                         (282,247),
                         (283,247),
                         (283,248),
                         (284,248),
                         (285,248),
                         (286,248),
                         (287,248)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      434 => ( Point => ( new Points'(
                         (282,246),
                         (283,246),
                         (284,246),
                         (284,247),
                         (285,247),
                         (286,247),
                         (287,247)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      435 => ( Point => ( new Points'(
                         (282,245),
                         (283,245),
                         (284,245),
                         (285,245),
                         (285,246),
                         (286,246),
                         (287,246)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      436 => ( Point => ( new Points'(
                         (282,244),
                         (283,244),
                         (284,244),
                         (285,244),
                         (286,244),
                         (286,245),
                         (287,245)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      437 => ( Point => ( new Points'(
                         (283,243),
                         (284,243),
                         (285,243),
                         (286,243),
                         (287,243),
                         (287,244)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      438 => ( Point => ( new Points'(
                         (283,242),
                         (284,242),
                         (285,242),
                         (286,242),
                         (287,242),
                         (288,242)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      439 => ( Point => ( new Points'(
                         (283,240),
                         (283,241),
                         (284,241),
                         (285,241),
                         (286,241),
                         (287,241),
                         (288,241)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      440 => ( Point => ( new Points'(
                         (284,239),
                         (284,240),
                         (285,240),
                         (286,240),
                         (287,240),
                         (288,240)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      441 => ( Point => ( new Points'(
                         (284,238),
                         (285,238),
                         (285,239),
                         (286,239),
                         (287,239),
                         (288,239)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      442 => ( Point => ( new Points'(
                         (284,237),
                         (285,237),
                         (286,237),
                         (286,238),
                         (287,238),
                         (288,238),
                         (289,238)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      443 => ( Point => ( new Points'(
                         (284,236),
                         (285,236),
                         (286,236),
                         (287,236),
                         (287,237),
                         (288,237),
                         (289,237)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      444 => ( Point => ( new Points'(
                         (284,235),
                         (285,235),
                         (286,235),
                         (287,235),
                         (288,235),
                         (288,236),
                         (289,236)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      445 => ( Point => ( new Points'(
                         (284,234),
                         (285,234),
                         (286,234),
                         (287,234),
                         (288,234),
                         (289,234),
                         (289,235)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      446 => ( Point => ( new Points'(
                         (284,233),
                         (285,233),
                         (286,233),
                         (287,233),
                         (288,233),
                         (289,233)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      447 => ( Point => ( new Points'(
                         (284,231),
                         (284,232),
                         (285,232),
                         (286,232),
                         (287,232),
                         (288,232),
                         (289,232)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      448 => ( Point => ( new Points'(
                         (285,230),
                         (285,231),
                         (286,231),
                         (287,231),
                         (288,231),
                         (289,231)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      449 => ( Point => ( new Points'(
                         (285,229),
                         (286,229),
                         (286,230),
                         (287,230),
                         (288,230),
                         (289,230)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      450 => ( Point => ( new Points'(
                         (285,228),
                         (286,228),
                         (287,228),
                         (287,229),
                         (288,229),
                         (289,229),
                         (290,229)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      451 => ( Point => ( new Points'(
                         (286,227),
                         (287,227),
                         (288,227),
                         (288,228),
                         (289,228),
                         (290,228)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      452 => ( Point => ( new Points'(
                         (286,226),
                         (287,226),
                         (288,226),
                         (289,226),
                         (289,227),
                         (290,227)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      453 => ( Point => ( new Points'(
                         (286,225),
                         (287,225),
                         (288,225),
                         (289,225),
                         (290,225),
                         (290,226)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      454 => ( Point => ( new Points'(
                         (286,224),
                         (287,224),
                         (288,224),
                         (289,224),
                         (290,224),
                         (291,224)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      455 => ( Point => ( new Points'(
                         (286,223),
                         (287,223),
                         (288,223),
                         (289,223),
                         (290,223),
                         (291,223)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      456 => ( Point => ( new Points'(
                         (287,221),
                         (287,222),
                         (288,222),
                         (289,222),
                         (290,222),
                         (291,222)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      457 => ( Point => ( new Points'(
                         (287,220),
                         (288,220),
                         (288,221),
                         (289,221),
                         (290,221),
                         (291,221)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      458 => ( Point => ( new Points'(
                         (287,219),
                         (288,219),
                         (289,219),
                         (289,220),
                         (290,220),
                         (291,220)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      459 => ( Point => ( new Points'(
                         (287,218),
                         (288,218),
                         (289,218),
                         (290,218),
                         (290,219),
                         (291,219)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      460 => ( Point => ( new Points'(
                         (287,217),
                         (288,217),
                         (289,217),
                         (290,217),
                         (291,217),
                         (291,218)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      461 => ( Point => ( new Points'(
                         (287,215),
                         (287,216),
                         (288,216),
                         (289,216),
                         (290,216),
                         (291,216),
                         (292,216)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      462 => ( Point => ( new Points'(
                         (288,214),
                         (288,215),
                         (289,215),
                         (290,215),
                         (291,215),
                         (292,215)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      463 => ( Point => ( new Points'(
                         (288,213),
                         (289,213),
                         (289,214),
                         (290,214),
                         (291,214),
                         (292,214)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      464 => ( Point => ( new Points'(
                         (288,212),
                         (289,212),
                         (290,212),
                         (290,213),
                         (291,213),
                         (292,213)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      465 => ( Point => ( new Points'(
                         (288,211),
                         (289,211),
                         (290,211),
                         (291,211),
                         (291,212),
                         (292,212)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      466 => ( Point => ( new Points'(
                         (288,210),
                         (289,210),
                         (290,210),
                         (291,210),
                         (292,210),
                         (292,211)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      467 => ( Point => ( new Points'(
                         (289,209),
                         (290,209),
                         (291,209),
                         (292,209),
                         (293,209)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      468 => ( Point => ( new Points'(
                         (289,207),
                         (289,208),
                         (290,208),
                         (291,208),
                         (292,208),
                         (293,208)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      469 => ( Point => ( new Points'(
                         (289,206),
                         (290,206),
                         (290,207),
                         (291,207),
                         (292,207),
                         (293,207)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      470 => ( Point => ( new Points'(
                         (289,205),
                         (290,205),
                         (291,205),
                         (291,206),
                         (292,206),
                         (293,206)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      471 => ( Point => ( new Points'(
                         (289,204),
                         (290,204),
                         (291,204),
                         (292,204),
                         (292,205),
                         (293,205)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      472 => ( Point => ( new Points'(
                         (289,203),
                         (290,203),
                         (291,203),
                         (292,203),
                         (293,203),
                         (293,204),
                         (294,204)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      473 => ( Point => ( new Points'(
                         (290,202),
                         (291,202),
                         (292,202),
                         (293,202),
                         (294,202),
                         (294,203)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      474 => ( Point => ( new Points'(
                         (290,201),
                         (291,201),
                         (292,201),
                         (293,201),
                         (294,201)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      475 => ( Point => ( new Points'(
                         (290,199),
                         (290,200),
                         (291,200),
                         (292,200),
                         (293,200),
                         (294,200)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      476 => ( Point => ( new Points'(
                         (291,198),
                         (291,199),
                         (292,199),
                         (293,199),
                         (294,199),
                         (295,199)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      477 => ( Point => ( new Points'(
                         (291,197),
                         (292,197),
                         (292,198),
                         (293,198),
                         (294,198),
                         (295,198)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      478 => ( Point => ( new Points'(
                         (291,196),
                         (292,196),
                         (293,196),
                         (293,197),
                         (294,197),
                         (295,197)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      479 => ( Point => ( new Points'(
                         (291,195),
                         (292,195),
                         (293,195),
                         (294,195),
                         (294,196),
                         (295,196)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      480 => ( Point => ( new Points'(
                         (291,194),
                         (292,194),
                         (293,194),
                         (294,194),
                         (295,194),
                         (295,195)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      481 => ( Point => ( new Points'(
                         (291,192),
                         (291,193),
                         (292,193),
                         (293,193),
                         (294,193),
                         (295,193)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      482 => ( Point => ( new Points'(
                         (292,191),
                         (292,192),
                         (293,192),
                         (294,192),
                         (295,192),
                         (296,192)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      483 => ( Point => ( new Points'(
                         (292,190),
                         (293,190),
                         (293,191),
                         (294,191),
                         (295,191),
                         (296,191)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      484 => ( Point => ( new Points'(
                         (292,189),
                         (293,189),
                         (294,189),
                         (294,190),
                         (295,190),
                         (296,190)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      485 => ( Point => ( new Points'(
                         (292,188),
                         (293,188),
                         (294,188),
                         (295,188),
                         (295,189),
                         (296,189)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      486 => ( Point => ( new Points'(
                         (292,187),
                         (293,187),
                         (294,187),
                         (295,187),
                         (296,187),
                         (296,188)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      487 => ( Point => ( new Points'(
                         (293,185),
                         (293,186),
                         (294,186),
                         (295,186),
                         (296,186),
                         (297,186)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      488 => ( Point => ( new Points'(
                         (293,184),
                         (294,184),
                         (294,185),
                         (295,185),
                         (296,185),
                         (297,185)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      489 => ( Point => ( new Points'(
                         (293,183),
                         (294,183),
                         (295,183),
                         (295,184),
                         (296,184),
                         (297,184)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      490 => ( Point => ( new Points'(
                         (293,182),
                         (294,182),
                         (295,182),
                         (296,182),
                         (296,183),
                         (297,183)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      491 => ( Point => ( new Points'(
                         (293,181),
                         (294,181),
                         (295,181),
                         (296,181),
                         (297,181),
                         (297,182)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      492 => ( Point => ( new Points'(
                         (294,180),
                         (295,180),
                         (296,180),
                         (297,180),
                         (298,180)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      493 => ( Point => ( new Points'(
                         (294,178),
                         (294,179),
                         (295,179),
                         (296,179),
                         (297,179),
                         (298,179)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      494 => ( Point => ( new Points'(
                         (294,177),
                         (295,177),
                         (295,178),
                         (296,178),
                         (297,178),
                         (298,178)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      495 => ( Point => ( new Points'(
                         (294,176),
                         (295,176),
                         (296,176),
                         (296,177),
                         (297,177),
                         (298,177)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      496 => ( Point => ( new Points'(
                         (294,175),
                         (295,175),
                         (296,175),
                         (297,175),
                         (297,176),
                         (298,176),
                         (299,176)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      497 => ( Point => ( new Points'(
                         (295,174),
                         (296,174),
                         (297,174),
                         (298,174),
                         (298,175),
                         (299,175)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      498 => ( Point => ( new Points'(
                         (295,173),
                         (296,173),
                         (297,173),
                         (298,173),
                         (299,173),
                         (299,174)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      499 => ( Point => ( new Points'(
                         (295,171),
                         (295,172),
                         (296,172),
                         (297,172),
                         (298,172),
                         (299,172)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      500 => ( Point => ( new Points'(
                         (295,170),
                         (296,170),
                         (296,171),
                         (297,171),
                         (298,171),
                         (299,171)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      501 => ( Point => ( new Points'(
                         (295,169),
                         (296,169),
                         (297,169),
                         (297,170),
                         (298,170),
                         (299,170)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      502 => ( Point => ( new Points'(
                         (296,168),
                         (297,168),
                         (298,168),
                         (298,169),
                         (299,169),
                         (300,169)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      503 => ( Point => ( new Points'(
                         (296,167),
                         (297,167),
                         (298,167),
                         (299,167),
                         (299,168),
                         (300,168)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      504 => ( Point => ( new Points'(
                         (296,166),
                         (297,166),
                         (298,166),
                         (299,166),
                         (300,166),
                         (300,167)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      505 => ( Point => ( new Points'(
                         (296,165),
                         (297,165),
                         (298,165),
                         (299,165),
                         (300,165)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      506 => ( Point => ( new Points'(
                         (296,163),
                         (296,164),
                         (297,164),
                         (298,164),
                         (299,164),
                         (300,164)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      507 => ( Point => ( new Points'(
                         (297,162),
                         (297,163),
                         (298,163),
                         (299,163),
                         (300,163),
                         (301,163)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      508 => ( Point => ( new Points'(
                         (297,161),
                         (298,161),
                         (298,162),
                         (299,162),
                         (300,162),
                         (301,162)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      509 => ( Point => ( new Points'(
                         (297,160),
                         (298,160),
                         (299,160),
                         (299,161),
                         (300,161),
                         (301,161)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      510 => ( Point => ( new Points'(
                         (297,159),
                         (298,159),
                         (299,159),
                         (300,159),
                         (300,160),
                         (301,160)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      511 => ( Point => ( new Points'(
                         (297,158),
                         (298,158),
                         (299,158),
                         (300,158),
                         (301,158),
                         (301,159)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      512 => ( Point => ( new Points'(
                         (298,157),
                         (299,157),
                         (300,157),
                         (301,157),
                         (302,157)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      513 => ( Point => ( new Points'(
                         (298,155),
                         (298,156),
                         (299,156),
                         (300,156),
                         (301,156),
                         (302,156)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      514 => ( Point => ( new Points'(
                         (298,154),
                         (299,154),
                         (299,155),
                         (300,155),
                         (301,155),
                         (302,155)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      515 => ( Point => ( new Points'(
                         (298,153),
                         (299,153),
                         (300,153),
                         (300,154),
                         (301,154),
                         (302,154)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      516 => ( Point => ( new Points'(
                         (298,152),
                         (299,152),
                         (300,152),
                         (301,152),
                         (301,153),
                         (302,153)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      517 => ( Point => ( new Points'(
                         (298,151),
                         (299,151),
                         (300,151),
                         (301,151),
                         (302,151),
                         (302,152)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      518 => ( Point => ( new Points'(
                         (299,149),
                         (299,150),
                         (300,150),
                         (301,150),
                         (302,150),
                         (303,150)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      519 => ( Point => ( new Points'(
                         (299,148),
                         (300,148),
                         (300,149),
                         (301,149),
                         (302,149),
                         (303,149)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      520 => ( Point => ( new Points'(
                         (299,147),
                         (300,147),
                         (301,147),
                         (301,148),
                         (302,148),
                         (303,148)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      521 => ( Point => ( new Points'(
                         (299,146),
                         (300,146),
                         (301,146),
                         (302,146),
                         (302,147),
                         (303,147)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      522 => ( Point => ( new Points'(
                         (300,144),
                         (300,145),
                         (301,145),
                         (302,145),
                         (303,145),
                         (303,146),
                         (304,146)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      523 => ( Point => ( new Points'(
                         (300,143),
                         (301,143),
                         (301,144),
                         (302,144),
                         (303,144),
                         (304,144),
                         (304,145)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      524 => ( Point => ( new Points'(
                         (300,142),
                         (301,142),
                         (302,142),
                         (302,143),
                         (303,143),
                         (304,143)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      525 => ( Point => ( new Points'(
                         (300,141),
                         (301,141),
                         (302,141),
                         (303,141),
                         (303,142),
                         (304,142)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      526 => ( Point => ( new Points'(
                         (300,140),
                         (301,140),
                         (302,140),
                         (303,140),
                         (304,140),
                         (304,141)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      527 => ( Point => ( new Points'(
                         (301,139),
                         (302,139),
                         (303,139),
                         (304,139),
                         (305,139)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      528 => ( Point => ( new Points'(
                         (301,137),
                         (301,138),
                         (302,138),
                         (303,138),
                         (304,138),
                         (305,138)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      529 => ( Point => ( new Points'(
                         (301,136),
                         (302,136),
                         (302,137),
                         (303,137),
                         (304,137),
                         (305,137)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      530 => ( Point => ( new Points'(
                         (301,135),
                         (302,135),
                         (303,135),
                         (303,136),
                         (304,136),
                         (305,136)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      531 => ( Point => ( new Points'(
                         (301,134),
                         (302,134),
                         (303,134),
                         (304,134),
                         (304,135),
                         (305,135)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      532 => ( Point => ( new Points'(
                         (302,133),
                         (303,133),
                         (304,133),
                         (305,133),
                         (305,134),
                         (306,134)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      533 => ( Point => ( new Points'(
                         (302,132),
                         (303,132),
                         (304,132),
                         (305,132),
                         (306,132),
                         (306,133)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      534 => ( Point => ( new Points'(
                         (302,130),
                         (302,131),
                         (303,131),
                         (304,131),
                         (305,131),
                         (306,131)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      535 => ( Point => ( new Points'(
                         (302,129),
                         (303,129),
                         (303,130),
                         (304,130),
                         (305,130),
                         (306,130)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      536 => ( Point => ( new Points'(
                         (302,128),
                         (303,128),
                         (304,128),
                         (304,129),
                         (305,129),
                         (306,129)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      537 => ( Point => ( new Points'(
                         (303,127),
                         (304,127),
                         (305,127),
                         (305,128),
                         (306,128)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      538 => ( Point => ( new Points'(
                         (303,126),
                         (304,126),
                         (305,126),
                         (306,126),
                         (306,127)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      539 => ( Point => ( new Points'(
                         (303,124),
                         (303,125),
                         (304,125),
                         (305,125),
                         (306,125),
                         (307,125)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      540 => ( Point => ( new Points'(
                         (303,123),
                         (304,123),
                         (304,124),
                         (305,124),
                         (306,124),
                         (307,124)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      541 => ( Point => ( new Points'(
                         (303,122),
                         (304,122),
                         (305,122),
                         (305,123),
                         (306,123),
                         (307,123)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      542 => ( Point => ( new Points'(
                         (304,120),
                         (304,121),
                         (305,121),
                         (306,121),
                         (306,122),
                         (307,122)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      543 => ( Point => ( new Points'(
                         (304,119),
                         (305,120),
                         (306,120),
                         (307,120),
                         (307,121)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      544 => ( Point => ( new Points'(
                         (304,118),
                         (305,118),
                         (305,119),
                         (306,119),
                         (307,119)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      545 => ( Point => ( new Points'(
                         (304,117),
                         (305,117),
                         (306,117),
                         (306,118),
                         (307,118),
                         (308,118)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      546 => ( Point => ( new Points'(
                         (305,115),
                         (305,116),
                         (306,116),
                         (307,116),
                         (307,117),
                         (308,117)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      547 => ( Point => ( new Points'(
                         (305,114),
                         (306,114),
                         (306,115),
                         (307,115),
                         (308,115),
                         (308,116)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      548 => ( Point => ( new Points'(
                         (305,113),
                         (306,113),
                         (307,113),
                         (307,114),
                         (308,114)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      549 => ( Point => ( new Points'(
                         (305,112),
                         (306,112),
                         (307,112),
                         (308,112),
                         (308,113)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      550 => ( Point => ( new Points'(
                         (305,111),
                         (306,111),
                         (307,111),
                         (308,111)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      551 => ( Point => ( new Points'(
                         (306,109),
                         (306,110),
                         (307,110),
                         (308,110),
                         (309,110)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      552 => ( Point => ( new Points'(
                         (306,108),
                         (307,108),
                         (307,109),
                         (308,109),
                         (309,109)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      553 => ( Point => ( new Points'(
                         (306,107),
                         (307,107),
                         (308,107),
                         (308,108),
                         (309,108)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      554 => ( Point => ( new Points'(
                         (306,105),
                         (306,106),
                         (307,106),
                         (308,106),
                         (309,106),
                         (309,107)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      555 => ( Point => ( new Points'(
                         (307,104),
                         (307,105),
                         (308,105),
                         (309,105),
                         (310,105)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      556 => ( Point => ( new Points'(
                         (307,103),
                         (308,103),
                         (308,104),
                         (309,104),
                         (310,104)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      557 => ( Point => ( new Points'(
                         (307,102),
                         (308,102),
                         (309,102),
                         (309,103),
                         (310,103)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      558 => ( Point => ( new Points'(
                         (307,101),
                         (308,101),
                         (309,101),
                         (310,101),
                         (310,102)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      559 => ( Point => ( new Points'(
                         (308,100),
                         (309,100),
                         (310,100),
                         (311,100)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      560 => ( Point => ( new Points'(
                         (308,98),
                         (308,99),
                         (309,99),
                         (310,99),
                         (311,99)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      561 => ( Point => ( new Points'(
                         (308,97),
                         (309,97),
                         (309,98),
                         (310,98),
                         (311,98)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      562 => ( Point => ( new Points'(
                         (308,96),
                         (309,96),
                         (310,96),
                         (310,97),
                         (311,97)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      563 => ( Point => ( new Points'(
                         (308,95),
                         (309,95),
                         (310,95),
                         (311,95),
                         (311,96)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      564 => ( Point => ( new Points'(
                         (309,94),
                         (310,94),
                         (311,94),
                         (312,94)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      565 => ( Point => ( new Points'(
                         (309,92),
                         (309,93),
                         (310,93),
                         (311,93),
                         (312,93)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      566 => ( Point => ( new Points'(
                         (309,91),
                         (310,91),
                         (310,92),
                         (311,92),
                         (312,92)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      567 => ( Point => ( new Points'(
                         (309,90),
                         (310,90),
                         (311,90),
                         (311,91),
                         (312,91)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      568 => ( Point => ( new Points'(
                         (310,89),
                         (311,89),
                         (312,89),
                         (312,90)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      569 => ( Point => ( new Points'(
                         (310,88),
                         (311,88),
                         (312,88),
                         (313,88)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      570 => ( Point => ( new Points'(
                         (310,86),
                         (310,87),
                         (311,87),
                         (312,87),
                         (313,87)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      571 => ( Point => ( new Points'(
                         (310,85),
                         (311,85),
                         (311,86),
                         (312,86),
                         (313,86)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      572 => ( Point => ( new Points'(
                         (310,84),
                         (311,84),
                         (312,84),
                         (312,85),
                         (313,85)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      573 => ( Point => ( new Points'(
                         (311,82),
                         (311,83),
                         (312,83),
                         (313,83),
                         (313,84)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      574 => ( Point => ( new Points'(
                         (311,81),
                         (312,81),
                         (312,82),
                         (313,82),
                         (314,82)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      575 => ( Point => ( new Points'(
                         (311,80),
                         (312,80),
                         (313,80),
                         (313,81),
                         (314,81)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      576 => ( Point => ( new Points'(
                         (311,79),
                         (312,79),
                         (313,79),
                         (314,79),
                         (314,80)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      577 => ( Point => ( new Points'(
                         (312,77),
                         (312,78),
                         (313,78),
                         (314,78),
                         (315,78)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      578 => ( Point => ( new Points'(
                         (312,76),
                         (313,76),
                         (313,77),
                         (314,77),
                         (315,77)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      579 => ( Point => ( new Points'(
                         (312,75),
                         (313,75),
                         (314,75),
                         (314,76),
                         (315,76)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      580 => ( Point => ( new Points'(
                         (312,74),
                         (313,74),
                         (314,74),
                         (315,74),
                         (315,75)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      581 => ( Point => ( new Points'(
                         (312,73),
                         (313,73),
                         (314,73),
                         (315,73)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      582 => ( Point => ( new Points'(
                         (313,71),
                         (313,72),
                         (314,72),
                         (315,72)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      583 => ( Point => ( new Points'(
                         (313,70),
                         (314,70),
                         (314,71),
                         (315,71)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      584 => ( Point => ( new Points'(
                         (313,69),
                         (314,69),
                         (315,69),
                         (315,70),
                         (316,70)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      585 => ( Point => ( new Points'(
                         (313,68),
                         (314,68),
                         (315,68),
                         (316,68),
                         (316,69)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      586 => ( Point => ( new Points'(
                         (314,67),
                         (315,67),
                         (316,67)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      587 => ( Point => ( new Points'(
                         (314,65),
                         (314,66),
                         (315,66),
                         (316,66)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      588 => ( Point => ( new Points'(
                         (314,64),
                         (315,64),
                         (315,65),
                         (316,65)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      589 => ( Point => ( new Points'(
                         (314,63),
                         (315,63),
                         (316,63),
                         (316,64)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      590 => ( Point => ( new Points'(
                         (314,62),
                         (315,62),
                         (316,62)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      591 => ( Point => ( new Points'(
                         (314,61),
                         (315,61),
                         (316,61),
                         (317,61)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      592 => ( Point => ( new Points'(
                         (315,59),
                         (315,60),
                         (316,60),
                         (317,60)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      593 => ( Point => ( new Points'(
                         (315,58),
                         (316,58),
                         (316,59),
                         (317,59)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      594 => ( Point => ( new Points'(
                         (315,57),
                         (316,57),
                         (317,57),
                         (317,58)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      595 => ( Point => ( new Points'(
                         (315,55),
                         (315,56),
                         (316,56),
                         (317,56),
                         (318,56)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      596 => ( Point => ( new Points'(
                         (316,55),
                         (317,55),
                         (318,55)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      597 => ( Point => ( new Points'(
                         (316,53),
                         (316,54),
                         (317,54),
                         (318,54)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      598 => ( Point => ( new Points'(
                         (316,52),
                         (317,52),
                         (317,53),
                         (318,53)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      599 => ( Point => ( new Points'(
                         (316,51),
                         (317,51),
                         (318,51),
                         (318,52)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      600 => ( Point => ( new Points'(
                         (316,49),
                         (316,50),
                         (317,50),
                         (318,50)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      601 => ( Point => ( new Points'(
                         (317,48),
                         (317,49),
                         (318,49),
                         (319,49)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      602 => ( Point => ( new Points'(
                         (317,47),
                         (318,47),
                         (318,48),
                         (319,48)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      603 => ( Point => ( new Points'(
                         (317,46),
                         (318,46),
                         (319,46),
                         (319,47),
                         (320,47)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      604 => ( Point => ( new Points'(
                         (317,45),
                         (318,45),
                         (319,45),
                         (320,45),
                         (320,46)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      605 => ( Point => ( new Points'(
                         (317,44),
                         (318,44),
                         (319,44),
                         (320,44)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      606 => ( Point => ( new Points'(
                         (318,42),
                         (318,43),
                         (319,43),
                         (320,43)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      607 => ( Point => ( new Points'(
                         (318,41),
                         (319,41),
                         (319,42),
                         (320,42)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      608 => ( Point => ( new Points'(
                         (318,40),
                         (319,40),
                         (320,40),
                         (320,41),
                         (321,41)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      609 => ( Point => ( new Points'(
                         (318,39),
                         (319,39),
                         (320,39),
                         (321,39),
                         (321,40)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      610 => ( Point => ( new Points'(
                         (318,37),
                         (318,38),
                         (319,38),
                         (320,38),
                         (321,38)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      611 => ( Point => ( new Points'(
                         (319,36),
                         (319,37),
                         (320,37),
                         (321,37)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      612 => ( Point => ( new Points'(
                         (319,35),
                         (320,35),
                         (320,36),
                         (321,36)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      613 => ( Point => ( new Points'(
                         (319,34),
                         (320,34),
                         (321,34),
                         (321,35)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      614 => ( Point => ( new Points'(
                         (319,32),
                         (319,33),
                         (320,33),
                         (321,33)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      615 => ( Point => ( new Points'(
                         (320,31),
                         (320,32),
                         (321,32),
                         (322,32)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      616 => ( Point => ( new Points'(
                         (320,30),
                         (321,30),
                         (321,31),
                         (322,31)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      617 => ( Point => ( new Points'(
                         (320,29),
                         (321,29),
                         (322,29),
                         (322,30)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      618 => ( Point => ( new Points'(
                         (320,28),
                         (321,28),
                         (322,28)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      619 => ( Point => ( new Points'(
                         (320,26),
                         (320,27),
                         (321,27),
                         (322,27)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      620 => ( Point => ( new Points'(
                         (321,25),
                         (321,26),
                         (322,26),
                         (323,26)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      621 => ( Point => ( new Points'(
                         (321,24),
                         (322,24),
                         (322,25),
                         (323,25)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      622 => ( Point => ( new Points'(
                         (321,23),
                         (322,23),
                         (323,23),
                         (323,24)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      623 => ( Point => ( new Points'(
                         (321,22),
                         (322,22),
                         (323,22)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      624 => ( Point => ( new Points'(
                         (321,20),
                         (321,21),
                         (322,21),
                         (323,21)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      625 => ( Point => ( new Points'(
                         (322,19),
                         (322,20),
                         (323,20),
                         (324,20)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      626 => ( Point => ( new Points'(
                         (322,18),
                         (323,18),
                         (323,19),
                         (324,19)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      627 => ( Point => ( new Points'(
                         (322,17),
                         (323,17),
                         (324,17),
                         (324,18)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      628 => ( Point => ( new Points'(
                         (322,15),
                         (322,16),
                         (323,16),
                         (324,16),
                         (325,16)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      629 => ( Point => ( new Points'(
                         (322,14),
                         (323,14),
                         (323,15),
                         (324,15),
                         (325,15)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      630 => ( Point => ( new Points'(
                         (323,13),
                         (324,13),
                         (324,14),
                         (325,14)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      631 => ( Point => ( new Points'(
                         (323,12),
                         (324,12),
                         (325,12),
                         (325,13)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      632 => ( Point => ( new Points'(
                         (323,10),
                         (323,11),
                         (324,11),
                         (325,11)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      633 => ( Point => ( new Points'(
                         (323,9),
                         (324,9),
                         (324,10),
                         (325,10)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      634 => ( Point => ( new Points'(
                         (323,8),
                         (324,8),
                         (325,8),
                         (325,9)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      635 => ( Point => ( new Points'(
                         (323,7),
                         (324,7),
                         (325,7),
                         (326,7)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      636 => ( Point => ( new Points'(
                         (324,5),
                         (324,6),
                         (325,6),
                         (326,6)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      637 => ( Point => ( new Points'(
                         (324,4),
                         (325,4),
                         (325,5),
                         (326,5)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      638 => ( Point => ( new Points'(
                         (324,3),
                         (325,3),
                         (326,3),
                         (326,4)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      639 => ( Point => ( new Points'(
                         (324,2),
                         (325,2),
                         (326,2),
                         (327,2)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      640 => ( Point => ( new Points'(
                         (324,1),
                         (325,1),
                         (326,1),
                         (327,1)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1),

      641 => ( Point => ( new Points'(
                         (324,0),
                         (325,0),
                         (326,0),
                         (327,0)
                        )),
              Threshold_Lower => -1,
              Threshold_Upper => -1,
              Link_Slice_Other_Rail => -1)
     );

   -- DATA END

end Adaimageprocessor.Image.Trackdata;
