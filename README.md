# Softball League Scheduler & Game-Bag Tracker

An R Shiny app for whoever runs the league next. It builds the season
schedule around blackout dates, and separately tracks which numbered
equipment bag (bases, etc.) goes where each week, so nobody shows up with
two bags (or none).

## Running it

1. Install R (4.x) from https://www.r-project.org, or use RStudio.
2. Install the three packages this app needs.
   ```r
   install.packages(c("shiny", "DT", "openxlsx"))
   ```
3. Open `app.R` in RStudio and click **Run App**, or from a terminal:
   ```r
   shiny::runApp("softball_scheduler.R")
   ```


## Using it

1. Set the number of teams; a name field appears for each one.
2. Set how many teams sit out (bye) each week. This must share the same
   parity as the team count -- with an odd number of teams you need an odd
   number of byes (1, 3, ...), with an even number of teams you need an
   even number of byes (0, 2, 4, ...) -- so the rest can pair up evenly. If
   your current season has 2 teams off every week, enter `2` here.
3. Pick a season start date and how many weeks of games you want.
4. Enter any league-wide blackout dates (holidays, field closures) and any
   team-specific blackout dates, each as a comma-separated list of
   `YYYY-MM-DD` dates.
5. Click **Generate Schedule**.

Four tabs come back:

- **Schedule & Bags**
- every week's matchups, which field each game is
  on, and which numbered bag the home team is carrying.
- **Off-Day Bag Transfers**
- the list of "Team X hands Bag N to Team Y before this week"
  instructions. Ideally this list is
  short or empty most weeks.
- **Bag Tracker (by bag #)**
- each bag's full season itinerary, if you
  want to trace one bag's whole path.
- **Field Fairness**
- each team's total low-field vs. good-field game
  count and the resulting percentage, sorted worst-off first, so you can
  eyeball that nobody's getting stuck on the bad fields.

The hard limit for overall league size is simultaneous games. Fields 9-15
can only host 7 games in a single week. So (teams - byes) / 2 must be 7 or
fewer -- a 20-team league works fine as long as at least 6 teams are on
bye every week (and everyone is happy).

If a team's blackout date genuinely can't be avoided (usually only happens
with very tight schedules or unusually many blackout dates), the app flags
it in a banner up top so you can adjust by hand e.g. swap two weeks, add an
extra week to the season, etc.

## How the scheduling works

- **Byes go to blacked-out teams first.** For each calendar date, the app
  checks which teams have a team-specific blackout on that exact date and
  gives them first claim on that week's bye slots. A team's blackout
  date automatically becomes one of their bye weeks whenever there's room.
  If more teams are blacked out on a date than there are bye slots (e.g.
  three teams have a conflict but you only allow 2 byes/week), the extras
  still get scheduled to play, and the app flags it in the banner up top.
- Any bye slots left over after that go to whichever teams have had the
  *fewest* byes so far this season, so bye weeks still end up spread
  evenly across the roster overall.
- Among the teams playing that date, opponents are matched by fewest prior
  meetings this season: so the schedule works toward a full round robin
  (everyone plays everyone) before it ever repeats a matchup.

A team with a lot of blackout dates will naturally rack up more total byes 
than a team with none, since their blackout dates keep claiming bye slots 
ahead of the fairness rule. Confirm with captains that they would prefer to 
have all byes on all blackout dates and play fewer total games.

## How the bag logic works

Every team either holds a bag or doesn't. Home team = whoever is currently
holding a bag; they bring it to the game. A "clean" week is one where every
matchup pairs a bag-holder against a non-holder.Easy peasy, the bag just 
goes home with whichever of the two teams needs it for their next game.

Occasionally the round-robin structure produces a week with two
bag-holders facing each other (and, elsewhere that week, two non-holders
facing each other). That's the only time an off-day transfer is necessary: 
one bag moves from the surplus team to the short team before game
day. After each week, the app looks one week ahead and picks whichever
team keeps each bag in a way that avoids creating extra transfers the
following week -- including making sure a team about to sit out on bye
isn't left holding a bag it won't use.

With two or more teams on bye every week, the schedule has more moving parts 
and a handful of transfers can show up over a season. 
Blackout-driven schedule tweaking can add a few more on top of that.

## How field assignment works

There are 7 fields (9-15): In my humble opinion, 
10-13 are equally desirable, and 9, 14, and 15 are the lower-quality fields 
(9 is far and often has trash on it. 14 and 15 are small and don't have 
protected dugouts). If you have different opinions about field quality, just 
change the fields listed as HIGH or LOW in the app file.
Each week, if there are more games than good fields, the app sends the 
*lowest-quality-field duty owed* matchups to the low fields. It ranks 
each week's matchups by how many low-field games the two teams involved 
have already racked up combined,and puts the pairs with the fewest onto 
fields 9, 14, and 15 first. Every other matchup gets one of the four good 
fields. Since a league of up to 6 teams (3 games/week) never needs more than 
the 4 good fields, low fields only come into play once a league is big enough 
to need them. from then on the app steers low-field duty 
toward whoever's had the least of it.
