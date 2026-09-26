"""Position crosswalk shared by all loaders.

nflverse rosters switch from fine codes (CB, DE, OLB, T, ...) in 2002-2015 to
coarse codes (DB, DL, LB, OL) from 2016, and OverTheCap uses market codes
(LT, IDL, ED, ...); Pro-Football-Reference adds side prefixes and
multi-position codes (LCB, ROLB, G/T, RDE/LDE). position_group() maps all of
them to one stable set: QB, RB, WR, TE, OL, DL, LB, DB, K, P, LS. Returns None
for unknown codes.
"""

POSITION_GROUP = {
    # offense
    "QB": "QB",
    "RB": "RB", "HB": "RB", "FB": "RB",
    "WR": "WR",
    "TE": "TE",
    "OL": "OL", "T": "OL", "OT": "OL", "LT": "OL", "RT": "OL",
    "G": "OL", "OG": "OL", "LG": "OL", "RG": "OL", "C": "OL", "IOL": "OL",
    # defense
    "DL": "DL", "DE": "DL", "DT": "DL", "NT": "DL", "IDL": "DL",
    "ED": "DL", "EDGE": "DL",   # OTC edge market; 3-4 OLBs appear as LB on rosters
    "LB": "LB", "OLB": "LB", "ILB": "LB", "MLB": "LB",
    "DB": "DB", "CB": "DB", "S": "DB", "SS": "DB", "FS": "DB", "SAF": "DB",
    # specialists
    "K": "K", "PK": "K",
    "P": "P",
    "LS": "LS",
}

OFFENSE = {"QB", "RB", "WR", "TE", "OL"}
DEFENSE = {"DL", "LB", "DB"}
SPECIALISTS = {"K", "P", "LS"}


def position_group(code):
    """Stable position group for any code; for multi-position codes the first
    listed position counts, and a PFR left/right prefix is dropped."""
    if code is None:
        return None
    first = str(code).replace("-", "/").split("/")[0].strip().upper()
    group = POSITION_GROUP.get(first)
    if group is None and first[:1] in ("L", "R"):
        group = POSITION_GROUP.get(first[1:])
    return group


def unit(group):
    """'offense', 'defense', 'special_teams', or None."""
    if group in OFFENSE:
        return "offense"
    if group in DEFENSE:
        return "defense"
    if group in SPECIALISTS:
        return "special_teams"
    return None
