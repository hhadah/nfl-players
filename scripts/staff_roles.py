"""Standardize raw NFL staff role titles from Wikipedia staff boxes.

standardize_role(role_raw, section) splits a raw title into its component
roles ("Assistant head coach/defensive line" -> ASST_HC + DL), maps each to a
role_std code, and returns one dict per role with:
  role_part       the fragment the code was assigned from
  role_std        code (see ROLE_GROUP for the full list)
  role_group      head_coach, coordinator, position_coach, assistant_coach,
                  strength_conditioning, support_staff, owner_executive,
                  general_manager, personnel_scouting, other_front_office
  unit            offense, defense, special_teams, front_office or none
  coach_position  position group the role concerns (QB, RB, WR, TE, OL, DL,
                  EDGE, LB, DB, K), also for assistants ("Assistant offensive
                  line" -> OFF_ASST with coach_position OL); None otherwise
  is_primary      True for the most senior role of the title (ties: the
                  first listed), so each listed person has exactly one
  interim         "interim"/"acting" appears in the title

Notes on the coding choices:
  - ASST_HC (assistant/associate head coach) is in role_group coordinator:
    it is a senior-staff title, usually held with a position role.
  - FOOTBALL_OPS_EXEC (president / executive or senior VP of football
    operations) and ASST_GM are in role_group general_manager with GM.
  - OTHER_COACH (assistants to the head coach, video, athletic trainers,
    player development, analysts without a unit) is role_group
    support_staff, not assistant_coach, so on-field coaching counts are not
    inflated by support staff.
  - Titles are split on "/" always; on a hyphen when every piece is a
    recognised role ("Head coach-general manager"); and on ",", "&" or
    "and" only when a piece is a stand-alone title ("President and CEO" -> PRESIDENT + CEO,
    but "Director of salary cap & player contracts" stays whole).
  - Abbreviations (HC, OC, DC, Def., Off., Assoc., Asst., S&C, ...) are
    expanded first.
  - An "offensive/defensive play caller" title is coded OC/DC. A bare
    "special teams" after an assistant role ("Quality control/special
    teams") is ST_ASST, not STC. "Interim" in a multi-role title marks only
    the head-coach role when there is one.
  - Teams without a titled coordinator (a head coach calling plays: SF under
    Shanahan, LA under McVay, ARI 2019-22, TB 2022-25 defense, NE defense)
    have no OC/DC row; this is not imputed.
"""
import re

ROLE_GROUP = {
    "HC": "head_coach",
    "OC": "coordinator", "DC": "coordinator", "STC": "coordinator",
    "ASST_HC": "coordinator", "PASS_GAME_COORD": "coordinator",
    "RUN_GAME_COORD": "coordinator",
    "QB": "position_coach", "RB": "position_coach", "WR": "position_coach",
    "TE": "position_coach", "OL": "position_coach", "DL": "position_coach",
    "EDGE_OLB": "position_coach", "LB": "position_coach", "ILB": "position_coach",
    "DB": "position_coach", "CB": "position_coach", "S": "position_coach",
    "NICKEL": "position_coach",
    "OFF_ASST": "assistant_coach", "DEF_ASST": "assistant_coach",
    "ST_ASST": "assistant_coach", "QC_OFF": "assistant_coach",
    "QC_DEF": "assistant_coach", "COACH_ASST": "assistant_coach",
    "S_AND_C": "strength_conditioning",
    "OTHER_COACH": "support_staff",
    "OWNER": "owner_executive", "CHAIR": "owner_executive",
    "VICE_CHAIR": "owner_executive", "CEO": "owner_executive",
    "PRESIDENT": "owner_executive",
    "GM": "general_manager", "ASST_GM": "general_manager",
    "FOOTBALL_OPS_EXEC": "general_manager",
    "VP_PLAYER_PERSONNEL": "personnel_scouting",
    "DIR_PLAYER_PERSONNEL": "personnel_scouting",
    "DIR_PRO_PERSONNEL": "personnel_scouting",
    "DIR_COLLEGE_SCOUTING": "personnel_scouting", "SCOUT": "personnel_scouting",
    "CAP_ADMIN": "other_front_office", "OTHER_FO": "other_front_office",
}
FRONT_OFFICE_GROUPS = {"owner_executive", "general_manager", "personnel_scouting",
                       "other_front_office"}
COACH_GROUPS = {"head_coach", "coordinator", "position_coach", "assistant_coach",
                "strength_conditioning"}

# Seniority used to pick the primary role of a multi-role title (lower = senior).
RANK = {"HC": 1, "OC": 2, "DC": 2, "STC": 2, "ASST_HC": 3, "PASS_GAME_COORD": 4,
        "RUN_GAME_COORD": 4, "S_AND_C": 7, "OTHER_COACH": 9,
        "OWNER": 1, "CHAIR": 2, "CEO": 2, "PRESIDENT": 3, "VICE_CHAIR": 4,
        "FOOTBALL_OPS_EXEC": 3, "GM": 3, "ASST_GM": 4, "VP_PLAYER_PERSONNEL": 5,
        "DIR_PLAYER_PERSONNEL": 6, "DIR_PRO_PERSONNEL": 6,
        "DIR_COLLEGE_SCOUTING": 6, "CAP_ADMIN": 6, "SCOUT": 7, "OTHER_FO": 9}
for _code, _group in ROLE_GROUP.items():
    RANK.setdefault(_code, 5 if _group == "position_coach" else 6)

# Titles that can stand alone after splitting "A, B and C" / "A & B".
STANDALONE = {"OWNER", "CHAIR", "VICE_CHAIR", "CEO", "PRESIDENT", "GM", "ASST_GM",
              "HC", "ASST_HC", "OC", "DC", "STC", "QB", "RB", "WR", "TE", "OL",
              "DL", "EDGE_OLB", "LB", "ILB", "DB", "CB", "S", "NICKEL"}
OFFENSE_CODES = {"OC", "QB", "RB", "WR", "TE", "OL", "OFF_ASST", "QC_OFF"}
DEFENSE_CODES = {"DC", "DL", "EDGE_OLB", "LB", "ILB", "DB", "CB", "S", "NICKEL",
                 "DEF_ASST", "QC_DEF"}
SPECIAL_TEAMS_CODES = {"STC", "ST_ASST"}
ASSISTANT_CODES = {c for c, g in ROLE_GROUP.items() if g == "assistant_coach"}

# Position the role concerns; checked in order (most specific first).
POSITIONS = [
    ("NICKEL", "DB", r"nickel|slot corner"),
    ("CB", "DB", r"cornerback|\bcorners\b|\bcbs?\b"),
    ("S", "DB", r"safet(y|ies)"),
    ("ILB", "LB", r"inside linebacker|\bilbs?\b|middle linebacker"),
    ("EDGE_OLB", "EDGE", r"outside linebacker|\bolbs?\b|\bedges?\b|pass[- ]rush|"
                         r"defensive ends?\b"),
    ("LB", "LB", r"linebacker|\blbs?\b"),
    ("DB", "DB", r"defensive back|secondary|\bdbs?\b|defensive backfield"),
    ("DL", "DL", r"defensive line|defense line|\bdl\b|defensive tackle|defensive front|interior"),
    ("QB", "QB", r"quarterback|\bqbs?\b"),
    ("RB", "RB", r"running back|\brbs?\b|fullback|halfback|offensive backfield"),
    ("WR", "WR", r"wide receiver|\breceivers?\b|\bwrs?\b"),
    ("TE", "TE", r"tight end|\btes?\b"),
    ("OL", "OL", r"offensive line|\bol\b|offensive tackle|\bguards\b|\bcenters\b"),
]
KICKING_RE = re.compile(r"kick(er|ers|ing)\b|kick returners|punter|long snap|specialists")

QUALIFIERS = {"offense", "offensive", "defense", "defensive", "pro", "college",
              "senior", "assistant"}
INTERIM_RE = re.compile(r"\b(interim|acting)\b")
# "Assistant to the head coach", "Senior advisor to the GM": support roles.
SUPPORT_TO_RE = re.compile(
    r"\b(assistant|advis[eo]r|aide|consultant|secretary|analyst|liaison|counsel)\b"
    r"[^/]*?\b(to|for)\b(?! (the )?(offense|defense|special teams|staff\b))"
    r"|special assistant|executive assistant|administrative assistant")
FO_WORDS_RE = re.compile(
    r"vice president|\bvp\b|\bevp\b|\bsvp\b|\bexecutive\b|officer\b|\bcounsel|"
    r"general manager|\bgm\b|owner|chair|president|\bceo\b|\bcoo\b|\bcfo\b|"
    r"scout|personnel|salary cap|football administration|business|marketing|"
    r"ticket|finance|communications|public relations|legal|partner|trustee|"
    r"founder|executor|board|sales|\bbrand\b|investment|accounting|security")


def section_std(section):
    """Map a staff-box section header to a standard section."""
    s = (section or "").lower()
    if not s:
        return "none"
    if "coach" in s and "head" in s and "support" not in s:
        return "head_coach"
    if re.search(r"offens", s):
        return "offense"
    if re.search(r"defens", s):
        return "defense"
    if re.search(r"special team", s):
        return "special_teams"
    if re.search(r"strength|conditioning|performance|sports science|medical|train", s):
        return "strength_conditioning"
    if re.search(r"coach|support|assistant|quality control|skill|development|"
                 r"fellow|minority|others|emotional", s):
        return "support"
    if re.search(r"front|owner|management|executive|administration|operations|"
                 r"scouting|personnel|business|marketing|ticket|research|"
                 r"analytics|strategy|football", s):
        return "front_office"
    return "other"


def _unit_word(text):
    if re.search(r"\bdefens(e|ive)\b", text):
        return "defense"
    if re.search(r"\boffens(e|ive)\b", text):
        return "offense"
    if re.search(r"special teams?", text):
        return "special_teams"
    return None


def _position(text):
    for code, pos, rx in POSITIONS:
        if re.search(rx, text):
            return code, pos
    if KICKING_RE.search(text):
        return None, "K"
    return None, None


def classify(fragment, section="none"):
    """Return (role_std, coach_position) for one role fragment (lowercase)."""
    f = fragment
    fo_context = section == "front_office" or bool(FO_WORDS_RE.search(f))
    unit = _unit_word(f) or (section if section in ("offense", "defense",
                                                    "special_teams") else None)
    # "Special assistant to the defense" or "... to the defensive
    # coordinator" is a defensive assistant; so is a "defensive special
    # assistant" or "special projects/defense" (unit named, no one assisted).
    if re.search(r"(assistant|consultant|advisor) to the (offense|defense|"
                 r"offensive coordinator|defensive coordinator)", f):
        return ("DEF_ASST" if "defens" in f else "OFF_ASST"), None
    if (re.search(r"special (assistant|projects)|\bprojects\b", f) and _unit_word(f)
            in ("offense", "defense") and not re.search(r"\b(to|for)\b", f)):
        return ("DEF_ASST" if _unit_word(f) == "defense" else "OFF_ASST"), None
    # Support roles attached to someone else ("assistant to the head coach").
    if SUPPORT_TO_RE.search(f):
        return ("OTHER_FO" if fo_context else "OTHER_COACH"), None
    # Ownership and top executives.
    if re.search(r"\b(co-)?owners?\b|owning entity|managing (general )?partner|"
                 r"general partner|limited partner|principal partner|\bpartners?\b|"
                 r"founder|trustee|executor|proprietor", f):
        return "OWNER", None
    if re.search(r"vice[- ]chair", f):
        return "VICE_CHAIR", None
    if re.search(r"\bchair(man|men|woman|women|person|people)?\b", f):
        return "CHAIR", None
    if re.search(r"\bceo\b|chief executive", f):
        return "CEO", None
    # Top football executive above or alongside the GM; a plain "vice
    # president of football operations" is not included (OTHER_FO).
    if re.search(r"((?<!vice )(?<!vice-)president|(executive|senior) vice president|"
                 r"\bevp\b|\bsvp\b|executive vp|\bhead|\bchief)\b.*football operations|"
                 r"chief football officer|"
                 r"executive vice president\W+(of )?football$", f):
        return "FOOTBALL_OPS_EXEC", None
    if re.search(r"(?<!vice )(?<!vice-)\bpresident\b", f) and \
            not re.search(r"business|enterprises|stadium|sales|marketing|"
                          r"communications|foundation", f):
        return "PRESIDENT", None
    if re.search(r"(assistant|asst\.?|associate|deputy) (general manager|gm)\b", f):
        return "ASST_GM", None
    if re.search(r"general manager|\bgm\b", f):
        return "GM", None
    # Personnel and scouting.
    if re.search(r"(vice president|\bvp\b|\bsvp\b|\bevp\b)\b.*"
                 r"(personnel|scouting|player evaluation)", f):
        return "VP_PLAYER_PERSONNEL", None
    if re.search(r"\bpro (player )?(personnel|personel|scouting|scout)|"
                 r"professional scouting", f):
        return ("DIR_PRO_PERSONNEL" if re.search(
            r"director|coordinator|manager|head|executive", f) else "SCOUT"), None
    if re.search(r"college (scouting|personnel|scout)|college$", f):
        return ("DIR_COLLEGE_SCOUTING" if re.search(
            r"director|coordinator|manager|head|executive|supervisor", f) else "SCOUT"), None
    if re.search(r"personnel|personel|player scouting|\bscouting\b", f):
        return ("DIR_PLAYER_PERSONNEL" if re.search(
            r"director|coordinator|executive|head|chief|manager", f) else "SCOUT"), None
    if re.search(r"\bscouts?\b|blesto", f):
        return "SCOUT", None
    if re.search(r"football (and |& )?(business )?administration|salary cap|"
                 r"\bcap\b|contracts?\b|player finance|football finance", f):
        return "CAP_ADMIN", None
    # Coaching roles.
    if re.fullmatch(r"(the )?head coach", f):
        return "HC", None
    if re.search(r"(assistant|associate|deputy) head coach", f):
        return "ASST_HC", None
    # A titled "offensive/defensive play caller" is the unit's coordinator
    # in all but name (New England 2021-22).
    if re.search(r"(^|co-)offensive coordinator|coordinator of offense|"
                 r"offensive play[- ]?caller", f) and not re.search(r"assistant", f):
        return "OC", None
    if re.search(r"(^|co-)defensive coordinator|coordinator of defense|"
                 r"defensive play[- ]?caller", f) and not re.search(r"assistant", f):
        return "DC", None
    if re.fullmatch(r"(co-)?special teams?( coordinator| coach)?|"
                    r"coordinator of special teams|special teams and \w+", f):
        return "STC", None
    if re.search(r"special teams?|kick(er|ers|ing)\b|\breturners\b|specialists|"
                 r"^specialist$|punters?|long snap", f):
        return "ST_ASST", ("K" if KICKING_RE.search(f) else None)
    if re.search(r"pass(ing)?( game)? coordinator|pass(ing)? defense coordinator|"
                 r"passing coordinator|^pass(ing)? game$", f):
        return "PASS_GAME_COORD", None
    if re.search(r"run(ning)?( game)? coordinator|rushing coordinator|"
                 r"run defense coordinator|^run(ning)? game$", f):
        return "RUN_GAME_COORD", None
    if re.search(r"strength|conditioning|athletic performance|sports performance|"
                 r"player performance|human performance|high performance|"
                 r"physical development|sports scien|speed training|"
                 r"performance (coach|manager|assistant|coordinator|analyst)|"
                 r"weight room|reconditioning|strength staff|\bstength\b|"
                 r"^performance$", f):
        return "S_AND_C", None
    code, pos = _position(f)
    if re.search(r"quality (control|coach)|^quality$|\bquality$", f):
        return {"offense": "QC_OFF", "defense": "QC_DEF",
                "special_teams": "ST_ASST"}.get(unit, "COACH_ASST"), pos
    assistant = re.search(r"\bassistants?\b|\basst\b|\bassociate\b|consultant|"
                          r"analyst|intern\b|specialist|advisor", f)
    if code and not assistant:
        return code, pos
    if pos and assistant:
        return ("DEF_ASST" if (unit == "defense" or code in DEFENSE_CODES)
                else "OFF_ASST"), pos
    if unit == "offense" and re.search(r"assistant|consultant|analyst|intern|"
                                       r"specialist|advisor|research|coach|"
                                       r"^offens(e|ive)$|game", f):
        return "OFF_ASST", pos
    if unit == "defense" and re.search(r"assistant|consultant|analyst|intern|"
                                       r"specialist|advisor|research|coach|"
                                       r"^defens(e|ive)$|game|package", f):
        return "DEF_ASST", pos
    if re.search(r"coaching (assistant|fellow|intern|associate|analyst)|fellowship|"
                 r"\bfellow\b|assistant coach|senior assistant|coaching assistants?$|"
                 r"minority coaching|diversity coaching|coaching intern|"
                 r"^assistants?$|squad development|situational|third down|"
                 r"game (management|manager)", f):
        return "COACH_ASST", pos
    if fo_context:
        return "OTHER_FO", None
    return "OTHER_COACH", None


# Abbreviations seen in staff boxes, expanded before classification.
ABBREVIATIONS = [
    (r"\bhc\b", "head coach"), (r"\boc\b", "offensive coordinator"),
    (r"\bdc\b", "defensive coordinator"), (r"\bstc\b", "special teams coordinator"),
    (r"\bdef\.", "defensive"), (r"\boff\.", "offensive"), (r"\bassoc\.", "associate"),
    (r"\bcoord\.?\b", "coordinator"), (r"\basst\.?(?=\s|$)", "assistant"),
    (r"\bst\b(?= coordinator| coach| assistant)", "special teams"),
    (r"\bs&c\b", "strength and conditioning"), (r"\bdir\.", "director"),
    (r"\bv\.p\.?", "vp"), (r"\bsr\.", "senior"), (r"\bexec\.", "executive"),
]


def _clean(text):
    """Lowercase, drop parentheticals, normalize dashes, expand abbreviations."""
    t = text.lower().replace("’", "'")
    t = re.sub(r"\([^)]*\)", " ", t)
    t = re.sub(r"[‐‑–—]", "-", t).replace("co–", "co-")
    for rx, full in ABBREVIATIONS:
        t = re.sub(rx, full, t)
    return re.sub(r"\s+", " ", t).strip(" ,.-:")


def _strip_coach(fragment):
    """'Quarterbacks coach' -> 'quarterbacks' (keeps 'head coach' etc.)."""
    if re.search(r"(head|assistant|associate|deputy) coach|^coach(es)?$", fragment):
        return fragment
    f = re.sub(r"\s+coach(es)?$", "", fragment)
    return re.sub(r"\s+(the|of)$", "", f).strip(" ,.-:")


def _hyphen_split(part, section):
    """Split 'Head coach-general manager' at a hyphen when every piece is a
    recognised role and one is a stand-alone title (keeps 'vice-chairman',
    'co-owner', 'pass-rush specialist' whole)."""
    pieces = [_strip_coach(x.strip()) for x in re.split(r"(?<!\bco)(?<!\bvice)\s*-\s*", part)
              if x.strip()]
    if len(pieces) < 2:
        return [part]
    codes = [classify(x, section)[0] for x in pieces]
    if (any(c in STANDALONE for c in codes)
            and all(c not in ("OTHER_FO", "OTHER_COACH") or x in QUALIFIERS
                    for c, x in zip(codes, pieces))):
        return pieces
    return [part]


def _split_fragments(role, section):
    """Split a cleaned title into role fragments (see module docstring)."""
    parts = []
    for p in role.split("/"):
        if p.strip(" ,.-"):
            parts += _hyphen_split(_strip_coach(p.strip(" ,.-")), section)
    # "Offensive/Defensive assistant": a leading bare qualifier borrows the
    # rest of the next fragment; a trailing one qualifies the previous one.
    # "Assistant linebackers/special teams" and "Fellowship coach-offense/
    # special teams" are assistant special-teams roles, not the coordinator.
    out = []
    for i, p in enumerate(parts):
        if p == "special teams" and out and "head coach" not in out[-1] and (
                out[-1].startswith("assistant")
                or classify(out[-1], section)[0] in ASSISTANT_CODES):
            out.append("assistant special teams")
            continue
        if p in QUALIFIERS:
            if not out and i + 1 < len(parts) and len(parts[i + 1].split()) > 1:
                out.append(f"{p} {parts[i + 1].split(' ', 1)[1]}")
            elif out:
                out[-1] = f"{out[-1]} {p}"
            else:
                out.append(p)
            continue
        out.append(p)
    # Split on ",", "&" and "and" only when a piece is a stand-alone title
    # (an executive, head coach, coordinator or position coach), so
    # "Executive vice president and head coach" -> OTHER_FO + HC but
    # "Director of salary cap & player contracts" stays whole.
    final = []
    for p in out:
        pieces = [_strip_coach(x.strip()) for x in
                  re.split(r",\s*(?:and\s+)?|\s+&\s+|\s+and\s+", p) if x.strip()]
        if (len(pieces) > 1 and not SUPPORT_TO_RE.search(p)
                and any(classify(x, section)[0] in STANDALONE for x in pieces)):
            final += pieces
        else:
            final.append(p)
    return final


def standardize_role(role_raw, section=None):
    """Split and code one raw role title. See the module docstring."""
    sec = section_std(section)
    text = _clean(role_raw or "")
    interim = bool(INTERIM_RE.search(text))
    text = re.sub(r"\s+", " ", INTERIM_RE.sub("", text)).strip(" ,.-")
    fragments = _split_fragments(text, sec) if text else []
    rows, seen = [], set()
    for frag in fragments or [""]:
        code, pos = classify(frag, sec) if frag else (
            "OTHER_FO" if sec == "front_office" else "OTHER_COACH", None)
        if code in seen:
            continue
        seen.add(code)
        group = ROLE_GROUP[code]
        if group in FRONT_OFFICE_GROUPS:
            unit = "front_office"
        elif code in OFFENSE_CODES:
            unit = "offense"
        elif code in DEFENSE_CODES:
            unit = "defense"
        elif code in SPECIAL_TEAMS_CODES:
            unit = "special_teams"
        elif code in ("PASS_GAME_COORD", "RUN_GAME_COORD", "ASST_HC"):
            unit = _unit_word(frag) or (sec if sec in ("offense", "defense")
                                        else ("offense" if code != "ASST_HC" else "none"))
        else:
            unit = "none"
        if pos is None and code in ROLE_GROUP and group == "position_coach":
            pos = _position(frag)[1]
        rows.append(dict(role_part=frag or None, role_std=code, role_group=group,
                         unit=unit, coach_position=pos, interim=interim))
    # "Interim head coach/special teams coordinator": the interim label
    # belongs to the head-coach role only.
    if interim and any(r["role_std"] == "HC" for r in rows):
        for r in rows:
            r["interim"] = r["role_std"] == "HC"
    best = min(range(len(rows)), key=lambda i: (RANK[rows[i]["role_std"]], i))
    for i, r in enumerate(rows):
        r["is_primary"] = i == best
    return rows
