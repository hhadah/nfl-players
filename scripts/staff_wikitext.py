"""Parse Wikipedia NFL staff boxes into (section, role, person) entries.

Used by 02b_load_nfl_staff.py for two sources:
  - revisions of "Template:<Team> staff" (2007 onward), and
  - the staff box inside "<season> <team> season" articles (1999-2006).

The markup changed over the years. parse_staff_box() handles every variant
found in those revisions:
  - section headers as '''Bold''', ;Definition, == Heading ==, a bold table
    cell, a bold-only bullet (sub-header), or a parameter of the
    {{NFL final staff}} template (Front Office= / front_office= ...);
  - entry lines starting with * : or #, with role and person separated by
    an en/em dash, a minus sign, a spaced hyphen or a colon;
  - several people per role (commas, "and", "&", <br>, {{ubl}} lists);
  - name-only lines (role = parenthetical note or the section header);
  - (interim)/(acting) notes, "Vacant"/"TBA", footnote markers (e.g. a
    suspended coach marked with a dagger) and {{small}}/{{nowrap}} wrappers.

Also provides person-name helpers (normalize_name, split_name) and the
race/ethnicity evidence-sentence extractor used for staff_person_wiki_signals.
"""
import html
import re
import unicodedata

import mwparserfromhell as mwp

# Templates whose first positional parameter is kept as plain text.
PASS_THROUGH = {"small", "nowrap", "nobold", "big", "resize", "smaller",
                "larger", "noitalic", "nobr", "sup", "sub", "mdash"}
# Templates that hold a list of items (joined with ", ").
LIST_TEMPLATES = {"ubl", "unbulleted list", "ublist", "unbulleted", "plainlist",
                  "flatlist", "hlist", "bulleted list", "blist", "plain list"}
# Characters used as footnote markers next to names.
FOOTNOTE_MARKS = "†‡§¤¶"
VACANT_WORDS = {"vacant", "tba", "tbd", "open", "none", "n/a", "position vacant",
                "to be announced", "to be determined", "vacancy"}
NAME_SUFFIXES = {"jr", "sr", "ii", "iii", "iv", "v"}
# Credentials that follow a name after a comma ("Jane Doe, PhD").
CREDENTIALS = {"phd", "md", "m d", "dds", "cpa", "esq", "atc", "cscs", "pt", "dpt",
               "rd", "ms", "mba", "jd", "do", "lat", "edd", "psyd", "pa c", "pa-c"}
# Lowercase words allowed inside a personal name.
NAME_PARTICLES = {"de", "del", "della", "der", "di", "da", "du", "la", "le", "van",
                  "von", "st", "dos", "das", "ter", "y", "bin", "al", "el", "mc"}
# Three lowercase words in a row: a prose note rather than a name list.
PROSE_RE = re.compile(r"\b[a-z]+\s+[a-z]+\s+[a-z]+\b")
# Roles that are not people (the 49ers once listed a therapy dog).
NON_PERSON_ROLE_RE = re.compile(r"therapy dog|mascot|team dog", re.I)
# Words that mark a non-person entity listed as an owner ("Paul G. Allen
# Trust", "Publicly held corporation", the Packers' "Board of Directors").
ENTITY_WORDS = re.compile(r"\b(family|families|trust|estate|group|inc|corporation|"
                          r"partners|partnership|llc|foundation|shareholders|"
                          r"publicly|holding|holdings|company|ownership|"
                          r"directors)\b", re.I)

LINK_RE = re.compile(r"\[\[([^\[\]|]+)(?:\|([^\[\]]*))?\]\]")
ENTRY_RE = re.compile(r"^([*#:]+)\s*(.*)$")
HEADER_RES = [re.compile(r"^;\s*(.+?)\s*:?\s*$"),
              re.compile(r"^'''\s*([^']+?)\s*'''\s*:?\s*$"),
              re.compile(r"^=+\s*(.+?)\s*=+$")]
BOLD_ONLY_RE = re.compile(r"^'''\s*([^']+?)\s*'''\s*:?\s*$")
# Role/person separator (links are masked with \x00 first): dash-like
# characters, a hyphen with a space on either side or directly before a
# link, or a colon followed by a space.
SEP_RE = re.compile(r"\s*(?:[–—−―‒]|\s-|-\s|-(?=\x00)|:(?=\s)|\s=\s)\s*")
TRAILING_SEP_RE = re.compile(r"\s*(?:[–—−―‒]|-|:|=)\s*$")
PAREN_RE = re.compile(r"\(([^()]*)\)")
SPLIT_PEOPLE_RE = re.compile(r"\s*(?:,|;|\s+and\s+|&|\s/\s|/)\s*")
INTERIM_RE = re.compile(r"\b(interim|acting)\b", re.I)
TAG_RE = re.compile(r"<[^<>]+>")
BR_RE = re.compile(r"<\s*br\s*/?\s*>|<\s*/\s*br\s*>", re.I)


# ============================================================================
# Markup clean-up
# ============================================================================

def _strip_blocks(text):
    """Drop comments, <ref> footnotes and <noinclude> blocks."""
    text = re.sub(r"<!--.*?(-->|$)", "", text, flags=re.S)
    text = re.sub(r"<ref[^>/]*/>", "", text, flags=re.I)
    text = re.sub(r"<ref[^>]*>.*?</ref\s*>", "", text, flags=re.I | re.S)
    text = re.sub(r"<noinclude>.*?(</noinclude>|$)", "", text, flags=re.I | re.S)
    return re.sub(r"</?(includeonly|onlyinclude)>", "", text, flags=re.I)


def _template_replacement(tpl):
    """Plain-text replacement for one template (children already replaced)."""
    name = str(tpl.name).strip().lower().replace("_", " ")
    positional = [str(p.value).strip() for p in tpl.params if not p.showkey]
    # {{NFL final staff}} and similar: every parameter holding a bulleted
    # list becomes a section header followed by its lines.
    sections = [p for p in tpl.params
                if re.search(r"(^|\n)\s*[*:]+\s*\S", str(p.value))]
    if sections and name not in LIST_TEMPLATES:
        parts = []
        for p in sections:
            header = str(p.name).strip().replace("_", " ") if p.showkey else ""
            parts.append(f"\n;{header}\n{str(p.value).strip()}\n" if header
                         else f"\n{str(p.value).strip()}\n")
        return "".join(parts)
    if name in LIST_TEMPLATES:
        items = []
        for v in positional:
            items += [re.sub(r"^[*#]\s*", "", x).strip() for x in v.split("\n")]
        return ", ".join(x for x in items if x)
    if name in PASS_THROUGH:
        return positional[0] if positional else ""
    if name == "sortname" and len(positional) >= 2:
        full = f"{positional[0]} {positional[1]}"
        if len(positional) >= 3 and positional[2] and positional[2] != "nolink":
            return f"[[{positional[2]}|{full}]]"
        if len(positional) >= 3 and positional[2] == "nolink":
            return full
        return f"[[{full}]]"
    if name in {"ill", "interlanguage link", "interlanguage link multi"} and positional:
        return positional[0]
    return ""


def flatten_templates(text):
    """Replace every template by its text content (innermost first)."""
    code = mwp.parse(text)
    # Reverse document order visits children before their parents.
    for tpl in reversed(code.filter_templates(recursive=True)):
        code.replace(tpl, _template_replacement(tpl))
    return str(code)


def _cell_content(line):
    """For a table-cell line ('| attrs | content'), return the content."""
    if not line.startswith(("|", "!")) or line.startswith(("|}", "|-", "|+", "{|")):
        return line
    masked = LINK_RE.sub(lambda m: "\x00" * len(m.group(0)), line)
    cut = masked.rfind("|")
    return line[cut + 1:].strip() if cut > 0 else line[1:].strip()


def _clean_inline(text):
    """Strip HTML tags, entities, bold/italic quotes and extra spaces."""
    text = BR_RE.sub(", ", text)
    text = TAG_RE.sub("", text)
    text = html.unescape(text).replace("\xa0", " ")
    text = re.sub(r"'{2,}", "", text)
    return re.sub(r"\s+", " ", text).strip()


# ============================================================================
# People
# ============================================================================

def parse_people(text):
    """Split the person part of an entry into people.

    Returns a list of dicts: name, link (raw wikilink target or None),
    interim, vacant, note (non-interim parenthetical), footnote (marker),
    entity (an organization, family or trust rather than a person).
    """
    links = []

    def _mask_link(m):
        links.append((m.group(1).strip(), (m.group(2) or m.group(1)).strip()))
        return f"\x00{len(links) - 1}\x00"

    notes = []

    def _mask_paren(m):
        notes.append(m.group(1).strip())
        return f"\x01{len(notes) - 1}\x01"

    masked = LINK_RE.sub(_mask_link, text)
    masked = PAREN_RE.sub(_mask_paren, masked)
    masked = _clean_inline(masked)
    people = []
    for frag in SPLIT_PEOPLE_RE.split(masked):
        frag = re.sub(r"^(?:and|&)\s+", "", frag.strip(" .:-–'\""))
        if not frag:
            continue
        # Parenthetical notes and footnote marks attach to this fragment.
        frag_notes = [notes[int(i)] for i in re.findall(r"\x01(\d+)\x01", frag)]
        frag = re.sub(r"\x01\d+\x01", " ", frag)
        marks = "".join(c for c in frag if c in FOOTNOTE_MARKS)
        frag = "".join(c for c in frag if c not in FOOTNOTE_MARKS).strip()
        interim = any(INTERIM_RE.search(n) for n in frag_notes)
        note = "; ".join(n for n in frag_notes if not INTERIM_RE.search(n)) or None
        link_ids = [int(i) for i in re.findall(r"\x00(\d+)\x00", frag)]
        residual = _clean_inline(re.sub(r"\x00\d+\x00", " ", frag))
        if INTERIM_RE.search(residual):
            interim = True
            residual = INTERIM_RE.sub("", residual).strip()
        # A bare suffix ("Jr.") continues the previous plain-text name;
        # a bare credential ("PhD") is dropped.
        bare = residual.lower().strip(".").replace(".", " ").strip()
        if not link_ids and bare in NAME_SUFFIXES and people:
            prev = people[-1]
            if prev["link"] is None and prev["name"]:
                prev["name"] = f"{prev['name']} {residual}"
            continue
        if not link_ids and bare in CREDENTIALS:
            continue
        # Text next to a name that names an organization ("[[Ralph Wilson]]
        # Trust", "the [[Santo Domingo family]]") marks a non-person entity.
        entity = bool(ENTITY_WORDS.search(residual))
        if link_ids:
            for i in link_ids:
                target, shown = links[i]
                shown = _clean_inline(shown)
                if shown.lower() in VACANT_WORDS:      # "[[Vacant]]"
                    people.append(dict(name=None, link=None, interim=interim,
                                       vacant=True, note=note,
                                       footnote=marks or None, entity=False))
                    continue
                people.append(dict(name=shown, link=target, interim=interim,
                                   vacant=False, note=note, footnote=marks or None,
                                   entity=entity or bool(ENTITY_WORDS.search(shown))))
            continue
        name = re.sub(r"^(Dr|Rev|Mr|Mrs|Ms)\.?\s+", "", residual).strip(" .,'\"")
        if not name or not re.search(r"[A-Za-z]", name):
            if frag_notes and people:   # "(interim)" after a separator
                people[-1]["interim"] |= interim
                if note:
                    people[-1]["note"] = note
            continue
        if name.lower() in VACANT_WORDS:
            people.append(dict(name=None, link=None, interim=interim, vacant=True,
                               note=note, footnote=marks or None, entity=False))
            continue
        if not _plausible_name(name):
            continue                  # prose, e.g. "Sale to Kim ... pending"
        people.append(dict(name=name, link=None, interim=interim, vacant=False,
                           note=note, footnote=marks or None, entity=entity))
    return people


def link_text(text):
    """Replace [[target|shown]] by its shown text."""
    return LINK_RE.sub(lambda m: (m.group(2) or m.group(1)), text)


def _split_role_person(content):
    """Return (role_text or None, person_text) for one entry line.

    The role ends at the last separator outside wikilinks whose two sides
    both contain text, so roles that contain a dash ("Assistant sports
    performance: speed training – John Shaw") or a linked role title
    ("[[Offensive coordinator]] – [[Kliff Kingsbury]]") split correctly.
    """
    masked = LINK_RE.sub(lambda m: "\x00" * len(m.group(0)), content)
    for m in reversed(list(SEP_RE.finditer(masked))):
        left, right = content[:m.start()].strip(), content[m.end():].strip()
        if re.search(r"[A-Za-z]", link_text(left)) and re.search(r"[A-Za-z]", right):
            return left, right
    # "Role –" with nobody listed: a vacancy.
    if TRAILING_SEP_RE.search(masked) and "\x00" not in masked:
        return TRAILING_SEP_RE.sub("", content), "Vacant"
    # "Assistant General Manager [[Tom Modrak]]": no separator before a
    # link that ends the line.
    first_link = masked.find("\x00")
    after_links = masked[masked.rfind("\x00") + 1:]
    if (first_link > 0 and re.search(r"[A-Za-z]{3,}", masked[:first_link])
            and not re.search(r"[A-Za-z]", PAREN_RE.sub("", after_links))):
        return content[:first_link].strip(), content[first_link:]
    return None, content


def _plausible_name(name):
    """True if every word is capitalized, an initial, a particle or a suffix."""
    words = name.replace(".", ". ").split()
    if not words or len(words) > 6:
        return False
    for w in words:
        core = w.strip(".,'\"()")
        if not core:
            continue
        if core.lower() in NAME_PARTICLES or core.lower() in NAME_SUFFIXES:
            continue
        if not (core[0].isupper() or core[0].isdigit()):
            return False
    return True


# ============================================================================
# Staff box
# ============================================================================

def parse_staff_box(wikitext):
    """Parse one staff box. Returns (entries, n_unparsed, footnotes).

    entries: list of dicts with section, subsection, role_text (None when
    the line had no role), person dict (see parse_people), entry_order
    (line number among entry lines), person_order, line_raw.
    n_unparsed: entry lines that produced neither a person nor a vacancy.
    """
    text = flatten_templates(_strip_blocks(wikitext))
    entries, footnotes = [], {}
    section, subsection = None, None
    n_unparsed, order = 0, 0
    for raw in text.splitlines():
        line = _cell_content(raw.strip())
        if not line:
            continue
        header = None
        for rx in HEADER_RES:
            m = rx.match(line)
            if m:
                header = _clean_inline(m.group(1))
                break
        if header:
            section, subsection = header, None
            continue
        m = ENTRY_RE.match(line)
        if not m:
            plain = _clean_inline(line)
            if plain and plain[0] in FOOTNOTE_MARKS:
                mark = "".join(c for c in plain[:3] if c in FOOTNOTE_MARKS)
                footnotes[mark] = plain.lstrip(FOOTNOTE_MARKS + " –-:").strip()
            continue
        content = m.group(2).strip()
        bold = BOLD_ONLY_RE.match(content)
        if bold:                      # bullet holding only a bold sub-header
            subsection = _clean_inline(bold.group(1))
            continue
        if not re.search(r"[A-Za-z]", _clean_inline(LINK_RE.sub(" x ", content))):
            continue                  # empty bullet left by a removed template
        role, person_text = _split_role_person(content)
        role = _clean_inline(link_text(role)) if role else None
        if role and NON_PERSON_ROLE_RE.search(role):
            continue
        if role is None and PROSE_RE.search(_clean_inline(LINK_RE.sub("X", content))):
            continue                  # a note, e.g. "Sale to Kim and [[Terry Pegula]] pending"
        order += 1
        people = parse_people(person_text)
        if not people:
            n_unparsed += 1
            continue
        for j, p in enumerate(people, start=1):
            entries.append(dict(section=section, subsection=subsection,
                                role_text=role, person=p, entry_order=order,
                                person_order=j, line_raw=_clean_inline(content)))
    for e in entries:
        mark = e["person"]["footnote"]
        e["footnote_text"] = footnotes.get(mark) if mark else None
    return entries, n_unparsed, footnotes


def extract_season_staff_box(article_text):
    """Return the wikitext of the staff box in a season article, or None.

    The box is either an {{NFL final staff}} template or a substituted
    toccolours table whose caption/header names the staff.
    """
    code = mwp.parse(article_text)
    for tpl in code.filter_templates(recursive=False):
        if str(tpl.name).strip().lower().replace("_", " ") == "nfl final staff":
            return str(tpl)
    for m in re.finditer(r"\{\|[^\n]*\n", article_text):
        start = m.start()
        head = article_text[start:start + 600]
        head_lines = [ln for ln in head.splitlines()[1:5] if ln.startswith(("!", "|+"))]
        if not any(re.search(r"staff", ln, re.I) for ln in head_lines):
            continue
        end = article_text.find("\n|}", start)
        if end > 0:
            return article_text[start:end + 3]
    return None


# {{Infobox NFL team season}} parameters naming staff, with the role title
# each is given in the synthetic staff box built from them.
INFOBOX_ROLES = [("owner", "Owner"), ("president", "President"),
                 ("general_manager", "General manager"), ("coach", "Head coach"),
                 ("off_coach", "Offensive coordinator"),
                 ("def_coach", "Defensive coordinator")]
INFOBOX_NAMES = {"infobox nfl team season", "infobox nfl season",
                 "infobox gridiron football team season"}


def extract_season_infobox(article_text):
    """Return the staff named in a season article's {{Infobox NFL team
    season}} (or a variant in INFOBOX_NAMES) as staff-box wikitext
    ("* Head coach – [[A]] (fired ...), [[B]] (interim; ...)"), or None when
    the article has no such infobox.

    The infobox lists every head coach of the season, interims included,
    with the GM, owner and (for some seasons) the coordinators, so it covers
    in-season changes and seasons whose article has no staff box.
    """
    for tpl in mwp.parse(article_text).filter_templates(recursive=False):
        if str(tpl.name).strip().lower().replace("_", " ") not in INFOBOX_NAMES:
            continue
        lines = [";Infobox"]
        for param, role in INFOBOX_ROLES:
            if tpl.has(param):
                value = " ".join(str(tpl.get(param).value).split())
                # Keep only "(interim)" of a note: "(fired December 4, 7–6
                # record)" would otherwise be split at its dash.
                value = PAREN_RE.sub(lambda m: " (interim)" if INTERIM_RE.search(m.group(1))
                                     else " ", value)
                if value:
                    lines.append(f"* {role} – {value}")
        return "\n".join(lines)
    return None


# ============================================================================
# Names
# ============================================================================

def normalize_name(name):
    """Lowercase ASCII name without punctuation (keeps suffixes)."""
    if not name:
        return None
    s = unicodedata.normalize("NFKD", name).encode("ascii", "ignore").decode()
    s = re.sub(r"[^A-Za-z0-9 ]+", " ", s.replace("'", "").replace(".", " "))
    s = re.sub(r"\s+", " ", s).strip().lower()
    # Collapse initials: "a j smith" -> "aj smith".
    s = re.sub(r"\b([a-z]) (?=[a-z]\b)", r"\1", s)
    return s or None


SURNAME_PARTICLES = {"de", "del", "della", "der", "di", "da", "du", "la", "le",
                     "van", "von", "st", "st.", "mac", "dos", "das", "ter"}


def split_name(name):
    """Return (first_name, last_name, suffix) from a display name."""
    if not name:
        return None, None, None
    s = re.sub(r'"[^"]*"|“[^”]*”', " ", name)          # drop quoted nicknames
    s = re.sub(r"\([^)]*\)", " ", s)
    tokens = [t for t in re.split(r"[\s,]+", s.strip()) if t]
    suffix = None
    if len(tokens) > 1 and tokens[-1].lower().strip(".") in NAME_SUFFIXES:
        suffix = tokens.pop()
    # Leading initials form one first name: "A. J. Smith" -> "A.J."
    while len(tokens) > 2 and re.fullmatch(r"[A-Z]\.?", tokens[0]) \
            and re.fullmatch(r"[A-Z]\.?", tokens[1]):
        tokens[:2] = [tokens[0].rstrip(".") + "." + tokens[1].rstrip(".") + "."]
    # An initials-only first name is written without periods ("A.J." ->
    # "AJ"), the form name lists (Census/SSA first names) use.
    if len(tokens) > 1 and re.fullmatch(r"(?:[A-Z]\.?)+", tokens[0]):
        tokens[0] = tokens[0].replace(".", "")
    if not tokens:
        return None, None, suffix
    if len(tokens) == 1:
        return tokens[0], None, suffix
    last_start = len(tokens) - 1
    while last_start > 1 and tokens[last_start - 1].lower() in SURNAME_PARTICLES:
        last_start -= 1
    return tokens[0], " ".join(tokens[last_start:]), suffix


def title_to_name(title):
    """Wiki title without its disambiguator: 'Danny Smith (coach)' -> 'Danny Smith'."""
    return re.sub(r"\s*\([^)]*\)\s*$", "", title).strip() if title else None


def given_names_match(shown, title):
    """Quick test that a link's shown name can be the titled person: the
    first given names start with the same letter ('Mike' ~ 'Michael')."""
    a = normalize_name(split_name(shown)[0])
    b = normalize_name(split_name(title_to_name(title))[0])
    return not a or not b or a[0] == b[0]


def intro_names_person(shown, title, intro):
    """True if every capitalized given name in `shown` occurs in the name
    that opens the article intro, so '[[Bud Adams|K.S. Adams, Jr.]]' (Kenneth
    Stanley "Bud" Adams Jr.) is kept but '[[Dean Spanos|A. G. Spanos]]' (a
    link to a relative) is not. An initial matches a word's first letter and
    a short form a word's start ('Cal' ~ 'Calhoun')."""
    head = re.split(r"\s\(|,|\s(?:is|was)\s", intro, maxsplit=1)[0]
    words = (normalize_name(head) or "").split()
    surname = set((normalize_name(split_name(title_to_name(title))[1]) or "").split())
    plain = unicodedata.normalize("NFKD", shown).encode("ascii", "ignore").decode()
    plain = plain.replace("'", "")                      # O'Brien -> OBrien
    given =[t.lower() for t in re.findall(r"[A-Z][A-Za-z]*", plain)
             if t.lower() not in surname | NAME_SUFFIXES]
    return all(any(w.startswith(t) for w in words) for t in given)


# ============================================================================
# Race/ethnicity evidence in article text (machine signal, not coding)
# ============================================================================

EVIDENCE_TERMS = [
    r"African[- ]Americans?", r"Afro-[A-Z][a-z]+", r"first Black", r"Black (?:head coach|coach|coaches|general manager|GM|owner|executive|coordinator|man|men|people|community|players?|quarterback|candidates?|assistant)",
    r"Hispanic", r"Latino", r"Latina", r"Latinx", r"Mexican[- ]American", r"Mexican",
    r"Puerto Rican", r"Cuban[- ]American", r"Cuban", r"Dominican", r"Samoan", r"Polynesian",
    r"Tongan", r"Native Hawaiian", r"Pacific Islander", r"Asian[- ]American",
    r"(?:Japanese|Korean|Chinese|Filipino|Vietnamese|Taiwanese|Indian)[- ]American",
    r"Filipino", r"Native American", r"American Indian", r"Cherokee", r"Navajo",
    r"Lakota", r"Choctaw", r"Chickasaw", r"Muscogee", r"Ojibwe", r"Comanche", r"Kiowa",
    r"Osage Nation", r"Lumbee", r"Blackfeet", r"Hopi", r"Mohawk", r"Oneida Nation",
    r"minority (?:head coach|coach|coaches|candidates?|hiring|general manager|GMs?|executives?|assistant)",
    r"minorities", r"Rooney Rule", r"Fritz Pollard",
]
# A term naming a place or an institution ("Cherokee High School", "Mexican
# authorities") is not evidence about the person.
EVIDENCE_RE = re.compile(r"\b(" + "|".join(EVIDENCE_TERMS) + r")\b"
                         r"(?! (?:High School|County|authorities|police|government|"
                         r"League|Winter League|food|restaurants?)\b)")


def article_plain_text(wikitext):
    """Readable text of an article (templates, refs, tables and files removed)."""
    text = _strip_blocks(wikitext)
    text = re.sub(r"\{\|.*?\n\|\}", " ", text, flags=re.S)       # tables
    code = mwp.parse(text)
    for tpl in code.filter_templates(recursive=False):
        code.remove(tpl)
    text = code.strip_code(normalize=True, collapse=True)
    text = re.sub(r"\b(?:File|Image|Category):[^\n]*", " ", text)
    return re.sub(r"\s+", " ", text).strip()


def evidence_sentences(wikitext, max_n=3):
    """Up to max_n (term, sentence) pairs mentioning race/ethnicity terms."""
    text = article_plain_text(wikitext)
    out = []
    for sent in re.split(r"(?<=[.!?])\s+(?=[A-Z\"“])", text):
        m = EVIDENCE_RE.search(sent)
        if m:
            out.append((m.group(1), sent.strip()[:600]))
            if len(out) >= max_n:
                break
    return out
