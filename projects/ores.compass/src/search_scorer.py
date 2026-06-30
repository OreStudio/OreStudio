"""
Explicit weighted scoring model for compass search.

All ranking signals are normalised to [0.0, 1.0] and combined via a
named-weight linear model to produce a single relevance percentage.
The pipeline is:

  QueryPlan   — tokenise, strip stopwords, split core vs full words
  DocSignals  — raw per-document signal values collected from FTS / doc index
  Weights     — named floats controlling each signal's contribution
  ScoreResult — final [0,1] float, integer %, and per-signal breakdown

The scorer has no live-DB dependency: pass pre-computed signals and it
returns a result.  This makes it unit-testable against JSON fixtures.

Rank normalisation uses Reciprocal Rank Fusion (RRF, Cormack et al. 2009)
with the standard k=60: score = k / (k + rank).  Rank 0 → 1.0,
rank 60 → 0.5, rank → ∞ → 0.0.  No hard cutoff, no tunable decay.
"""

from __future__ import annotations

import re
from dataclasses import dataclass, field

# ── Stopword list (shared with compass.py) ────────────────────────────────────
STOPWORDS: frozenset[str] = frozenset({
    "a", "an", "the", "is", "it", "in", "on", "at", "to", "for",
    "of", "and", "or", "but", "not", "with", "this", "that", "are",
    "was", "be", "by", "from", "as", "into", "about", "its", "i",
    "my", "we", "our", "do", "does", "how", "what", "when", "where",
    "which", "who", "will", "can", "has", "have", "had", "been",
    "would", "could", "should", "may", "might", "shall",
})

# Verbs that express intent rather than topic in natural-language queries.
# "how do I CREATE a new sprint" → core = {"new", "sprint"}.
# The full pass still includes these so docs whose titles contain the
# question form ("How do I open a new sprint?") get a title-match boost.
QUESTION_VERBS: frozenset[str] = frozenset({
    "create", "make", "add", "get", "set", "use", "find", "show",
    "list", "build", "run", "install", "configure", "update", "open",
    "close", "start", "stop", "enable", "disable", "check", "view",
    "do", "generate", "implement", "write", "define", "change",
    "remove", "delete", "edit", "see", "read", "load", "apply",
    "go", "move", "switch", "reset", "restore", "fix", "debug",
})

# ── Query plan ────────────────────────────────────────────────────────────────

@dataclass
class QueryPlan:
    """Parsed and enriched form of the user's raw query string."""
    raw: str
    tokens: list[str]       # stopword-filtered lowercase words
    core_words: list[str]   # tokens minus question verbs (topic nouns)
    full_words: list[str]   # all content tokens (= tokens)
    is_question: bool
    is_how_do: bool
    is_folder_slug: bool    # single underscore-joined slug token

    @classmethod
    def from_query(cls, query: str) -> "QueryPlan":
        raw_words = re.findall(r"\w+", query)
        tokens = [w.lower() for w in raw_words if w.lower() not in STOPWORDS]
        core_words = [w for w in tokens if w not in QUESTION_VERBS]
        q = query.strip().lower()
        is_question = q.endswith("?") or q.startswith(("how", "what", "where", "when", "why", "who", "which", "can", "does", "is ", "are "))
        is_how_do = is_question and q.startswith("how do")
        is_folder_slug = (
            len(raw_words) == 1
            and "_" in raw_words[0]
            and raw_words[0].islower()
        )
        return cls(
            raw=query,
            tokens=tokens,
            core_words=core_words,
            full_words=tokens,
            is_question=is_question,
            is_how_do=is_how_do,
            is_folder_slug=is_folder_slug,
        )


# ── Document signals ──────────────────────────────────────────────────────────

@dataclass
class DocSignals:
    """
    Raw, unnormalised signals for one document against one query.

    Collected by the search command before scoring; no weights here.
    """
    # Rank positions (0-indexed) from title+description FTS AND queries.
    # None means the document did not match the query for that pass.
    core_rank: int | None = None   # core_words AND match in {title description}
    full_rank: int | None = None   # full_words AND match in {title description}

    # BM25 value from SQLite FTS5 ORDER BY rank.  Negative; less negative
    # means better.  Typical range: roughly [-10, 0].  0.0 if no body match.
    body_bm25: float = 0.0

    # Inbound-link count: how many documents in the corpus link to this one.
    inbound_count: int = 0

    # True if the document's type is recipe/runbook/skill/manual/memory.
    is_recipe_type: bool = False

    # True if the query is question-shaped and the doc is a recipe type.
    recipe_for_question: bool = False

    # Position in the initial FTS pool (0 = first returned by SQLite).
    # Used as a final tie-breaker when all other signals are equal.
    pool_position: int = 0


# ── Weights ───────────────────────────────────────────────────────────────────

@dataclass
class Weights:
    """
    Named weights for each scoring signal.  All weight_* values should
    sum to 1.0 (enforced by score_document normalising by their sum).

    Constants control how raw signals are mapped to [0, 1] before
    weighting.  Tune these first, then tune the weight_* values.
    """
    # Signal weights (should sum to 1.0)
    weight_title_core: float = 0.45   # core words in title/description
    weight_title_full: float = 0.20   # full words in title/description
    weight_body:       float = 0.20   # body BM25
    weight_inbound:    float = 0.10   # inbound-link count (cheap PageRank proxy)
    weight_recipe_q:   float = 0.05   # recipe bonus for question queries

    # Normalisation constants
    # RRF k parameter (Cormack 2009 standard: 60).
    # rank_to_score(rank) = k / (k + rank) → rank 0 = 1.0, rank 60 = 0.5
    rrf_k: float = 60.0

    # BM25 reference scale: a BM25 of -bm25_scale maps to 0.0.
    # Adjust if your corpus returns scores outside [-bm25_scale, 0].
    bm25_scale: float = 8.0

    # Inbound saturation: inbound_scale or more links → full 1.0 contribution.
    inbound_scale: float = 10.0

    # Absolute floor: documents below this percentage are always excluded.
    threshold_pct: int = 10

    # Relative dropout ratio: after scoring all results, compute
    # global_floor = max_score * dropout_ratio and exclude anything below it.
    # E.g. ratio=0.25: top at 80% → floor 20%; top at 20% → floor 5%
    # (absolute threshold_pct still applies as a hard lower bound).
    # Set to 0.0 to disable relative filtering (--all-buckets behaviour).
    dropout_ratio: float = 0.25


# ── Score result ──────────────────────────────────────────────────────────────

@dataclass
class ScoreResult:
    """Final score for one document."""
    total: float                    # [0.0, 1.0]
    pct: int                        # round(total * 100)
    breakdown: dict[str, float]     # signal_name → weighted contribution
    label: str                      # short display string, e.g. "87%"

    def __repr__(self) -> str:
        parts = ", ".join(
            f"{k}={v:.3f}" for k, v in self.breakdown.items() if v > 0
        )
        return f"ScoreResult({self.label}, [{parts}])"


# ── Normalisation helpers ─────────────────────────────────────────────────────

def rank_to_score(rank: int | None, k: float) -> float:
    """Reciprocal Rank Fusion normalisation.  rank=0 → 1.0, rank=k → 0.5."""
    if rank is None:
        return 0.0
    return k / (k + rank)


def bm25_to_score(bm25: float, scale: float) -> float:
    """Map SQLite BM25 (negative, less-negative = better) to [0, 1]."""
    # bm25=0.0 (best possible) → 1.0; bm25=-scale → 0.0; clamped below 0
    return max(0.0, min(1.0, (bm25 + scale) / scale))


def inbound_to_score(count: int, scale: float) -> float:
    """Linear saturation: 0 → 0.0, scale → 1.0, capped."""
    return min(1.0, count / scale) if scale > 0 else 0.0


# ── Core scoring function ─────────────────────────────────────────────────────

def score_document(
    signals: DocSignals,
    weights: Weights,
    *,
    query: QueryPlan | None = None,
) -> ScoreResult:
    """
    Combine document signals into a single relevance score.

    Returns a ScoreResult with the percentage and per-signal breakdown.
    Does NOT access any database; all inputs are pre-computed.
    """
    w = weights

    raw_signals = {
        "title_core": w.weight_title_core * rank_to_score(signals.core_rank, w.rrf_k),
        "title_full": w.weight_title_full * rank_to_score(signals.full_rank, w.rrf_k),
        "body":       w.weight_body       * bm25_to_score(signals.body_bm25, w.bm25_scale),
        "inbound":    w.weight_inbound    * inbound_to_score(signals.inbound_count, w.inbound_scale),
        "recipe_q":   w.weight_recipe_q   * (1.0 if signals.recipe_for_question else 0.0),
    }

    total = min(1.0, sum(raw_signals.values()))
    pct = round(total * 100)

    return ScoreResult(
        total=total,
        pct=pct,
        breakdown=raw_signals,
        label=f"{pct}%",
    )


# ── Global relevance floor ───────────────────────────────────────────────────

def global_floor(
    scores: dict[str, "ScoreResult"],
    weights: Weights,
    all_buckets: bool = False,
) -> int:
    """
    Compute the minimum score (as an integer percentage) that a result
    must reach to be shown.

    The floor is max(absolute_threshold, max_score * dropout_ratio).
    With all_buckets=True (or dropout_ratio=0.0) only the absolute
    threshold applies — every bucket shows whatever it has.
    """
    if not scores:
        return weights.threshold_pct
    max_pct = max(r.pct for r in scores.values())
    if all_buckets or weights.dropout_ratio <= 0.0:
        return weights.threshold_pct
    relative_floor = round(max_pct * weights.dropout_ratio)
    return max(weights.threshold_pct, relative_floor)


# ── Corpus evaluation helpers ─────────────────────────────────────────────────

def precision_at_k(
    results: list[tuple[ScoreResult, str]],  # (score, doc_id)
    relevant: set[str],
    k: int,
) -> float:
    """Precision@K: fraction of top-K results that are relevant."""
    top_k = [doc_id for _, doc_id in results[:k]]
    hits = sum(1 for d in top_k if d in relevant)
    return hits / k if k else 0.0


def ndcg_at_k(
    results: list[tuple[ScoreResult, str]],
    relevant: set[str],
    k: int,
) -> float:
    """Normalised Discounted Cumulative Gain @ K (binary relevance)."""
    import math
    top_k = [doc_id for _, doc_id in results[:k]]
    dcg = sum(
        1.0 / math.log2(i + 2)
        for i, doc_id in enumerate(top_k)
        if doc_id in relevant
    )
    ideal = sum(
        1.0 / math.log2(i + 2)
        for i in range(min(len(relevant), k))
    )
    return dcg / ideal if ideal else 0.0
