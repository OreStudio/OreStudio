"""
Tests for the compass search scoring model.

Run with:  python -m pytest projects/ores.compass/tests/test_search_scorer.py -v
No live database or file system access required.
"""

import json
import sys
from pathlib import Path

# Allow importing from the src directory without installing the package.
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from search_scorer import (
    DocSignals, QueryPlan, ScoreResult, Weights,
    rank_to_score, bm25_to_score, inbound_to_score,
    score_document, global_floor, precision_at_k, ndcg_at_k,
)

# ── Unit tests for normalisation helpers ─────────────────────────────────────

def test_rank_to_score_rrf():
    w = Weights()
    assert rank_to_score(0, w.rrf_k) == 1.0
    assert rank_to_score(None, w.rrf_k) == 0.0
    # rank = k → 0.5 exactly
    assert rank_to_score(int(w.rrf_k), w.rrf_k) == 0.5
    # monotone decreasing
    scores = [rank_to_score(r, w.rrf_k) for r in range(100)]
    assert all(scores[i] > scores[i + 1] for i in range(len(scores) - 1))


def test_bm25_to_score():
    w = Weights()
    assert bm25_to_score(0.0, w.bm25_scale) == 1.0
    assert bm25_to_score(-w.bm25_scale, w.bm25_scale) == 0.0
    assert bm25_to_score(-100.0, w.bm25_scale) == 0.0   # clamped
    assert 0 < bm25_to_score(-2.0, w.bm25_scale) < 1.0


def test_inbound_to_score():
    w = Weights()
    assert inbound_to_score(0, w.inbound_scale) == 0.0
    assert inbound_to_score(int(w.inbound_scale), w.inbound_scale) == 1.0
    assert inbound_to_score(100, w.inbound_scale) == 1.0  # clamped


# ── Score function behaviour ──────────────────────────────────────────────────

def test_perfect_title_core_match():
    """A doc at rank 0 in the core-words pass should score very high."""
    w = Weights()
    s = DocSignals(core_rank=0, body_bm25=-1.0)
    r = score_document(s, w)
    # title_core contribution = 0.45 * 1.0 = 0.45
    assert r.breakdown["title_core"] == w.weight_title_core * 1.0
    assert r.pct >= 50


def test_no_match_scores_zero():
    s = DocSignals()  # all defaults: no rank, bm25=0.0 → body=1.0 but no title
    w = Weights()
    r = score_document(s, w)
    # body_bm25=0.0 maps to bm25_to_score=1.0, weight=0.20 → 20%
    assert r.breakdown["body"] == w.weight_body * 1.0
    assert r.pct == 20


def test_breakdown_sums_to_total():
    s = DocSignals(core_rank=5, full_rank=10, body_bm25=-2.0, inbound_count=3)
    w = Weights()
    r = score_document(s, w)
    assert abs(sum(r.breakdown.values()) - r.total) < 1e-9


def test_recipe_question_bonus():
    w = Weights()
    s_recipe = DocSignals(core_rank=5, recipe_for_question=True)
    s_plain  = DocSignals(core_rank=5, recipe_for_question=False)
    r_recipe = score_document(s_recipe, w)
    r_plain  = score_document(s_plain,  w)
    assert r_recipe.total > r_plain.total
    assert r_recipe.breakdown["recipe_q"] == w.weight_recipe_q


def test_core_beats_full():
    """A core-words match ranks above a full-words-only match."""
    w = Weights()
    s_core = DocSignals(core_rank=0)
    s_full = DocSignals(full_rank=0)
    r_core = score_document(s_core, w)
    r_full = score_document(s_full, w)
    assert r_core.total > r_full.total


def test_total_capped_at_one():
    """Combined signals can exceed 1.0 in theory — result is clamped."""
    s = DocSignals(
        core_rank=0, full_rank=0, body_bm25=0.0,
        inbound_count=100, recipe_for_question=True,
    )
    r = score_document(s, Weights())
    assert r.total <= 1.0
    assert r.pct <= 100


def test_threshold_semantics():
    """Documents below threshold_pct should be filterable."""
    w = Weights(threshold_pct=40)
    s_low  = DocSignals(body_bm25=-7.0)  # low body score
    s_high = DocSignals(core_rank=0)
    r_low  = score_document(s_low,  w)
    r_high = score_document(s_high, w)
    assert r_low.pct  < w.threshold_pct
    assert r_high.pct >= w.threshold_pct


# ── Query plan parsing ────────────────────────────────────────────────────────

def test_query_plan_core_words():
    q = QueryPlan.from_query("how do i create a new sprint")
    assert "create" not in q.core_words
    assert "new"    in q.core_words
    assert "sprint" in q.core_words
    # stopwords stripped
    assert "i"  not in q.core_words
    assert "a"  not in q.core_words
    assert q.is_question
    assert q.is_how_do


def test_query_plan_non_question():
    q = QueryPlan.from_query("sprint planning")
    assert not q.is_question
    assert "sprint" in q.core_words
    assert "planning" in q.core_words


def test_query_plan_folder_slug():
    q = QueryPlan.from_query("compass_quality_of_life_sprint_21")
    assert q.is_folder_slug


# ── Global floor ─────────────────────────────────────────────────────────────

def _scores_from_pcts(pcts: list[int]) -> dict[str, ScoreResult]:
    return {
        str(i): ScoreResult(pct / 100, pct, {}, f"{pct}%")
        for i, pct in enumerate(pcts)
    }


def test_global_floor_ratio():
    w = Weights(threshold_pct=10, dropout_ratio=0.25)
    # top is 80% → relative floor = 20%, above absolute 10%
    scores = _scores_from_pcts([80, 50, 20, 15])
    assert global_floor(scores, w) == 20


def test_global_floor_low_top():
    w = Weights(threshold_pct=25, dropout_ratio=0.25)
    # top is 20% → relative floor = 5%, absolute 25% wins
    scores = _scores_from_pcts([20, 18, 15])
    assert global_floor(scores, w) == 25


def test_global_floor_all_buckets():
    w = Weights(threshold_pct=25, dropout_ratio=0.25)
    scores = _scores_from_pcts([80, 20, 15])
    # all_buckets disables ratio; only absolute floor applies
    assert global_floor(scores, w, all_buckets=True) == 25


def test_global_floor_zero_ratio():
    w = Weights(threshold_pct=25, dropout_ratio=0.0)
    scores = _scores_from_pcts([80, 20])
    assert global_floor(scores, w) == 25


def test_global_floor_empty():
    w = Weights(threshold_pct=25, dropout_ratio=0.25)
    assert global_floor({}, w) == 25


# ── Corpus evaluation helpers ─────────────────────────────────────────────────

def _make_result(pct: int, doc_id: str) -> tuple[ScoreResult, str]:
    r = ScoreResult(total=pct / 100, pct=pct, breakdown={}, label=f"{pct}%")
    return (r, doc_id)


def test_precision_at_k():
    results = [
        _make_result(90, "A"),  # relevant
        _make_result(80, "B"),  # not relevant
        _make_result(70, "C"),  # relevant
        _make_result(60, "D"),  # not relevant
    ]
    relevant = {"A", "C"}
    assert precision_at_k(results, relevant, k=2) == 0.5   # A yes, B no → 1/2
    assert precision_at_k(results, relevant, k=4) == 0.5   # A,C yes → 2/4


def test_ndcg_at_k_perfect():
    results = [_make_result(90, "A"), _make_result(80, "B")]
    assert ndcg_at_k(results, {"A", "B"}, k=2) == 1.0


def test_ndcg_at_k_worst():
    results = [_make_result(90, "A"), _make_result(80, "B")]
    assert ndcg_at_k(results, {"C", "D"}, k=2) == 0.0


# ── Corpus: synonym normalisation in QueryPlan ────────────────────────────────

def test_synonym_normalization_in_tokens():
    """'rebuild' in title AND tokens should normalise to 'build' (first synonym)."""
    q = QueryPlan.from_query("site rebuild direct")
    assert "build"   in q.tokens
    assert "rebuild" not in q.tokens
    assert "rebuild" in q.tokens_expanded
    assert "build"   in q.tokens_expanded


def test_synonym_expansion_in_tokens_expanded():
    """Body OR query gets all variants; title AND query gets the canonical form."""
    q = QueryPlan.from_query("deploy site")
    assert "build"  in q.tokens
    assert "deploy" not in q.tokens
    assert "deploy"  in q.tokens_expanded
    assert "build"   in q.tokens_expanded
    assert "rebuild" in q.tokens_expanded


def test_non_synonym_token_unchanged():
    """Tokens without a synonym entry pass through unmodified."""
    q = QueryPlan.from_query("sprint planning")
    assert "sprint"   in q.tokens
    assert "planning" in q.tokens
    assert q.tokens == q.tokens_expanded


# ── Corpus evaluation: fixture-driven ordering assertions ────────────────────
# Loads tests/data/corpus/*.json (documents) and tests/data/queries/queries.json
# (pre-computed signals + pairwise assertions).  Each query entry's assertions
# are run against score_document() using the stored DocSignals.
# See tests/data/methodology.txt for how to add documents and queries.

_DATA_DIR   = Path(__file__).parent / "data"
_CORPUS_DIR = _DATA_DIR / "corpus"
_QUERIES_FILE = _DATA_DIR / "queries" / "queries.json"


def _load_corpus() -> dict[str, dict]:
    """Return {doc_id: doc_dict} for every file in tests/data/corpus/."""
    corpus = {}
    for p in _CORPUS_DIR.glob("*.json"):
        doc = json.loads(p.read_text())
        corpus[doc["id"]] = doc
    return corpus


def _load_queries() -> list[dict]:
    if not _QUERIES_FILE.exists():
        return []
    return json.loads(_QUERIES_FILE.read_text())["queries"]


def _signals_from_dict(d: dict) -> DocSignals:
    return DocSignals(
        core_rank=d.get("core_rank"),
        full_rank=d.get("full_rank"),
        body_bm25=d.get("body_bm25", 0.0),
        inbound_count=d.get("inbound_count", 0),
        is_recipe_type=d.get("is_recipe_type", False),
        recipe_for_question=d.get("recipe_for_question", False),
        pool_position=d.get("pool_position", 0),
    )


def test_corpus_documents_are_valid():
    """Every corpus JSON file must parse and carry required fields."""
    corpus = _load_corpus()
    assert len(corpus) > 0, "corpus is empty — run build_corpus.py to populate it"
    required = {"id", "title", "doctype", "rel_path"}
    for doc_id, doc in corpus.items():
        missing = required - doc.keys()
        assert not missing, f"corpus doc {doc_id} missing fields: {missing}"


def test_query_assertions():
    """
    For every query in queries.json, score all docs using their stored signals
    and verify every 'ranks_above' assertion holds.
    """
    w = Weights()
    corpus = _load_corpus()
    queries = _load_queries()
    assert len(queries) > 0, "no queries found — add entries to queries/queries.json"

    failures = []
    for entry in queries:
        qid = entry["id"]
        signals_map = entry.get("doc_signals", {})

        scores: dict[str, ScoreResult] = {}
        for doc_id, sig_dict in signals_map.items():
            # Corpus doc must exist so we can label failures meaningfully.
            assert doc_id in corpus, (
                f"query '{qid}' references doc {doc_id} not found in corpus/"
            )
            scores[doc_id] = score_document(_signals_from_dict(sig_dict), w)

        for assertion in entry.get("assertions", []):
            if assertion["type"] != "ranks_above":
                continue
            winner, loser = assertion["winner"], assertion["loser"]
            w_score = scores[winner].pct
            l_score = scores[loser].pct
            if w_score <= l_score:
                winner_label = corpus[winner]["title"]
                loser_label  = corpus[loser]["title"]
                failures.append(
                    f"[{qid}] '{winner_label}' ({w_score}%) should rank above "
                    f"'{loser_label}' ({l_score}%) — {assertion.get('note', '')}"
                )

    assert not failures, "Corpus ranking assertions failed:\n" + "\n".join(failures)
