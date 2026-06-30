"""
Tests for the compass search scoring model.

Run with:  python -m pytest projects/ores.compass/tests/test_search_scorer.py -v
No live database or file system access required.
"""

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
# These tests encode the scoring contract for synonym handling.  A regression
# here (e.g. re-introducing raw tokens in title AND queries) breaks ranking.

def test_synonym_normalization_in_tokens():
    """'rebuild' in title AND tokens should normalise to 'build' (first synonym)."""
    q = QueryPlan.from_query("site rebuild direct")
    # title AND tokens: rebuild → build (so docs with 'build' in title match)
    assert "build"   in q.tokens
    assert "rebuild" not in q.tokens
    # body OR expansion: both rebuild and build should be present
    assert "rebuild" in q.tokens_expanded
    assert "build"   in q.tokens_expanded


def test_synonym_expansion_in_tokens_expanded():
    """Body OR query gets all variants; title AND query gets the canonical form."""
    q = QueryPlan.from_query("deploy site")
    # deploy → first synonym is "build" (from SYNONYMS["deploy"] = ["build", "rebuild"])
    assert "build"  in q.tokens          # normalised for title AND
    assert "deploy" not in q.tokens
    # body expansion keeps original too
    assert "deploy"  in q.tokens_expanded
    assert "build"   in q.tokens_expanded
    assert "rebuild" in q.tokens_expanded


def test_non_synonym_token_unchanged():
    """Tokens without a synonym entry pass through unmodified."""
    q = QueryPlan.from_query("sprint planning")
    assert "sprint"   in q.tokens
    assert "planning" in q.tokens
    assert q.tokens == q.tokens_expanded  # no expansion for these


# ── Corpus: scoring ordering contracts ───────────────────────────────────────
# Each test encodes a query scenario and asserts a relevance ordering between
# two documents.  Adding signals drawn from real search observations lets us
# detect regressions without a live DB.

def test_title_match_beats_body_only():
    """A doc with a title match should outscore a body-only match."""
    w = Weights()
    doc_title = DocSignals(core_rank=0, body_bm25=-3.0)
    doc_body  = DocSignals(body_bm25=-1.0)            # better body score but no title
    r_title = score_document(doc_title, w)
    r_body  = score_document(doc_body,  w)
    assert r_title.total > r_body.total, (
        f"Title match ({r_title.pct}%) should beat body-only ({r_body.pct}%)"
    )


def test_rebuild_query_finds_build_docs():
    """
    Query 'site rebuild direct': after synonym normalisation 'rebuild'→'build',
    a doc with build+site+direct in its title/description should get a title
    AND match (core_rank is not None) rather than being a body-only hit.

    This is the regression test for the scoring fix: using raw 'rebuild' in
    the title AND query would miss any doc that only contains 'build'.
    """
    # Simulate: doc has 'build', 'site', 'direct' in title/description.
    # After normalisation, tokens = ['site', 'build', 'direct']
    # core_words = ['site', 'build', 'direct'] (none are QUESTION_VERBS here
    # — 'build' IS a question verb, so core_words = ['site', 'direct'])
    q = QueryPlan.from_query("site rebuild direct")
    assert "rebuild" not in q.tokens,  "raw token should be normalised to 'build'"
    assert "build"   in q.tokens,      "normalised token 'build' must appear"
    # The title AND query is built from core_words / full_words, which use q.tokens.
    # A doc containing 'site' and 'direct' (and 'build') in its description
    # should receive a core_rank hit.  We simulate this with core_rank=2.
    w = Weights()
    doc_with_title = DocSignals(core_rank=2, body_bm25=-4.0)
    doc_body_only  = DocSignals(body_bm25=-2.0)
    r_title = score_document(doc_with_title, w)
    r_body  = score_document(doc_body_only,  w)
    assert r_title.total > r_body.total, (
        f"Synonym-normalised title hit ({r_title.pct}%) must beat body-only ({r_body.pct}%)"
    )
