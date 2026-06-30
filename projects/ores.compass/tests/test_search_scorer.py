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
    score_document, precision_at_k, ndcg_at_k,
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
    w = Weights(threshold_pct=30)
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
