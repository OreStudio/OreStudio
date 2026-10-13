# -*- coding: utf-8 -*-
"""Tests of which hand-written C++ files the entity-table write check lets through."""

import importlib.util
import sys
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[1] / "scripts" / "check_entity_table_writes.py"
spec = importlib.util.spec_from_file_location("check_entity_table_writes", SCRIPT)
check = importlib.util.module_from_spec(spec)
sys.modules["check_entity_table_writes"] = check
spec.loader.exec_module(check)


def own(path, *tables):
    return check.is_own_repository(Path(path), set(tables))


def test_a_repository_of_the_table_it_writes_is_its_own_repository():
    assert own("a/work_ledger_repository.cpp", "ores_compute_work_ledger_tbl")


def test_the_plural_of_the_name_is_the_same_table():
    assert own("a/work_tenant_repository.cpp", "ores_compute_work_tenants_tbl")


def test_a_repository_that_also_writes_another_table_is_not_exempt():
    assert not own(
        "a/work_ledger_repository.cpp",
        "ores_compute_work_ledger_tbl",
        "ores_compute_results_tbl",
    )


def test_a_file_that_is_not_a_repository_is_not_exempt():
    assert not own("a/claim_handler.hpp", "ores_compute_work_ledger_tbl")
    assert not own("a/publish_service.cpp", "ores_dq_datasets_tbl")


def test_a_repository_of_another_name_is_not_exempt():
    assert not own("a/work_ledger_repository.cpp", "ores_compute_work_tenants_tbl")


def test_a_repository_with_no_name_is_not_exempt():
    assert not own("a/_repository.cpp", "ores_compute_work_ledger_tbl")
