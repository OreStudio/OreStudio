/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.scheduler.core/repository/store_bounds_repository.hpp"
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <string>
#include <vector>

// The view hands back text, so the mapping states what each column means and what
// an absent one is worth. The tests state a row the way the view does, which is
// the only way to see that a missing sample does not become a size of zero.

using ores::scheduler::repository::store_bound_from_row;
using row_type = std::vector<std::optional<std::string>>;

namespace {

row_type bounded_row() {
    return {"marketdata.delivery_counts",
            "Market data delivery counts",
            "How many items each feed delivered.",
            "marketdata",
            "table",
            "public.ores_marketdata_delivery_counts_tbl",
            "policy",
            "delete_rows",
            "30",
            "delivery_counts_30d",
            "t",
            "sweep",
            "t",
            "applied",
            "2026-11-27T03:00:00Z",
            "2",
            "",
            "32768",
            "27",
            "2026-11-27T03:15:00Z"};
}

row_type unmeasured_row() {
    return {"compute.job_inputs",
            "Compute job inputs",
            "The input archives a run hands to the grid.",
            "compute",
            "object_prefix",
            "compute/input/",
            "policy",
            "expire_prefix",
            "7",
            "job_inputs_7d",
            "t",
            "storage_lifecycle",
            "f",
            "skipped",
            "2026-11-27T03:00:00Z",
            "0",
            "the storage lifecycle expires an object prefix",
            std::nullopt,
            std::nullopt,
            std::nullopt};
}

}

TEST_CASE("a bounds row states the store, its bound and its size", "[repository][bounds]") {
    const auto bound = store_bound_from_row(bounded_row());

    CHECK(bound.code == "marketdata.delivery_counts");
    CHECK(bound.display_name == "Market data delivery counts");
    CHECK(bound.owner_component == "marketdata");
    CHECK(bound.target_kind == "table");
    CHECK(bound.bound_kind == "policy");
    REQUIRE(bound.bound.action.has_value());
    CHECK(*bound.bound.action == "delete_rows");
    REQUIRE(bound.bound.retention_days.has_value());
    CHECK(*bound.bound.retention_days == 30);
    REQUIRE(bound.bound.policy_code.has_value());
    CHECK(*bound.bound.policy_code == "delivery_counts_30d");
    CHECK(bound.bound.policy_enabled);
    CHECK(bound.bound.mechanism == "sweep");
    CHECK(bound.bound.bounded_now);
    REQUIRE(bound.last_run.outcome.has_value());
    CHECK(*bound.last_run.outcome == "applied");
    CHECK(bound.last_run.removed == 2);
    CHECK(bound.size.size_bytes == 32768);
    CHECK(bound.size.measure_count == 27);
    REQUIRE(bound.size.sampled_at.has_value());
    CHECK(*bound.size.sampled_at == "2026-11-27T03:15:00Z");
}

TEST_CASE("a store nothing has measured reports no size rather than zero", "[repository][bounds]") {
    const auto bound = store_bound_from_row(unmeasured_row());

    CHECK(bound.code == "compute.job_inputs");
    CHECK(bound.bound.mechanism == "storage_lifecycle");
    CHECK_FALSE(bound.bound.bounded_now);
    CHECK_FALSE(bound.size.sampled_at.has_value());
    CHECK(bound.size.size_bytes == 0);
    REQUIRE(bound.last_run.detail.has_value());
    CHECK(*bound.last_run.detail == "the storage lifecycle expires an object prefix");
}

TEST_CASE("a store with no policy reports an absent action and period", "[repository][bounds]") {
    row_type row{"refdata.calendar_dates",
                 "Calendar dates",
                 "The business days.",
                 "refdata",
                 "chunked_table",
                 "public.ores_refdata_calendar_dates_tbl",
                 "none",
                 std::nullopt,
                 std::nullopt,
                 std::nullopt,
                 "f",
                 "none",
                 "f",
                 std::nullopt,
                 std::nullopt,
                 "0",
                 std::nullopt,
                 "57344",
                 "486540",
                 "2026-11-27T03:15:00Z"};

    const auto bound = store_bound_from_row(row);

    CHECK(bound.bound_kind == "none");
    CHECK_FALSE(bound.bound.action.has_value());
    CHECK_FALSE(bound.bound.retention_days.has_value());
    CHECK_FALSE(bound.bound.policy_code.has_value());
    CHECK(bound.bound.mechanism == "none");
    CHECK_FALSE(bound.bound.bounded_now);
    CHECK(bound.size.size_bytes == 57344);
    CHECK(bound.size.measure_count == 486540);
}
