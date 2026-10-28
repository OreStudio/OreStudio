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
#include "ores.marketdata.service/app/fixing_ingest.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.marketdata.core/repository/market_fixing_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <vector>

namespace {

using namespace std::chrono;
using ores::marketdata::service::app::adds_to;
using ores::marketdata::service::app::consumer;
using ores::marketdata::service::app::fixing;
using ores::marketdata::service::app::fixing_store;
using ores::marketdata::service::app::stored_fixings;

constexpr year_month_day first_day{year{2026}, month{10}, day{1}};

fixing fixing_on(const std::string& index_uri, int days_after, const std::string& value) {
    return {.index_uri = index_uri,
            .date = year_month_day{sys_days{first_day} + days{days_after}},
            .value = value,
            .source = "public-test"};
}

/// An index of this run's own, so a second run of the suite reads none of the first's rows.
std::string run_index_uri() {
    static boost::uuids::random_generator uuid;
    auto tag = boost::uuids::to_string(uuid()).substr(0, 8);
    std::ranges::transform(tag, tag.begin(), [](char c) { return c >= 'a' ? c - 'a' + 'A' : c; });
    return "oresmd://fx/EUR?type=fixing&index=fx&source=T" + tag + "&ccy=USD";
}

}

TEST_CASE("a_fixing_adds_a_missing_date_or_a_new_value_and_nothing_else", "[app][fixing_ingest]") {
    const auto uri = run_index_uri();
    const stored_fixings stored{{first_day, "1.0842"}};

    CHECK_FALSE(adds_to(stored, fixing_on(uri, 0, "1.0842")));
    CHECK(adds_to(stored, fixing_on(uri, 0, "1.0843")));
    CHECK(adds_to(stored, fixing_on(uri, 1, "1.0842")));
}

TEST_CASE("a_backfill_is_stored_once_and_a_correction_adds_one_row", "[app][fixing_ingest]") {
    ores::testing::scoped_database_helper h;
    REQUIRE(h.tenant_id().to_string() != ores::database::service::tenant_context::system_tenant_id);
    auto gen = ores::testing::make_generation_context(h);
    auto party = ores::refdata::generators::generate_synthetic_party(gen);
    const auto existing = ores::refdata::repository::party_repository{}.read_latest(h.context());
    if (!existing.empty())
        party.parent_party_id = existing.front().id;
    ores::refdata::repository::party_repository{}.write(h.context(), party);

    consumer owner;
    owner.tenant_id = h.tenant_id();
    owner.party_id = party.id;
    owner.provider_code = "ore";
    owner.source_name = "public-test";

    const auto uri = run_index_uri();
    std::vector<fixing> backfill;
    for (int d = 0; d < 10; ++d)
        backfill.push_back(fixing_on(uri, d, "1.08" + std::to_string(40 + d)));

    const auto stored_rows = [&] {
        const auto series = ores::marketdata::repository::market_series_repository{}
                                .read_latest_by_key(h.context(), uri, "ore",
                                                    boost::uuids::to_string(party.id));
        REQUIRE(series.size() == 1);
        return ores::marketdata::repository::market_fixing_repository{}.read_latest_for_series(
            h.context(), series.front().id);
    };

    fixing_store store(h.context());
    CHECK(store.store({&owner, 1}, backfill) == 10);
    const auto first = stored_rows();
    REQUIRE(first.size() == 10);
    CHECK(first.front().fixing_date == first_day);
    CHECK(first.front().value == "1.0840");
    CHECK(first.front().source == "public-test");

    // A restart sends the backfill again to a store that holds nothing in memory.
    fixing_store restarted(h.context());
    CHECK(restarted.store({&owner, 1}, backfill) == 0);
    CHECK(stored_rows().size() == 10);

    const std::vector<fixing> correction{fixing_on(uri, 3, "1.0899")};
    CHECK(restarted.store({&owner, 1}, correction) == 1);
    const auto after = stored_rows();
    REQUIRE(after.size() == 10);
    const auto corrected = std::ranges::find(after, correction.front().date,
                                             &ores::marketdata::domain::market_fixing::fixing_date);
    REQUIRE(corrected != after.end());
    CHECK(corrected->value == "1.0899");
}
