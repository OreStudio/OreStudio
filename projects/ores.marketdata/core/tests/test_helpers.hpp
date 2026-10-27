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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_MARKETDATA_CORE_TESTS_TEST_HELPERS_HPP_
#define ORES_MARKETDATA_CORE_TESTS_TEST_HELPERS_HPP_

#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "tick_test_support.hpp"

using ores::marketdata::tests::zero_identity;
using ores::marketdata::tests::fresh_curve_id;
using ores::marketdata::tests::zero_uri;
#include <catch2/catch_test_macros.hpp>

using ores::marketdata::messaging::resolve_series_identity_request;
using ores::testing::scoped_database_helper;

inline constexpr auto check_violation = "violates check constraint";

inline resolve_series_identity_request fx_option_identity(const std::string& unit_ccy,
                                                          const std::string& ccy) {
    resolve_series_identity_request id;
    id.asset = "fx";
    id.scope = unit_ccy;
    id.instrument_type = "fx_option";
    id.quote_type = "rate_lnvol";
    id.fields.push_back({"ccy", ccy});
    return id;
}

/// A spot rate on a pair, as the resolver names one. It carries no coordinate,
/// so it has no grid.
inline resolve_series_identity_request fx_spot_identity(const std::string& unit_ccy,
                                                        const std::string& ccy) {
    resolve_series_identity_request id;
    id.asset = "fx";
    id.scope = unit_ccy;
    id.instrument_type = "fx_spot";
    id.quote_type = "rate";
    id.fields.push_back({"ccy", ccy});
    return id;
}

inline std::chrono::system_clock::time_point instant(const std::string& iso) {
    return ores::platform::time::datetime::from_iso8601_utc(iso);
}

struct two_parties {
    boost::uuids::uuid a;
    boost::uuids::uuid b;
    ores::database::context a_context;
    ores::database::context b_context;
};

inline two_parties make_two_parties(scoped_database_helper& h) {
    auto& ctx = h.context();
    const auto tenant = h.tenant_id();
    ores::refdata::repository::party_repository repo;

    boost::uuids::uuid system_party{};
    for (const auto& p : repo.read_latest(ctx)) {
        if (p.tenant_id == tenant && p.party_category == "System")
            system_party = p.id;
    }
    REQUIRE(system_party != boost::uuids::uuid{});

    auto generation = ores::testing::make_generation_context(h);
    const auto make_party = [&] {
        auto party = ores::refdata::generators::generate_synthetic_party(generation);
        party.party_category = "Operational";
        party.parent_party_id = system_party;
        party.change_reason_code = "system.test";
        repo.write(ctx, party);
        return party.id;
    };
    const auto a = make_party();
    const auto b = make_party();
    return {a, b, ctx.with_party(tenant, a, {a}, ""), ctx.with_party(tenant, b, {b}, "")};
}

inline std::chrono::system_clock::time_point now_second() {
    return std::chrono::floor<std::chrono::seconds>(std::chrono::system_clock::now());
}

inline constexpr auto unique_violation = "duplicate key value violates unique constraint";

#endif
