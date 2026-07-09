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
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <map>

using Catch::Approx;

/**
 * @file xml_fx_convention_mapper_roundtrip_tests.cpp
 * @brief Round-trips ORE's conventions.xml <FX> element through
 * conventions_mapper::map_fx/reverse against a real example file, not just
 * the migration task's unit-level acceptance criteria.
 *
 * The real example file (external/ore/examples/Input/conventions.xml)
 * surfaces a genuine data-loss case: two <FX> entries
 * (EUR-USD-FX-CONVENTIONS, FX-ECB-EUR-USD) both target the EUR/USD pair
 * with different SpotDays. Since currency_pair_convention is keyed 1:1 by
 * pair_code (by design — see ores.refdata.currency_pair_convention.org),
 * only one survives an import; this is documented here, not silently
 * accepted.
 */

namespace {

const std::string_view test_suite("ores.ore.fx_convention.mapper.roundtrip.tests");
const std::string tags("[ore][xml][mapper][roundtrip][fx_convention]");

using ores::ore::domain::conventions;
using ores::ore::domain::conventions_mapper;
using ores::ore::domain::mapped_conventions;
using ores::ore::domain::mapped_fx;
using namespace ores::logging;

/**
 * @brief Local mirror of conventions_mapper::parse_bool (private, not
 * usable from tests) — same truthy-variant switch, for comparing bool_
 * values by meaning rather than by exact enum variant.
 */
bool bool_value(ores::ore::domain::bool_ v) {
    using b = ores::ore::domain::bool_;
    switch (v) {
        case b::Y:
        case b::YES:
        case b::TRUE_:
        case b::True:
        case b::true_:
        case b::_1:
            return true;
        default:
            return false;
    }
}

std::filesystem::path conventions_path() {
    return ores::testing::project_root::resolve("external/ore/examples/Input/conventions.xml");
}

conventions load_conventions() {
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(conventions_path());
    conventions c;
    ores::ore::domain::load_data(content, c);
    return c;
}

/**
 * @brief Round-trip a single fxType through map_fx then back through the
 * aggregate reverse() (individual reverse_fx is not exposed publicly).
 */
ores::ore::domain::fxType roundtrip_single(const ores::ore::domain::fxType& original) {
    const mapped_fx mapped = conventions_mapper::map_fx(original);

    mapped_conventions wrapper;
    wrapper.fx.push_back(mapped);
    const conventions reversed = conventions_mapper::reverse(wrapper);

    REQUIRE(reversed.FX.size() == 1);
    return reversed.FX.front();
}

} // namespace

// =============================================================================
// Real example file: field-by-field round-trip for every <FX> entry
// =============================================================================

TEST_CASE("fx_convention_roundtrip_real_example_file_field_values", tags) {
    auto lg(make_logger(test_suite));
    const auto original = load_conventions();
    REQUIRE(!original.FX.empty());

    for (const auto& fx : original.FX) {
        const auto roundtripped = roundtrip_single(fx);

        CHECK(ores::ore::domain::to_string(roundtripped.SourceCurrency) ==
             ores::ore::domain::to_string(fx.SourceCurrency));
        CHECK(ores::ore::domain::to_string(roundtripped.TargetCurrency) ==
             ores::ore::domain::to_string(fx.TargetCurrency));
        CHECK(static_cast<int64_t>(roundtripped.SpotDays) == static_cast<int64_t>(fx.SpotDays));
        CHECK(static_cast<double>(roundtripped.PointsFactor) ==
             Approx(static_cast<double>(fx.PointsFactor)).epsilon(1e-9));

        if (fx.AdvanceCalendar) {
            REQUIRE(roundtripped.AdvanceCalendar);
            CHECK(std::string(*roundtripped.AdvanceCalendar) == std::string(*fx.AdvanceCalendar));
        } else {
            CHECK(!roundtripped.AdvanceCalendar);
        }

        if (fx.SpotRelative) {
            REQUIRE(roundtripped.SpotRelative);
            CHECK(bool_value(*roundtripped.SpotRelative) == bool_value(*fx.SpotRelative));
        }

        // Id is NOT preserved: reverse_fx regenerates it as
        // "<base>-<quote>-FX-CONVENTIONS" rather than keeping the source
        // XML's original Id (e.g. "FX-ECB-EUR-USD" becomes
        // "EUR-USD-FX-CONVENTIONS"). Documented, not asserted equal.
    }

    BOOST_LOG_SEV(lg, info) << "Field-by-field FX convention round-trip passed for "
                            << original.FX.size() << " entries";
}

// =============================================================================
// Documented data-loss case: multiple <FX> entries per pair collide
// =============================================================================

TEST_CASE("fx_convention_roundtrip_duplicate_pair_collision_is_documented", tags) {
    auto lg(make_logger(test_suite));
    const auto original = load_conventions();

    std::map<std::pair<std::string, std::string>, int> pair_counts;
    for (const auto& fx : original.FX)
        ++pair_counts[{ores::ore::domain::to_string(fx.SourceCurrency),
                       ores::ore::domain::to_string(fx.TargetCurrency)}];

    // The real example file (as of this writing) has exactly one colliding
    // pair: EUR/USD, entered twice (EUR-USD-FX-CONVENTIONS with SpotDays=0
    // and FX-ECB-EUR-USD with SpotDays=2). currency_pair_convention is
    // keyed 1:1 by pair_code (base_currency + "/" + quote_currency), so
    // importing both into ORES loses one — whichever is written last wins
    // (upsert-by-key semantics, same pattern documented in task
    // 03FDF102-2379-4C73-BFD8-FA71BF670BF5 for currency_pair itself).
    int duplicated_pairs = 0;
    for (const auto& [pair, count] : pair_counts) {
        if (count > 1) {
            ++duplicated_pairs;
            BOOST_LOG_SEV(lg, info) << "Colliding FX convention pair: " << pair.first << "/"
                                    << pair.second << " (" << count << " entries in source XML, "
                                    << "only 1 survives import)";
        }
    }

    CHECK(duplicated_pairs == 1);
    CHECK(original.FX.size() == 24);
    CHECK(pair_counts.size() == 23);
}

// =============================================================================
// Synthetic case: EOM and Convention fields (not exercised by the real file)
// =============================================================================

TEST_CASE("fx_convention_roundtrip_synthetic_eom_and_business_day_convention", tags) {
    ores::ore::domain::fxType fx;
    static_cast<std::string&>(fx.Id) = "EUR-USD-FX-CONVENTIONS";
    fx.SpotDays = 2;
    fx.SourceCurrency = ores::ore::domain::currencyCode::EUR;
    fx.TargetCurrency = ores::ore::domain::currencyCode::USD;
    fx.PointsFactor = 10000.0;
    fx.EOM = ores::ore::domain::bool_::true_;
    fx.Convention = ores::ore::domain::businessDayConvention::Following;

    const auto roundtripped = roundtrip_single(fx);

    REQUIRE(roundtripped.EOM);
    CHECK(bool_value(*roundtripped.EOM) == true);
    REQUIRE(roundtripped.Convention);
    CHECK(*roundtripped.Convention == ores::ore::domain::businessDayConvention::Following);
}
