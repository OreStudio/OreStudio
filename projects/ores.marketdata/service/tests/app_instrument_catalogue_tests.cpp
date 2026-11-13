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
#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.api/datum/schema.hpp"
#include "ores.marketdata.service/app/instrument_naming.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

using ores::marketdata::service::app::kind_of;
using ores::marketdata::service::app::name_of;

namespace dt = ores::marketdata::datum;

/// The series a URI names, as the catalogue reads it: the datum's own identity
/// and the schema row that says which fields are identity and which are axes.
struct parsed_series {
    dt::market_datum datum;
    const dt::schema_row* row;
};

parsed_series parse(const std::string& uri) {
    auto datum = dt::oresmd_uri_codec::read(uri);
    REQUIRE(datum.has_value());
    REQUIRE(datum->is_series());
    return {*datum, &dt::schema_of(datum->type())};
}

std::string spelling_of(const dt::market_datum& d) {
    return std::string(dt::oresmd_uri_codec::instrument_spelling(d.type()));
}

constexpr const char* catalogue_tags = "[instrument_catalogue]";

}

TEST_CASE("a_spot_pair_has_no_axes_and_is_spelled_base_over_quote", catalogue_tags) {
    // The deployment's own spelling of an AUD/USD spot series.
    const auto s = parse("oresmd://fx/AUD?type=series&instrument=fx_spot&quote=rate&ccy=USD");

    CHECK(kind_of(*s.row, 0) == "pair");
    CHECK(name_of(s.datum, *s.row, "pair", spelling_of(s.datum)) == "AUD/USD");
}

TEST_CASE("an_ir_swap_is_one_ladder_and_its_name_carries_what_tells_peers_apart", catalogue_tags) {
    // One series holds the whole ladder across its term axis, so it has one
    // coordinate field and is a ladder, not a curve.
    const auto s = parse("oresmd://ir/AUD?type=series&instrument=ir_swap&quote=rate"
                         "&fwd_start=0D&tenor=1D");

    CHECK(kind_of(*s.row, 1) == "ladder");

    const auto name = name_of(s.datum, *s.row, "ladder", spelling_of(s.datum));
    CHECK(name.find("AUD") != std::string::npos);
    CHECK(name.find("0D") != std::string::npos);
    CHECK(name.find("1D") != std::string::npos);
}

TEST_CASE("a_curve_is_named_by_its_curve_id", catalogue_tags) {
    const auto s = parse("oresmd://ir/AUD?type=series&instrument=discount&quote=rate"
                         "&curve_id=AUD-AONIA");

    CHECK(kind_of(*s.row, 1) == "curve");
    CHECK(name_of(s.datum, *s.row, "curve", spelling_of(s.datum)) == "AUD-AONIA");
}

TEST_CASE("a_surface_has_two_axes", catalogue_tags) {
    const auto s = parse("oresmd://fx/AUD?type=series&instrument=fx_option&quote=rate_lnvol"
                         "&ccy=USD");

    CHECK(kind_of(*s.row, 2) == "surface");

    const auto name = name_of(s.datum, *s.row, "surface", spelling_of(s.datum));
    CHECK(name.find("AUD") != std::string::npos);
    CHECK(name.find("USD") != std::string::npos);
}

TEST_CASE("an_instrument_outside_the_five_kinds_is_not_named", catalogue_tags) {
    // A hazard rate has a term axis, so it is a ladder by shape; one with no
    // axis at all and no pair spelling is nothing the catalogue can name.
    const auto s = parse("oresmd://fx/AUD?type=series&instrument=fx_spot&quote=rate&ccy=JPY");
    CHECK(kind_of(*s.row, 5) == "pair");
}
