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
#include "ores.refdata.core/mapping/fx_option_convention_mapping.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

using namespace ores::refdata::mapping;
namespace qd = ores::analytics::quant::domain;
namespace qm = ores::analytics::quant::math;

ores::refdata::domain::fx_option_convention eur_usd(std::string delta, std::string long_term_delta) {
    ores::refdata::domain::fx_option_convention c;
    c.id = "EUR-USD-FXOPTION";
    c.atm_type = "AtmDeltaNeutral";
    c.delta_type = std::move(delta);
    c.switch_tenor = "2Y";
    c.long_term_atm_type = "AtmDeltaNeutral";
    c.long_term_delta_type = std::move(long_term_delta);
    c.risk_reversal_in_favor_of = "Call";
    c.butterfly_style = "Broker";
    return c;
}

qd::date_period tenor(std::string_view text) {
    return *parse_tenor_text(text);
}

}

TEST_CASE("an expiry at the switch tenor takes the long-term convention", "[fx_option_convention_mapping]") {
    const auto c = eur_usd("PaSpot", "PaFwd");
    const auto short_term = to_quote_convention(c, tenor("1Y"));
    const auto at_switch = to_quote_convention(c, tenor("24M"));
    REQUIRE(short_term);
    REQUIRE(at_switch);
    CHECK(short_term->delta == qm::fx_delta_type::premium_adjusted_spot);
    CHECK(at_switch->delta == qm::fx_delta_type::premium_adjusted_forward);
    CHECK(at_switch->atm == qm::fx_atm_type::delta_neutral);
    CHECK(at_switch->favour == qm::fx_risk_reversal_favour::call);
    CHECK(at_switch->butterfly == qm::fx_butterfly_style::broker);

    auto no_switch = c;
    no_switch.switch_tenor = "0D";
    CHECK(to_quote_convention(no_switch, tenor("2Y"))->delta ==
          qm::fx_delta_type::premium_adjusted_spot);
}

TEST_CASE("a convention type the quant library does not model is refused by name",
          "[fx_option_convention_mapping]") {
    auto c = eur_usd("Spot", "Fwd");
    c.atm_type = "AtmPutCall50";
    const auto refused = to_quote_convention(c, tenor("1Y"));
    REQUIRE_FALSE(refused);
    CHECK(refused.error().find("AtmPutCall50") != std::string::npos);

    auto unstated = eur_usd("Spot", "Fwd");
    unstated.risk_reversal_in_favor_of.reset();
    unstated.butterfly_style.reset();
    const auto defaults = to_quote_convention(unstated, tenor("1Y"));
    REQUIRE(defaults);
    CHECK(defaults->favour == qm::fx_risk_reversal_favour::call);
    CHECK(defaults->butterfly == qm::fx_butterfly_style::smile);
}

TEST_CASE("an expiry the switch tenor cannot be ordered against is refused",
          "[fx_option_convention_mapping]") {
    auto c = eur_usd("Spot", "Fwd");
    c.switch_tenor = "1M";
    const auto refused = to_quote_convention(c, tenor("4W"));
    REQUIRE_FALSE(refused);
    CHECK(refused.error().find("1M") != std::string::npos);
    CHECK_FALSE(to_quote_convention(c, qd::date_period{}));
    c.switch_tenor = "two years";
    CHECK_FALSE(to_quote_convention(c, tenor("1Y")));
}

TEST_CASE("a convention with no switch tenor keeps its short-term types at every expiry",
          "[fx_option_convention_mapping]") {
    auto c = eur_usd("Spot", "Fwd");
    c.switch_tenor.reset();
    const auto long_dated = to_quote_convention(c, tenor("10Y"));
    REQUIRE(long_dated);
    CHECK(long_dated->delta == qm::fx_delta_type::spot);
}

TEST_CASE("a tenor reads as a period", "[fx_option_convention_mapping]") {
    CHECK(tenor("1W") == qd::date_period{qd::period_unit::week, 1});
    CHECK(tenor("18M") == qd::date_period{qd::period_unit::month, 18});
    CHECK(tenor("2Y") == qd::date_period{qd::period_unit::year, 2});
    CHECK_FALSE(parse_tenor_text("Y"));
    CHECK_FALSE(parse_tenor_text("1Q"));
    CHECK_FALSE(parse_tenor_text("-1M"));
}
