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
#include "ores.marketdata.api/domain/tick_subjects.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

const std::string tags("[domain][tick_subjects]");

}

TEST_CASE("a_producer_publishes_on_its_source_subject", tags) {
    CHECK(ores::marketdata::domain::synthetic_tick_subject("usd.sofr") ==
          "synthetic.v1.tick.usd.sofr");
}

TEST_CASE("a_consumer_reads_a_datum_under_its_lower_cased_ore_key", tags) {
    CHECK(ores::marketdata::domain::market_tick_subject("t", "p", "FX/RATE/EUR/USD") ==
          "marketdata.v1.tick.t.p.fx.rate.eur.usd");
}

TEST_CASE("the_consumer_subject_starts_with_the_market_tick_subject", tags) {
    const auto subject =
        ores::marketdata::domain::market_tick_subject("t", "p", "IR_SWAP/RATE/USD/0D/1D/30D");
    CHECK(subject.starts_with(std::string(ores::marketdata::messaging::market_tick::nats_subject) +
                              "."));
    CHECK(subject.ends_with(".ir_swap.rate.usd.0d.1d.30d"));
}
