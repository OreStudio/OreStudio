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
#include "ores.marketdata.api/domain/adapter_limits.hpp"
#include "ores.marketdata.service/app/adapter_tick_translator.hpp"
#include <boost/uuid/random_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

namespace {

using namespace std::chrono_literals;
using ores::marketdata::service::app::adapter_tick_drop;
using ores::marketdata::service::app::adapter_tick_translator;

const std::string eur_usd_spot = "oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD";

struct catalogue {
    ores::marketdata::domain::market_data_source live;
    ores::marketdata::domain::market_data_source simulated;
    ores::marketdata::domain::market_data_vendor_instrument instrument;
    ores::marketdata::domain::market_data_mapping mapping;
    std::chrono::system_clock::time_point now = std::chrono::system_clock::now();

    catalogue() {
        boost::uuids::random_generator gen;
        live.id = gen();
        live.name = "bloomberg-live";
        live.provider_code = "bloomberg";
        live.kind = "live";
        simulated.id = gen();
        simulated.name = "bloomberg-sim";
        simulated.provider_code = "bloomberg";
        simulated.kind = "simulated";
        instrument.id = gen();
        instrument.provider_code = "bloomberg";
        instrument.id_scheme = "bbg_ticker";
        instrument.external_id = "EURUSD Curncy";
        instrument.field = "PX_LAST";
        mapping.id = gen();
        mapping.vendor_instrument_id = instrument.id;
        mapping.oresmd_uri = eur_usd_spot;
        mapping.effective_from = now - 24h;
    }

    adapter_tick_translator translator() const {
        return adapter_tick_translator({live, simulated}, {instrument}, {mapping});
    }

    ores::marketdata::messaging::adapter_tick tick(const std::string& value = "1.0842") const {
        ores::marketdata::messaging::adapter_tick t;
        t.id_scheme = "bbg_ticker";
        t.external_id = "EURUSD Curncy";
        t.field = "PX_LAST";
        t.value = value;
        t.vendor_time = now - 1s;
        t.sequence = 1;
        return t;
    }
};

}

TEST_CASE("a_mapped_tick_becomes_the_tick_of_its_item_and_source",
          "[app][adapter_tick_translator]") {
    catalogue c;
    auto sut = c.translator();

    const auto out = sut.translate("bloomberg-live", c.tick(), c.now);

    REQUIRE(out.has_value());
    CHECK(out->tick.oresmd_uri == eur_usd_spot);
    CHECK(out->tick.value == "1.0842");
    CHECK(out->tick.source == "bloomberg-live");
    CHECK(out->tick.observation_time == c.now - 1s);
    CHECK(out->provider_code == "bloomberg");
    CHECK(out->source_id == c.live.id);
}

TEST_CASE("a_live_and_a_simulated_source_take_the_same_path", "[app][adapter_tick_translator]") {
    catalogue c;
    auto sut = c.translator();

    const auto live = sut.translate("bloomberg-live", c.tick(), c.now);
    const auto simulated = sut.translate("bloomberg-sim", c.tick(), c.now);

    REQUIRE(live.has_value());
    REQUIRE(simulated.has_value());
    CHECK(live->tick.oresmd_uri == simulated->tick.oresmd_uri);
    CHECK(live->tick.value == simulated->tick.value);
    CHECK(live->tick.observation_time == simulated->tick.observation_time);
    CHECK(live->provider_code == simulated->provider_code);
    CHECK(live->source_id != simulated->source_id);
}

TEST_CASE("a_tick_with_no_vendor_instrument_is_unmapped", "[app][adapter_tick_translator]") {
    catalogue c;
    auto sut = c.translator();

    auto unknown_id = c.tick();
    unknown_id.external_id = "GBPUSD Curncy";
    auto unknown_field = c.tick();
    unknown_field.field = "PX_BID";
    auto unknown_scheme = c.tick();
    unknown_scheme.id_scheme = "ric";

    for (const auto& tick : {unknown_id, unknown_field, unknown_scheme}) {
        const auto out = sut.translate("bloomberg-live", tick, c.now);
        REQUIRE_FALSE(out.has_value());
        CHECK(out.error() == adapter_tick_drop::unmapped);
    }
    CHECK(sut.drops("bloomberg-live", adapter_tick_drop::unmapped) == 3);
}

TEST_CASE("a_tick_of_an_adapter_that_is_no_source_is_unmapped", "[app][adapter_tick_translator]") {
    catalogue c;
    auto sut = c.translator();

    const auto out = sut.translate("nobody", c.tick(), c.now);

    REQUIRE_FALSE(out.has_value());
    CHECK(out.error() == adapter_tick_drop::unmapped);
}

TEST_CASE("a_mapping_that_does_not_hold_now_leaves_a_tick_unmapped",
          "[app][adapter_tick_translator]") {
    catalogue c;
    c.mapping.effective_from = c.now + 1h;
    CHECK(c.translator().translate("bloomberg-live", c.tick(), c.now).error() ==
          adapter_tick_drop::unmapped);

    c.mapping.effective_from = c.now - 2h;
    c.mapping.effective_to = c.now - 1h;
    CHECK(c.translator().translate("bloomberg-live", c.tick(), c.now).error() ==
          adapter_tick_drop::unmapped);
}

TEST_CASE("the_newest_mapping_that_holds_names_the_item", "[app][adapter_tick_translator]") {
    catalogue c;
    auto newer = c.mapping;
    newer.id = boost::uuids::random_generator{}();
    newer.oresmd_uri = "oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=CHF";
    newer.effective_from = c.now - 1h;
    adapter_tick_translator sut({c.live}, {c.instrument}, {c.mapping, newer});

    const auto out = sut.translate("bloomberg-live", c.tick(), c.now);

    REQUIRE(out.has_value());
    CHECK(out->tick.oresmd_uri == newer.oresmd_uri);
}

TEST_CASE("a_value_that_is_not_a_number_is_dropped", "[app][adapter_tick_translator]") {
    catalogue c;
    auto sut = c.translator();

    for (const auto* value : {"abc", "", "nan", "inf", "1.0.2"}) {
        CAPTURE(value);
        const auto out = sut.translate("bloomberg-live", c.tick(value), c.now);
        REQUIRE_FALSE(out.has_value());
        CHECK(out.error() == adapter_tick_drop::not_a_number);
    }
}

TEST_CASE("a_value_outside_the_bounds_is_dropped", "[app][adapter_tick_translator]") {
    catalogue c;
    auto sut = c.translator();

    const auto out = sut.translate("bloomberg-live", c.tick("1e13"), c.now);

    REQUIRE_FALSE(out.has_value());
    CHECK(out.error() == adapter_tick_drop::out_of_range);
}

TEST_CASE("a_tick_older_than_the_age_limit_is_stale", "[app][adapter_tick_translator]") {
    catalogue c;
    auto sut = c.translator();

    auto old = c.tick();
    old.vendor_time = c.now - 6s;
    const auto out = sut.translate("bloomberg-live", old, c.now);

    REQUIRE_FALSE(out.has_value());
    CHECK(out.error() == adapter_tick_drop::stale);
}

TEST_CASE("the_limits_an_adapter_reads_are_the_limits_the_service_applies",
          "[app][adapter_tick_translator]") {
    // The simulator injects its faults from these limits, so a tick one step beyond them is
    // dropped for exactly the reason the fault is meant to show.
    using ores::marketdata::domain::adapter_limits;
    catalogue c;
    auto sut = c.translator();

    const auto beyond_range = sut.translate(
        "bloomberg-live", c.tick(std::to_string(adapter_limits::max_abs_value * 10)), c.now);
    REQUIRE_FALSE(beyond_range.has_value());
    CHECK(beyond_range.error() == adapter_tick_drop::out_of_range);

    auto delayed = c.tick();
    delayed.vendor_time = c.now - adapter_limits::max_tick_age - 1ms;
    const auto beyond_age = sut.translate("bloomberg-live", delayed, c.now);
    REQUIRE_FALSE(beyond_age.has_value());
    CHECK(beyond_age.error() == adapter_tick_drop::stale);
}

TEST_CASE("a_tick_before_the_newest_stored_one_is_out_of_order", "[app][adapter_tick_translator]") {
    catalogue c;
    auto sut = c.translator();

    auto later = c.tick();
    later.vendor_time = c.now - 1s;
    auto earlier = c.tick();
    earlier.vendor_time = c.now - 2s;
    auto same = c.tick();
    same.vendor_time = c.now - 1s;

    REQUIRE(sut.translate("bloomberg-live", later, c.now).has_value());
    const auto out = sut.translate("bloomberg-live", earlier, c.now);
    REQUIRE_FALSE(out.has_value());
    CHECK(out.error() == adapter_tick_drop::out_of_order);
    CHECK(sut.translate("bloomberg-live", same, c.now).has_value());
}

TEST_CASE("the_order_of_a_source_does_not_depend_on_another_source",
          "[app][adapter_tick_translator]") {
    catalogue c;
    auto sut = c.translator();

    auto later = c.tick();
    later.vendor_time = c.now - 1s;
    auto earlier = c.tick();
    earlier.vendor_time = c.now - 2s;

    REQUIRE(sut.translate("bloomberg-live", later, c.now).has_value());
    CHECK(sut.translate("bloomberg-sim", earlier, c.now).has_value());
}

TEST_CASE("each_drop_is_counted_for_its_adapter_and_reason", "[app][adapter_tick_translator]") {
    catalogue c;
    auto sut = c.translator();

    (void)sut.translate("bloomberg-live", c.tick("abc"), c.now);
    (void)sut.translate("bloomberg-live", c.tick("abc"), c.now);
    (void)sut.translate("bloomberg-sim", c.tick("1e13"), c.now);

    CHECK(sut.drops("bloomberg-live", adapter_tick_drop::not_a_number) == 2);
    CHECK(sut.drops("bloomberg-live", adapter_tick_drop::out_of_range) == 0);
    CHECK(sut.drops("bloomberg-sim", adapter_tick_drop::out_of_range) == 1);
    CHECK(sut.drops("bloomberg-sim", adapter_tick_drop::not_a_number) == 0);
}
