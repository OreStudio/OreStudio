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
#include "../src/curve_pillar_reader.hpp"
#include "ores.marketdata.core/oresmd/pillar_quote_key.hpp"
#include "ores.marketdata.core/repository/market_observations_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.testing/database_helper.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <format>
#include <string>
#include <vector>

namespace {

const std::string tags("[curve_pillar_reader]");

using ores::marketdata::domain::market_observation;
using ores::marketdata::domain::market_series;
using ores::marketdata::repository::market_observations_repository;
using ores::marketdata::repository::market_series_repository;
using ores::marketdata::service::curve_republish_refdata_context;
using ores::marketdata::service::read_pillar_rates;
using ores::marketdata::service::resolve_tenor_date;
using ores::refdata::domain::ir_curve_bootstrap_config;
using ores::refdata::domain::ir_curve_bootstrap_pillar;
using ores::refdata::domain::tenor;
using ores::refdata::domain::tenor_convention_resolution;
using ores::testing::database_helper;

constexpr auto horizon = std::chrono::year{2026} / 1 / 1;
const auto quote_time = std::chrono::sys_days{horizon} + std::chrono::hours{1};
const auto read_as_of = std::chrono::sys_days{horizon} + std::chrono::hours{12};

tenor make_tenor(const std::string& code, const std::string& unit, int multiplier) {
    tenor t;
    t.code = code;
    t.kind = "PERIOD";
    t.unit = unit;
    t.multiplier = multiplier;
    return t;
}

tenor_convention_resolution make_resolution(const std::string& tenor_code) {
    tenor_convention_resolution r;
    r.convention_code = "RATES_SPOT_FORWARD";
    r.tenor_code = tenor_code;
    return r;
}

// A spot anchor and two period tenors on the calendar axis: enough for a
// SPOT-starting pillar and a 1Y-starting one to resolve without a calendar.
curve_republish_refdata_context make_context() {
    curve_republish_refdata_context ctx;
    ctx.horizon = horizon;
    ctx.tenors_by_code.emplace("SPOT", make_tenor("SPOT", "DAY", 0));
    ctx.tenors_by_code.emplace("1Y", make_tenor("1Y", "YEAR", 1));
    ctx.tenors_by_code.emplace("2Y", make_tenor("2Y", "YEAR", 2));
    for (const auto& code : {"1Y", "2Y"})
        ctx.resolutions_by_tenor.emplace(code, make_resolution(code));
    ctx.convention.code = "RATES_SPOT_FORWARD";
    ctx.convention.measured_from = "SPOT";
    ctx.convention.resolution_algorithm = "ANCHOR_OFFSET";
    return ctx;
}

ir_curve_bootstrap_pillar make_pillar(int seq, const std::string& start, const std::string& end) {
    ir_curve_bootstrap_pillar p;
    p.sequence_index = seq;
    p.start_tenor_code = start;
    p.end_tenor_code = end;
    p.curve_role_code = "SWAP";
    return p;
}

const std::string spot_first = "1Y";
const std::string first_second = "2Y";

// The two pillars the cases below read, in the shape the FOMC config carries:
// each one starts where the previous ended.
std::vector<ir_curve_bootstrap_pillar> make_pillars() {
    return {make_pillar(0, "SPOT", spot_first), make_pillar(1, spot_first, first_second)};
}

// The key the feed's tick for this pillar carries, built the way the feed builds
// it, and its series identity.
ores::marketdata::core::pillar_quote_key pillar_key(const curve_republish_refdata_context& ctx,
                                                    const std::string& start,
                                                    const std::string& end) {
    return ores::marketdata::core::make_pillar_quote_key(
        "USD", start, resolve_tenor_date(ctx, start), resolve_tenor_date(ctx, end));
}

std::string pillar_point(const curve_republish_refdata_context& ctx, const std::string& end) {
    return std::format("{:%Y%m%d}", resolve_tenor_date(ctx, end));
}

struct fixture {
    database_helper h;
    boost::uuids::uuid party_id = boost::uuids::random_generator{}();
    boost::uuids::random_generator uuid_gen;

    ir_curve_bootstrap_config make_config() {
        ir_curve_bootstrap_config c;
        c.id = uuid_gen();
        c.version = 0;
        c.tenant_id = h.tenant_id();
        c.party_id = party_id;
        c.currency_code = "USD";
        c.tenor_convention_code = "RATES_SPOT_FORWARD";
        return c;
    }

    void write_pillar_series(const boost::uuids::uuid& id,
                             const curve_republish_refdata_context& ctx,
                             const std::string& start,
                             const std::string& end) {
        const auto key = pillar_key(ctx, start, end);
        market_series s;
        s.id = id;
        s.version = 0;
        s.tenant_id = h.tenant_id();
        s.party_id = party_id;
        s.oresmd_uri = ores::marketdata::core::pillar_series_uri(key);
        s.series_subclass = "yield";
        s.derivation_kind = "OBSERVED";
        s.derivation_config_id = boost::uuids::nil_uuid();
        s.derivation_config_version = 0;
        s.modified_by = h.db_user();
        s.performed_by = h.db_user();
        s.change_reason_code = "system.test";
        s.change_commentary = "curve_pillar_reader test";
        market_series_repository repo;
        repo.write(h.context(), s);
    }

    void write_quote(const boost::uuids::uuid& series_id,
                     const std::string& point_id,
                     const std::string& value) {
        market_observation o;
        o.id = uuid_gen();
        o.tenant_id = h.tenant_id();
        o.party_id = party_id;
        o.series_id = series_id;
        o.observation_datetime = quote_time;
        o.point_id = point_id;
        o.value = value;
        o.source = "curve_pillar_reader test";
        market_observations_repository repo;
        repo.write(h.context(), o);
    }
};

}

TEST_CASE("a_pillar_is_read_from_the_series_its_own_key_names", tags) {
    fixture f;
    const auto ctx = make_context();
    const auto config = f.make_config();
    const auto series_id = f.uuid_gen();

    f.write_pillar_series(series_id, ctx, "SPOT", spot_first);
    f.write_quote(series_id, pillar_point(ctx, spot_first), "0.0432");

    const auto read = read_pillar_rates(
        f.h.context(), config, {make_pillar(0, "SPOT", spot_first)}, ctx, read_as_of);

    REQUIRE(read.rates_by_point_id.size() == 1);
    CHECK(read.rates_by_point_id.at(spot_first) == Catch::Approx(0.0432));
    REQUIRE(read.series_ids.size() == 1);
    CHECK(read.series_ids.front() == boost::uuids::to_string(series_id));
}

TEST_CASE("a_pillar_with_no_series_of_its_own_gets_no_rate", tags) {
    fixture f;
    const auto ctx = make_context();
    const auto config = f.make_config();

    const auto read = read_pillar_rates(
        f.h.context(), config, {make_pillar(0, "SPOT", spot_first)}, ctx, read_as_of);

    CHECK(read.rates_by_point_id.empty());
    CHECK(read.series_ids.empty());
}

TEST_CASE("a_pillar_whose_series_has_no_quote_at_its_point_gets_no_rate", tags) {
    fixture f;
    const auto ctx = make_context();
    const auto config = f.make_config();
    const auto series_id = f.uuid_gen();

    // The series is published, but not at the point this read derived, which a
    // horizon the feed did not publish under produces.
    f.write_pillar_series(series_id, ctx, "SPOT", spot_first);
    f.write_quote(series_id, "19991231", "0.9999");

    const auto read = read_pillar_rates(
        f.h.context(), config, {make_pillar(0, "SPOT", spot_first)}, ctx, read_as_of);

    CHECK(read.rates_by_point_id.empty());
    CHECK(read.series_ids.empty());
}

TEST_CASE("the_lineage_names_only_the_series_a_rate_was_read_from", tags) {
    fixture f;
    const auto ctx = make_context();
    const auto config = f.make_config();
    const auto spot_id = f.uuid_gen();

    // The first pillar is published under its own identity; the second one's
    // series does not exist at all.
    f.write_pillar_series(spot_id, ctx, "SPOT", spot_first);
    f.write_quote(spot_id, pillar_point(ctx, spot_first), "0.0432");

    const auto read = read_pillar_rates(f.h.context(), config, make_pillars(), ctx, read_as_of);

    REQUIRE(read.rates_by_point_id.size() == 1);
    CHECK(read.rates_by_point_id.at(spot_first) == Catch::Approx(0.0432));
    REQUIRE(read.series_ids.size() == 1);
    CHECK(read.series_ids.front() == boost::uuids::to_string(spot_id));
}
