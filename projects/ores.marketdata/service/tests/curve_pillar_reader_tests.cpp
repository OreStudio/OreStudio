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
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/oresmd/pillar_quote.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.testing/database_helper.hpp"
#include "test_helpers.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

using ores::marketdata::domain::market_observation;
using ores::marketdata::domain::market_series;
using ores::marketdata::repository::market_observation_repository;
using ores::marketdata::repository::market_series_repository;
using ores::marketdata::service::curve_republish_refdata_context;
using ores::marketdata::service::read_pillar_rates;
using ores::marketdata::service::resolve_tenor_date;
using ores::refdata::domain::ir_curve_bootstrap_pillar;
using ores::refdata::domain::tenor;
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

// A spot anchor and two period tenors: enough for a spot-starting pillar and a
// 1Y-starting one to resolve without a calendar.
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

ir_curve_bootstrap_pillar
make_pillar(int seq, const std::string& start, const std::string& end, boost::uuids::uuid series) {
    ir_curve_bootstrap_pillar p;
    p.sequence_index = seq;
    p.start_tenor_code = start;
    p.end_tenor_code = end;
    p.curve_role_code = "SWAP";
    p.market_series_id = series;
    return p;
}

const std::string spot_first = "1Y";
const std::string first_second = "2Y";

// The quote of one pillar, built as the feed builds it: the same datum builder,
// given the dates the conventions resolve.
ores::marketdata::core::pillar_quote pillar_quote_of(const curve_republish_refdata_context& ctx,
                                                     const std::string& start,
                                                     const std::string& end) {
    return {"USD",
            "1D",
            start,
            end,
            resolve_tenor_date(ctx, start),
            resolve_tenor_date(ctx, end)};
}

ores::marketdata::datum::market_datum
quote_datum(const curve_republish_refdata_context& ctx,
            const std::string& start,
            const std::string& end) {
    const auto made = ores::marketdata::core::pillar_quote_datum(pillar_quote_of(ctx, start, end));
    if (!made)
        throw std::runtime_error("no datum for the pillar " + start + " to " + end + ": " +
                                 made.error());
    return *made;
}

/// The series the feed's datum for this pillar belongs to.
std::string quote_series_uri(const curve_republish_refdata_context& ctx,
                             const std::string& start,
                             const std::string& end) {
    const auto uri = ores::marketdata::datum::oresmd_uri_codec::write(
        ores::marketdata::datum::series_of(quote_datum(ctx, start, end)));
    if (!uri)
        throw std::runtime_error("no series for the pillar " + start + " to " + end);
    return *uri;
}

/// The datum URI the feed publishes for this pillar.
std::string quote_datum_uri(const curve_republish_refdata_context& ctx,
                            const std::string& start,
                            const std::string& end) {
    const auto uri = ores::marketdata::datum::oresmd_uri_codec::write(quote_datum(ctx, start, end));
    if (!uri)
        throw std::runtime_error("no datum URI for the pillar " + start + " to " + end);
    return *uri;
}

struct fixture {
    database_helper h;
    boost::uuids::uuid party_id = boost::uuids::random_generator{}();
    boost::uuids::random_generator uuid_gen;

    /// Writes the series @p uri names and returns its id, as the ingest creates it.
    boost::uuids::uuid write_series(const std::string& uri) {
        market_series s;
        s.id = uuid_gen();
        s.version = 0;
        s.tenant_id = h.tenant_id();
        s.party_id = party_id;
        s.oresmd_uri = uri;
        s.series_subclass = "yield";
        s.provider_code = "ore";
        s.derivation_kind = "OBSERVED";
        s.derivation_config_id = boost::uuids::nil_uuid();
        s.derivation_config_version = 0;
        s.modified_by = h.db_user();
        s.performed_by = h.db_user();
        s.change_reason_code = "system.test";
        s.change_commentary = "curve_pillar_reader test";
        market_series_repository repo;
        repo.write(h.context(), s);
        return s.id;
    }

    /// Writes the quote @p datum_uri names into the series @p series_id, at the shared quote time.
    void write_quote(const boost::uuids::uuid& series_id,
                     const std::string& datum_uri,
                     const std::string& value) {
        market_observation o;
        o.id = uuid_gen();
        o.tenant_id = h.tenant_id();
        o.party_id = party_id;
        o.series_id = series_id;
        o.observation_datetime = quote_time;
        // The datum's URI, the same projection the feed writes, so the test
        // pins the agreement rather than a point string.
        o.oresmd_uri = datum_uri;
        o.value = value;
        o.source = "curve_pillar_reader test";
        market_observation_repository repo;
        repo.insert(h.context(), o);
    }
};

}

TEST_CASE("a_pillar_is_read_from_the_series_it_names", "[curve_pillar_reader]") {
    fixture f;
    const auto ctx = make_context();
    const auto series_id = f.write_series(quote_series_uri(ctx, "SPOT", spot_first));
    f.write_quote(series_id, quote_datum_uri(ctx, "SPOT", spot_first), "0.0432");

    const auto read = read_pillar_rates(
        f.h.context(), {make_pillar(0, "SPOT", spot_first, series_id)}, ctx, read_as_of);

    REQUIRE(read.rates_by_point_id.size() == 1);
    CHECK(read.rates_by_point_id.at(spot_first) == Catch::Approx(0.0432));
    REQUIRE(read.series_ids.size() == 1);
    CHECK(read.series_ids.front() == boost::uuids::to_string(series_id));
}

TEST_CASE("one series answers every rung of the ladder its pillars name", "[curve_pillar_reader]") {
    fixture f;
    const auto ctx = make_context();
    // Both pillars start at spot, so both read the one series and differ only by
    // the term they take from it. The second pillar is the first one's own quote
    // shifted to the longer tenor.
    const auto series_id = f.write_series(quote_series_uri(ctx, "SPOT", spot_first));
    f.write_quote(series_id, quote_datum_uri(ctx, "SPOT", spot_first), "0.0432");
    f.write_quote(series_id, quote_datum_uri(ctx, "SPOT", first_second), "0.0410");

    const auto read = read_pillar_rates(f.h.context(),
                                        {make_pillar(0, "SPOT", spot_first, series_id),
                                         make_pillar(1, "SPOT", first_second, series_id)},
                                        ctx,
                                        read_as_of);

    REQUIRE(read.rates_by_point_id.size() == 2);
    CHECK(read.rates_by_point_id.at(spot_first) == Catch::Approx(0.0432));
    CHECK(read.rates_by_point_id.at(first_second) == Catch::Approx(0.0410));
    // Both rates came out of one series, and the lineage names it once.
    REQUIRE(read.series_ids.size() == 1);
    CHECK(read.series_ids.front() == boost::uuids::to_string(series_id));
}

TEST_CASE("a_pillar_naming_no_series_gets_no_rate", "[curve_pillar_reader]") {
    fixture f;
    const auto ctx = make_context();
    // A series exists and holds a quote, but no pillar names it: the read asks
    // for the series the pillar carries and not for the one its tenors imply.
    const auto series_id = f.write_series(quote_series_uri(ctx, "SPOT", spot_first));
    f.write_quote(series_id, quote_datum_uri(ctx, "SPOT", spot_first), "0.0432");

    const auto read = read_pillar_rates(
        f.h.context(), {make_pillar(0, "SPOT", spot_first, f.uuid_gen())}, ctx, read_as_of);

    CHECK(read.rates_by_point_id.empty());
    CHECK(read.series_ids.empty());
}

TEST_CASE("a_pillar_whose_series_has_no_quote_at_its_term_gets_no_rate", "[curve_pillar_reader]") {
    fixture f;
    const auto ctx = make_context();
    const auto series_id = f.write_series(quote_series_uri(ctx, "SPOT", spot_first));

    // The series is published, but not at the term this read derived: the row
    // carries the second rung, which a feed that published only the long end
    // leaves behind.
    f.write_quote(series_id, quote_datum_uri(ctx, "SPOT", first_second), "0.9999");

    const auto read = read_pillar_rates(
        f.h.context(), {make_pillar(0, "SPOT", spot_first, series_id)}, ctx, read_as_of);

    CHECK(read.rates_by_point_id.empty());
    CHECK(read.series_ids.empty());
}

TEST_CASE("the_lineage_names_only_the_series_a_rate_was_read_from", "[curve_pillar_reader]") {
    fixture f;
    const auto ctx = make_context();
    const auto spot_id = f.write_series(quote_series_uri(ctx, "SPOT", spot_first));
    f.write_quote(spot_id, quote_datum_uri(ctx, "SPOT", spot_first), "0.0432");

    // The first pillar's series holds its quote; the second pillar names a series
    // nothing holds.
    const auto read = read_pillar_rates(f.h.context(),
                                        {make_pillar(0, "SPOT", spot_first, spot_id),
                                         make_pillar(1, spot_first, first_second, f.uuid_gen())},
                                        ctx,
                                        read_as_of);

    REQUIRE(read.rates_by_point_id.size() == 1);
    CHECK(read.rates_by_point_id.at(spot_first) == Catch::Approx(0.0432));
    REQUIRE(read.series_ids.size() == 1);
    CHECK(read.series_ids.front() == boost::uuids::to_string(spot_id));
}
