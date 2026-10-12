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
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/repository/market_series_identity_reader.hpp"
#include "ores.marketdata.core/service/import_service.hpp"
#include "ores.marketdata.core/service/manual_point_service.hpp"
#include "ores.marketdata.core/service/series_snapshot_reader.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.testing/database_helper.hpp"
#include <boost/uuid/random_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

using namespace ores::logging;

namespace {

const std::string_view test_suite("ores.marketdata.core.tests");
const std::string tags("[service][manual_point_service]");

using ores::marketdata::messaging::clear_manual_point_request;
using ores::marketdata::messaging::get_curve_snapshot_request;
using ores::marketdata::messaging::key_manual_point_request;
using ores::marketdata::messaging::resolve_series_identity_request;
using ores::marketdata::service::manual_point_service;
using ores::marketdata::service::series_snapshot_reader;
using ores::testing::database_helper;

void import(database_helper& h, const std::string& content) {
    ores::nats::service::nats_client auth_nats;
    ores::marketdata::service::import_service svc(h.context(), auth_nats);
    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = content;
    req.source = "test.manual_point_service";
    const auto resp = svc.import(req);
    REQUIRE(resp.success);
}

resolve_series_identity_request fx_option_identity(const std::string& unit_ccy,
                                                   const std::string& ccy) {
    resolve_series_identity_request id;
    id.asset = "fx";
    id.scope = unit_ccy;
    id.instrument_type = "fx_option";
    id.quote_type = "rate_lnvol";
    id.fields.push_back({"ccy", ccy});
    return id;
}

std::chrono::system_clock::time_point instant(const std::string& iso) {
    return ores::platform::time::datetime::from_iso8601_utc(iso);
}

/// The context of the party that owns the series, as a person acting for it has.
ores::database::context
owner_context(database_helper& h, const resolve_series_identity_request& identity) {
    const auto series =
        ores::marketdata::repository::market_series_identity_reader{}.read(h.context(), identity);
    REQUIRE(series.size() == 1);
    return h.context().with_party(
        h.tenant_id(), series.front().party_id, {series.front().party_id}, h.db_user());
}

key_manual_point_request key_request(const resolve_series_identity_request& identity,
                                     std::vector<std::string> coordinates,
                                     const std::string& value) {
    key_manual_point_request req;
    req.identity = identity;
    req.coordinates = std::move(coordinates);
    req.value = value;
    req.change_reason_code = "system.test";
    req.change_commentary = "operator correction";
    return req;
}

clear_manual_point_request clear_request(const resolve_series_identity_request& identity,
                                         std::vector<std::string> coordinates) {
    clear_manual_point_request req;
    req.identity = identity;
    req.coordinates = std::move(coordinates);
    return req;
}

/// The snapshot with provenance, read by a caller who may see every party.
ores::marketdata::messaging::get_curve_snapshot_response
snapshot(database_helper& h, const resolve_series_identity_request& identity) {
    get_curve_snapshot_request req;
    req.identity = identity;
    req.include_provenance = true;
    return series_snapshot_reader::read(h.context(), req, instant("2017-09-12T00:00:00Z"));
}

}

TEST_CASE("a_keyed_point_owns_its_node_and_a_clear_gives_the_feed_value_back", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    import(h,
           "20170910 FX_OPTION/RATE_LNVOL/NZD/ISK/1Y/ATM 0.080\n"
           "20170910 FX_OPTION/RATE_LNVOL/NZD/ISK/2Y/ATM 0.085\n");
    const auto identity = fx_option_identity("NZD", "ISK");
    const auto ctx = owner_context(h, identity);

    const auto keyed = manual_point_service::key(
        ctx, key_request(identity, {"2Y", "ATM"}, "0.090"), instant("2017-09-10T12:00:00Z"));
    REQUIRE(keyed.success);

    const auto after_key = snapshot(h, identity);
    REQUIRE(after_key.success);
    REQUIRE(after_key.values.size() == 2);
    CHECK(after_key.values[0] == "0.080");
    CHECK(after_key.values[1] == "0.090");
    CHECK(after_key.provenance[0].source_kind == "quoted");
    CHECK(after_key.provenance[1].source_kind == "manual");
    CHECK(after_key.provenance[1].change_reason_code == "system.test");
    CHECK(after_key.provenance[1].change_commentary == "operator correction");

    const auto cleared = manual_point_service::clear(ctx, clear_request(identity, {"2Y", "ATM"}));
    REQUIRE(cleared.success);

    const auto after_clear = snapshot(h, identity);
    REQUIRE(after_clear.success);
    CHECK(after_clear.values == std::vector<std::string>{"0.080", "0.085"});
    CHECK(after_clear.provenance[1].source_kind == "quoted");
}

TEST_CASE("a_clear_releases_a_node_that_was_keyed_more_than_once", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    import(h, "20170910 FX_OPTION/RATE_LNVOL/NZD/TRY/1Y/ATM 0.080\n");
    const auto identity = fx_option_identity("NZD", "TRY");
    const auto ctx = owner_context(h, identity);

    REQUIRE(manual_point_service::key(
                ctx, key_request(identity, {"1Y", "ATM"}, "0.090"), instant("2017-09-10T06:00:00Z"))
                .success);
    REQUIRE(manual_point_service::key(
                ctx, key_request(identity, {"1Y", "ATM"}, "0.095"), instant("2017-09-10T18:00:00Z"))
                .success);
    CHECK(snapshot(h, identity).values == std::vector<std::string>{"0.095"});

    REQUIRE(manual_point_service::clear(ctx, clear_request(identity, {"1Y", "ATM"})).success);
    CHECK(snapshot(h, identity).values == std::vector<std::string>{"0.080"});
}

TEST_CASE("a_node_the_shape_does_not_hold_is_refused_and_nothing_is_written", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    import(h, "20170910 FX_OPTION/RATE_LNVOL/NZD/THB/1Y/ATM 0.080\n");
    const auto identity = fx_option_identity("NZD", "THB");
    const auto ctx = owner_context(h, identity);

    const auto unknown_expiry =
        manual_point_service::key(ctx, key_request(identity, {"9Y", "ATM"}, "0.090"));
    CHECK_FALSE(unknown_expiry.success);
    CHECK_FALSE(unknown_expiry.message.empty());

    const auto too_few = manual_point_service::key(ctx, key_request(identity, {"1Y"}, "0.090"));
    CHECK_FALSE(too_few.success);

    const auto after = snapshot(h, identity);
    CHECK(after.values == std::vector<std::string>{"0.080"});
    CHECK(after.provenance[0].source_kind == "quoted");
}

TEST_CASE("a_value_that_is_no_number_or_a_missing_reason_is_refused", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    import(h, "20170910 FX_OPTION/RATE_LNVOL/NZD/MXN/1Y/ATM 0.080\n");
    const auto identity = fx_option_identity("NZD", "MXN");
    const auto ctx = owner_context(h, identity);

    CHECK_FALSE(manual_point_service::key(ctx, key_request(identity, {"1Y", "ATM"}, "abc")).success);
    CHECK_FALSE(manual_point_service::key(ctx, key_request(identity, {"1Y", "ATM"}, "0.09x")).success);
    CHECK_FALSE(manual_point_service::key(ctx, key_request(identity, {"1Y", "ATM"}, "")).success);

    auto no_reason = key_request(identity, {"1Y", "ATM"}, "0.090");
    no_reason.change_reason_code.clear();
    CHECK_FALSE(manual_point_service::key(ctx, no_reason).success);

    auto no_commentary = key_request(identity, {"1Y", "ATM"}, "0.090");
    no_commentary.change_commentary.clear();
    CHECK_FALSE(manual_point_service::key(ctx, no_commentary).success);

    CHECK(snapshot(h, identity).values == std::vector<std::string>{"0.080"});
}

TEST_CASE("clearing_a_node_with_no_manual_point_says_so", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    import(h, "20170910 FX_OPTION/RATE_LNVOL/NZD/KRW/1Y/ATM 0.080\n");
    const auto identity = fx_option_identity("NZD", "KRW");
    const auto ctx = owner_context(h, identity);

    const auto cleared = manual_point_service::clear(ctx, clear_request(identity, {"1Y", "ATM"}));
    CHECK_FALSE(cleared.success);
    CHECK_FALSE(cleared.message.empty());
}

TEST_CASE("a_context_for_another_party_cannot_key_on_the_series", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    import(h, "20170910 FX_OPTION/RATE_LNVOL/NZD/ILS/1Y/ATM 0.080\n");
    const auto identity = fx_option_identity("NZD", "ILS");

    const auto stranger = boost::uuids::random_generator()();
    const auto other = h.context().with_party(h.tenant_id(), stranger, {stranger}, h.db_user());
    const auto refused = manual_point_service::key(other, key_request(identity, {"1Y", "ATM"}, "0.090"));
    CHECK_FALSE(refused.success);

    CHECK(snapshot(h, identity).provenance[0].source_kind == "quoted");
}
