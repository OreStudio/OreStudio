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
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http/routes/marketdata/curve_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::marketdata::curve_routes;
using ores::http::marketdata::tests::answer_at;
using namespace ores::logging;

namespace {

/// The identity every read of one curve takes, in the shape the screen sends it.
const std::string identity = R"({"asset":"ir","scope":"global","instrument_type":"curve",)"
                             R"("quote_type":"discount","party_id":"",)"
                             R"("provider_code":"","fields":[]})";

/// The body of one route, with the identity it names.
std::string with_identity(const std::string& members) {
    return "{\"identity\":" + identity + members + "}";
}

http_status curve_status(const std::string& address, const std::string& body) {
    http_request req;
    req.body = body;
    return answer_at<curve_routes>(http_method::post, address, req, address).status;
}

}

TEST_CASE("curves_refuse_a_body_they_do_not_read", "[curve]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the curve routes' bodies";

    CHECK(curve_status("/api/curves/series", "") == http_status::bad_request);
    CHECK(curve_status("/api/curves/series", R"({"identity":{}})") == http_status::bad_request);
    CHECK(curve_status("/api/curves/series", with_identity(R"(,"colour":"red")")) ==
          http_status::bad_request);
    CHECK(curve_status("/api/curves/snapshot",
                       R"({"identity":{"asset":"ir","scope":"global","instrument_type":"curve",)"
                       R"("quote_type":"discount","party_id":"not-an-id","provider_code":"",)"
                       R"("fields":[]}})") == http_status::bad_request);
    CHECK(curve_status("/api/curves/compute", R"({"as_of":"2026-03-14"})") ==
          http_status::bad_request);
    CHECK(curve_status("/api/curves/compute",
                       R"({"bootstrap_config_id":"00000000-0000-0000-0000-0000000000a1",)"
                       R"("as_of":"not an instant"})") == http_status::bad_request);
}

TEST_CASE("curves_refuse_a_slice_whose_range_ends_before_it_starts", "[curve]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a slice's range";

    const auto slice = [](const std::string& from, const std::string& to) {
        return with_identity(R"(,"component_field":"strike_label","component_value":"ATM",)"
                             R"("from_instant":")" +
                             from + R"(","to_instant":")" + to + R"(")");
    };

    CHECK(
        curve_status("/api/curves/slice", slice("2026-03-15 00:00:00Z", "2026-03-14 00:00:00Z")) ==
        http_status::bad_request);
    CHECK(
        curve_status("/api/curves/slice", slice("2026-03-14 00:00:00Z", "2026-03-15 00:00:00Z")) ==
        http_status::bad_gateway);
}

TEST_CASE("curves_refuse_an_evolution_that_names_no_instant", "[curve]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing an evolution without a range";

    CHECK(curve_status("/api/curves/evolution", with_identity("")) == http_status::bad_request);
    CHECK(curve_status("/api/curves/evolution",
                       with_identity(R"(,"from_instant":"2026-03-14 00:00:00Z")")) ==
          http_status::bad_request);
    CHECK(curve_status("/api/curves/evolution",
                       with_identity(R"(,"instants":["2026-03-14 00:00:00Z"])")) ==
          http_status::bad_gateway);
}

TEST_CASE("curves_refuse_a_point_that_states_no_reason", "[curve]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a keyed manual point";

    const auto with_point = [](const std::string& tail) {
        return with_identity(R"(,"coordinates":["5Y"],"value":"1.25")" + tail);
    };
    CHECK(curve_status("/api/curves/manual-point", with_point("")) == http_status::bad_request);
    CHECK(curve_status("/api/curves/manual-point",
                       with_point(R"(,"change_reason_code":"manual.override")")) ==
          http_status::bad_request);
    CHECK(curve_status("/api/curves/manual-point",
                       with_identity(R"(,"coordinates":[],"value":"1.25",)"
                                     R"("change_reason_code":"manual.override",)"
                                     R"("change_commentary":"A correction.")")) ==
          http_status::bad_request);
    CHECK(curve_status("/api/curves/manual-point",
                       with_point(R"(,"change_reason_code":"manual.override",)"
                                  R"("change_commentary":"A correction.")")) ==
          http_status::bad_gateway);
    CHECK(curve_status("/api/curves/manual-point/clear",
                       with_identity(R"(,"coordinates":["5Y"])")) == http_status::bad_gateway);
}

TEST_CASE("curves_refuse_a_page_that_asks_for_nothing", "[curve]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a published list's page";

    CHECK(curve_status("/api/curves/published", R"({"limit":0})") == http_status::bad_request);
    CHECK(curve_status("/api/curves/published", R"({"offset":-1})") == http_status::bad_request);
    CHECK(curve_status("/api/curves/published", R"({"request_id_one_of":["not-an-id"]})") ==
          http_status::bad_request);
    CHECK(curve_status("/api/curves/published", "{}") == http_status::bad_gateway);
    CHECK(curve_status("/api/curves/published-sets", R"({"series_id":""})") ==
          http_status::bad_request);
    CHECK(curve_status("/api/curves/published-sets",
                       R"({"series_id":"00000000-0000-7000-8000-0000000000a1","limit":21})") ==
          http_status::bad_request);
}

TEST_CASE("curves_refuse_a_withdrawal_that_names_no_set", "[curve]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a withdrawal's body";

    CHECK(curve_status("/api/curves/published/withdraw", R"({"reason_code":"system.retired"})") ==
          http_status::bad_request);
    CHECK(curve_status("/api/curves/published/withdraw",
                       R"({"publish_id":"00000000-0000-0000-0000-0000000000a1"})") ==
          http_status::bad_request);
    CHECK(curve_status("/api/curves/published/withdraw",
                       R"({"publish_id":"00000000-0000-0000-0000-0000000000a1",)"
                       R"("reason_code":"system.retired"})") == http_status::bad_gateway);
}

TEST_CASE("curves_read_the_series_of_a_party_without_asking_for_ids", "[curve]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a read of the party's series";

    CHECK(curve_status("/api/curves/series-by-id", "{}") == http_status::bad_gateway);
    CHECK(curve_status("/api/curves/series-by-id", R"({"ids":["not-an-id"]})") ==
          http_status::bad_request);
    CHECK(curve_status("/api/curves/series-list", "{}") == http_status::bad_gateway);
    CHECK(curve_status("/api/curves/fx-forward-ladder", R"({"pair_code":"EUR/USD"})") ==
          http_status::bad_request);
}
