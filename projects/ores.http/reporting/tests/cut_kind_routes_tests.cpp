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
#include "ores.http/routes/reporting/cut_kind_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::reporting::tests::answer_at;
using ores::http::routes::reporting::cut_kind_routes;
using namespace ores::logging;

namespace {

const std::string kinds_address = "/api/reporting/cut-kinds";
const std::string kind_address = "/api/reporting/cut-kinds/end_of_day";
const std::string kind_pattern = "/api/reporting/cut-kinds/{code}";

/// The intent every write states.
const std::string an_intent = R"("intent":{"reason_code":"refdata.initial_load","commentary":""})";

/// A change the form offers, with the retention it states and one member appended.
std::string a_change(int retention_months = 12, const std::string& extra = "") {
    std::string change =
        R"("name":"End of day","description":"The close","familyCode":"end_of_day",)";
    change += R"("approvalKindCode":null,"outputIsIndicative":false,)";
    change += R"("resolutionRuleCode":"nearest","retentionMonths":)";
    change += std::to_string(retention_months) + ",";
    change += R"("legalHoldDefault":false,"displayOrder":0)";
    if (!extra.empty())
        change += "," + extra;
    return "{" + change + "}";
}

/// One put of a body, at the kind's own address.
http_status put_of(const std::string& body) {
    http_request req;
    req.body = body;
    req.path_params["code"] = "end_of_day";
    return answer_at<cut_kind_routes>(http_method::put, kind_address, req, kind_pattern).status;
}

}

TEST_CASE("the_kinds_of_cut_refuse_a_page_they_do_not_read", "[cut_kind]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the kind list's page and order";

    // A page states a limit, as the BFF's own schema required.
    CHECK(answer_at<cut_kind_routes>(http_method::get, kinds_address, http_request{}, kinds_address)
              .status == http_status::bad_request);

    http_request sorted;
    sorted.query_params["limit"] = "10";
    sorted.query_params["sort"] = "retention";
    CHECK(
        answer_at<cut_kind_routes>(http_method::get, kinds_address, sorted, kinds_address).status ==
        http_status::bad_request);
}

TEST_CASE("the_kinds_of_cut_refuse_a_query_they_do_not_read", "[cut_kind]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the kind list's strict query";

    http_request req;
    req.query_params["limit"] = "10";
    req.query_params["family"] = "end_of_day";
    CHECK(answer_at<cut_kind_routes>(http_method::get, kinds_address, req, kinds_address).status ==
          http_status::bad_request);
}

TEST_CASE("a_kind_of_cut_refuses_a_write_it_does_not_state", "[cut_kind]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a kind's write refusals";

    // A write states why it is made, and the version it was read at.
    CHECK(put_of(R"({"change":{}}") == http_status::bad_request);
    CHECK(put_of(R"({"version":1,"change":{}})") == http_status::bad_request);

    // A retention of more than a century is not a retention the form offers.
    const auto too_long = "{\"version\":1,\"change\":" + a_change() + "," + an_intent + "}";
    CHECK(put_of(too_long) == http_status::bad_gateway);

    CHECK(put_of("{\"version\":1,\"change\":" + a_change(2000) + "," + an_intent + "}") ==
          http_status::bad_request);
}

TEST_CASE("a_kind_keeps_the_retention_destination_the_form_never_offers", "[cut_kind]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a kind's write is rebuilt from the row it read";

    // The form offers nine of the row's fields, and the destination is not one
    // of them: the write is assembled from the row just read, so a body that
    // states a destination is ignored and the row's stands.
    const auto body =
        "{\"version\":1,\"change\":" + a_change(12, R"("retentionDestinationCode":"cold")") + "," +
        an_intent + "}";
    CHECK(put_of(body) == http_status::bad_gateway);
}

TEST_CASE("a_kind_of_cut_read_with_no_service_is_refused_as_upstream", "[cut_kind]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a kind read with no service behind it";

    CHECK(answer_at<cut_kind_routes>(http_method::get, kind_address, http_request{}, kind_pattern)
              .status == http_status::bad_gateway);

    http_request req;
    req.query_params["limit"] = "10";
    CHECK(answer_at<cut_kind_routes>(http_method::get, kinds_address, req, kinds_address).status ==
          http_status::bad_gateway);
}
