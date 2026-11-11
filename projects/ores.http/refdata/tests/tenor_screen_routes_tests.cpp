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
#include "ores.http/routes/refdata/tenor_screen_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/tenor_codec.hpp"
#include "ores.refdata.api/domain/tenor_convention_codec.hpp"
#include "ores.refdata.api/domain/tenor_convention_resolution_codec.hpp"
#include "ores.refdata.api/messaging/tenor_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::refdata::tests::answer_at;
using ores::http::routes::refdata::tenor_screen_routes;
using namespace ores::logging;

namespace {

const std::string resolutions = "/api/tenors/conventions/{code}/resolutions";
const std::string membership = "/api/tenors/conventions/{code}/resolutions/{tenor}";
const std::string stated_reason = R"("intent":{"reason_code":"system.new_record"})";

http_request tenor_body(std::string body) {
    http_request req;
    req.body = std::move(body);
    return req;
}

http_response tenor_answer(http_method method,
                           const std::string& path,
                           http_request req,
                           const std::string& pattern) {
    return answer_at<tenor_screen_routes>(method, path, std::move(req), pattern);
}

}

TEST_CASE("tenor_routes_refuse_a_malformed_request_before_forwarding", "[tenor_screen]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing each tenor route's refusals";

    const auto refused = [](const http_response& answer) {
        CHECK(answer.status == http_status::bad_request);
    };

    refused(
        tenor_answer(http_method::post,
                     "/api/tenors/resolve",
                     tenor_body(R"({"tenor_code":"  ","convention_code":"STD",)"
                                R"("calendar_code":"TARGET","horizon":"2026-01-02","spot":null})"),
                     "/api/tenors/resolve"));
    refused(tenor_answer(http_method::post,
                         "/api/tenors/resolve",
                         tenor_body(R"({"tenor_code":"1M","convention_code":"STD",)"
                                    R"("calendar_code":"TARGET","horizon":"soon","spot":null})"),
                         "/api/tenors/resolve"));
    refused(tenor_answer(http_method::post,
                         "/api/tenors/resolve-convention",
                         tenor_body(R"({"convention_code":"STD","calendar_code":"",)"
                                    R"("horizon":"2026-01-02","spot":null})"),
                         "/api/tenors/resolve-convention"));
    refused(tenor_answer(http_method::post,
                         "/api/tenors/preview-resolution",
                         tenor_body(R"({"calendar_code":"TARGET"})"),
                         "/api/tenors/preview-resolution"));
    refused(tenor_answer(http_method::get,
                         "/api/tenors/conventions/" + std::string(101, 'X') + "/resolutions",
                         http_request{},
                         resolutions));
    refused(tenor_answer(http_method::put,
                         "/api/tenors/tenors",
                         tenor_body(R"({"version":null,"write":{"code":"1M"}})"),
                         "/api/tenors/tenors"));
    refused(tenor_answer(http_method::put,
                         "/api/tenors/conventions/STD/resolutions/1M",
                         tenor_body(R"({"version":null,"write":{}})"),
                         membership));
    refused(tenor_answer(http_method::delete_,
                         "/api/tenors/conventions/STD/resolutions/1M",
                         tenor_body("{" + stated_reason + "}"),
                         membership));
}

TEST_CASE("tenor_routes_forward_a_well_formed_request", "[tenor_screen]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that an accepted tenor request reaches the service";

    // The client is not connected, so an accepted request fails at the forward step.
    const auto unanswered = [](const http_response& answer) {
        CHECK(answer.status == http_status::bad_gateway);
    };

    unanswered(
        tenor_answer(http_method::post,
                     "/api/tenors/resolve",
                     tenor_body(R"({"tenor_code":" 1M ","convention_code":"STD",)"
                                R"("calendar_code":"TARGET","horizon":"2026-01-02","spot":null})"),
                     "/api/tenors/resolve"));
    unanswered(tenor_answer(http_method::post,
                            "/api/tenors/resolve-convention",
                            tenor_body(R"({"convention_code":"STD","calendar_code":"TARGET",)"
                                       R"("horizon":"2026-01-02","spot":"2026-01-04"})"),
                            "/api/tenors/resolve-convention"));

    ores::refdata::messaging::preview_tenor_resolution_request draft;
    draft.calendar_code = "TARGET";
    draft.horizon = std::chrono::year_month_day{
        std::chrono::year{2026}, std::chrono::month{1}, std::chrono::day{2}};
    unanswered(tenor_answer(http_method::post,
                            "/api/tenors/preview-resolution",
                            tenor_body(ores::utility::codec::json::write(draft)),
                            "/api/tenors/preview-resolution"));

    unanswered(tenor_answer(
        http_method::get, "/api/tenors/conventions/STD/resolutions", http_request{}, resolutions));
    unanswered(
        tenor_answer(http_method::put,
                     "/api/tenors/tenors",
                     tenor_body("{" + stated_reason +
                                R"(,"version":null,"write":{"code":"1M","display_name":"1M",)"
                                R"("description":"","sort_order":1,"kind":"TERM",)"
                                R"("unit":"M","multiplier":1,"modified_by":"x"}})"),
                     "/api/tenors/tenors"));
    unanswered(tenor_answer(http_method::put,
                            "/api/tenors/conventions",
                            tenor_body("{" + stated_reason +
                                       R"(,"version":3,"write":{"code":"STD","description":"",)"
                                       R"("measured_from":"SPOT","resolution_algorithm":"OFFSET",)"
                                       R"("business_day_convention_type":"MF"}})"),
                            "/api/tenors/conventions"));

    // The address names the membership row, so the body leaves its codes out.
    unanswered(tenor_answer(http_method::put,
                            "/api/tenors/conventions/STD/resolutions/1M",
                            tenor_body("{" + stated_reason + R"(,"version":null,"write":{}})"),
                            membership));
    unanswered(tenor_answer(http_method::delete_,
                            "/api/tenors/conventions/STD/resolutions/1M",
                            tenor_body("{" + stated_reason + R"(,"version":2})"),
                            membership));
}
