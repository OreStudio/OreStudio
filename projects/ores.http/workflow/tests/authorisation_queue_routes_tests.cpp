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
#include "ores.http/routes/workflow/authorisation_queue_routes.hpp"
#include "ores.http/routes/workflow/workflow_routes.hpp"
#include "ores.logging/logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <utility>
#include <vector>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::workflow::authorisation_queue_routes;
using ores::http::routes::workflow::workflow_routes;
using ores::http::workflow::tests::answer_at;
using namespace ores::logging;

namespace {

const std::string queue_pattern = "/api/queue/authorisations";

const std::string queue_page_refused = "A page names an offset and a limit of 1 to 100.";

}

TEST_CASE("the_authorisation_queue_is_one_page_or_a_refusal", "[workflow_queue]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the page the queue takes";

    for (const auto& page : std::vector<std::pair<std::string, std::string>>{
             {"offset", "-1"}, {"limit", "0"}, {"limit", "101"}, {"limit", "ten"}}) {
        http_request req;
        req.query_params[page.first] = page.second;
        const auto refused =
            answer_at<workflow_routes>(http_method::get, queue_pattern, req, queue_pattern);
        CHECK(refused.status == http_status::bad_request);
        CHECK(refused.body.find(queue_page_refused) != std::string::npos);
    }

    http_request unknown;
    unknown.query_params["search"] = "x";
    CHECK(answer_at<workflow_routes>(http_method::get, queue_pattern, unknown, queue_pattern)
              .status == http_status::bad_request);
}

TEST_CASE("the_authorisation_queue_reaches_the_service", "[workflow_queue]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the queue with no service behind it";

    CHECK(answer_at<workflow_routes>(http_method::get, queue_pattern, http_request{}, queue_pattern)
              .status == http_status::bad_gateway);
}

TEST_CASE("a_queue_row_states_the_deal_and_whether_it_moved", "[workflow_queue]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the row the queue draws";

    authorisation_queue_routes::queue_row row;
    row.request_id = "11111111-1111-4111-8111-111111111111";
    row.request_version = 3;
    row.kind_code = "trading.operational_authorisation";
    row.state_code = "waiting";
    row.requested_by = "alice";
    row.requested_at = "2026-11-18 09:00:00.000000Z";
    row.reason = "a booking awaits Operations";
    row.trade_id = "22222222-2222-4222-8222-222222222222";
    row.reference = "IRS-1";
    row.request_trade_version = 3;
    row.trade_version = 4;
    row.class_code = "non-flow";
    row.book_id = "33333333-3333-4333-8333-333333333333";
    row.counterparty_id = "44444444-4444-4444-8444-444444444444";
    row.trade_type = "irs";
    row.trade_type_name = "Interest rate swap";
    row.book = "Rates London";
    row.counterparty = "Acme Bank";
    row.trade_date = "2026-11-17";
    row.status = "awaiting_authorisation";

    const auto written = authorisation_queue_routes::row_json(row);
    CHECK(written.find("\"requestVersion\":3") != std::string::npos);
    CHECK(written.find("\"requestedBy\":\"alice\"") != std::string::npos);
    CHECK(written.find("\"reference\":\"IRS-1\"") != std::string::npos);
    CHECK(written.find("\"requestTradeVersion\":3") != std::string::npos);
    CHECK(written.find("\"tradeVersion\":4") != std::string::npos);
    CHECK(written.find("\"changed\":true") != std::string::npos);
    CHECK(written.find("\"classCode\":\"non-flow\"") != std::string::npos);
    CHECK(written.find("\"tradeTypeName\":\"Interest rate swap\"") != std::string::npos);
    CHECK(written.find("\"book\":\"Rates London\"") != std::string::npos);
    CHECK(written.find("\"counterparty\":\"Acme Bank\"") != std::string::npos);
    CHECK(written.find("\"tradeDate\":\"2026-11-17\"") != std::string::npos);
    CHECK(written.find("\"status\":\"awaiting_authorisation\"") != std::string::npos);

    row.trade_version = 3;
    CHECK(authorisation_queue_routes::row_json(row).find("\"changed\":false") != std::string::npos);
}

TEST_CASE("the_authorisation_queue_is_not_kept", "[workflow_queue]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the cache policy the queue carries";

    http_request req;
    const auto answer = authorisation_queue_routes::queue_answer(req, "{}");
    CHECK(answer.status == http_status::ok);
    CHECK(answer.headers.at("Cache-Control") == "no-store");
    CHECK(!answer.headers.contains("ETag"));
}
