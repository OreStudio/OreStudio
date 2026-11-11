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
#include "ores.http.nats/forwarding.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <stdexcept>
#include <string>

namespace fw = ores::http::forwarding;
namespace rr = ores::http::net::record_routes;
using ores::http::domain::http_status;

namespace {

template <typename F>
std::optional<rr::refusal> forwarding_refusal_of(F&& call) {
    try {
        call();
    } catch (const rr::refusal& r) {
        return r;
    }
    return std::nullopt;
}

}

TEST_CASE("forwarding_maps_each_known_service_refusal_to_its_status", "[forwarding]") {
    CHECK(fw::refusal_for("forbidden").status == http_status::forbidden);
    CHECK(fw::refusal_for("forbidden").code == "forbidden");
    CHECK(fw::refusal_for("bad_request").status == http_status::bad_request);
    CHECK(fw::refusal_for("bad_request").code == "invalid-request");
    CHECK(fw::refusal_for("unauthorized").status == http_status::unauthorized);
    CHECK(fw::refusal_for("unauthorized").code == "not-authenticated");
}

TEST_CASE("forwarding_answers_a_refusal_it_cannot_name_as_the_service_not_answering",
          "[forwarding]") {
    const auto r = fw::refusal_for("something_new");
    CHECK(r.status == http_status::bad_gateway);
    CHECK(r.code == "upstream-unavailable");
    CHECK(r.message.find("something_new") == std::string::npos);
}

TEST_CASE("forwarding_throws_the_refusal_each_failure_names", "[forwarding]") {
    const auto refused = forwarding_refusal_of([] {
        fw::refuse_refused(ores::nats::service::request_refused_error("forbidden", "a.v1.b.c"));
    });
    REQUIRE(refused.has_value());
    CHECK(refused->status == http_status::forbidden);

    const auto lapsed = forwarding_refusal_of([] {
        fw::refuse_lapsed(ores::nats::service::session_expired_error("Session has expired."));
    });
    REQUIRE(lapsed.has_value());
    CHECK(lapsed->status == http_status::unauthorized);
    CHECK(lapsed->code == "session-expired");

    const auto undecodable =
        forwarding_refusal_of([] { fw::refuse_undecodable("a.v1.b.c", "bad bytes"); });
    REQUIRE(undecodable.has_value());
    CHECK(undecodable->status == http_status::bad_gateway);
    CHECK(undecodable->message.find("bad bytes") == std::string::npos);
}

TEST_CASE("forwarding_keeps_a_transport_failure_out_of_the_body", "[forwarding]") {
    const auto response = fw::unanswered("a.v1.b.c", std::runtime_error("socket detail"));
    CHECK(response.status == http_status::bad_gateway);
    CHECK(response.body.find("socket detail") == std::string::npos);
    CHECK(response.body.find("\"code\":\"internal\"") != std::string::npos);
}
