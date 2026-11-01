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
#include "ores.logging/boost_severity.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.reporting.core/service/cut_read_service.hpp"
#include "ores.reporting.core/service/cut_state.hpp"
#include "test_suite.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <vector>

// The decision these pin: what a month a person reads is, and which body
// replaced a cut. Both are rules the list draws on every row, and neither needs
// a store to state: a month is a range the caller's clock decides, and the body
// that replaced a cut is the earliest later body a run may price from.

using namespace ores::logging;
using namespace ores::reporting;
using ores::platform::time::datetime;

namespace {

using namespace std::chrono;

domain::cut a_cut(const std::string& id,
                  const std::string& timepoint,
                  const std::string& kind = "EOD",
                  const std::string& scope = "ACME-LONDON",
                  const std::string& centre = "LONDON") {
    domain::cut cut;
    cut.id = boost::uuids::string_generator()(id);
    cut.kind_code = kind;
    cut.party_id = boost::uuids::string_generator()("77777777-7777-4777-8777-777777777777");
    cut.scope_code = scope;
    cut.business_centre_code = centre;
    cut.timepoint = datetime::from_iso8601_utc(timepoint);
    cut.timepoint_timezone = "Europe/London";
    return cut;
}

/** A body a run priced from: taken, and authorised where the kind demands one. */
domain::cut authorised(domain::cut cut) {
    cut.taken_by = "a.reyes";
    cut.taken_at = cut.timepoint;
    cut.requires_authorisation = true;
    cut.approved_by = "m.silva";
    cut.approved_at = cut.timepoint;
    return cut;
}

const auto first = a_cut("11111111-1111-4111-8111-111111111111", "2026-10-06T16:00:00Z");
const auto second = a_cut("22222222-2222-4222-8222-222222222222", "2026-10-07T16:00:00Z");

}

TEST_CASE("a month is a range the caller's clock decides", "[cut_read_service]") {

    auto lg(make_logger(test_suite));
    const auto window = service::cut_window_of("2026-10");
    REQUIRE(window.has_value());

    REQUIRE(datetime::to_iso8601_utc(window->first) == "2026-10-01 00:00:00.000000Z");
    REQUIRE(datetime::to_iso8601_utc(window->second) == "2026-11-01 00:00:00.000000Z");
}

TEST_CASE("a month that ends a year reads into the next one", "[cut_read_service]") {

    auto lg(make_logger(test_suite));
    const auto window = service::cut_window_of("2026-12");
    REQUIRE(window.has_value());

    REQUIRE(datetime::to_iso8601_utc(window->first) == "2026-12-01 00:00:00.000000Z");
    REQUIRE(datetime::to_iso8601_utc(window->second) == "2027-01-01 00:00:00.000000Z");
}

TEST_CASE("a month a person stated wrongly is a refusal, and not an exception",
          "[cut_read_service]") {

    auto lg(make_logger(test_suite));

    // The month is the caller's own input, so the read answers it rather than
    // throwing past the handler that must turn it into a bad request.
    for (const auto& month : {"2026-13", "October", "2026-1", "2026/10", "2026-1x"}) {
        const auto window = service::cut_window_of(month);
        REQUIRE_FALSE(window.has_value());
        REQUIRE(window.error().find("YYYY-MM") != std::string::npos);
    }
}

TEST_CASE("an empty month reads everything the store holds", "[cut_read_service]") {

    auto lg(make_logger(test_suite));
    const auto window = service::cut_window_of("");
    REQUIRE(window.has_value());

    REQUIRE(datetime::to_iso8601_utc(window->first) == "1970-01-01 00:00:00.000000Z");
    REQUIRE(window->second > system_clock::now());
}

TEST_CASE("the body that replaced a cut is the earliest later body a run may price from",
          "[cut_read_service]") {

    auto lg(make_logger(test_suite));
    const auto later =
        authorised(a_cut("33333333-3333-4333-8333-333333333333", "2026-10-08T16:00:00Z"));
    const auto later_still =
        authorised(a_cut("44444444-4444-4444-8444-444444444444", "2026-10-09T16:00:00Z"));

    const auto replacement =
        service::replacement_of(first, {later, later_still, authorised(second)});

    // The nearest later body is the one a person is pointed at, and every body
    // after it replaced that one in turn.
    REQUIRE(replacement == authorised(second).id);
}

TEST_CASE("a cut no later body holds a place for is replaced by nothing", "[cut_read_service]") {

    auto lg(make_logger(test_suite));
    REQUIRE_FALSE(service::replacement_of(authorised(second), {first}));
    REQUIRE_FALSE(service::replacement_of(authorised(second), {}));
}

TEST_CASE("a later body that waits on a person holds no place", "[cut_read_service]") {

    auto lg(make_logger(test_suite));
    // Taken, and the kind demands a signature nobody gave: a run may not price
    // from it, so it replaced nothing.
    auto waiting = a_cut("33333333-3333-4333-8333-333333333333", "2026-10-08T16:00:00Z");
    waiting.taken_by = "a.reyes";
    waiting.taken_at = waiting.timepoint;
    waiting.requires_authorisation = true;

    REQUIRE_FALSE(service::replacement_of(first, {waiting}));
}

TEST_CASE("a later body of another kind, scope or centre holds no place", "[cut_read_service]") {

    auto lg(make_logger(test_suite));
    REQUIRE_FALSE(service::replacement_of(
        first,
        {authorised(
            a_cut("33333333-3333-4333-8333-333333333333", "2026-10-08T16:00:00Z", "FLASH"))}));
    REQUIRE_FALSE(service::replacement_of(first,
                                          {authorised(a_cut("33333333-3333-4333-8333-333333333333",
                                                            "2026-10-08T16:00:00Z",
                                                            "EOD",
                                                            "ACME-NEW-YORK"))}));
    REQUIRE_FALSE(service::replacement_of(first,
                                          {authorised(a_cut("33333333-3333-4333-8333-333333333333",
                                                            "2026-10-08T16:00:00Z",
                                                            "EOD",
                                                            "ACME-LONDON",
                                                            "NEW-YORK"))}));
}

TEST_CASE("a supersession leaves the body's other states as they were", "[cut_read_service]") {

    auto lg(make_logger(test_suite));
    // The rule the list leans on: supersession moves a usable body and leaves
    // every other state alone.
    auto refused = a_cut("33333333-3333-4333-8333-333333333333", "2026-10-05T16:00:00Z");
    refused.taken_by = "a.reyes";
    refused.taken_at = refused.timepoint;
    refused.requires_authorisation = true;
    refused.refused_at = refused.timepoint;

    REQUIRE(service::derive_state(refused, 0, true) == service::cut_state::refused);
    REQUIRE(service::derive_state(authorised(second), 0, true) == service::cut_state::superseded);
    REQUIRE(service::derive_state(authorised(second), 0, false) == service::cut_state::authorised);

    auto in_use = authorised(second);
    in_use.requires_authorisation = false;
    REQUIRE(service::derive_state(in_use, 0, true) == service::cut_state::superseded);
    REQUIRE(service::derive_state(in_use, 0, false) == service::cut_state::in_use);
}
