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
#include "curve_publish_fixture.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.service/app/curve_analytics_reader.hpp"
#include "ores.marketdata.service/app/curve_publish_gate.hpp"
#include "ores.marketdata.service/app/curve_republish_service.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cmath>
#include <string>

constexpr const char* analytics_tags = "[curve_analytics][database]";

namespace {

using ores::marketdata::messaging::get_curve_analytics_request;
using ores::inbox::service::approval_lifecycle;
using ores::marketdata::service::app::curve_republish_service;
using ores::marketdata::service::app::get_curve_analytics;
using ores::marketdata::service::app::raise_publish_request;
using ores::marketdata::service::app::record_publication;
using ores::marketdata::service::app::verdict_on_publish;
using ores::marketdata::service::tests::seed_person;
using ores::marketdata::service::tests::curve_fixture;

get_curve_analytics_request request_for(const curve_fixture& f) {
    get_curve_analytics_request req;
    req.instrument_id = boost::uuids::to_string(f.config_id);
    return req;
}

}

TEST_CASE("the_analytics_state_the_conventions_and_pair_every_pillar", analytics_tags) {
    curve_fixture f;

    const auto resp = get_curve_analytics(*f.party, request_for(f));

    INFO("message: " << resp.message);
    REQUIRE(resp.success);
    REQUIRE(resp.bootstrapped.empty() == false);
    CHECK(resp.market_pillars.size() == resp.bootstrapped.size());
    CHECK(resp.conventions.pillar_count == static_cast<int>(resp.bootstrapped.size()));
    CHECK(resp.conventions.day_count.empty() == false);
    CHECK(resp.conventions.interpolation.empty() == false);

    // A market pillar and a bootstrapped point for the same tenor are a pair,
    // and both state the same maturity, so a reader can check them together.
    for (std::size_t i = 0; i < resp.bootstrapped.size(); ++i) {
        CHECK(resp.market_pillars[i].tenor == resp.bootstrapped[i].tenor);
        CHECK(resp.market_pillars[i].maturity_date == resp.bootstrapped[i].maturity_date);
        CHECK(resp.bootstrapped[i].discount_factor > 0.0);
    }
}

TEST_CASE("the_zero_rate_is_the_discount_factor_restated", analytics_tags) {
    curve_fixture f;

    const auto resp = get_curve_analytics(*f.party, request_for(f));

    REQUIRE(resp.success);
    for (const auto& point : resp.bootstrapped) {
        REQUIRE(point.year_fraction > 0.0);
        const auto recomputed = -std::log(point.discount_factor) / point.year_fraction;
        CHECK(std::abs(recomputed - point.zero_rate) < 1e-10);
    }
}

TEST_CASE("an_unknown_curve_is_refused_in_words", analytics_tags) {
    curve_fixture f;

    get_curve_analytics_request req;
    req.instrument_id = "11111111-1111-1111-1111-111111111111";
    const auto resp = get_curve_analytics(*f.party, req);

    CHECK(resp.success == false);
    CHECK(resp.message.empty() == false);
}

TEST_CASE("the_verdict_is_the_servers_of_when_the_market_was_written", analytics_tags) {
    curve_fixture f;

    const auto resp = get_curve_analytics(*f.party, request_for(f));

    REQUIRE(resp.success);
    // The fixture's market is dated in the past, so the curve reads as stale: the
    // verdict measures the newest observation, not the instant the read was made.
    CHECK(resp.stale == true);
    for (const auto& pillar : resp.market_pillars)
        CHECK(pillar.as_of != std::chrono::system_clock::time_point{});
}

TEST_CASE("a_comparison_with_no_published_set_draws_no_prior_and_says_so", analytics_tags) {
    curve_fixture f;

    auto req = request_for(f);
    // A day before the fixture's market, where the party published nothing.
    req.compare_at = f.subject().as_of - std::chrono::hours{24};
    const auto resp = get_curve_analytics(*f.party, req);

    INFO("message: " << resp.message);
    REQUIRE(resp.success);
    // Nothing was current then, so there is nothing to compare against: the fields
    // stay absent rather than carrying a zero, which a person would read as a flat day.
    CHECK(resp.compare_at == std::chrono::system_clock::time_point{});
    for (const auto& point : resp.bootstrapped) {
        CHECK(point.prior_zero_rate.has_value() == false);
        CHECK(point.change_bps.has_value() == false);
    }
}

TEST_CASE("the_prior_close_is_the_published_set_that_was_current_then", analytics_tags) {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto decider = seed_person(f.h);

    // Publish the fixture's curve the way a person does: raise, decide, apply, stamp.
    const auto subject = f.subject();
    const auto raised =
        raise_publish_request(maker.ctx, subject, "The desk prices from this curve");
    const auto id = boost::uuids::to_string(raised.id);
    REQUIRE(approval_lifecycle(maker.ctx)
                .decide(id, raised.version, "approve", decider.account.id, "")
                .state_code == "approved");
    const auto verdict = verdict_on_publish(maker.ctx, id, subject);
    REQUIRE(verdict.approved);
    REQUIRE(verdict.record.has_value());
    curve_republish_service::republish(*f.party, subject.bootstrap_config_id, subject.as_of);
    const auto published = record_publication(*f.party, *verdict.record, maker.account.username);
    REQUIRE(published.published_at.has_value());

    auto req = request_for(f);
    req.compare_at = *published.published_at + std::chrono::seconds{1};
    const auto resp = get_curve_analytics(*f.party, req);

    INFO("message: " << resp.message);
    REQUIRE(resp.success);
    // The set was current then, so every pillar states what it held and how far the
    // curve ahead of it has moved.
    CHECK(resp.compare_at != std::chrono::system_clock::time_point{});
    REQUIRE(resp.bootstrapped.empty() == false);
    CHECK(resp.bootstrapped.front().prior_zero_rate.has_value());
    CHECK(resp.bootstrapped.front().change_bps.has_value());
}
