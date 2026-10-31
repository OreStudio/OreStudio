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
#include "ores.http.core/stream/change_feed.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/headers.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

using namespace ores::http::stream;
using namespace ores::logging;

namespace {

/// A message as a store publishes it: the subject names the event, the headers the
/// tenancy.
ores::nats::message entity_event(std::string subject, std::string tenant, std::string party) {
    ores::nats::message msg;
    msg.subject = std::move(subject);
    if (!tenant.empty())
        msg.headers.emplace(std::string(ores::nats::headers::x_tenant_id), std::move(tenant));
    if (!party.empty())
        msg.headers.emplace(std::string(ores::nats::headers::x_party_id), std::move(party));
    return msg;
}

}

TEST_CASE("a_subject_names_its_component_collection_and_action", "[change_feed]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that an event subject is read from its end";

    const auto address = parse_entity_event_subject("iam.v1.tenant_types_events.created");

    REQUIRE(address.has_value());
    REQUIRE(address->component == "iam");
    // The suffix is the collection's, not the name's.
    REQUIRE(address->collection == "tenant_types");
    REQUIRE(address->action == "created");
}

TEST_CASE("the_deployment_prefix_in_front_of_a_subject_is_ignored", "[change_feed]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a prefixed subject still parses";

    const auto address =
        parse_entity_event_subject("ores.dev.local.trading.v1.fx_spot_trades_events.updated");

    REQUIRE(address.has_value());
    REQUIRE(address->component == "trading");
    REQUIRE(address->collection == "fx_spot_trades");
    REQUIRE(address->action == "updated");
}

TEST_CASE("a_subject_that_is_not_an_entity_event_is_refused", "[change_feed]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the subjects the feed must not record";

    // A command subject, an events collection with no action, and a version that is
    // not one.
    REQUIRE(!parse_entity_event_subject("iam.v1.tenant_types_events"));
    REQUIRE(!parse_entity_event_subject("iam.v1.tenant_types.created"));
    REQUIRE(!parse_entity_event_subject("iam.x1.tenant_types_events.created"));
    REQUIRE(!parse_entity_event_subject("iam.v1._events.created"));
    REQUIRE(!parse_entity_event_subject(""));
}

TEST_CASE("an_event_records_the_collection_its_tenant_and_its_party", "[change_feed]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that an event becomes a change";

    change_coalescer coalescer;
    change_feed feed(coalescer);

    REQUIRE(feed.notify(entity_event("iam.v1.tenant_types_events.created", "acme", "ops")));

    const auto frames = coalescer.drain();
    REQUIRE(frames.size() == 1);
    REQUIRE(frames.front().tenant == "acme");
    REQUIRE(frames.front().party == "ops");
    REQUIRE(frames.front().component == "iam");
    REQUIRE(frames.front().entity == "tenant_types");
}

TEST_CASE("an_event_of_one_collection_from_two_tenants_is_two_changes", "[change_feed]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that tenancy is part of the change";

    change_coalescer coalescer;
    change_feed feed(coalescer);

    feed.notify(entity_event("iam.v1.tenant_types_events.created", "acme", ""));
    feed.notify(entity_event("iam.v1.tenant_types_events.created", "globex", ""));

    const auto frames = coalescer.drain();
    REQUIRE(frames.size() == 2);
    REQUIRE(frames[0].tenant == "acme");
    REQUIRE(frames[1].tenant == "globex");
}

TEST_CASE("an_event_with_no_tenant_records_nothing", "[change_feed]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that an event nobody could be shown is dropped";

    change_coalescer coalescer;
    change_feed feed(coalescer);

    REQUIRE(!feed.notify(entity_event("iam.v1.tenant_types_events.created", "", "ops")));
    REQUIRE(coalescer.pending() == 0);
}

TEST_CASE("a_subject_that_is_not_an_event_records_nothing", "[change_feed]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a stray message is dropped";

    change_coalescer coalescer;
    change_feed feed(coalescer);

    REQUIRE(!feed.notify(entity_event("iam.v1.tenant_type_get_request", "acme", "ops")));
    REQUIRE(coalescer.pending() == 0);
}
