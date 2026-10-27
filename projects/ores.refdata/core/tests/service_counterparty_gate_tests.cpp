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
#include "counterparty_proposal_fixtures.hpp"
#include "ores.refdata.api/generators/counterparty_contact_information_generator.hpp"
#include "ores.refdata.api/messaging/counterparty_contact_information_protocol.hpp"
#include "ores.refdata.api/messaging/counterparty_identifier_protocol.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"
#include "ores.refdata.api/messaging/party_counterparty_protocol.hpp"
#include "ores.refdata.core/service/counterparty_contact_information_service.hpp"
#include "ores.refdata.core/service/counterparty_identifier_service.hpp"
#include "ores.refdata.core/service/party_counterparty_service.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>

// The approval policy gates the counterparty, its identifier and the link that
// lets a party trade with it. A write that does not carry out a line of an
// approved request is refused with approval_required, and nothing is written.
// A contact change has no policy row and is written directly. These cases call
// the services the handlers call, as an ordinary caller does.

namespace {

using namespace ores::refdata::tests;
using ores::refdata::service::counterparty_identifier_service;
using ores::refdata::service::counterparty_proposal_service;
using ores::refdata::service::counterparty_service;
using ores::refdata::service::party_counterparty_service;
using ores::utility::domain::outcome;

namespace domain = ores::refdata::domain;
namespace messaging = ores::refdata::messaging;

messaging::put_counterparty_request put_of(const domain::counterparty_change& l) {
    messaging::put_counterparty_request req;
    req.change.write = {.id = l.entity_id,
                        .short_code = l.short_code,
                        .full_name = l.full_name,
                        .transliterated_name = l.transliterated_name,
                        .party_type = l.party_type,
                        .parent_counterparty_id = l.parent_counterparty_id,
                        .status = l.status,
                        .image_id = l.image_id};
    req.change.precondition = creates();
    req.intent = {"system.test", "A gate test"};
    return req;
}

messaging::put_counterparty_identifier_request
put_of(const domain::counterparty_identifier_change& l) {
    messaging::put_counterparty_identifier_request req;
    req.change.write = {.id = l.entity_id,
                        .counterparty_id = l.counterparty_id,
                        .id_scheme = l.id_scheme,
                        .id_value = l.id_value,
                        .description = l.description,
                        .is_authoritative = l.is_authoritative};
    req.change.precondition = creates();
    req.intent = {"system.test", "A gate test"};
    return req;
}

messaging::put_party_counterparty_request put_of(const domain::party_counterparty_change& l) {
    messaging::put_party_counterparty_request req;
    req.change.write = {.party_id = l.party_id, .counterparty_id = l.counterparty_id};
    req.change.precondition = creates();
    req.intent = {"system.test", "A gate test"};
    return req;
}

messaging::delete_party_counterparty_request
removal_of(const domain::party_counterparty_change& l) {
    messaging::delete_party_counterparty_request req;
    req.removal.key = {.party_id = l.party_id, .counterparty_id = l.counterparty_id};
    req.intent = {"system.test", "A gate test"};
    return req;
}

bool linked(const world& w, const domain::party_counterparty_change& l) {
    messaging::get_party_counterparty_request req;
    req.key = {.party_id = l.party_id, .counterparty_id = l.counterparty_id};
    return party_counterparty_service(w.ctx)
        .get_party_counterparty(req)
        .party_counterparty.has_value();
}

}

TEST_CASE("a_counterparty_put_with_no_request_writes_nothing_and_answers_approval_required",
          "[counterparty_gate]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    const auto line = new_counterparty_line(h);

    const auto response = counterparty_service(w.ctx).put_counterparty(put_of(line));

    CHECK(response.result.outcome == outcome::denied);
    CHECK(response.result.code == "approval_required");
    CHECK_FALSE(exists(w, line.entity_id));
}

TEST_CASE("an_identifier_put_with_no_request_writes_nothing_and_answers_approval_required",
          "[counterparty_gate]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    const auto owner = make_live(w, new_counterparty_line(h));
    const auto line = new_identifier_line(h, owner.id);

    const auto response =
        counterparty_identifier_service(w.ctx).put_counterparty_identifier(put_of(line));

    CHECK(response.result.code == "approval_required");
    CHECK_FALSE(counterparty_identifier_service(w.ctx)
                    .get_counterparty_identifier(line.entity_id)
                    .has_value());
}

TEST_CASE("a_link_put_and_a_link_removal_with_no_request_write_nothing", "[counterparty_gate]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    const auto live = make_live(w, new_counterparty_line(h));
    const auto line = new_link_line(h, w.party_id, live.id);
    party_counterparty_service svc(w.ctx);

    CHECK(svc.put_party_counterparty(put_of(line)).result.code == "approval_required");
    CHECK_FALSE(linked(w, line));

    link_live(w, w.party_id, live.id);
    CHECK(svc.delete_party_counterparty(removal_of(line)).result.code == "approval_required");
    CHECK(linked(w, line));
}

TEST_CASE("a_request_that_is_not_yet_approved_opens_nothing", "[counterparty_gate]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    counterparty_lines lines;
    lines.counterparties.push_back(new_counterparty_line(h));
    const auto proposal = counterparty_proposal_service(w.ctx).raise(lines, "Onboard it");
    REQUIRE(proposal.request);
    counterparty_service svc(w.ctx);
    svc.carry_out_request(boost::uuids::to_string(proposal.request->id));

    const auto response = svc.put_counterparty(put_of(lines.counterparties.front()));

    CHECK(response.result.code == "approval_required");
    CHECK_FALSE(exists(w, lines.counterparties.front().entity_id));
}

TEST_CASE("an_approved_request_opens_the_gate_only_for_the_lines_it_holds", "[counterparty_gate]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    counterparty_lines lines;
    lines.counterparties.push_back(new_counterparty_line(h));
    const auto other = new_counterparty_line(h);
    const auto proposal = counterparty_proposal_service(w.ctx).raise(lines, "Onboard it");
    REQUIRE(proposal.request);
    approve_every_part(w, h, *proposal.request);
    counterparty_service svc(w.ctx);
    svc.carry_out_request(boost::uuids::to_string(proposal.request->id));

    CHECK(svc.put_counterparty(put_of(lines.counterparties.front())).result.outcome == outcome::ok);
    CHECK(exists(w, lines.counterparties.front().entity_id));
    CHECK(svc.put_counterparty(put_of(other)).result.code == "approval_required");
    CHECK_FALSE(exists(w, other.entity_id));
}

TEST_CASE("an_approved_request_opens_a_link_put_and_a_link_removal", "[counterparty_gate]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    const auto live = make_live(w, new_counterparty_line(h));
    counterparty_lines lines;
    lines.links.push_back(new_link_line(h, w.party_id, live.id));
    const auto proposal = counterparty_proposal_service(w.ctx).raise(lines, "Let it trade");
    REQUIRE(proposal.request);
    approve_every_part(w, h, *proposal.request);
    party_counterparty_service svc(w.ctx);
    svc.carry_out_request(boost::uuids::to_string(proposal.request->id));

    CHECK(svc.put_party_counterparty(put_of(lines.links.front())).result.outcome == outcome::ok);
    CHECK(linked(w, lines.links.front()));
}

TEST_CASE("a_contact_change_is_written_directly", "[counterparty_gate]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    const auto live = make_live(w, new_counterparty_line(h));
    auto gctx = ores::testing::make_generation_context(h);
    auto contact =
        ores::refdata::generators::generate_synthetic_counterparty_contact_information(gctx);
    messaging::put_counterparty_contact_information_request req;
    req.change.write = {.id = fresh(),
                        .counterparty_id = live.id,
                        .contact_type = contact.contact_type,
                        .street_line_1 = contact.street_line_1,
                        .street_line_2 = contact.street_line_2,
                        .city = contact.city,
                        .state = contact.state,
                        .country_code = contact.country_code,
                        .postal_code = contact.postal_code,
                        .phone = contact.phone,
                        .email = contact.email,
                        .web_page = contact.web_page};
    // A contact is named by its type, which other cases of the shared tenant may
    // hold already, so the write replaces whatever is there.
    req.change.precondition = {precondition_kind::any, std::nullopt};
    req.intent = {"system.test", "A contact"};

    const auto response = ores::refdata::service::counterparty_contact_information_service(w.ctx)
                              .put_counterparty_contact_information(req);

    CHECK(response.result.outcome == outcome::ok);
}

TEST_CASE("a_batch_link_removal_opens_only_for_the_link_the_request_holds", "[counterparty_gate]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    const auto held = make_live(w, new_counterparty_line(h));
    const auto other = make_live(w, new_counterparty_line(h));
    link_live(w, w.party_id, held.id);
    link_live(w, w.party_id, other.id);
    counterparty_lines lines;
    auto removal = new_link_line(h, w.party_id, held.id);
    removal.operation = "delete";
    lines.links.push_back(removal);
    const auto proposal = counterparty_proposal_service(w.ctx).raise(lines, "Stop trading");
    REQUIRE(proposal.request);
    approve_every_part(w, h, *proposal.request);
    party_counterparty_service svc(w.ctx);
    svc.carry_out_request(boost::uuids::to_string(proposal.request->id));
    const auto batch_of = [&](const boost::uuids::uuid& counterparty) {
        messaging::delete_many_party_counterparties_request batch;
        batch.removals.push_back(
            {.key = {.party_id = w.party_id, .counterparty_id = counterparty}});
        batch.intent = {"system.test", "A gate test"};
        return batch;
    };

    // A link line states no version, so a batch, which carries none, matches it.
    // The line names one link, and a batch over another is refused.
    CHECK(svc.delete_many_party_counterparties(batch_of(other.id)).result.code ==
          "approval_required");
    CHECK(linked(w, new_link_line(h, w.party_id, other.id)));
    CHECK(svc.delete_many_party_counterparties(batch_of(held.id)).result.outcome == outcome::ok);
    CHECK_FALSE(linked(w, removal));
}

TEST_CASE("a_link_line_opens_the_gate_once", "[counterparty_gate]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    const auto live = make_live(w, new_counterparty_line(h));
    counterparty_lines lines;
    lines.links.push_back(new_link_line(h, w.party_id, live.id));
    counterparty_proposal_service proposals(w.ctx);
    const auto proposal = proposals.raise(lines, "Let it trade");
    REQUIRE(proposal.request);
    approve_every_part(w, h, *proposal.request);
    const auto id = boost::uuids::to_string(proposal.request->id);
    REQUIRE(proposals.apply(id).applied);
    party_counterparty_service svc(w.ctx);
    svc.carry_out_request(id);

    CHECK(svc.put_party_counterparty(put_of(lines.links.front())).result.code ==
          "approval_required");
}
