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
#include "ores.marketdata.api/messaging/published_curves_operations_protocol.hpp"
#include "ores.marketdata.core/repository/curve_publish_repository.hpp"
#include "ores.marketdata.service/app/curve_republish_service.hpp"
#include "ores.marketdata.service/app/curve_withdrawal.hpp"
#include "ores.marketdata.service/app/published_curve_reader.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <algorithm>
#include <chrono>
#include <optional>
#include <string>

constexpr const char* published_curve_tags = "[curve_publish][published_curves]";

namespace {

using ores::inbox::service::approval_lifecycle;
using ores::marketdata::service::app::curve_republish_service;
using ores::marketdata::service::app::get_published_curve_sets;
using ores::marketdata::service::app::list_published_curves;
using ores::marketdata::service::app::raise_publish_request;
using ores::marketdata::service::app::record_publication;
using ores::marketdata::service::app::verdict_on_publish;
using ores::marketdata::service::app::withdraw_curve;
using namespace ores::marketdata::service::tests;

/**
 * @brief Publishes the fixture's curve once, through the gate, and answers the
 * request that authorised it.
 *
 * The act is the whole chain a person walks: raise, decide, apply, stamp. A test
 * that reads a published set needs the record the chain writes, and the request
 * it names.
 */
struct published_set {
    boost::uuids::uuid publish_id;
    boost::uuids::uuid request_id;
};

published_set publish_one(curve_fixture& f,
                          const person& maker,
                          const person& decider,
                          std::chrono::system_clock::time_point at = as_of) {
    const auto subject = f.subject(at);
    const auto raised = raise_publish_request(maker.ctx, subject, "The desk prices from this curve");
    const auto id = boost::uuids::to_string(raised.id);
    const auto decided =
        approval_lifecycle(maker.ctx).decide(id, raised.version, "approve", decider.account.id, "");
    REQUIRE(decided.state_code == "approved");

    const auto verdict = verdict_on_publish(maker.ctx, id, subject);
    REQUIRE(verdict.approved);
    REQUIRE(verdict.record.has_value());
    curve_republish_service::republish(*f.party, subject.bootstrap_config_id, subject.as_of);
    const auto published = record_publication(*f.party, *verdict.record, maker.account.username);
    return {.publish_id = published.id, .request_id = raised.id};
}

/// The short code a person reads where the set's party is drawn.
std::string party_code_of(const curve_fixture& f, const ores::database::context& ctx) {
    ores::refdata::repository::party_repository repo;
    const auto parties = repo.read_latest(ctx, boost::uuids::to_string(f.party_id));
    return parties.empty() ? std::string{} : parties.front().short_code;
}

/**
 * @brief A list read narrowed to the fixture's own party.
 *
 * A test run shares one tenant, so a suite's rows stand beside the rows another
 * test case wrote. The party is the fixture's own and no other case names it, so
 * every count below is the count this suite made.
 */
ores::marketdata::messaging::list_published_curves_request scoped(const curve_fixture& f) {
    ores::marketdata::messaging::list_published_curves_request request;
    request.party_id = boost::uuids::to_string(f.party_id);
    return request;
}

/// The set the list draws by itself: the sets in use and no others.
ores::marketdata::messaging::list_published_curves_response list_of(
    const curve_fixture& f, const ores::database::context& ctx) {
    return list_published_curves(ctx, scoped(f));
}

}

TEST_CASE("the_list_answers_every_set_with_its_curve_party_publisher_and_state", published_curve_tags) {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto decider = seed_person(f.h);
    const auto published = publish_one(f, maker, decider);
    const auto publish_id = published.publish_id;

    const auto listed = list_of(f, decider.ctx);
    REQUIRE(listed.success);
    REQUIRE(listed.sets.size() == 1);
    CHECK(listed.total == 1);

    const auto& set = listed.sets.front();
    CHECK(set.publish_id == boost::uuids::to_string(publish_id));
    CHECK(set.curve == "USD-SOFR-FIXTURE");
    CHECK(set.party == party_code_of(f, decider.ctx));
    CHECK(set.party_id == boost::uuids::to_string(f.party_id));
    CHECK(set.published_by == maker.account.username);
    CHECK(set.state_code == "published");
    CHECK(set.point_count == 2);
    CHECK(set.as_of == as_of);
    REQUIRE(set.published_at.has_value());
    CHECK(set.request_id == boost::uuids::to_string(published.request_id));
    // The list states how many pillars a set holds and not the pillars: the
    // points are the record page's read.
    CHECK(set.points.empty());
}

TEST_CASE("the_list_hides_a_candidate_and_the_sets_that_ended", published_curve_tags) {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto decider = seed_person(f.h);
    publish_one(f, maker, decider);

    // A second request that nobody has decided, so nobody has published it.
    const auto later = f.subject(as_of + std::chrono::hours{24});
    raise_publish_request(maker.ctx, later, "Tomorrow's curve");

    const auto defaults = list_of(f, decider.ctx);
    REQUIRE(defaults.success);
    CHECK(defaults.total == 1);
    CHECK(std::ranges::none_of(defaults.sets,
                               [](const auto& s) { return s.state_code == "candidate"; }));

    auto asking = scoped(f);
    asking.include_candidates = true;
    const auto with_candidates = list_published_curves(decider.ctx, asking);
    REQUIRE(with_candidates.success);
    REQUIRE(with_candidates.total == 2);

    const auto candidate =
        std::ranges::find_if(with_candidates.sets,
                             [](const auto& s) { return s.state_code == "candidate"; });
    REQUIRE(candidate != with_candidates.sets.end());
    // A candidate names no publisher and no instant, so a reader cannot take it
    // for the set a desk prices from.
    CHECK(candidate->published_by.empty());
    CHECK_FALSE(candidate->published_at.has_value());
    CHECK(candidate->curve == "USD-SOFR-FIXTURE");
    CHECK(candidate->as_of == as_of + std::chrono::hours{24});
}

TEST_CASE("an_ended_set_reads_as_its_last_known_values_and_states_that_it_ended", published_curve_tags) {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto decider = seed_person(f.h);
    const auto publish_id = publish_one(f, maker, decider).publish_id;

    ores::marketdata::messaging::withdraw_curve_request withdrawal;
    withdrawal.publish_id = boost::uuids::to_string(publish_id);
    withdrawal.reason_code = "common.stale_data";
    withdrawal.comment = "The 2Y pillar was stale.";
    const auto ended = withdraw_curve(maker.ctx, withdrawal);
    REQUIRE(ended.success);
    REQUIRE(ended.withdrawn_by == maker.account.username);

    // The set in use is gone from the default view, and the values are not: a
    // reader reaches the ended set on purpose and reads what it held.
    const auto defaults = list_of(f, decider.ctx);
    REQUIRE(defaults.success);
    CHECK(defaults.total == 0);

    auto asking = scoped(f);
    asking.include_ended = true;
    const auto ended_rows = list_published_curves(decider.ctx, asking);
    REQUIRE(ended_rows.success);
    REQUIRE(ended_rows.total == 1);
    const auto& set = ended_rows.sets.front();
    CHECK(set.state_code == "withdrawn");
    CHECK(set.withdrawn_by == maker.account.username);
    CHECK(set.withdrawn_at == ended.withdrawn_at);
    CHECK(set.withdrawn_reason_code == "common.stale_data");
    CHECK(set.point_count == 2);
    // The published facts stay: the set a desk priced from is still readable.
    CHECK(set.published_by == maker.account.username);
    CHECK(set.published_at.has_value());

    // And the values themselves: a withdrawn curve is not an empty series.
    ores::marketdata::messaging::get_published_curve_sets_request one;
    one.series_id = boost::uuids::to_string(f.output_series_id);
    const auto sets = get_published_curve_sets(decider.ctx, one);
    REQUIRE(sets.success);
    REQUIRE(sets.sets.size() == 1);
    CHECK(sets.sets.front().state_code == "withdrawn");
    CHECK(sets.sets.front().points.size() == 2);
    CHECK(sets.sets.front().points.front().value.empty() == false);
}

TEST_CASE("a_second_publish_supersedes_the_set_it_replaced", published_curve_tags) {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto decider = seed_person(f.h);
    publish_one(f, maker, decider);
    const auto successor = publish_one(f, maker, decider);

    auto asking = scoped(f);
    asking.include_ended = true;
    const auto listed = list_published_curves(decider.ctx, asking);
    REQUIRE(listed.success);
    REQUIRE(listed.total == 2);

    const auto live = std::ranges::count_if(listed.sets,
                                            [](const auto& s) { return s.state_code == "published"; });
    // One live set is the promise a reader is given, whatever the series holds.
    CHECK(live == 1);

    const auto superseded =
        std::ranges::find_if(listed.sets,
                             [](const auto& s) { return s.state_code == "superseded"; });
    REQUIRE(superseded != listed.sets.end());
    CHECK(superseded->superseded_by_request_id ==
          boost::uuids::to_string(successor.request_id));
    CHECK(superseded->superseded_as_of == as_of);
    REQUIRE(superseded->superseded_at.has_value());
    // A supersession is not a withdrawal: the set was replaced, not taken away.
    CHECK(superseded->withdrawn_at.has_value() == false);
    CHECK(superseded->withdrawn_by.empty());
}

TEST_CASE("the_sets_of_one_curve_come_back_oldest_first_with_their_points", published_curve_tags) {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto decider = seed_person(f.h);
    publish_one(f, maker, decider);
    publish_one(f, maker, decider);

    ores::marketdata::messaging::get_published_curve_sets_request request;
    request.series_id = boost::uuids::to_string(f.output_series_id);
    request.limit = 5;
    const auto sets = get_published_curve_sets(decider.ctx, request);
    REQUIRE(sets.success);
    REQUIRE(sets.sets.size() == 2);
    CHECK(sets.published_count == 2);

    // Oldest first, so a reader draws them in the order they happened and
    // colours the last one strongest.
    REQUIRE(sets.sets.front().published_at.has_value());
    REQUIRE(sets.sets.back().published_at.has_value());
    CHECK(*sets.sets.front().published_at <= *sets.sets.back().published_at);
    CHECK(sets.sets.front().state_code == "superseded");
    CHECK(sets.sets.back().state_code == "published");

    // Each set holds the pillars the publish wrote, with the coordinate the
    // datum stands at and what its value denotes.
    const auto& points = sets.sets.back().points;
    REQUIRE(points.size() == 2);
    CHECK(points[0].pillar == "12M");
    CHECK(points[1].pillar == "2Y");
    CHECK(points[0].value_kind == "discount");
    CHECK(points[0].value.empty() == false);
    CHECK(points[0].observation_datetime == as_of);

    // The count the caller asks for is counted from the newest.
    request.limit = 1;
    const auto newest_only = get_published_curve_sets(decider.ctx, request);
    REQUIRE(newest_only.success);
    REQUIRE(newest_only.sets.size() == 1);
    CHECK(newest_only.sets.front().state_code == "published");
    CHECK(newest_only.published_count == 2);
}

TEST_CASE("the_list_filters_by_party_and_by_the_requests_a_screen_holds", published_curve_tags) {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto decider = seed_person(f.h);

    const auto published = publish_one(f, maker, decider);
    const auto request_id = boost::uuids::to_string(published.request_id);

    auto by_party = scoped(f);
    const auto party_rows = list_published_curves(decider.ctx, by_party);
    CHECK(party_rows.success);
    CHECK(party_rows.total == 1);

    auto by_other_party = by_party;
    by_other_party.party_id = boost::uuids::to_string(boost::uuids::random_generator{}());
    CHECK(list_published_curves(decider.ctx, by_other_party).total == 0);

    // The queue's read: a page of requests asks for their subjects in one call.
    auto by_request = scoped(f);
    by_request.request_id_one_of = {request_id};
    const auto rows = list_published_curves(decider.ctx, by_request);
    REQUIRE(rows.success);
    REQUIRE(rows.total == 1);
    CHECK(rows.sets.front().request_id == request_id);
}

TEST_CASE("a_withdrawal_refuses_a_set_that_is_not_in_use_and_a_reason_cut_short", published_curve_tags) {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto decider = seed_person(f.h);

    // A candidate is not in use, so there is nothing to take out of use.
    const auto subject = f.subject();
    const auto raised = raise_publish_request(maker.ctx, subject, "Not decided yet");
    const auto candidate = ores::marketdata::repository::curve_publish_repository{}
                               .read_latest_by_request(maker.ctx, boost::uuids::to_string(raised.id));
    REQUIRE(candidate.has_value());

    ores::marketdata::messaging::withdraw_curve_request withdrawal;
    withdrawal.publish_id = boost::uuids::to_string(candidate->id);
    withdrawal.reason_code = "common.stale_data";
    const auto not_in_use = withdraw_curve(maker.ctx, withdrawal);
    CHECK_FALSE(not_in_use.success);
    CHECK(not_in_use.message.find("not been published") != std::string::npos);

    // A published set with no reason is refused: the reason is what the record
    // keeps against the act.
    const auto publish_id = publish_one(f, maker, decider).publish_id;
    auto no_reason = withdrawal;
    no_reason.publish_id = boost::uuids::to_string(publish_id);
    no_reason.reason_code = "";
    const auto refused = withdraw_curve(maker.ctx, no_reason);
    CHECK_FALSE(refused.success);
    CHECK(refused.message.find("why") != std::string::npos);

    // A set the store does not hold is refused by its id.
    auto unknown = withdrawal;
    unknown.publish_id = boost::uuids::to_string(boost::uuids::random_generator{}());
    CHECK_FALSE(withdraw_curve(maker.ctx, unknown).success);

    // The set is still in use: a refused act wrote nothing.
    CHECK(list_of(f, decider.ctx).total == 1);
}

TEST_CASE("a_withdrawal_writes_a_new_version_and_keeps_the_published_facts", published_curve_tags) {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto decider = seed_person(f.h);
    const auto publish_id = publish_one(f, maker, decider).publish_id;

    const auto before = ores::marketdata::repository::curve_publish_repository{}
                            .read_latest(maker.ctx, boost::uuids::to_string(publish_id))
                            .front();
    REQUIRE(before.version == 2);

    ores::marketdata::messaging::withdraw_curve_request withdrawal;
    withdrawal.publish_id = boost::uuids::to_string(publish_id);
    withdrawal.reason_code = "common.rectification";
    withdrawal.comment = "A duplicate publish.";
    const auto ended = withdraw_curve(maker.ctx, withdrawal);
    REQUIRE(ended.success);

    const auto after = ores::marketdata::repository::curve_publish_repository{}
                           .read_latest(maker.ctx, boost::uuids::to_string(publish_id))
                           .front();
    // A withdrawal is a new version of the record and not a deletion of it.
    CHECK(after.version == before.version + 1);
    CHECK(after.published_by == before.published_by);
    CHECK(after.published_at == before.published_at);
    CHECK(after.as_of == before.as_of);
    CHECK(after.withdrawn_by == maker.account.username);
    CHECK(after.withdrawn_reason_code == "common.rectification");
    CHECK(after.change_reason_code == "system.curve_withdrawn");
    CHECK(after.change_commentary == "A duplicate publish.");
    REQUIRE(after.withdrawn_at.has_value());
    CHECK(*after.withdrawn_at == ended.withdrawn_at);

    // A set that has ended cannot be ended again.
    CHECK_FALSE(withdraw_curve(maker.ctx, withdrawal).success);
}

TEST_CASE("a_withdrawal_request_that_nobody_approved_authorises_nothing", published_curve_tags) {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto decider = seed_person(f.h);
    const auto publish_id = publish_one(f, maker, decider).publish_id;

    // The withdrawal kind is seeded, so a person can raise the ask. It is
    // waiting here, and a waiting request authorises no act.
    approval_lifecycle lifecycle(maker.ctx);
    const auto kind = lifecycle.kind(std::string(ores::marketdata::service::app::
                                                     curve_withdrawal_kind_code));
    REQUIRE(kind.has_value());
    const auto raised = lifecycle.raise(*kind, "The 2Y pillar was stale.", maker.account.id);

    ores::marketdata::messaging::withdraw_curve_request withdrawal;
    withdrawal.publish_id = boost::uuids::to_string(publish_id);
    withdrawal.reason_code = "common.stale_data";
    withdrawal.approval_request_id = boost::uuids::to_string(raised.id);

    const auto waiting = withdraw_curve(maker.ctx, withdrawal);
    CHECK_FALSE(waiting.success);
    CHECK(waiting.message.find("waiting") != std::string::npos);

    // Approved, the same request authorises the act and the record names it.
    REQUIRE(lifecycle.decide(boost::uuids::to_string(raised.id),
                             raised.version,
                             "approve",
                             decider.account.id,
                             "")
                .state_code == "approved");
    const auto ended = withdraw_curve(maker.ctx, withdrawal);
    REQUIRE(ended.success);

    const auto record = ores::marketdata::repository::curve_publish_repository{}
                            .read_latest(maker.ctx, boost::uuids::to_string(publish_id))
                            .front();
    REQUIRE(record.withdrawal_request_id.has_value());
    CHECK(*record.withdrawal_request_id == raised.id);

    // One decision authorises one withdrawal: the request cannot end a second
    // set, and the gate's own single-use rule is untouched by this act.
    const auto another = publish_one(f, maker, decider).publish_id;
    auto second = withdrawal;
    second.publish_id = boost::uuids::to_string(another);
    const auto refused = withdraw_curve(maker.ctx, second);
    CHECK_FALSE(refused.success);
    CHECK(refused.message.find("already ended") != std::string::npos);
}

TEST_CASE("a_publish_request_does_not_authorise_a_withdrawal", published_curve_tags) {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto decider = seed_person(f.h);
    const auto publish_id = publish_one(f, maker, decider).publish_id;

    const auto record = ores::marketdata::repository::curve_publish_repository{}
                            .read_latest(maker.ctx, boost::uuids::to_string(publish_id))
                            .front();

    ores::marketdata::messaging::withdraw_curve_request withdrawal;
    withdrawal.publish_id = boost::uuids::to_string(publish_id);
    withdrawal.reason_code = "common.stale_data";
    withdrawal.approval_request_id = boost::uuids::to_string(record.request_id);

    const auto refused = withdraw_curve(maker.ctx, withdrawal);
    CHECK_FALSE(refused.success);
    CHECK(refused.message.find("marketdata.curve_publish") != std::string::npos);
    CHECK(refused.message.find("marketdata.curve_withdrawal") != std::string::npos);
}
