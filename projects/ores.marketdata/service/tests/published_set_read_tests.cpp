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
#include "curve_publish_steps.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.marketdata.core/service/published_set_reader.hpp"
#include "ores.marketdata.service/app/curve_republish_service.hpp"
#include "ores.marketdata.service/app/curve_withdrawal.hpp"
#include "ores.platform/time/datetime.hpp"
#include "test_suite.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <vector>

using ores::inbox::service::approval_lifecycle;
using ores::marketdata::messaging::get_latest_published_sets_request;
using ores::marketdata::messaging::get_latest_published_sets_response;
using ores::marketdata::service::published_set_reader;
using ores::marketdata::service::app::curve_republish_service;
using ores::marketdata::service::tests::raise_publish_request;
using ores::marketdata::service::tests::record_publication;
using ores::marketdata::service::tests::verdict_on_publish;
using ores::marketdata::service::app::withdraw_curve;
using namespace ores::marketdata::service::tests;
using ores::platform::time::datetime;
using ores::utility::domain::outcome;

namespace {

constexpr const char* read_tags = "[curve_publish][published_set_read]";

/** Publishes the fixture's curve once, through the gate, as a person would. */
void publish_one(curve_fixture& f, const person& maker, const person& decider) {
    const auto subject = f.subject(as_of);
    const auto raised =
        raise_publish_request(maker.ctx, subject, "The desk prices from this curve");
    const auto id = boost::uuids::to_string(raised.id);
    const auto decided =
        approval_lifecycle(maker.ctx).decide(id, raised.version, "approve", decider.account.id, "");
    REQUIRE(decided.state_code == "approved");
    const auto verdict = verdict_on_publish(maker.ctx, id, subject);
    REQUIRE(verdict.approved);
    REQUIRE(verdict.record.has_value());
    const auto built =
        curve_republish_service::republish(*f.party, subject.bootstrap_config_id, subject.as_of);
    REQUIRE(built.has_value());
    record_publication(*f.party, *verdict.record, maker.account.username);
}

get_latest_published_sets_request asking_for(const std::vector<boost::uuids::uuid>& series_ids) {
    get_latest_published_sets_request request;
    request.series_ids = series_ids;
    return request;
}

}

TEST_CASE("the_latest_publish_read_answers_a_curves_newest_published_set", read_tags) {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto decider = seed_person(f.h);
    publish_one(f, maker, decider);

    const auto answer = published_set_reader::latest(decider.ctx, asking_for({f.output_series_id}));
    REQUIRE(answer.result.outcome == outcome::ok);
    REQUIRE(answer.series_ids.size() == 1);
    CHECK(answer.series_ids.front() == f.output_series_id);
    REQUIRE(answer.state_codes.size() == 1);
    CHECK(answer.state_codes.front() == "published");
    REQUIRE(answer.as_ofs.size() == 1);
    CHECK(datetime::from_db_string(answer.as_ofs.front()) == as_of);
}

TEST_CASE("a_candidate_is_not_the_set_a_reader_reads_as_published", read_tags) {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto decider = seed_person(f.h);
    publish_one(f, maker, decider);

    // A later request nobody decided: its record holds no published instant, so
    // the newest set a reader prices from is still the one that was applied.
    raise_publish_request(maker.ctx, f.subject(as_of + std::chrono::hours{24}), "Tomorrow's curve");

    const auto answer = published_set_reader::latest(decider.ctx, asking_for({f.output_series_id}));
    REQUIRE(answer.result.outcome == outcome::ok);
    REQUIRE(answer.series_ids.size() == 1);
    CHECK(answer.state_codes.front() == "published");
    CHECK(datetime::from_db_string(answer.as_ofs.front()) == as_of);
}

TEST_CASE("a_withdrawn_set_answers_that_it_is_withdrawn", read_tags) {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto decider = seed_person(f.h);
    const auto subject = f.subject(as_of);
    const auto raised =
        raise_publish_request(maker.ctx, subject, "The desk prices from this curve");
    const auto id = boost::uuids::to_string(raised.id);
    const auto decided =
        approval_lifecycle(maker.ctx).decide(id, raised.version, "approve", decider.account.id, "");
    REQUIRE(decided.state_code == "approved");
    const auto verdict = verdict_on_publish(maker.ctx, id, subject);
    REQUIRE(verdict.approved);
    REQUIRE(verdict.record.has_value());
    const auto built =
        curve_republish_service::republish(*f.party, subject.bootstrap_config_id, subject.as_of);
    REQUIRE(built.has_value());
    const auto published = record_publication(*f.party, *verdict.record, maker.account.username);

    ores::marketdata::messaging::withdraw_curve_request withdrawal;
    withdrawal.publish_id = boost::uuids::to_string(published.id);
    withdrawal.reason_code = "common.stale_data";
    withdrawal.comment = "The 2Y pillar was stale.";
    REQUIRE(withdraw_curve(maker.ctx, withdrawal).success);

    const auto answer = published_set_reader::latest(decider.ctx, asking_for({f.output_series_id}));
    REQUIRE(answer.result.outcome == outcome::ok);
    REQUIRE(answer.series_ids.size() == 1);
    CHECK(answer.state_codes.front() == "withdrawn");
    CHECK(datetime::from_db_string(answer.as_ofs.front()) == as_of);
}

TEST_CASE("a_series_that_never_published_is_not_answered", read_tags) {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto decider = seed_person(f.h);
    publish_one(f, maker, decider);

    const auto unknown = boost::uuids::random_generator{}();
    const auto answer =
        published_set_reader::latest(decider.ctx, asking_for({f.output_series_id, unknown}));
    REQUIRE(answer.result.outcome == outcome::ok);
    REQUIRE(answer.series_ids.size() == 1);
    CHECK(answer.series_ids.front() == f.output_series_id);
}

TEST_CASE("a_batch_answers_each_series_once_however_often_it_is_named", read_tags) {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto decider = seed_person(f.h);
    publish_one(f, maker, decider);

    const auto answer = published_set_reader::latest(
        decider.ctx, asking_for({f.output_series_id, f.output_series_id}));
    REQUIRE(answer.result.outcome == outcome::ok);
    REQUIRE(answer.series_ids.size() == 1);
    CHECK(answer.as_ofs.size() == 1);
    CHECK(answer.state_codes.size() == 1);
}

TEST_CASE("the_reader_refuses_a_batch_over_its_ceiling", read_tags) {
    curve_fixture f;
    const auto decider = seed_person(f.h);

    get_latest_published_sets_request request;
    request.series_ids.resize(published_set_reader::max_batch + 1, boost::uuids::nil_uuid());
    const auto answer = published_set_reader::latest(decider.ctx, request);
    CHECK(answer.result.outcome == outcome::invalid);
    CHECK_FALSE(answer.result.message.empty());
    CHECK(answer.series_ids.empty());
}
