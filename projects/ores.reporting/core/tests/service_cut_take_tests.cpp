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
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/generators/business_centre_generator.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/business_centre_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.reporting.core/repository/cut_approval_detail_repository.hpp"
#include "ores.reporting.core/repository/cut_gap_repository.hpp"
#include "ores.reporting.core/repository/cut_kind_repository.hpp"
#include "ores.reporting.core/repository/cut_repository.hpp"
#include "ores.reporting.core/service/cut_state.hpp"
#include "ores.reporting.service/service/cut_take.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "test_suite.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <memory>
#include <string>

// The decision these pin: the take is one act. The body, the gaps it could not
// fill and the approval request it raises land together or not at all, the
// retention and the authorisation basis are the kind's and never the caller's,
// and a body with a gap is kept as a draft that raises nothing.

using namespace ores::reporting;
using ores::platform::time::datetime;

namespace {

/** A party, an account that takes, and the context the take acts in. */
struct world {
    boost::uuids::uuid party_id;
    std::string taker;
    ores::database::context ctx;
};

/**
 * @brief What the store would answer about each part, stated by the test.
 *
 * The assembly reads the components' own operations through this seam, so a
 * test states what they answered without a trading service or a NATS broker.
 */
struct stub_reader final : ores::reporting::service::cut_body_reader {
    ores::reporting::service::cut_body_facts answer;

    [[nodiscard]] ores::reporting::service::cut_body_facts
    read(const ores::reporting::service::cut_body_query&) override {
        return answer;
    }
};

/** Facts a store that answers every part gives, at the demo's own sizes. */
ores::reporting::service::cut_body_facts full_answers() {
    ores::reporting::service::cut_body_facts facts;
    facts.deal_count = 1284;
    facts.market_data_set_count = 7;
    facts.market_data_as_of = "2026-10-06 16:00:00.000000Z";
    facts.reference_data_version = "4 report configuration(s) in force, newest version 7";
    facts.engine_version = "1.8.17.0";
    facts.wrapper_version = "1.0.0";
    return facts;
}

world seed_world(ores::testing::scoped_database_helper& h, const std::string& suffix) {
    auto gen = ores::testing::make_generation_context(h);
    auto& base = h.context();

    const auto taker = "cut-taker-" + suffix;
    auto account = ores::iam::generators::generate_synthetic_account(gen);
    account.username = taker;
    account.account_type = "service";
    account.change_reason_code = "system.test";
    ores::iam::repository::account_repository{}.write(base, account);

    auto party = ores::refdata::generators::generate_synthetic_party(gen);
    party.change_reason_code = "system.test";
    for (const auto& existing : ores::refdata::repository::party_repository{}.read_latest(base)) {
        if (existing.tenant_id == party.tenant_id) {
            party.parent_party_id = existing.id;
            break;
        }
    }
    ores::refdata::repository::party_repository{}.write(base, party);

    // A cut names the rates centre its scope revalues against, and the store
    // checks the code against the business centres the session may see, so the
    // tenant a case writes into needs one.
    auto centre = ores::refdata::generators::generate_synthetic_business_centre(gen);
    centre.code = "GBLO";
    centre.change_reason_code = "system.test";
    ores::refdata::repository::business_centre_repository{}.write(base, centre);

    return {.party_id = party.id,
            .taker = taker,
            .ctx = base.with_party(base.tenant_id(), party.id, {party.id}, taker)};
}

/** A request to take a cut of one kind, fixed a day ago so it is never ahead. */
ores::reporting::messaging::take_cut_request a_take(const std::string& kind_code) {
    return {.kind_code = kind_code,
            .scope_code = "ACME-LONDON",
            .business_centre_code = "GBLO",
            .timepoint =
                datetime::to_iso8601_utc(std::chrono::system_clock::now() - std::chrono::hours(24)),
            .timepoint_timezone = "Europe/London",
            .cut_time_code = "",
            .reason = "The day is closed and the reports wait on the body",
            .preview = false};
}

std::size_t cut_count(const ores::database::context& ctx) {
    return repository::cut_repository{}.read_latest(ctx).size();
}

}

TEST_CASE("a take writes the body with the kind's retention and raises its request", "[cut_take]") {

    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;
    const auto suffix = boost::uuids::to_string(boost::uuids::random_generator{}()).substr(0, 8);
    const auto w = seed_world(h, suffix);

    stub_reader reader;
    reader.answer = full_answers();
    service::cut_take_service take(w.ctx, reader);

    const auto answer = take.take(a_take("EOD"));
    REQUIRE(answer.result.outcome == ores::utility::domain::outcome::ok);
    REQUIRE(answer.cut_id != "");
    REQUIRE(answer.cut_state == "taken");
    REQUIRE(answer.requires_authorisation);
    REQUIRE(answer.approval_request_id != "");
    REQUIRE(answer.gaps.empty());

    // The retention and the basis are the kind's, copied at the take.
    REQUIRE(answer.retention_months == 25);
    REQUIRE(answer.retention_destination_code == "cold");

    const auto cut_id = boost::uuids::string_generator()(answer.cut_id);
    const auto rows = repository::cut_repository{}.read_latest(w.ctx, answer.cut_id);
    REQUIRE(rows.size() == 1);
    const auto& cut = rows.front();
    REQUIRE(cut.party_id == w.party_id);
    REQUIRE(cut.kind_code == "EOD");
    REQUIRE(cut.scope_code == "ACME-LONDON");
    REQUIRE(cut.business_centre_code == "GBLO");
    REQUIRE(cut.retention_months == 25);
    REQUIRE(cut.retention_destination_code == "cold");
    REQUIRE(cut.requires_authorisation);
    REQUIRE_FALSE(cut.output_is_indicative);
    REQUIRE(cut.taken_by == w.taker);
    REQUIRE(cut.taken_at.has_value());
    REQUIRE(cut.take_ordinal == 1);
    REQUIRE(cut.approval_request_id.has_value());
    REQUIRE(boost::uuids::to_string(*cut.approval_request_id) == answer.approval_request_id);

    // The request is the inbox's record of the wait, of the kind the type names.
    ores::inbox::service::approval_lifecycle lifecycle(w.ctx);
    const auto request = lifecycle.request(answer.approval_request_id);
    REQUIRE(request.has_value());
    REQUIRE(request->kind_code == "reporting.eod_cut_authorisation");
    REQUIRE(request->state_code == "waiting");

    // The subject the request is about: the cut, with the counts of its parts.
    repository::cut_approval_detail_repository details;
    const auto detail = details.read_latest_by_request(w.ctx, answer.approval_request_id);
    REQUIRE(detail.has_value());
    REQUIRE(detail->cut_id == cut_id);
    REQUIRE(detail->party_id == w.party_id);
    REQUIRE(detail->deal_count == 1284);
    REQUIRE(detail->market_data_set_count == 7);
    REQUIRE(detail->gap_count == 0);
    REQUIRE_FALSE(detail->applied_at.has_value());
    REQUIRE(detail->applied_by == "");
}

TEST_CASE("a take whose body has a gap writes a draft and raises nothing", "[cut_take]") {

    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;
    const auto suffix = boost::uuids::to_string(boost::uuids::random_generator{}()).substr(0, 8);
    const auto w = seed_world(h, suffix);

    stub_reader reader;
    reader.answer = full_answers();
    reader.answer.deal_count = 0; // the store holds no population at the instant
    service::cut_take_service take(w.ctx, reader);

    const auto answer = take.take(a_take("EOD"));
    REQUIRE(answer.result.outcome == ores::utility::domain::outcome::ok);
    REQUIRE(answer.cut_id != "");
    REQUIRE(answer.cut_state == "incomplete");
    REQUIRE(answer.gaps.size() == 1);
    REQUIRE(answer.gaps.front().part_code == "deal_population");
    REQUIRE(answer.approval_request_id == "");

    const auto rows = repository::cut_repository{}.read_latest(w.ctx, answer.cut_id);
    REQUIRE(rows.size() == 1);
    // Nothing is taken: no taking instant holds and no request names the body.
    REQUIRE_FALSE(rows.front().taken_at.has_value());
    REQUIRE_FALSE(rows.front().approval_request_id.has_value());
    // The retention is still copied, because it is what the draft promises.
    REQUIRE(rows.front().retention_months == 25);

    // The gap is the record a person chases, and it is the whole list.
    repository::cut_gap_repository gaps;
    const auto written =
        gaps.read_latest_by_cut_id(w.ctx, boost::uuids::to_string(rows.front().id), 0, 100);
    REQUIRE(written.size() == 1);
    REQUIRE(written.front().part_code == "deal_population");
    REQUIRE(written.front().reason.find("no deal at the instant") != std::string::npos);
}

TEST_CASE("a take of a kind that requires none is in use when the press lands", "[cut_take]") {

    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;
    const auto suffix = boost::uuids::to_string(boost::uuids::random_generator{}()).substr(0, 8);
    const auto w = seed_world(h, suffix);

    stub_reader reader;
    reader.answer = full_answers();
    service::cut_take_service take(w.ctx, reader);

    // A start of day cut needs no reason: nobody is asked to sign it.
    auto request = a_take("SOD");
    request.reason.clear();
    const auto answer = take.take(request);

    REQUIRE(answer.result.outcome == ores::utility::domain::outcome::ok);
    REQUIRE_FALSE(answer.requires_authorisation);
    REQUIRE(answer.approval_request_id == "");
    REQUIRE(answer.cut_state == "in_use");

    const auto rows = repository::cut_repository{}.read_latest(w.ctx, answer.cut_id);
    REQUIRE(rows.size() == 1);
    REQUIRE(rows.front().taken_at.has_value());
    REQUIRE_FALSE(rows.front().approval_request_id.has_value());
    REQUIRE_FALSE(rows.front().requires_authorisation);
}

TEST_CASE("a preview answers the body and writes nothing", "[cut_take]") {

    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;
    const auto suffix = boost::uuids::to_string(boost::uuids::random_generator{}()).substr(0, 8);
    const auto w = seed_world(h, suffix);
    const auto before = cut_count(w.ctx);

    stub_reader reader;
    reader.answer = full_answers();
    service::cut_take_service take(w.ctx, reader);

    auto request = a_take("EOD");
    request.preview = true;
    const auto answer = take.take(request);

    REQUIRE(answer.result.outcome == ores::utility::domain::outcome::ok);
    REQUIRE(answer.cut_id == "");
    REQUIRE(answer.cut_state == "");
    REQUIRE(answer.parts.size() == 5);
    REQUIRE(answer.gaps.empty());
    REQUIRE(cut_count(w.ctx) == before);
}

TEST_CASE("a take of an instant that has not happened writes nothing", "[cut_take]") {

    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;
    const auto suffix = boost::uuids::to_string(boost::uuids::random_generator{}()).substr(0, 8);
    const auto w = seed_world(h, suffix);
    const auto before = cut_count(w.ctx);

    stub_reader reader;
    reader.answer = full_answers();
    service::cut_take_service take(w.ctx, reader);

    auto request = a_take("EOD");
    request.timepoint =
        datetime::to_iso8601_utc(std::chrono::system_clock::now() + std::chrono::hours(48));
    const auto answer = take.take(request);

    REQUIRE(answer.result.outcome == ores::utility::domain::outcome::invalid);
    REQUIRE(answer.result.message.find("future") != std::string::npos);
    REQUIRE(answer.cut_id == "");
    REQUIRE(cut_count(w.ctx) == before);
}

TEST_CASE("a take of a kind the store does not hold is refused", "[cut_take]") {

    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;
    const auto suffix = boost::uuids::to_string(boost::uuids::random_generator{}()).substr(0, 8);
    const auto w = seed_world(h, suffix);
    const auto before = cut_count(w.ctx);

    stub_reader reader;
    reader.answer = full_answers();
    service::cut_take_service take(w.ctx, reader);

    const auto answer = take.take(a_take("NO-SUCH-KIND"));
    REQUIRE(answer.result.outcome == ores::utility::domain::outcome::missing);
    REQUIRE(cut_count(w.ctx) == before);
}

TEST_CASE("a retake of one instant is the second body of that occasion", "[cut_take]") {

    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;
    const auto suffix = boost::uuids::to_string(boost::uuids::random_generator{}()).substr(0, 8);
    const auto w = seed_world(h, suffix);

    stub_reader reader;
    reader.answer = full_answers();
    service::cut_take_service take(w.ctx, reader);

    // One instant, twice: a retake is a second body of the same occasion.
    const auto request = a_take("EOD");
    const auto first = take.take(request);
    REQUIRE(first.result.outcome == ores::utility::domain::outcome::ok);
    const auto second = take.take(request);
    REQUIRE(second.result.outcome == ores::utility::domain::outcome::ok);

    const auto rows = repository::cut_repository{}.read_latest(w.ctx, second.cut_id);
    REQUIRE(rows.size() == 1);
    REQUIRE(rows.front().take_ordinal == 2);
    // The label states the ordinal, so a person reads which body is meant. The
    // first body's label carries a date and no ordinal, and the second states
    // its own at the end.
    REQUIRE(second.cut_label.ends_with(".2"));
    REQUIRE_FALSE(first.cut_label.ends_with(".2"));
    REQUIRE(first.cut_label == second.cut_label.substr(0, second.cut_label.size() - 2));
}
