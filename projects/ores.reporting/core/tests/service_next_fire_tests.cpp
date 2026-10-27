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
#include "ores.database/domain/context.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/config/nats_options.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.nats/service/subscription.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.reporting.api/generators/report_definition_generator.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.reporting.core/messaging/report_scheduling_handler.hpp"
#include "ores.reporting.core/repository/report_definition_repository.hpp"
#include "ores.reporting.core/service/next_fire_plan.hpp"
#include "ores.scheduler.api/messaging/scheduling_operations_protocol.hpp"
#include "ores.testing/handler_test_support.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "test_suite.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cstddef>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

// The decision these pin: the next fire time is the scheduler's, joined to a
// definition by the scheduler job the definition holds. A definition with no job
// is run on demand and states no time; a job the scheduler states no time for
// states none either. Neither is a time the reporting service made up.
//
// A scheduler that cannot be reached is a second decision: the read still
// answers, and its result says which of three unlike failures happened, so a
// caller tells "you may not see this" from "the scheduler is down" from "the
// scheduler is slow".

namespace {

const std::string failure_tags("[service][database][nats]");

/**
 * @brief A party of this test's own, with the session scoped to it.
 */
ores::database::context
write_test_party_and_scope_context(ores::testing::scoped_database_helper& h,
                                   ores::utility::generation::generation_context& gen) {
    using ores::refdata::repository::party_repository;
    party_repository parties;
    auto party = ores::refdata::generators::generate_synthetic_party(gen);
    party.change_reason_code = "system.test";
    for (const auto& existing : parties.read_latest(h.context())) {
        if (existing.tenant_id == party.tenant_id) {
            party.parent_party_id = existing.id;
            break;
        }
    }
    parties.write(h.context(), party);
    return h.context().with_party(h.tenant_id(), party.id, {party.id}, h.db_user());
}

/**
 * @brief Writes a definition, scheduled or not, and answers its id.
 */
boost::uuids::uuid write_definition(const ores::database::context& party_ctx,
                                    ores::utility::generation::generation_context& gen,
                                    const std::optional<boost::uuids::uuid>& scheduler_job_id) {
    auto definition = ores::reporting::generators::generate_synthetic_report_definition(gen);
    definition.change_reason_code = "system.test";
    definition.party_id = *party_ctx.party_id();
    definition.scheduler_job_id = scheduler_job_id;
    ores::reporting::repository::report_definition_repository().write(party_ctx, definition);
    return definition.id;
}

std::optional<std::string>
next_fire_of(const std::vector<ores::reporting::messaging::definition_next_fire>& schedules,
             const boost::uuids::uuid& definition_id) {
    for (const auto& schedule : schedules) {
        if (schedule.definition_id == definition_id)
            return schedule.next_fire_at;
    }
    return std::nullopt;
}

}

using ores::testing::scoped_database_helper;

TEST_CASE("a definition's schedule reads the next fire the scheduler states for its job",
          "[service][database]") {
    auto lg(ores::logging::make_logger(test_suite));

    scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = write_test_party_and_scope_context(h, gen);

    // One definition the party scheduled and one it runs on demand. The job id
    // is the definition's own, stated by the test so the join can be read back.
    const auto job_id = gen.generate_uuid();
    const auto scheduled = write_definition(ctx, gen, job_id);
    const auto on_demand = write_definition(ctx, gen, std::nullopt);

    // The definitions are read back the way the read reads them, so the join
    // column is proved to survive the store.
    ores::reporting::repository::report_definition_repository definitions;
    const auto read_back = definitions.read_latest(
        ctx, {boost::uuids::to_string(scheduled), boost::uuids::to_string(on_demand)});
    REQUIRE(read_back.size() == 2);

    const auto job_of =
        [&read_back](const boost::uuids::uuid& definition_id) -> std::optional<boost::uuids::uuid> {
        for (const auto& definition : read_back) {
            if (definition.id == definition_id)
                return definition.scheduler_job_id;
        }
        return std::nullopt;
    };
    REQUIRE(job_of(scheduled).has_value());
    CHECK(*job_of(scheduled) == job_id);
    CHECK_FALSE(job_of(on_demand).has_value());

    // The scheduler states a time for the job it runs, and states none for a
    // paused one. A job no definition holds is nobody's next fire.
    const std::string stated_next_fire("2026-10-18T09:00:00Z");
    const std::vector<ores::scheduler::messaging::job_schedule_status> running_job{
        {.job_definition_id = boost::uuids::to_string(job_id),
         .job_name = "report",
         .schedule_expression = "0 9 * * *",
         .is_active = true,
         .next_fire_at = stated_next_fire},
        {.job_definition_id = boost::uuids::to_string(gen.generate_uuid()),
         .job_name = "another component's job",
         .schedule_expression = "0 10 * * *",
         .is_active = true,
         .next_fire_at = std::string("2026-10-18T10:00:00Z")}};

    const auto schedules = ores::reporting::service::plan_next_fire(read_back, running_job);
    REQUIRE(schedules.size() == 2);
    CHECK(next_fire_of(schedules, scheduled) == stated_next_fire);
    CHECK_FALSE(next_fire_of(schedules, on_demand).has_value());

    // A paused job states no next fire, and the definition that holds it reads
    // none rather than a stale or invented time.
    const std::vector<ores::scheduler::messaging::job_schedule_status> paused_job{
        {.job_definition_id = boost::uuids::to_string(job_id),
         .job_name = "report",
         .schedule_expression = "0 9 * * *",
         .is_active = false,
         .next_fire_at = std::nullopt}};
    const auto paused = ores::reporting::service::plan_next_fire(read_back, paused_job);
    CHECK_FALSE(next_fire_of(paused, scheduled).has_value());
}

namespace {

const std::string_view failure_subject_prefix = "ores.test.bright_faraday.next_fire_failure";

const std::vector<std::string> reader_permissions{"reporting::report_definitions:read"};

/**
 * @brief The next fire read, driven over NATS as a screen drives it.
 *
 * Every subject carries a prefix of this test's own, so the reporting and
 * scheduler services running on the same server, which subscribe the real
 * subjects, never answer here: what answers the scheduler's status subject is
 * whatever this fixture subscribes, and nothing when it subscribes nothing.
 */
struct next_fire_fixture {
    ores::testing::scoped_database_helper db;
    ores::nats::service::client nats;
    ores::testing::handler_test_keys keys;
    ores::nats::service::nats_client svc;
    std::optional<ores::reporting::messaging::report_scheduling_handler> handler;
    std::vector<ores::nats::service::subscription> subs;
    std::string reader_token;
    boost::uuids::uuid definition_id;

    static ores::nats::config::nats_options options() {
        auto opts = ores::testing::make_nats_options();
        opts.subject_prefix = std::string(failure_subject_prefix);
        return opts;
    }

    next_fire_fixture()
        : nats(options()) {
        nats.connect();
        REQUIRE(nats.is_connected());

        auto gen = ores::testing::make_generation_context(db);
        const auto party_ctx = write_test_party_and_scope_context(db, gen);
        const auto definition = write_definition(party_ctx, gen, gen.generate_uuid());
        definition_id = definition;

        reader_token =
            keys.token(db.tenant_id().to_string(), *party_ctx.party_id(), reader_permissions);
        // The reporting service calls the scheduler on its own credential and
        // relays the caller's beside it. What this fixture states is the
        // response the caller reads, so the service credential only has to be a
        // credential.
        svc = ores::nats::service::nats_client(nats, [this](bool) { return reader_token; });
    }

    ~next_fire_fixture() {
        subs.clear();
        handler.reset();
        svc = ores::nats::service::nats_client();
    }

    /**
     * @brief Serves the read under the subject the protocol names.
     */
    void serve_the_read() {
        handler.emplace(nats, db.context(), std::optional(keys.verifier()), svc);
        subs.push_back(nats.queue_subscribe(
            std::string(ores::reporting::messaging::get_next_fire_request::nats_subject),
            "ores.test",
            [this](ores::nats::message msg) { handler->get_next_fire(std::move(msg)); }));
    }

    /**
     * @brief Stands in for a scheduler that refuses every request.
     */
    void refuse_the_scheduler_status() {
        subs.push_back(nats.queue_subscribe(
            std::string(ores::scheduler::messaging::get_scheduler_status_request::nats_subject),
            "ores.test",
            [this](ores::nats::message msg) {
                nats.publish(msg.reply_subject,
                             std::span<const std::byte>{},
                             {{std::string(ores::nats::headers::x_error), "forbidden"}});
            }));
    }

    /**
     * @brief Stands in for a scheduler that is there and never answers.
     */
    void stall_the_scheduler_status() {
        subs.push_back(nats.queue_subscribe(
            std::string(ores::scheduler::messaging::get_scheduler_status_request::nats_subject),
            "ores.test",
            [](ores::nats::message) {}));
    }

    ores::reporting::messaging::get_next_fire_response read() {
        // One case waits out the service's own request deadline, so the caller
        // states a deadline longer than the one it is testing.
        const auto reply = nats.request_sync(
            std::string(ores::reporting::messaging::get_next_fire_request::nats_subject),
            ores::nats::default_wire_codec().encode(
                ores::reporting::messaging::get_next_fire_request{
                    .definition_ids = {boost::uuids::to_string(definition_id)}}),
            {{std::string(ores::nats::headers::authorization),
              std::string(ores::nats::headers::bearer_prefix) + reader_token}},
            std::chrono::seconds(45));
        const auto response =
            ores::testing::decode_reply<ores::reporting::messaging::get_next_fire_response>(reply);
        REQUIRE(response);
        return *response;
    }
};

}

// The three failures the scheduler's status call can meet, each read as the
// caller reads it: which outcome, and which code, so a client can branch without
// parsing a sentence. A refusal is an answer to act on, an absent scheduler is
// an absence to wait out, and a deadline that passed is worth another attempt.

TEST_CASE("a scheduler that refuses reads as a refusal, not an internal error", failure_tags) {
    auto lg(ores::logging::make_logger(test_suite));

    next_fire_fixture f;
    f.refuse_the_scheduler_status();
    f.serve_the_read();

    const auto response = f.read();
    CHECK(response.result.outcome == ores::utility::domain::outcome::denied);
    CHECK(response.result.code == "forbidden");
    // The refusal's own words travel in the message, so a person is told which
    // permission to ask for rather than that the reporting service broke.
    CHECK(response.result.message.find("forbidden") != std::string::npos);
    // The read still answers: a next fire it could not learn is absent, and the
    // definitions the caller asked for are not lost with it.
    CHECK(response.schedules.empty());
}

TEST_CASE("a scheduler nobody serves reads as unavailable", failure_tags) {
    auto lg(ores::logging::make_logger(test_suite));

    next_fire_fixture f;
    f.serve_the_read();

    const auto response = f.read();
    CHECK(response.result.outcome == ores::utility::domain::outcome::unavailable);
    CHECK(response.result.code == "services_unavailable");
    CHECK(response.schedules.empty());
}

TEST_CASE("a scheduler that does not answer reads as a timeout", failure_tags) {
    auto lg(ores::logging::make_logger(test_suite));

    next_fire_fixture f;
    f.stall_the_scheduler_status();
    f.serve_the_read();

    // The read waits out the platform's request deadline, which is what makes
    // this case unlike the two above.
    const auto response = f.read();
    CHECK(response.result.outcome == ores::utility::domain::outcome::unavailable);
    CHECK(response.result.code == "request_timeout");
    CHECK(response.schedules.empty());
}
