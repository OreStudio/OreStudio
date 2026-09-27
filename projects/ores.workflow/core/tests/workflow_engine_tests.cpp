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
#include "ores.database/service/tenant_context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.workflow.api/messaging/workflow_events.hpp"
#include "ores.workflow.api/service/workflow_definition.hpp"
#include "ores.workflow.api/service/workflow_registry.hpp"
#include "ores.workflow.core/repository/workflow_instance_repository.hpp"
#include "ores.workflow.core/repository/workflow_step_repository.hpp"
#include "ores.workflow.core/service/fsm_state_map.hpp"
#include "ores.workflow.core/service/workflow_engine.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <memory>
#include <optional>
#include <string>
#include <thread>
#include <vector>

// The four engine paths a shell script cannot reach. A script can only start a
// run and watch it, so a start the engine refuses, a completion delivered
// twice, and a recovery pass have no configuration that covers them. Here the
// handlers are called directly, with the real store and the real bus either
// side of them.
//
// The engine is wired the way ores.workflow.service wires it, with one
// difference: the run lives in the test tenant, so a case writes rows nobody
// else reads. The state ids come from the system tenant, where dq seeds them,
// and they carry no foreign key to a machine, so the two halves do not have to
// agree on a tenant.

namespace {

using namespace ores::logging;

const std::string_view test_suite("ores.workflow.tests");
const std::string tags("[engine][integration]");

// Both subjects are test-local: nothing in the product subscribes to them, so
// a command the engine publishes here cannot reach a service.
const std::string step_subject("test.workflow.engine.step");
const std::string compensation_subject("test.workflow.engine.compensate");

using ores::nats::service::client;
using ores::workflow::messaging::start_workflow_message;
using ores::workflow::messaging::step_completed_event;
using ores::workflow::messaging::step_outcome;
using ores::workflow::repository::workflow_instance_repository;
using ores::workflow::repository::workflow_step_repository;
using ores::workflow::service::fsm_state_map;
using ores::workflow::service::load_fsm_states;
using ores::workflow::service::workflow_definition;
using ores::workflow::service::workflow_engine;
using ores::workflow::service::workflow_registry;
using ores::workflow::service::workflow_step_def;

/**
 * @brief The engine over a test tenant, with the bus and store either side.
 */
struct fixture {
    ores::testing::scoped_database_helper h;
    client nats{ores::testing::make_nats_options()};
    std::shared_ptr<workflow_registry> registry = std::make_shared<workflow_registry>();
    std::shared_ptr<workflow_engine> engine;
    fsm_state_map instance_states;
    fsm_state_map step_states;

    fixture() {
        nats.connect();
        // The machine's states are dq's seed data, in the system tenant. The
        // run's rows are this case's own, in the test tenant.
        const auto sys_ctx =
            ores::database::service::tenant_context::with_system_tenant(h.context());
        instance_states = load_fsm_states(sys_ctx, "workflow_instance");
        step_states = load_fsm_states(sys_ctx, "workflow_step");
        engine = std::make_shared<workflow_engine>(nats,
                                                   h.context(),
                                                   registry,
                                                   instance_states,
                                                   step_states,
                                                   // No key in a test process, which the
                                                   // engine documents as "attribute
                                                   // every record to the service
                                                   // account".
                                                   std::nullopt);
    }

    /** @brief The tenant a run started here belongs to. */
    std::string tenant() {
        return h.context().tenant_id().to_string();
    }

    void register_steps(const std::string& type, const std::vector<std::string>& names) {
        workflow_definition def;
        def.type_name = type;
        def.description = "engine fixture";
        def.build_steps = [names](
                              const std::string& request, const std::string&, const std::string&) {
            std::vector<workflow_step_def> steps;
            steps.reserve(names.size());
            for (const auto& name : names) {
                workflow_step_def s;
                s.name = name;
                s.description = name;
                s.command_subject = step_subject;
                s.compensation_subject = compensation_subject;
                s.build_command = [request](const std::string&, const std::vector<std::string>&) {
                    return request;
                };
                s.build_compensation = [](const std::string& command, const std::string&) {
                    return command;
                };
                steps.push_back(std::move(s));
            }
            return steps;
        };
        registry->register_definition(std::move(def));
    }
};

template <typename T>
ores::nats::message as_message(const T& payload) {
    ores::nats::message msg;
    msg.data = ores::nats::default_wire_codec().encode(payload);
    return msg;
}

start_workflow_message
start_for(const std::string& type, const std::string& tenant, const std::string& instance_id) {
    start_workflow_message req;
    req.type = type;
    req.tenant_id = tenant;
    req.request_json = R"({"steps":[]})";
    req.instance_id = instance_id;
    return req;
}

step_completed_event completion_for(const std::string& instance_id,
                                    const std::string& step_id,
                                    step_outcome outcome,
                                    const std::string& error = "") {
    step_completed_event event;
    event.workflow_instance_id = instance_id;
    event.step_id = step_id;
    event.outcome = outcome;
    event.error_message = error;
    return event;
}

/**
 * @brief The commands on @p sub that belong to @p instance_id.
 *
 * Cases share a tenant, so a recovery pass re-dispatches whatever another case
 * left in progress. Counting the subject would count those too, and the count
 * would depend on the order the cases ran in.
 */
std::vector<ores::nats::message> commands_for(ores::nats::service::buffered_subscription& sub,
                                              const std::string& instance_id) {
    const std::string header(ores::workflow::messaging::instance_id_header);
    std::vector<ores::nats::message> mine;
    for (const auto& msg : sub.snapshot()) {
        const auto it = msg.headers.find(header);
        if (it != msg.headers.end() && it->second == instance_id)
            mine.push_back(msg);
    }
    return mine;
}

/**
 * @brief Polls until @p instance_id has @p wanted commands, or time runs out.
 *
 * The engine's publish is asynchronous, so a case that assumed it had already
 * arrived would be asserting the timing rather than the dispatch.
 */
std::vector<ores::nats::message> wait_for_instance(ores::nats::service::buffered_subscription& sub,
                                                   const std::string& instance_id,
                                                   std::size_t wanted,
                                                   std::chrono::milliseconds timeout) {
    const auto deadline = std::chrono::steady_clock::now() + timeout;
    auto mine = commands_for(sub, instance_id);
    while (mine.size() < wanted && std::chrono::steady_clock::now() < deadline) {
        std::this_thread::sleep_for(std::chrono::milliseconds(20));
        mine = commands_for(sub, instance_id);
    }
    return mine;
}

} // namespace

TEST_CASE("workflow_engine starts nothing for a type it does not know", tags) {
    auto lg(make_logger(test_suite));

    fixture f;
    f.register_steps("test_known_workflow", {"one"});
    const auto refused_id = boost::uuids::to_string(boost::uuids::random_generator()());

    auto commands = f.nats.subscribe_buffered(step_subject, 10);
    f.engine->on_start_workflow(
        as_message(start_for("test_no_such_workflow", f.tenant(), refused_id)));

    // An unregistered type is a refusal, not a run: no instance, and no step
    // command for a service to answer.
    workflow_instance_repository instances;
    CHECK(instances.read_latest(f.h.context(), refused_id).empty());
    CHECK(wait_for_instance(commands, refused_id, 1, std::chrono::milliseconds(300)).empty());

    // The control is the same call, harness and tenant with only the type
    // changed, so the two assertions above are about the refusal and not about
    // a start that could not have worked either way.
    const auto accepted_id = boost::uuids::to_string(boost::uuids::random_generator()());
    f.engine->on_start_workflow(
        as_message(start_for("test_known_workflow", f.tenant(), accepted_id)));
    CHECK(instances.read_latest(f.h.context(), accepted_id).size() == 1);
    CHECK(wait_for_instance(commands, accepted_id, 1, std::chrono::seconds(5)).size() == 1);
    BOOST_LOG_SEV(lg, debug) << "Unregistered type created nothing.";
}

TEST_CASE("workflow_engine starts nothing for a definition with no steps", tags) {
    auto lg(make_logger(test_suite));

    fixture f;
    f.register_steps("test_empty_workflow", {});
    f.register_steps("test_one_step_workflow", {"one"});
    const auto empty_id = boost::uuids::to_string(boost::uuids::random_generator()());

    auto commands = f.nats.subscribe_buffered(step_subject, 10);
    f.engine->on_start_workflow(as_message(start_for("test_empty_workflow", f.tenant(), empty_id)));

    // The definition is registered, so the type resolves; it simply declares
    // no work. An instance with no steps could never advance, so the engine
    // must not leave one behind for a caller to wait on.
    workflow_instance_repository instances;
    CHECK(instances.read_latest(f.h.context(), empty_id).empty());
    CHECK(wait_for_instance(commands, empty_id, 1, std::chrono::milliseconds(300)).empty());

    // Same fixture, same tenant, one step declared instead of none.
    const auto accepted_id = boost::uuids::to_string(boost::uuids::random_generator()());
    f.engine->on_start_workflow(
        as_message(start_for("test_one_step_workflow", f.tenant(), accepted_id)));
    CHECK(instances.read_latest(f.h.context(), accepted_id).size() == 1);
    CHECK(wait_for_instance(commands, accepted_id, 1, std::chrono::seconds(5)).size() == 1);
    BOOST_LOG_SEV(lg, debug) << "Empty definition created nothing.";
}

TEST_CASE("workflow_engine advances once when a step completes twice", tags) {
    auto lg(make_logger(test_suite));

    fixture f;
    f.register_steps("test_two_step_workflow", {"one", "two"});
    const auto instance_id = boost::uuids::to_string(boost::uuids::random_generator()());

    auto commands = f.nats.subscribe_buffered(step_subject, 10);
    f.engine->on_start_workflow(
        as_message(start_for("test_two_step_workflow", f.tenant(), instance_id)));
    REQUIRE(wait_for_instance(commands, instance_id, 1, std::chrono::seconds(5)).size() == 1);

    workflow_step_repository steps;
    auto rows = steps.read_latest_by_workflow_id(f.h.context(), instance_id, 0, 100);
    REQUIRE(rows.size() == 1);
    const auto first_step_id = boost::uuids::to_string(rows.front().id);

    // Step one completes, which dispatches step two.
    f.engine->on_step_completed(
        as_message(completion_for(instance_id, first_step_id, step_outcome::completed)));
    REQUIRE(wait_for_instance(commands, instance_id, 2, std::chrono::seconds(5)).size() == 2);

    // The same completion arrives again, as a redelivery would. With two steps
    // declared, a second advance would end the run rather than repeat a step,
    // so the instance's state is what tells the two apart.
    f.engine->on_step_completed(
        as_message(completion_for(instance_id, first_step_id, step_outcome::completed)));

    workflow_instance_repository instances;
    const auto instance = instances.read_latest(f.h.context(), instance_id);
    REQUIRE(instance.size() == 1);
    CHECK(instance.front().state_id == f.instance_states.require("in_progress"));
    CHECK(instance.front().current_step_index == 1);

    // Two step rows, not three: the duplicate must not have dispatched again.
    rows = steps.read_latest_by_workflow_id(f.h.context(), instance_id, 0, 100);
    CHECK(rows.size() == 2);
    CHECK(wait_for_instance(commands, instance_id, 3, std::chrono::milliseconds(300)).size() == 2);
    BOOST_LOG_SEV(lg, debug) << "Duplicate completion advanced nothing.";
}

TEST_CASE("workflow_engine recovery re-dispatches the step that was in progress", tags) {
    auto lg(make_logger(test_suite));

    fixture f;
    f.register_steps("test_recovery_workflow", {"one", "two"});
    const auto instance_id = boost::uuids::to_string(boost::uuids::random_generator()());

    auto commands = f.nats.subscribe_buffered(step_subject, 10);
    f.engine->on_start_workflow(
        as_message(start_for("test_recovery_workflow", f.tenant(), instance_id)));
    const auto first = wait_for_instance(commands, instance_id, 1, std::chrono::seconds(5));
    REQUIRE(first.size() == 1);
    const auto step_id =
        first.front().headers.at(std::string(ores::workflow::messaging::step_id_header));

    // Nothing has answered, which is the state a service restart leaves
    // behind: the instance is in progress and its first step is in progress.
    f.engine->recover_in_progress();

    const auto after = wait_for_instance(commands, instance_id, 2, std::chrono::seconds(5));
    REQUIRE(after.size() == 2);

    // The re-dispatch carries the same step id, because that is the
    // idempotency key a service deduplicates on: a recovery that minted a new
    // id would ask for the work twice.
    const auto redispatched =
        after.back().headers.at(std::string(ores::workflow::messaging::step_id_header));
    CHECK(redispatched == step_id);

    workflow_step_repository steps;
    const auto rows = steps.read_latest_by_workflow_id(f.h.context(), instance_id, 0, 100);
    CHECK(rows.size() == 1);
    BOOST_LOG_SEV(lg, debug) << "Recovery re-dispatched step " << step_id;
}
