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
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.security/jwt/jwt_claims.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.workflow.api/messaging/workflow_protocol.hpp"
#include "ores.workflow.api/service/workflow_definition.hpp"
#include "ores.workflow.api/service/workflow_registry.hpp"
#include "ores.workflow.core/messaging/workflow_handler.hpp"
#include "ores.workflow.core/messaging/workflow_query_handler.hpp"
#include "ores.workflow.core/repository/workflow_instance_repository.hpp"
#include "ores.workflow.core/repository/workflow_step_repository.hpp"
#include "ores.workflow.core/service/fsm_state_map.hpp"
#include "ores.workflow.core/service/workflow_engine.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <thread>
#include <vector>

// The engine paths a shell script cannot reach. A script can only start a run
// and watch it, so a start the engine refuses, a completion delivered twice, a
// recovery pass, and a run that belongs to a tenant other than the service's
// own have no configuration that covers them. Here the handlers are called
// directly, with the real store and the real bus either side of them.
//
// The engine is wired the way ores.workflow.service wires it, with one
// difference: the run lives in the test tenant, so a case writes rows nobody
// else reads. The state ids come from the system tenant, where dq seeds them,
// and they carry no foreign key to a machine, so the two halves do not have to
// agree on a tenant.
//
// There are two arrangements, picked per case by the fixture. Most cases run
// the engine in the tenant the run belongs to. The cross-tenant cases run it in
// the system tenant, which is what a deployed service holds, so the run and the
// engine's own context are in different tenants and every read crosses the
// boundary.

namespace {

using namespace ores::logging;

const std::string_view test_suite("ores.workflow.tests");
const std::string tags("[engine][integration]");

// Both subjects are test-local: nothing in the product subscribes to them, so
// a command the engine publishes here cannot reach a service.
const std::string step_subject("test.workflow.engine.step");
const std::string compensation_subject("test.workflow.engine.compensate");

// Where the query handler's answer lands in the case that calls it directly.
const std::string reply_subject("test.workflow.step-result.reply");

/**
 * @brief Which tenant the engine holds.
 *
 * A deployed service holds the system tenant and drives runs that belong to
 * whoever asked for it; a case that is not about the boundary keeps the engine
 * in the run's own tenant so the two cannot be confused.
 */
enum class engine_tenant { run, service };

using ores::nats::service::client;
using ores::workflow::messaging::start_workflow_message;
using ores::workflow::messaging::step_completed_event;
using ores::workflow::messaging::step_outcome;
using ores::workflow::repository::workflow_instance_repository;
using ores::workflow::repository::workflow_step_repository;
using ores::workflow::service::failure_policy;
using ores::workflow::service::write_step_timeout;
using ores::workflow::service::fsm_state_map;
using ores::workflow::service::load_fsm_states;
using ores::workflow::service::workflow_definition;
using ores::workflow::service::workflow_engine;
using ores::workflow::service::workflow_registry;
using ores::workflow::service::workflow_step_def;
using ores::workflow::service::workflow_step_results;

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

    /**
     * @param where Which tenant the engine holds. engine_tenant::service is
     * the arrangement a deployed service runs with: the engine holds the system
     * tenant while the run belongs to whoever asked for it.
     */
    explicit fixture(engine_tenant where = engine_tenant::run) {
        nats.connect();
        // The machine's states are dq's seed data, in the system tenant. The
        // run's rows are this case's own, in the test tenant.
        instance_states = load_fsm_states(service_context(), "workflow_instance");
        step_states = load_fsm_states(service_context(), "workflow_step");
        engine = std::make_shared<workflow_engine>(
            nats,
            where == engine_tenant::service ? service_context() : h.context(),
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

    /**
     * @brief The context the deployed service runs with: the system tenant.
     *
     * A read through it sees every tenant's rows, which is what lets a case
     * tell "the engine wrote this run somewhere" from "the engine can find it
     * again".
     */
    ores::database::context service_context() {
        return ores::database::service::tenant_context::with_system_tenant(h.context());
    }

    /** @brief The tenant the service itself runs in: the system tenant. */
    std::string service_tenant_id() {
        return service_context().tenant_id().to_string();
    }

    void register_steps(const std::string& type,
                        const std::vector<std::string>& names,
                        failure_policy on_failure = failure_policy::compensate,
                        std::chrono::seconds timeout = write_step_timeout) {
        workflow_definition def;
        def.type_name = type;
        def.description = "engine fixture";
        def.on_failure = on_failure;
        def.build_steps =
            [names, timeout](const std::string& request, const std::string&, const std::string&) {
                std::vector<workflow_step_def> steps;
                steps.reserve(names.size());
                for (const auto& name : names) {
                    workflow_step_def s;
                    s.name = name;
                    s.description = name;
                    s.command_subject = step_subject;
                    s.timeout = timeout;
                    s.compensation_subject = compensation_subject;
                    s.build_command = [request](const std::string&, const workflow_step_results&) {
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

    /**
     * @brief Register a definition whose steps declare the steps they read.
     *
     * register_steps builds a chain of bare steps, which is what most of these
     * tests want. This one exists so a test can state a chain that reads a step
     * it does not contain, or one that comes after it, and watch the engine
     * refuse it.
     */
    void
    register_chain(const std::string& type,
                   const std::vector<std::pair<std::string, std::vector<std::string>>>& steps) {
        workflow_definition def;
        def.type_name = type;
        def.description = "engine fixture";
        def.on_failure = failure_policy::compensate;
        def.build_steps =
            [steps](const std::string& request, const std::string&, const std::string&) {
                std::vector<workflow_step_def> built;
                built.reserve(steps.size());
                for (const auto& [name, consumes] : steps) {
                    workflow_step_def s;
                    s.name = name;
                    s.description = name;
                    s.command_subject = step_subject;
                    s.timeout = write_step_timeout;
                    s.compensation_subject = compensation_subject;
                    s.consumes = consumes;
                    s.build_command = [request](const std::string&, const workflow_step_results&) {
                        return request;
                    };
                    s.build_compensation = [](const std::string& command, const std::string&) {
                        return command;
                    };
                    built.push_back(std::move(s));
                }
                return built;
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
/// The recovery pass re-dispatches every in-progress instance the database
/// holds, and the database is shared: the other test binaries ctest runs
/// beside this one start their own workflows in their own tenants, and the
/// engine reads them all. A subscription that only wants its own instance's
/// commands therefore still needs room for the others -- a ten-message buffer
/// drops the test's own command once the deployment is busy, which is how this
/// case flapped on a loaded runner while passing on a quiet one.
constexpr std::size_t step_command_buffer = 512;

/// The same breadth makes a pass slower than a quiet machine's, so the wait is
/// a ceiling on a loaded runner rather than an expectation.
constexpr auto recovery_wait = std::chrono::seconds(20);

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

/**
 * @brief Polls for the reply that arrives after @p from messages, or gives up.
 *
 * A handler answers by publishing to the request's reply subject, so a case
 * that calls one directly reads its answer back off the bus.
 */
template <typename Response>
std::optional<Response> await_reply(ores::nats::service::buffered_subscription& sub,
                                    std::size_t from,
                                    std::chrono::milliseconds timeout) {
    const auto deadline = std::chrono::steady_clock::now() + timeout;
    while (std::chrono::steady_clock::now() < deadline) {
        const auto all = sub.snapshot();
        if (all.size() > from) {
            const auto decoded = ores::nats::default_wire_codec().decode<Response>(all.back().data);
            if (decoded)
                return *decoded;
        }
        std::this_thread::sleep_for(std::chrono::milliseconds(20));
    }
    return std::nullopt;
}


/**
 * @brief A request a caller in @p tenant signs, with @p roles as its grants.
 *
 * The handlers build their context from the bearer token, so a test that
 * drives one directly signs the token the way the gateway would.
 */
ores::nats::message signed_request(const std::string& secret,
                                   const std::string& tenant,
                                   const std::vector<std::string>& roles) {
    ores::security::jwt::jwt_claims claims;
    claims.subject = boost::uuids::to_string(boost::uuids::random_generator()());
    claims.username = "workflow_handler_test";
    claims.tenant_id = tenant;
    claims.roles = roles;
    claims.issued_at = std::chrono::system_clock::now();
    claims.expires_at = claims.issued_at + std::chrono::hours(1);
    const auto token =
        ores::security::jwt::jwt_authenticator::create_hs256(secret).create_token(claims);
    REQUIRE(token.has_value());
    ores::nats::message msg;
    msg.headers[std::string(ores::nats::headers::authorization)] =
        std::string(ores::nats::headers::bearer_prefix) + *token;
    msg.reply_subject = reply_subject;
    return msg;
}

const std::string handler_secret = "workflow-handler-test-secret";

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

TEST_CASE("workflow_engine refuses a chain that reads a step it does not contain", tags) {
    auto lg(make_logger(test_suite));

    fixture f;
    // "two" reads "nowhere", which no earlier step of this chain produces. That
    // is a mistake in the definition and not a state of the run, so the engine
    // must refuse it before it publishes anything: the alternative is a run
    // that discovers the typo after it has done work it then has to roll back.
    f.register_chain("test_unknown_input_workflow", {{"one", {}}, {"two", {"nowhere"}}});
    // "two" reads a step that comes after it, which is the same mistake spelled
    // so that the name does exist in the chain.
    f.register_chain("test_forward_input_workflow", {{"one", {"two"}}, {"two", {}}});
    f.register_steps("test_well_formed_workflow", {"one"});

    const auto unknown_id = boost::uuids::to_string(boost::uuids::random_generator()());
    const auto forward_id = boost::uuids::to_string(boost::uuids::random_generator()());
    auto commands = f.nats.subscribe_buffered(step_subject, 10);

    f.engine->on_start_workflow(
        as_message(start_for("test_unknown_input_workflow", f.tenant(), unknown_id)));
    f.engine->on_start_workflow(
        as_message(start_for("test_forward_input_workflow", f.tenant(), forward_id)));

    workflow_instance_repository instances;
    CHECK(instances.read_latest(f.h.context(), unknown_id).empty());
    CHECK(instances.read_latest(f.h.context(), forward_id).empty());
    CHECK(wait_for_instance(commands, unknown_id, 1, std::chrono::milliseconds(300)).empty());
    CHECK(wait_for_instance(commands, forward_id, 1, std::chrono::milliseconds(300)).empty());

    // The control is the same call, harness and tenant with only the
    // declarations changed, so the refusals above are about the declarations
    // and not about a start that could not have worked either way.
    const auto accepted_id = boost::uuids::to_string(boost::uuids::random_generator()());
    f.engine->on_start_workflow(
        as_message(start_for("test_well_formed_workflow", f.tenant(), accepted_id)));
    CHECK(instances.read_latest(f.h.context(), accepted_id).size() == 1);
    CHECK(wait_for_instance(commands, accepted_id, 1, std::chrono::seconds(5)).size() == 1);
    BOOST_LOG_SEV(lg, debug) << "A chain that cannot deliver its inputs created nothing.";
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

TEST_CASE("workflow_engine stores the target a start names", tags) {
    auto lg(make_logger(test_suite));

    fixture f;
    f.register_steps("test_targeted_workflow", {"one"});
    const auto instance_id = boost::uuids::to_string(boost::uuids::random_generator()());
    const auto target = boost::uuids::random_generator()();

    auto req = start_for("test_targeted_workflow", f.tenant(), instance_id);
    req.target_kind = "tenant";
    req.target_id = boost::uuids::to_string(target);
    f.engine->on_start_workflow(as_message(req));

    // The target is what the run acts on, so a reader finds the run by it
    // without parsing the payload.
    workflow_instance_repository instances;
    const auto rows = instances.read_latest(f.h.context(), instance_id);
    REQUIRE(rows.size() == 1);
    CHECK(rows.front().target_kind == "tenant");
    CHECK(rows.front().target_id == target);

    // A start that names no target stores none.
    const auto untargeted_id = boost::uuids::to_string(boost::uuids::random_generator()());
    f.engine->on_start_workflow(
        as_message(start_for("test_targeted_workflow", f.tenant(), untargeted_id)));
    const auto untargeted = instances.read_latest(f.h.context(), untargeted_id);
    REQUIRE(untargeted.size() == 1);
    CHECK(untargeted.front().target_kind.empty());
    CHECK(untargeted.front().target_id == boost::uuids::uuid{});
    BOOST_LOG_SEV(lg, debug) << "Target stored as named.";
}

TEST_CASE("workflow_engine refuses a start that names half a target", tags) {
    auto lg(make_logger(test_suite));

    fixture f;
    f.register_steps("test_half_target_workflow", {"one"});
    workflow_instance_repository instances;

    // A run stored without the target it was given cannot be found by what it
    // acts on, so each half on its own, and an id that is not one, is refused.
    auto kind_only = start_for("test_half_target_workflow",
                               f.tenant(),
                               boost::uuids::to_string(boost::uuids::random_generator()()));
    kind_only.target_kind = "tenant";

    auto id_only = start_for("test_half_target_workflow",
                             f.tenant(),
                             boost::uuids::to_string(boost::uuids::random_generator()()));
    id_only.target_id = boost::uuids::to_string(boost::uuids::random_generator()());

    auto bad_id = start_for("test_half_target_workflow",
                            f.tenant(),
                            boost::uuids::to_string(boost::uuids::random_generator()()));
    bad_id.target_kind = "tenant";
    bad_id.target_id = "not-a-uuid";

    for (const auto& req : {kind_only, id_only, bad_id}) {
        f.engine->on_start_workflow(as_message(req));
        CHECK(instances.read_latest(f.h.context(), req.instance_id).empty());
    }

    // The control is the same start with both halves, so the refusals above are
    // about the half target and not about a start that could not have worked:
    // an engine that does nothing passes the checks above and fails this one.
    auto whole = start_for("test_half_target_workflow",
                           f.tenant(),
                           boost::uuids::to_string(boost::uuids::random_generator()()));
    whole.target_kind = "tenant";
    whole.target_id = boost::uuids::to_string(boost::uuids::random_generator()());
    f.engine->on_start_workflow(as_message(whole));
    CHECK(instances.read_latest(f.h.context(), whole.instance_id).size() == 1);
    BOOST_LOG_SEV(lg, debug) << "Half targets refused.";
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

    auto commands = f.nats.subscribe_buffered(step_subject, step_command_buffer);
    f.engine->on_start_workflow(
        as_message(start_for("test_recovery_workflow", f.tenant(), instance_id)));
    const auto first = wait_for_instance(commands, instance_id, 1, recovery_wait);
    REQUIRE(first.size() == 1);
    const auto step_id =
        first.front().headers.at(std::string(ores::workflow::messaging::step_id_header));

    // Nothing has answered, which is the state a service restart leaves
    // behind: the instance is in progress and its first step is in progress.
    f.engine->recover_in_progress();

    const auto after = wait_for_instance(commands, instance_id, 2, recovery_wait);
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

TEST_CASE("a definition that declares stop keeps its completed steps on a failure", tags) {
    auto lg(make_logger(test_suite));

    fixture f;
    f.register_steps("test_stop_policy_workflow", {"one", "two"}, failure_policy::stop);
    const auto instance_id = boost::uuids::to_string(boost::uuids::random_generator()());

    auto commands = f.nats.subscribe_buffered(step_subject, 10);
    f.engine->on_start_workflow(
        as_message(start_for("test_stop_policy_workflow", f.tenant(), instance_id)));
    REQUIRE(wait_for_instance(commands, instance_id, 1, std::chrono::seconds(5)).size() == 1);

    workflow_step_repository steps;
    workflow_instance_repository instances;
    auto rows = steps.read_latest_by_workflow_id(f.h.context(), instance_id, 0, 100);
    REQUIRE(rows.size() == 1);

    // Step one answers a result, so the run holds work a rollback would undo.
    auto done = completion_for(
        instance_id, boost::uuids::to_string(rows.front().id), step_outcome::completed);
    done.result_json = R"({"kept":"yes"})";
    f.engine->on_step_completed(as_message(done));
    REQUIRE(wait_for_instance(commands, instance_id, 2, std::chrono::seconds(5)).size() == 2);

    rows = steps.read_latest_by_workflow_id(f.h.context(), instance_id, 0, 100);
    REQUIRE(rows.size() == 2);
    const auto second_step_id = boost::uuids::to_string(rows.back().id);

    f.engine->on_step_completed(
        as_message(completion_for(instance_id, second_step_id, step_outcome::failed, "two broke")));

    // The run stops where it is: failed, with the error standing against it.
    auto instance = instances.read_latest(f.h.context(), instance_id);
    REQUIRE(instance.size() == 1);
    CHECK(instance.front().state_id == f.instance_states.require("failed"));
    CHECK(instance.front().error == "two broke");
    CHECK(instance.front().current_step_index == 1);

    // The failed step keeps its error, and the completed step keeps its result.
    rows = steps.read_latest_by_workflow_id(f.h.context(), instance_id, 0, 100);
    REQUIRE(rows.size() == 2);
    CHECK(rows.front().state_id == f.step_states.require("completed"));
    CHECK(rows.front().response_json == R"({"kept": "yes"})");
    CHECK(rows.back().state_id == f.step_states.require("failed"));
    CHECK(rows.back().error == "two broke");

    // A stop is not a rollback: nothing else was dispatched, which is what
    // tells this policy from the default.
    CHECK(wait_for_instance(commands, instance_id, 3, std::chrono::milliseconds(300)).size() == 2);
    BOOST_LOG_SEV(lg, debug) << "Stop policy left the run on its failed step.";
}

TEST_CASE("a retry re-dispatches the failed step under its own identity", tags) {
    auto lg(make_logger(test_suite));

    fixture f;
    f.register_steps("test_retry_workflow", {"one", "two", "three"}, failure_policy::stop);
    const auto instance_id = boost::uuids::to_string(boost::uuids::random_generator()());

    auto commands = f.nats.subscribe_buffered(step_subject, 10);
    f.engine->on_start_workflow(
        as_message(start_for("test_retry_workflow", f.tenant(), instance_id)));
    REQUIRE(wait_for_instance(commands, instance_id, 1, std::chrono::seconds(5)).size() == 1);

    workflow_step_repository steps;
    workflow_instance_repository instances;
    auto rows = steps.read_latest_by_workflow_id(f.h.context(), instance_id, 0, 100);
    REQUIRE(rows.size() == 1);
    auto done = completion_for(
        instance_id, boost::uuids::to_string(rows.front().id), step_outcome::completed);
    done.result_json = R"({"kept":"yes"})";
    f.engine->on_step_completed(as_message(done));
    REQUIRE(wait_for_instance(commands, instance_id, 2, std::chrono::seconds(5)).size() == 2);

    rows = steps.read_latest_by_workflow_id(f.h.context(), instance_id, 0, 100);
    REQUIRE(rows.size() == 2);
    const auto failed_step_id = boost::uuids::to_string(rows.back().id);
    f.engine->on_step_completed(
        as_message(completion_for(instance_id, failed_step_id, step_outcome::failed, "two broke")));

    auto instance = instances.read_latest(f.h.context(), instance_id);
    REQUIRE(instance.size() == 1);
    REQUIRE(instance.front().state_id == f.instance_states.require("failed"));

    // The retry names no step, so the step that failed is the one.
    const auto outcome = f.engine->retry_instance(
        boost::uuids::string_generator{}(instance_id), "", f.h.context().tenant_id());
    CHECK(outcome.resumed);
    CHECK(outcome.step_index == 1);
    CHECK(outcome.step_name == "two");

    // The command goes out again under the step's own id: that identity is the
    // idempotency key the service deduplicates on, so a retry that minted a
    // new one would ask for the work twice.
    const auto after = wait_for_instance(commands, instance_id, 3, std::chrono::seconds(5));
    REQUIRE(after.size() == 3);
    CHECK(after.back().headers.at(std::string(ores::workflow::messaging::step_id_header)) ==
          failed_step_id);

    // The run is running again with no error of its own, and the failure that
    // stood against the step is gone: the retry re-runs it.
    instance = instances.read_latest(f.h.context(), instance_id);
    REQUIRE(instance.size() == 1);
    CHECK(instance.front().state_id == f.instance_states.require("in_progress"));
    CHECK(instance.front().error.empty());
    CHECK(instance.front().current_step_index == 1);

    // The completed step is untouched, result and all, and only one command
    // was published: a retry resumes, it does not repeat the run.
    rows = steps.read_latest_by_workflow_id(f.h.context(), instance_id, 0, 100);
    REQUIRE(rows.size() == 2);
    CHECK(rows.front().state_id == f.step_states.require("completed"));
    CHECK(rows.front().response_json == R"({"kept": "yes"})");
    CHECK(rows.back().state_id == f.step_states.require("in_progress"));
    CHECK(rows.back().error.empty());
    CHECK(wait_for_instance(commands, instance_id, 4, std::chrono::milliseconds(300)).size() == 3);
    BOOST_LOG_SEV(lg, debug) << "Retry re-dispatched step " << failed_step_id;
}

TEST_CASE("a retry refuses a run that has not stopped", tags) {
    auto lg(make_logger(test_suite));

    fixture f;
    f.register_steps("test_retry_refusal_workflow", {"one", "two"}, failure_policy::stop);
    const auto instance_id = boost::uuids::to_string(boost::uuids::random_generator()());

    auto commands = f.nats.subscribe_buffered(step_subject, 10);
    f.engine->on_start_workflow(
        as_message(start_for("test_retry_refusal_workflow", f.tenant(), instance_id)));
    REQUIRE(wait_for_instance(commands, instance_id, 1, std::chrono::seconds(5)).size() == 1);

    // Running, not stopped: there is no failed step to resume.
    const auto refused = f.engine->retry_instance(
        boost::uuids::string_generator{}(instance_id), "", f.h.context().tenant_id());
    CHECK_FALSE(refused.resumed);
    CHECK(refused.reason.find("has not stopped") != std::string::npos);

    // And a run that never started is not the caller's to resume either.
    const auto unknown =
        f.engine->retry_instance(boost::uuids::random_generator{}(), "", f.h.context().tenant_id());
    CHECK_FALSE(unknown.resumed);
    CHECK(unknown.reason == "Workflow instance not found.");
    BOOST_LOG_SEV(lg, debug) << "Retry refused runs that had not stopped.";
}

TEST_CASE("a retry refuses a step the run does not hold", tags) {
    auto lg(make_logger(test_suite));

    fixture f;
    f.register_steps("test_retry_name_workflow", {"one", "two", "three"}, failure_policy::stop);
    const auto instance_id = boost::uuids::to_string(boost::uuids::random_generator()());

    auto commands = f.nats.subscribe_buffered(step_subject, 10);
    f.engine->on_start_workflow(
        as_message(start_for("test_retry_name_workflow", f.tenant(), instance_id)));
    REQUIRE(wait_for_instance(commands, instance_id, 1, std::chrono::seconds(5)).size() == 1);

    workflow_step_repository steps;
    auto rows = steps.read_latest_by_workflow_id(f.h.context(), instance_id, 0, 100);
    REQUIRE(rows.size() == 1);
    f.engine->on_step_completed(as_message(completion_for(
        instance_id, boost::uuids::to_string(rows.front().id), step_outcome::failed, "one broke")));

    // The run stopped at step one, and no step of its chain is named three.
    const auto refused = f.engine->retry_instance(
        boost::uuids::string_generator{}(instance_id), "three", f.h.context().tenant_id());
    CHECK_FALSE(refused.resumed);
    CHECK(refused.reason.find("'three'") != std::string::npos);
    BOOST_LOG_SEV(lg, debug) << "Retry refused a step the run does not hold.";
}

TEST_CASE("a retry refuses a stopped run that has no failed step", tags) {
    auto lg(make_logger(test_suite));

    fixture f;
    f.register_steps("test_retry_no_failed_step_workflow", {"one", "two"}, failure_policy::stop);
    const auto instance_id = boost::uuids::to_string(boost::uuids::random_generator()());

    auto commands = f.nats.subscribe_buffered(step_subject, 10);
    f.engine->on_start_workflow(
        as_message(start_for("test_retry_no_failed_step_workflow", f.tenant(), instance_id)));
    REQUIRE(wait_for_instance(commands, instance_id, 1, std::chrono::seconds(5)).size() == 1);

    // A run can rest in failed with no failed step: a stop that left every
    // step complete. The state is written here rather than provoked, because
    // the engine has no product path to it, and what the case pins is the
    // answer a person sees.
    workflow_step_repository steps;
    workflow_instance_repository instances;
    const auto rows = steps.read_latest_by_workflow_id(f.h.context(), instance_id, 0, 100);
    REQUIRE(rows.size() == 1);
    const auto instances_before = instances.read_latest(f.h.context(), instance_id);
    REQUIRE(instances_before.size() == 1);

    auto stopped = instances_before.front();
    stopped.state_id = f.instance_states.require("failed");
    stopped.error = "The run stopped without failing a step.";
    instances.write(f.h.context(), stopped);

    auto finished = rows.front();
    finished.state_id = f.step_states.require("completed");
    steps.write(f.h.context(), finished);

    const auto refused = f.engine->retry_instance(
        boost::uuids::string_generator{}(instance_id), "", f.h.context().tenant_id());
    CHECK_FALSE(refused.resumed);
    CHECK(refused.reason == "The run holds no failed step to resume from.");
    BOOST_LOG_SEV(lg, debug) << "Retry refused a stopped run with no failed step.";
}

TEST_CASE("a retry is confined to the caller's own tenant", tags) {
    auto lg(make_logger(test_suite));

    // The engine holds the system tenant, as the deployed service does, while
    // the run belongs to the test tenant.
    fixture f(engine_tenant::service);
    f.register_steps("test_retry_tenant_workflow", {"one", "two"}, failure_policy::stop);
    const auto instance_id = boost::uuids::to_string(boost::uuids::random_generator()());
    const auto run_tenant = f.tenant();
    REQUIRE(run_tenant != f.service_tenant_id());

    auto commands = f.nats.subscribe_buffered(step_subject, 10);
    f.engine->on_start_workflow(
        as_message(start_for("test_retry_tenant_workflow", run_tenant, instance_id)));
    REQUIRE(wait_for_instance(commands, instance_id, 1, std::chrono::seconds(5)).size() == 1);

    workflow_step_repository steps;
    const auto rows = steps.read_latest_by_workflow_id(f.service_context(), instance_id, 0, 100);
    REQUIRE(rows.size() == 1);
    f.engine->on_step_completed(as_message(completion_for(
        instance_id, boost::uuids::to_string(rows.front().id), step_outcome::failed, "one broke")));

    // A caller in another tenant is told the run does not exist, not that it
    // exists and is not theirs: the answer must not disclose the run.
    const auto refused = f.engine->retry_instance(
        boost::uuids::string_generator{}(instance_id), "", f.service_context().tenant_id());
    CHECK_FALSE(refused.resumed);
    CHECK(refused.reason == "Workflow instance not found.");

    // A retry that reads, mutates and re-dispatches nothing is what the
    // refusal above must leave behind, so the run's own tenant can still
    // resume it.
    const auto accepted = f.engine->retry_instance(
        boost::uuids::string_generator{}(instance_id), "", f.h.context().tenant_id());
    CHECK(accepted.resumed);
    CHECK(accepted.step_index == 0);
    BOOST_LOG_SEV(lg, debug) << "Retry confined to the caller's tenant.";
}

TEST_CASE("a definition that declares compensate still rolls its work back", tags) {
    auto lg(make_logger(test_suite));

    fixture f;
    // The default, stated: the fixture's steps carry a compensation subject.
    f.register_steps("test_compensate_policy_workflow", {"one", "two"});
    const auto instance_id = boost::uuids::to_string(boost::uuids::random_generator()());

    auto commands = f.nats.subscribe_buffered(step_subject, 10);
    auto compensation = f.nats.subscribe_buffered(compensation_subject, 10);
    f.engine->on_start_workflow(
        as_message(start_for("test_compensate_policy_workflow", f.tenant(), instance_id)));
    REQUIRE(wait_for_instance(commands, instance_id, 1, std::chrono::seconds(5)).size() == 1);

    workflow_step_repository steps;
    workflow_instance_repository instances;
    auto rows = steps.read_latest_by_workflow_id(f.h.context(), instance_id, 0, 100);
    REQUIRE(rows.size() == 1);
    f.engine->on_step_completed(as_message(completion_for(
        instance_id, boost::uuids::to_string(rows.front().id), step_outcome::completed)));
    REQUIRE(wait_for_instance(commands, instance_id, 2, std::chrono::seconds(5)).size() == 2);

    rows = steps.read_latest_by_workflow_id(f.h.context(), instance_id, 0, 100);
    REQUIRE(rows.size() == 2);
    f.engine->on_step_completed(as_message(completion_for(
        instance_id, boost::uuids::to_string(rows.back().id), step_outcome::failed, "two broke")));

    // The rollback is the default policy's answer: the run compensates and the
    // completed step's compensation goes out.
    const auto instance = instances.read_latest(f.h.context(), instance_id);
    REQUIRE(instance.size() == 1);
    CHECK(instance.front().state_id == f.instance_states.require("compensating"));
    CHECK(wait_for_instance(compensation, instance_id, 1, std::chrono::seconds(5)).size() == 1);
    BOOST_LOG_SEV(lg, debug) << "Default policy still compensates.";
}

TEST_CASE("workflow_engine drives a run that belongs to another tenant", tags) {
    auto lg(make_logger(test_suite));

    fixture f(engine_tenant::service);
    f.register_steps("test_cross_tenant_workflow", {"one", "two"});
    const auto instance_id = boost::uuids::to_string(boost::uuids::random_generator()());
    const auto run_tenant = f.tenant();
    // Without this the case would assert nothing: if the test tenant were the
    // system tenant, the engine and the run would share a tenant and every
    // boundary below would be imaginary.
    REQUIRE(run_tenant != f.service_tenant_id());

    auto commands = f.nats.subscribe_buffered(step_subject, 1000);
    f.engine->on_start_workflow(
        as_message(start_for("test_cross_tenant_workflow", run_tenant, instance_id)));
    REQUIRE(wait_for_instance(commands, instance_id, 1, std::chrono::seconds(5)).size() == 1);

    // The engine holds the system tenant, so the rows it wrote belong to the
    // run's tenant. Reading them back through the engine's own context is what
    // every later step of the run depends on: the published marker, the step
    // progress and the completion.
    const auto service_ctx = f.service_context();
    workflow_instance_repository instances;
    workflow_step_repository steps;

    auto instance_rows = instances.read_latest(service_ctx, instance_id);
    REQUIRE(instance_rows.size() == 1);
    CHECK(instance_rows.front().tenant_id == f.h.context().tenant_id());

    auto step_rows = steps.read_latest_by_workflow_id(service_ctx, instance_id, 0, 100);
    REQUIRE(step_rows.size() == 1);

    // A start is done only once the instance, its first step and the step's
    // published marker are all written.
    CHECK(step_rows.front().command_published_at.has_value());

    const auto first_step_id = boost::uuids::to_string(step_rows.front().id);
    f.engine->on_step_completed(
        as_message(completion_for(instance_id, first_step_id, step_outcome::completed)));
    REQUIRE(wait_for_instance(commands, instance_id, 2, std::chrono::seconds(5)).size() == 2);

    instance_rows = instances.read_latest(service_ctx, instance_id);
    REQUIRE(instance_rows.size() == 1);
    CHECK(instance_rows.front().current_step_index == 1);

    step_rows = steps.read_latest_by_workflow_id(service_ctx, instance_id, 0, 100);
    REQUIRE(step_rows.size() == 2);

    // The second step is the last, so completing it ends the run.
    const auto second_step_id = boost::uuids::to_string(step_rows.back().id);
    f.engine->on_step_completed(
        as_message(completion_for(instance_id, second_step_id, step_outcome::completed)));

    instance_rows = instances.read_latest(service_ctx, instance_id);
    REQUIRE(instance_rows.size() == 1);
    CHECK(instance_rows.front().state_id == f.instance_states.require("completed"));

    // Two steps, two commands: running to the end must not mean running twice.
    CHECK(wait_for_instance(commands, instance_id, 3, std::chrono::milliseconds(300)).size() == 2);
    BOOST_LOG_SEV(lg, debug) << "Cross-tenant run completed.";
}

TEST_CASE("workflow_engine recovery finds a run in another tenant", tags) {
    auto lg(make_logger(test_suite));

    fixture f(engine_tenant::service);
    f.register_steps("test_cross_tenant_recovery_workflow", {"one"});
    const auto instance_id = boost::uuids::to_string(boost::uuids::random_generator()());
    REQUIRE(f.tenant() != f.service_tenant_id());

    // A recovery pass re-dispatches every in-progress step it can see, and the
    // buffer drops the oldest message once it is full, so it has to hold the
    // whole pass rather than the few messages this case is about.
    auto commands = f.nats.subscribe_buffered(step_subject, 1000);
    f.engine->on_start_workflow(
        as_message(start_for("test_cross_tenant_recovery_workflow", f.tenant(), instance_id)));
    const auto first = wait_for_instance(commands, instance_id, 1, std::chrono::seconds(5));
    REQUIRE(first.size() == 1);
    const auto step_id =
        first.front().headers.at(std::string(ores::workflow::messaging::step_id_header));

    // Nothing answered the first dispatch, which is the state a service restart
    // leaves behind: the run is in progress and its step is in progress.
    f.engine->recover_in_progress();

    const auto after = wait_for_instance(commands, instance_id, 2, std::chrono::seconds(10));
    REQUIRE(after.size() == 2);

    // The re-dispatch carries the same step id, the idempotency key a service
    // deduplicates on, so the pass re-asked for the same work rather than
    // starting a second one.
    const auto redispatched =
        after.back().headers.at(std::string(ores::workflow::messaging::step_id_header));
    CHECK(redispatched == step_id);

    // The run the pass found is the other tenant's row, read here through the
    // engine's own context.
    workflow_instance_repository instances;
    const auto rows = instances.read_latest(f.service_context(), instance_id);
    REQUIRE(rows.size() == 1);
    CHECK(rows.front().tenant_id == f.h.context().tenant_id());
    BOOST_LOG_SEV(lg, debug) << "Recovery re-dispatched " << step_id << " in another tenant.";
}

TEST_CASE("workflow_query_handler answers for the tenant a request names", tags) {
    auto lg(make_logger(test_suite));

    using ores::workflow::messaging::get_step_result_request;
    using ores::workflow::messaging::get_step_result_response;
    using ores::workflow::messaging::workflow_query_handler;

    fixture f(engine_tenant::service);
    f.register_steps("test_step_result_workflow", {"one", "two"});
    const auto instance_id = boost::uuids::to_string(boost::uuids::random_generator()());

    auto commands = f.nats.subscribe_buffered(step_subject, 10);
    f.engine->on_start_workflow(
        as_message(start_for("test_step_result_workflow", f.tenant(), instance_id)));
    REQUIRE(wait_for_instance(commands, instance_id, 1, std::chrono::seconds(5)).size() == 1);

    // Complete the first step, so the run has a terminal step result to find.
    workflow_step_repository steps;
    const auto first = steps.read_latest_by_workflow_id(f.service_context(), instance_id, 0, 100);
    REQUIRE(first.size() == 1);
    const auto step_id = boost::uuids::to_string(first.front().id);
    f.engine->on_step_completed(
        as_message(completion_for(instance_id, step_id, step_outcome::completed)));

    // The handler holds the service's own context, which reaches every tenant's
    // steps. What confines the answer is the tenant the request names, and this
    // case is the one that holds it to that. The handler never reads the
    // verifier on this path, so an unconfigured one is enough to build it.
    auto handler = std::make_shared<workflow_query_handler>(
        f.nats,
        f.service_context(),
        ores::security::jwt::jwt_authenticator::create_hs256(""),
        f.instance_states,
        f.step_states,
        f.registry);

    auto replies = f.nats.subscribe_buffered(reply_subject, 10);
    const auto ask = [&](const std::string& tenant) {
        get_step_result_request req;
        req.step_id = step_id;
        req.tenant_id = tenant;
        const auto before = replies.size();
        ores::nats::message msg;
        msg.data = ores::nats::default_wire_codec().encode(req);
        msg.reply_subject = reply_subject;
        handler->get_step_result(std::move(msg));
        return await_reply<get_step_result_response>(replies, before, std::chrono::seconds(5));
    };

    // The step belongs to the run's tenant, so naming that tenant finds it.
    const auto mine = ask(f.tenant());
    REQUIRE(mine.has_value());
    CHECK(mine->found);
    CHECK(mine->success);
    CHECK(mine->outcome == step_outcome::completed);

    // Naming another tenant must not disclose the step, and naming none at all
    // must not fall back to the service's own.
    const auto other = ask(f.service_tenant_id());
    REQUIRE(other.has_value());
    CHECK_FALSE(other->found);

    const auto unnamed = ask("");
    REQUIRE(unnamed.has_value());
    CHECK_FALSE(unnamed->found);
    BOOST_LOG_SEV(lg, debug) << "Step result confined to the requested tenant.";
}

TEST_CASE("workflow_query_handler lists a definition that builds its steps from its request",
          tags) {
    auto lg(make_logger(test_suite));

    using ores::workflow::messaging::list_workflow_definitions_request;
    using ores::workflow::messaging::list_workflow_definitions_response;
    using ores::workflow::messaging::workflow_query_handler;

    fixture f(engine_tenant::service);
    f.register_steps("test_listed_fixed_workflow", {"one", "two"});

    // Tenant provisioning builds one step per kind its request orders, so it
    // refuses the empty request the definitions read describes it with. One
    // such definition used to fail the whole read for every caller.
    workflow_definition request_built;
    request_built.type_name = "test_listed_request_built_workflow";
    request_built.description = "steps come from the request";
    request_built.steps_depend_on_request = true;
    request_built.build_steps = [](const std::string&,
                                   const std::string&,
                                   const std::string&) -> std::vector<workflow_step_def> {
        throw std::runtime_error("This definition cannot read an empty request.");
    };
    f.registry->register_definition(std::move(request_built));

    // A builder that throws without saying its steps come from the request is
    // a defect. It is listed with no steps and logged, and the read answers.
    workflow_definition broken;
    broken.type_name = "test_listed_broken_workflow";
    broken.description = "a builder with a defect";
    broken.build_steps = [](const std::string&,
                            const std::string&,
                            const std::string&) -> std::vector<workflow_step_def> {
        throw std::runtime_error("A defect in the builder.");
    };
    f.registry->register_definition(std::move(broken));

    auto handler = std::make_shared<workflow_query_handler>(
        f.nats,
        f.service_context(),
        ores::security::jwt::jwt_authenticator::create_hs256(""),
        f.instance_states,
        f.step_states,
        f.registry);

    auto replies = f.nats.subscribe_buffered(reply_subject, 10);
    const auto before = replies.size();
    ores::nats::message msg;
    msg.data = ores::nats::default_wire_codec().encode(list_workflow_definitions_request{});
    msg.reply_subject = reply_subject;
    handler->list_definitions(std::move(msg));
    const auto answer =
        await_reply<list_workflow_definitions_response>(replies, before, std::chrono::seconds(5));

    REQUIRE(answer.has_value());
    CHECK(answer->success);
    const auto find = [&](const std::string& type) {
        return std::ranges::find_if(answer->definitions,
                                    [&](const auto& d) { return d.type_name == type; });
    };
    const auto fixed = find("test_listed_fixed_workflow");
    REQUIRE(fixed != answer->definitions.end());
    CHECK(fixed->step_count == 2);
    const auto built = find("test_listed_request_built_workflow");
    REQUIRE(built != answer->definitions.end());
    CHECK(built->step_count == 0);
    const auto defective = find("test_listed_broken_workflow");
    REQUIRE(defective != answer->definitions.end());
    CHECK(defective->step_count == 0);
    BOOST_LOG_SEV(lg, debug) << "Definitions listed: " << answer->definitions.size();
}

TEST_CASE("workflow_query_handler filters runs by type and by target", tags) {
    auto lg(make_logger(test_suite));

    using ores::workflow::messaging::list_workflow_instance_summaries_request;
    using ores::workflow::messaging::list_workflow_instance_summaries_response;
    using ores::workflow::messaging::workflow_query_handler;

    fixture f;
    f.register_steps("test_filter_workflow_a", {"one"});
    f.register_steps("test_filter_workflow_b", {"one"});
    const auto first = boost::uuids::to_string(boost::uuids::random_generator()());
    const auto second = boost::uuids::to_string(boost::uuids::random_generator()());

    // Four runs the filters must tell apart: two of type a acting on two
    // tenants, one of type b acting on a party with the first tenant's id, and
    // one of type a acting on nothing.
    const auto start =
        [&](const std::string& type, const std::string& kind, const std::string& target) {
            auto req = start_for(
                type, f.tenant(), boost::uuids::to_string(boost::uuids::random_generator()()));
            req.target_kind = kind;
            req.target_id = target;
            f.engine->on_start_workflow(as_message(req));
            return req.instance_id;
        };
    const auto a_first = start("test_filter_workflow_a", "tenant", first);
    const auto a_second = start("test_filter_workflow_a", "tenant", second);
    const auto b_party = start("test_filter_workflow_b", "party", first);
    const auto a_none = start("test_filter_workflow_a", "", "");

    auto handler = std::make_shared<workflow_query_handler>(
        f.nats,
        f.service_context(),
        ores::security::jwt::jwt_authenticator::create_hs256(handler_secret),
        f.instance_states,
        f.step_states,
        f.registry);

    auto replies = f.nats.subscribe_buffered(reply_subject, 10);
    const auto ask = [&](const list_workflow_instance_summaries_request& req) {
        auto msg = signed_request(handler_secret, f.tenant(), {});
        msg.data = ores::nats::default_wire_codec().encode(req);
        const auto before = replies.size();
        handler->list_instances(std::move(msg));
        const auto answer = await_reply<list_workflow_instance_summaries_response>(
            replies, before, std::chrono::seconds(5));
        REQUIRE(answer.has_value());
        REQUIRE(answer->success);
        std::vector<std::string> ids;
        for (const auto& run : answer->instances)
            ids.push_back(run.id);
        return std::make_pair(ids, answer->instances);
    };
    const auto has = [](const std::vector<std::string>& ids, const std::string& id) {
        return std::ranges::find(ids, id) != ids.end();
    };

    // Kind and identity together name one run.
    list_workflow_instance_summaries_request exact;
    exact.target_kind_filter = "tenant";
    exact.target_id_filter = first;
    const auto [exact_ids, exact_runs] = ask(exact);
    REQUIRE(exact_ids.size() == 1);
    CHECK(exact_ids.front() == a_first);
    CHECK(exact_runs.front().target_kind == "tenant");
    CHECK(exact_runs.front().target_id == first);

    // The identity alone matches across kinds; a run with no target matches none.
    list_workflow_instance_summaries_request by_id;
    by_id.target_id_filter = first;
    const auto [id_ids, id_runs] = ask(by_id);
    CHECK(id_ids.size() == 2);
    CHECK(has(id_ids, a_first));
    CHECK(has(id_ids, b_party));

    // The type alone keeps its own runs, targeted or not, and no other type's.
    list_workflow_instance_summaries_request by_type;
    by_type.type_filter = "test_filter_workflow_a";
    const auto [type_ids, type_runs] = ask(by_type);
    CHECK(has(type_ids, a_first));
    CHECK(has(type_ids, a_second));
    CHECK(has(type_ids, a_none));
    CHECK_FALSE(has(type_ids, b_party));
    BOOST_LOG_SEV(lg, debug) << "Filters checked over " << type_runs.size() << " run(s).";
}

TEST_CASE("workflow_handler retries a run only for a permitted caller in its tenant", tags) {
    auto lg(make_logger(test_suite));

    using ores::workflow::messaging::retry_workflow_instance_request;
    using ores::workflow::messaging::retry_workflow_instance_response;
    using ores::workflow::messaging::workflow_handler;

    fixture f;
    f.register_steps("test_handler_retry_workflow", {"one"}, failure_policy::stop);
    const auto instance_id = boost::uuids::to_string(boost::uuids::random_generator()());

    auto commands = f.nats.subscribe_buffered(step_subject, 10);
    f.engine->on_start_workflow(
        as_message(start_for("test_handler_retry_workflow", f.tenant(), instance_id)));
    REQUIRE(wait_for_instance(commands, instance_id, 1, std::chrono::seconds(5)).size() == 1);

    workflow_step_repository steps;
    const auto rows = steps.read_latest_by_workflow_id(f.h.context(), instance_id, 0, 100);
    REQUIRE(rows.size() == 1);
    f.engine->on_step_completed(as_message(completion_for(
        instance_id, boost::uuids::to_string(rows.front().id), step_outcome::failed, "broke")));

    auto handler = std::make_shared<workflow_handler>(
        f.nats,
        f.service_context(),
        ores::security::jwt::jwt_authenticator::create_hs256(handler_secret),
        f.engine);

    auto replies = f.nats.subscribe_buffered(reply_subject, 10);
    const auto send = [&](const std::string& tenant,
                          const std::vector<std::string>& roles,
                          const std::string& id) {
        auto msg = signed_request(handler_secret, tenant, roles);
        retry_workflow_instance_request req;
        req.workflow_instance_id = id;
        msg.data = ores::nats::default_wire_codec().encode(req);
        const auto before = replies.size();
        handler->retry_instance(std::move(msg));
        const auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(5);
        while (replies.size() <= before && std::chrono::steady_clock::now() < deadline)
            std::this_thread::sleep_for(std::chrono::milliseconds(20));
        REQUIRE(replies.size() > before);
        return replies.snapshot().back();
    };
    const auto decode = [](const ores::nats::message& reply) {
        const auto answer =
            ores::nats::default_wire_codec().decode<retry_workflow_instance_response>(reply.data);
        REQUIRE(answer);
        return *answer;
    };

    // A caller whose grants name other permissions is refused before anything.
    const auto forbidden = send(f.tenant(), {"iam::accounts:read"}, instance_id);
    CHECK(forbidden.headers.at(std::string(ores::nats::headers::x_error)) == "forbidden");

    // An identifier that is not one is answered, not thrown.
    const auto malformed = decode(send(f.tenant(), {"workflow::*"}, "not-a-uuid"));
    CHECK_FALSE(malformed.success);
    CHECK(malformed.message == "Invalid workflow_instance_id.");

    // Another tenant cannot resume this tenant's run.
    const auto elsewhere = decode(send(f.service_tenant_id(), {"workflow::*"}, instance_id));
    CHECK_FALSE(elsewhere.success);

    // The run's own tenant, with the grant, resumes it from the failed step.
    const auto resumed = decode(send(f.tenant(), {"workflow::*"}, instance_id));
    CHECK(resumed.success);
    CHECK(resumed.step_name == "one");
    CHECK(resumed.step_index == 0);
    BOOST_LOG_SEV(lg, debug) << "Retry refused twice, then resumed.";
}

TEST_CASE("workflow repositories list one tenant's runs for a tenant and all for the service",
          tags) {
    auto lg(make_logger(test_suite));

    fixture f(engine_tenant::service);
    f.register_steps("test_list_scope_workflow", {"one"});

    // Two runs: one in the test tenant, one in the tenant the service itself
    // holds. The second is what tells the two read scopes apart, because only a
    // system-tenant session can see it.
    const auto run_tenant = f.tenant();
    const auto service_tenant = f.service_tenant_id();
    REQUIRE(run_tenant != service_tenant);
    const auto run_in_tenant = boost::uuids::to_string(boost::uuids::random_generator()());
    const auto run_in_service = boost::uuids::to_string(boost::uuids::random_generator()());

    auto commands = f.nats.subscribe_buffered(step_subject, 1000);
    f.engine->on_start_workflow(
        as_message(start_for("test_list_scope_workflow", run_tenant, run_in_tenant)));
    f.engine->on_start_workflow(
        as_message(start_for("test_list_scope_workflow", service_tenant, run_in_service)));
    REQUIRE(wait_for_instance(commands, run_in_tenant, 1, std::chrono::seconds(5)).size() == 1);
    REQUIRE(wait_for_instance(commands, run_in_service, 1, std::chrono::seconds(5)).size() == 1);

    const auto has = [](const std::vector<ores::workflow::domain::workflow_instance>& rows,
                        const std::string& id) {
        return std::ranges::any_of(
            rows, [&](const auto& row) { return boost::uuids::to_string(row.id) == id; });
    };

    workflow_instance_repository instances;

    // The service's own context is the platform-wide view: the generic list and
    // count that the generated service exposes reach every tenant's runs. This
    // is the widening the shared read scope buys, and it is what the engine
    // needs; nothing else in the component depends on the older, narrower view.
    const auto all = instances.read_latest(f.service_context(), 0, 1000);
    CHECK(has(all, run_in_tenant));
    CHECK(has(all, run_in_service));

    // A tenant's own context still reaches its runs and only its runs. The
    // workflow tables' policy is own-tenant-or-system-*session*, so a tenant
    // never inherits the service's rows the way a shared reference table's
    // rows would be inherited, and a list cannot show a run twice.
    const auto own = instances.read_latest(f.h.context(), 0, 1000);
    CHECK(has(own, run_in_tenant));
    CHECK_FALSE(has(own, run_in_service));

    // The count follows the list, so the wider view is not list-only.
    CHECK(instances.get_total_instance_count(f.service_context()) >
          instances.get_total_instance_count(f.h.context()));
    BOOST_LOG_SEV(lg, debug) << "List and count follow the reading tenant.";
}

/**
 * @brief Winds a step's dispatch back so its deadline has passed.
 *
 * The rule is about elapsed time, so a case moves the elapsed time rather than
 * sleeping through it: the alternative is a case that takes a minute to assert
 * a rule about a minute.
 */
void backdate_dispatch(ores::database::context ctx,
                       workflow_step_repository& steps,
                       const std::string& instance_id,
                       std::chrono::seconds by) {
    const auto rows = steps.read_latest_by_workflow_id(ctx, instance_id, 0, 100);
    REQUIRE(!rows.empty());
    auto step = rows.front();
    REQUIRE(step.command_published_at.has_value());
    step.command_published_at = *step.command_published_at - by;
    steps.write(ctx, step);
}

TEST_CASE("workflow_engine stops a run whose step outlives its deadline", tags) {
    auto lg(make_logger(test_suite));

    fixture f;
    // The definition would roll a failure back. A deadline stops regardless,
    // because a step that went silent is a step whose work is unknown.
    f.register_steps(
        "test_deadline_workflow", {"one"}, failure_policy::compensate, std::chrono::seconds{60});
    const auto instance_id = boost::uuids::to_string(boost::uuids::random_generator()());

    auto commands = f.nats.subscribe_buffered(step_subject, 10);
    f.engine->on_start_workflow(
        as_message(start_for("test_deadline_workflow", f.tenant(), instance_id)));
    REQUIRE(wait_for_instance(commands, instance_id, 1, std::chrono::seconds(5)).size() == 1);

    workflow_step_repository steps;
    backdate_dispatch(f.h.context(), steps, instance_id, std::chrono::seconds(120));

    CHECK(f.engine->expire_overdue_steps() == 1);

    const auto rows = steps.read_latest_by_workflow_id(f.h.context(), instance_id, 0, 100);
    REQUIRE(rows.size() == 1);
    CHECK(rows.front().state_id == f.step_states.require("failed"));
    // The reason names what never answered and for how long, so a person reads
    // a cause rather than a step that looks like it is still running.
    CHECK(rows.front().error.find("did not answer within 1m") != std::string::npos);
    CHECK(rows.front().error.find(step_subject) != std::string::npos);

    workflow_instance_repository instances;
    const auto instance = instances.read_latest(f.h.context(), instance_id);
    REQUIRE(instance.size() == 1);
    CHECK(instance.front().state_id == f.instance_states.require("failed"));
    CHECK(instance.front().error.find("did not answer within") != std::string::npos);

    // A second pass finds nothing: the step it failed is no longer running, so
    // a pass that ran twice cannot fail the same step twice.
    CHECK(f.engine->expire_overdue_steps() == 0);
    BOOST_LOG_SEV(lg, debug) << "An overdue step stopped its run.";
}

TEST_CASE("workflow_engine keeps a failure when the step answers later", tags) {
    auto lg(make_logger(test_suite));

    fixture f;
    f.register_steps(
        "test_late_answer_workflow", {"one"}, failure_policy::stop, std::chrono::seconds{60});
    const auto instance_id = boost::uuids::to_string(boost::uuids::random_generator()());

    auto commands = f.nats.subscribe_buffered(step_subject, 10);
    f.engine->on_start_workflow(
        as_message(start_for("test_late_answer_workflow", f.tenant(), instance_id)));
    REQUIRE(wait_for_instance(commands, instance_id, 1, std::chrono::seconds(5)).size() == 1);

    workflow_step_repository steps;
    const auto first = steps.read_latest_by_workflow_id(f.h.context(), instance_id, 0, 100);
    REQUIRE(first.size() == 1);
    const auto step_id = boost::uuids::to_string(first.front().id);

    backdate_dispatch(f.h.context(), steps, instance_id, std::chrono::seconds(120));
    REQUIRE(f.engine->expire_overdue_steps() == 1);

    // The service was slow rather than gone, and finishes after the engine has
    // given up on it. The work it did is real, and the run still says failed:
    // the record is the one a person was already shown, and a late report does
    // not un-say it. The retry that follows is what advances the run.
    f.engine->on_step_completed(
        as_message(completion_for(instance_id, step_id, step_outcome::completed)));

    const auto rows = steps.read_latest_by_workflow_id(f.h.context(), instance_id, 0, 100);
    REQUIRE(rows.size() == 1);
    CHECK(rows.front().state_id == f.step_states.require("failed"));

    workflow_instance_repository instances;
    const auto instance = instances.read_latest(f.h.context(), instance_id);
    REQUIRE(instance.size() == 1);
    CHECK(instance.front().state_id == f.instance_states.require("failed"));

    // One retry recovers the run, and it recovers it under the step's own
    // identity: that id is the idempotency key the service deduplicates on, so
    // the service that already did the work replays its outcome instead of
    // doing it twice.
    const auto outcome = f.engine->retry_instance(
        boost::uuids::string_generator{}(instance_id), "", f.h.context().tenant_id());
    CHECK(outcome.resumed);

    const auto after = wait_for_instance(commands, instance_id, 2, std::chrono::seconds(5));
    REQUIRE(after.size() == 2);
    CHECK(after.back().headers.at(std::string(ores::workflow::messaging::step_id_header)) ==
          step_id);

    const auto retried = steps.read_latest_by_workflow_id(f.h.context(), instance_id, 0, 100);
    REQUIRE(retried.size() == 1);
    CHECK(retried.front().state_id == f.step_states.require("in_progress"));
    CHECK(retried.front().error.empty());
    BOOST_LOG_SEV(lg, debug) << "A late answer left the failure standing, and a retry resumed it.";
}

TEST_CASE("workflow_engine starts nothing for a definition whose step states no deadline", tags) {
    auto lg(make_logger(test_suite));

    fixture f;
    // Zero is what a definition that forgot to say leaves behind.
    f.register_steps(
        "test_undeadlined_workflow", {"one"}, failure_policy::stop, std::chrono::seconds{0});
    f.register_steps("test_deadlined_workflow", {"one"});
    const auto refused_id = boost::uuids::to_string(boost::uuids::random_generator()());

    auto commands = f.nats.subscribe_buffered(step_subject, 10);
    f.engine->on_start_workflow(
        as_message(start_for("test_undeadlined_workflow", f.tenant(), refused_id)));

    // A step with no deadline is a command the engine would wait on for ever,
    // so the run is refused rather than left for a person to wait on.
    workflow_instance_repository instances;
    CHECK(instances.read_latest(f.h.context(), refused_id).empty());
    CHECK(wait_for_instance(commands, refused_id, 1, std::chrono::milliseconds(300)).empty());

    // The control is the same fixture and the same call with one number
    // changed, so the assertion above is about the deadline.
    const auto accepted_id = boost::uuids::to_string(boost::uuids::random_generator()());
    f.engine->on_start_workflow(
        as_message(start_for("test_deadlined_workflow", f.tenant(), accepted_id)));
    CHECK(instances.read_latest(f.h.context(), accepted_id).size() == 1);
    CHECK(wait_for_instance(commands, accepted_id, 1, std::chrono::seconds(5)).size() == 1);
    BOOST_LOG_SEV(lg, debug) << "A definition with no deadline started nothing.";
}
