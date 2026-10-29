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
#include "ores.workflow.core/service/workflow_engine.hpp"
#include "ores.eventing.api/domain/entity_change_event.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.workflow.api/domain/workflow_plan_dependency.hpp"
#include "ores.workflow.api/domain/workflow_plan_step.hpp"
#include "ores.workflow.api/messaging/workflow_claim_messages.hpp"
#include "ores.workflow.api/messaging/workflow_protocol.hpp"
#include "ores.workflow.core/service/workflow_actor.hpp"
#include "ores.workflow.core/service/workflow_graph.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <cstddef>
#include <format>
#include <ranges>
#include <rfl/json.hpp>
#include <set>
#include <span>
#include <string>
#include <unordered_map>

namespace ores::workflow::service {

namespace {

/**
 * @brief The deadline each step of a run was started with, by step name.
 *
 * The run is the authority on this rather than the definition: a run started
 * under a longer deadline keeps it, however the definition changes afterwards.
 * A run whose chain cannot be read yields no deadlines, and the expiry pass
 * leaves such a run alone rather than guessing at one -- a run it cannot
 * reason about is a run it must not kill.
 */
std::unordered_map<std::string, std::chrono::seconds>
deadlines_of(const std::vector<domain::workflow_plan_step>& chain) {
    std::unordered_map<std::string, std::chrono::seconds> deadlines;
    for (const auto& step : chain)
        if (step.timeout_seconds > 0)
            deadlines.emplace(step.name, std::chrono::seconds{step.timeout_seconds});
    return deadlines;
}

/**
 * @brief The reason a built step list cannot be run, or nothing when it can.
 *
 * A step with no deadline is a command the engine would wait on for ever, and
 * no default is right for it: the number depends on what the step does. The
 * refusal is here rather than at registration because a definition builds its
 * steps per run, so this is the first place a step exists at all.
 */
std::optional<std::string> undeclared_deadline(const std::vector<workflow_step_def>& steps) {
    for (const auto& step : steps)
        if (step.timeout.count() <= 0)
            return "The definition built step '" + step.name +
                   "' with no deadline, so the engine would wait on it for ever.";
    return std::nullopt;
}

/**
 * @brief The chain a run was started with, as the graph.
 *
 * The graph is drawn from the rows the run started with rather than from the
 * definition as it stands now, for the same reason the deadline is carried: a
 * definition edited after a run started must not change what that run was
 * waiting for, and a run recovered after a restart must be judged by the chain
 * it was actually given.
 *
 * An edge names its steps by position. A position no step has cannot be read
 * as a name, so it is carried as one no step produces and the graph refuses
 * the chain: a store that lost a step is a chain this engine must not guess
 * at.
 */
std::vector<workflow_node>
nodes_of_chain(const std::vector<domain::workflow_plan_step>& chain,
               const std::vector<domain::workflow_plan_dependency>& edges) {
    std::unordered_map<int, std::string> name_at;
    for (const auto& step : chain)
        name_at.emplace(step.step_index, step.name);

    auto ordered = chain;
    std::ranges::sort(ordered, {}, &domain::workflow_plan_step::step_index);

    std::unordered_map<std::string, std::size_t> position;
    std::vector<workflow_node> nodes;
    nodes.reserve(ordered.size());
    for (const auto& step : ordered) {
        position.emplace(step.name, nodes.size());
        nodes.push_back(workflow_node{.name = step.name, .consumes = {}});
    }

    const auto name_of = [&name_at](int index) {
        const auto found = name_at.find(index);
        return found == name_at.end() ? "#" + std::to_string(index) : found->second;
    };

    for (const auto& edge : edges) {
        const auto consumer = position.find(name_of(edge.consumer_step_index));
        if (consumer == position.end())
            continue;
        nodes[consumer->second].consumes.push_back(name_of(edge.producer_step_index));
    }
    return nodes;
}

/**
 * @brief The chain a run is about to take, as rows of its own.
 *
 * Each row takes the run's tenant rather than the session's. The insert
 * trigger validates the workflow_id within the row's own tenant, so a row left
 * to the system tenant is refused for every run started on behalf of anyone
 * else -- and the engine holds the system tenant while it starts all of them.
 */
std::vector<domain::workflow_plan_step> plan_steps_of(const domain::workflow_instance& instance,
                                                      const std::vector<workflow_step_def>& steps,
                                                      const std::string& actor) {
    std::vector<domain::workflow_plan_step> rows;
    rows.reserve(steps.size());
    for (std::size_t i = 0; i < steps.size(); ++i) {
        const auto& step = steps[i];
        domain::workflow_plan_step row;
        row.tenant_id = instance.tenant_id;
        row.id = boost::uuids::random_generator()();
        row.workflow_id = instance.id;
        row.step_index = static_cast<int>(i);
        row.name = step.name;
        row.label = step.label;
        row.description = step.description;
        row.command_subject = step.command_subject;
        row.compensation_subject = step.compensation_subject;
        row.timeout_seconds = static_cast<std::int32_t>(step.timeout.count());
        row.modified_by = actor;
        row.performed_by = actor;
        rows.push_back(std::move(row));
    }
    return rows;
}

/**
 * @brief The edges of that chain, each naming its steps by position.
 *
 * A consumer names a step the same chain contains, because the graph refused
 * the definition otherwise before any of this was written. The lookup states
 * that: a name that is not there is a broken invariant and throws rather than
 * writing an edge that points at nothing.
 */
std::vector<domain::workflow_plan_dependency>
plan_dependencies_of(const domain::workflow_instance& instance,
                     const std::vector<workflow_step_def>& steps,
                     const std::string& actor) {
    std::unordered_map<std::string, int> index_of;
    for (std::size_t i = 0; i < steps.size(); ++i)
        index_of.emplace(steps[i].name, static_cast<int>(i));

    std::vector<domain::workflow_plan_dependency> rows;
    for (std::size_t i = 0; i < steps.size(); ++i)
        for (const auto& input : steps[i].consumes) {
            domain::workflow_plan_dependency row;
            row.tenant_id = instance.tenant_id;
            row.id = boost::uuids::random_generator()();
            row.workflow_id = instance.id;
            row.consumer_step_index = static_cast<int>(i);
            row.producer_step_index = index_of.at(input);
            row.modified_by = actor;
            row.performed_by = actor;
            rows.push_back(std::move(row));
        }
    return rows;
}

/**
 * @brief A duration a person reads, at the resolution a log is read at.
 */
std::string describe_seconds(std::chrono::seconds value) {
    const auto total = value.count();
    const auto minutes = total / 60;
    const auto seconds = total % 60;
    if (minutes == 0)
        return std::to_string(seconds) + "s";
    if (seconds == 0)
        return std::to_string(minutes) + "m";
    return std::to_string(minutes) + "m " + std::to_string(seconds) + "s";
}

/**
 * @brief The component a command subject belongs to.
 *
 * Subjects are @c <component>.v1.<resource>.<verb> throughout, and a service
 * reports itself as @c ores.<component>.service, so the first token is what
 * ties a step to the service that owes it an answer.
 */
std::string component_of_subject(const std::string& subject) {
    const auto dot = subject.find('.');
    return dot == std::string::npos ? subject : subject.substr(0, dot);
}

/**
 * @brief How many step budgets a command may wait in a queue before it fails.
 *
 * A step no worker has claimed is not silent, so its budget measures the wrong
 * thing and the sweep waits. But waiting must end: a service that is alive and
 * reporting, with no consumer bound to the command's stream, would otherwise
 * hold the step for ever and leave the run in progress with no error. Ten
 * budgets is long enough for any queue this deployment produces and short
 * enough that a missing consumer is reported rather than waited out.
 */
constexpr int unclaimed_queue_budget_multiple = 10;

/// Whether a service name is the one a component's commands belong to.
bool service_owns_component(const std::string& service_name, const std::string& component) {
    return service_name == "ores." + component + ".service" ||
           service_name.starts_with("ores." + component + ".");
}

/**
 * @brief The name a step state id carries, or the id when the map has no name.
 *
 * The map is loaded from the store at start, so a name is absent only for a
 * state the store gained after this engine started. A log that named no state
 * at all would be worse than one naming the id, so the id is the fallback.
 */
std::string state_name(const fsm_state_map& states, const boost::uuids::uuid& state_id) {
    for (const auto& [name, id] : states.states)
        if (id == state_id)
            return name;
    return boost::uuids::to_string(state_id);
}

} // namespace

using namespace ores::logging;

workflow_engine::workflow_engine(ores::nats::service::client& nats,
                                 ores::database::context ctx,
                                 std::shared_ptr<const workflow_registry> registry,
                                 fsm_state_map instance_states,
                                 fsm_state_map step_states,
                                 std::optional<ores::security::jwt::jwt_authenticator> verifier)
    : nats_(nats)
    , ctx_(std::move(ctx))
    , registry_(std::move(registry))
    , instance_states_(std::move(instance_states))
    , step_states_(std::move(step_states))
    , verifier_(std::move(verifier)) {}

void workflow_engine::publish_command(const domain::workflow_step& step,
                                      const boost::uuids::uuid& instance_id,
                                      const boost::uuids::uuid& tenant_id) {

    const auto step_id_str = boost::uuids::to_string(step.id);
    const auto inst_id_str = boost::uuids::to_string(instance_id);
    const auto tenant_id_str = boost::uuids::to_string(tenant_id);

    BOOST_LOG_SEV(lg(), info) << "Publishing step command:" << " workflow=" << inst_id_str
                              << " step_index=" << step.step_index << " step_name=" << step.name
                              << " step_id=" << step_id_str << " subject=" << step.command_subject
                              << " via stream";

    const auto data = std::as_bytes(std::span{step.command_json.data(), step.command_json.size()});

    using namespace ores::workflow::messaging;
    // The command goes to a stream rather than to core NATS, so the server
    // holds it until the serving service acknowledges it. A step dispatched
    // while that service was restarting used to be discarded with no trace,
    // and the run expired twenty minutes later; on a stream it waits instead.
    //
    // The step id travels as the correlation id as well as its own header, so
    // the handler's entry line carries the same key this dispatch logs and the
    // two can be joined by one grep. with_correlation_id() only applies to
    // request/reply, so a command has to set it here.
    try {
        // The step id is the identity of this piece of work, so the stream can
        // deduplicate a re-dispatch of it: a retry after a lost publish ack
        // stores one command, not two.
        nats_.js_publish(step.command_subject,
                         data,
                         {{std::string(step_id_header), step_id_str},
                          {std::string(instance_id_header), inst_id_str},
                          {std::string(tenant_id_header), tenant_id_str},
                          {std::string(ores::nats::headers::nats_msg_id), step_id_str},
                          {std::string(ores::nats::headers::nats_correlation_id), step_id_str}});
    } catch (const std::exception& e) {
        // Nothing was accepted, so nothing will answer and the step would sit
        // awaiting until its deadline. Say why here rather than leave the
        // deadline text to guess at it.
        BOOST_LOG_SEV(lg(), error)
            << "Step command could not be published to a stream, so no service was asked to "
               "act: workflow="
            << inst_id_str << " step=" << step.name << " step_id=" << step_id_str
            << " subject=" << step.command_subject << " error=" << e.what();
        throw;
    }
}

void workflow_engine::publish_status_event(const boost::uuids::uuid& instance_id,
                                           const boost::uuids::uuid& tenant_id) {

    using ev = ores::eventing::domain::entity_change_event;
    ev e;
    e.entity = "ores.workflow.workflow_instance";
    e.timestamp = std::chrono::system_clock::now();
    e.entity_ids = {boost::uuids::to_string(instance_id)};
    e.tenant_id = boost::uuids::to_string(tenant_id);

    try {
        nats_.publish("ores.workflow.workflow_instance_changed",
                      ores::nats::default_wire_codec().encode(e),
                      {});
    } catch (const std::exception& ex) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to publish workflow status event: " << ex.what();
    }
}

void workflow_engine::set_instance_state(const boost::uuids::uuid& instance_id,
                                         const boost::uuids::uuid& state_id,
                                         const std::string& result_json,
                                         const std::string& error) {
    const auto rows = instance_repo_.read_latest(ctx_, boost::uuids::to_string(instance_id));
    if (rows.empty())
        return;
    auto instance = rows.front();
    instance.state_id = state_id;
    if (!result_json.empty())
        instance.result_json = result_json;
    instance.error = error;
    // Every transition says the run was acted on, which is what a reader
    // watching for silence polls.
    instance.last_event_at = std::chrono::system_clock::now();
    if (is_instance_terminal(state_id)) {
        // The first time the run stopped is the one that counts, so a later
        // write to a run that has already stopped -- a redelivery, a recovery,
        // a compensation that follows -- cannot move it on.
        if (!instance.completed_at)
            instance.completed_at = instance.last_event_at;
    } else {
        // A run that is going again has not finished. The retry is the case
        // that matters: leaving the failure's completion time on a resumed run
        // would have it say both that it stopped and that it is running.
        instance.completed_at.reset();
    }
    instance_repo_.write(ctx_, instance);
}

void workflow_engine::note_instance_activity(const boost::uuids::uuid& instance_id) {
    const auto rows = instance_repo_.read_latest(ctx_, boost::uuids::to_string(instance_id));
    if (rows.empty())
        return;
    auto instance = rows.front();
    // A run that has finished is not made live again by a late answer, a
    // recovery pass or a rollback that follows it. What this column is for is
    // telling work in flight from work abandoned, and a finished run is
    // neither: it says when it stopped, and that is the whole answer.
    if (instance.completed_at)
        return;
    instance.last_event_at = std::chrono::system_clock::now();
    instance_repo_.write(ctx_, instance);
}

bool workflow_engine::is_instance_terminal(const boost::uuids::uuid& state_id) const {
    return state_id == instance_states_.require("completed") ||
           state_id == instance_states_.require("failed") ||
           state_id == instance_states_.require("compensated");
}

bool workflow_engine::is_step_terminal(const boost::uuids::uuid& state_id) const {
    return state_id == step_states_.require("completed") ||
           state_id == step_states_.require("completed_with_warnings") ||
           state_id == step_states_.require("failed") ||
           state_id == step_states_.require("compensated");
}

void workflow_engine::stamp_command_published(const boost::uuids::uuid& step_id) {
    const auto rows = step_repo_.read_latest(ctx_, boost::uuids::to_string(step_id));
    if (rows.empty())
        return;
    auto step = rows.front();
    // A re-dispatch -- a retry, or a recovery -- starts the step's silence
    // again, so the deadline it is judged by starts again with it.
    step.command_published_at = std::chrono::system_clock::now();
    step_repo_.write(ctx_, step);
}

void workflow_engine::settle_abandoned_forward_steps(const boost::uuids::uuid& instance_id,
                                                     const std::string& reason) {
    const auto mine =
        step_repo_.read_latest_by_workflow_id(ctx_, boost::uuids::to_string(instance_id), 0, 1000);
    // The step did not fail on its own account, so its error says what
    // happened to the run rather than inventing a failure of the step.
    const auto abandoned = "The run rolled back while this step was still running. " + reason;
    for (const auto& step : mine) {
        if (step.step_index < 0)
            continue;
        if (is_step_terminal(step.state_id))
            continue;
        set_step_state(step, step_states_.require("failed"), "", abandoned, {});
    }
}

void workflow_engine::note_awaiting(const domain::workflow_step& step,
                                    const boost::uuids::uuid& instance_id,
                                    const boost::uuids::uuid& tenant_id) {
    const auto chain = plan_step_repo_.read_latest_by_workflow_id(
        ctx_, boost::uuids::to_string(instance_id), 0, 1000);
    const auto deadlines = deadlines_of(chain);
    const auto budget = deadlines.find(step.name);
    if (budget == deadlines.end() || budget->second.count() <= 0)
        return;
    awaiting_.insert_or_assign(boost::uuids::to_string(step.id),
                               awaiting_step{.instance_id = instance_id,
                                             .tenant_id = tenant_id,
                                             .name = step.name,
                                             .budget = budget->second});
}

void workflow_engine::set_step_state(const domain::workflow_step& step,
                                     const boost::uuids::uuid& state_id,
                                     const std::string& response_json,
                                     const std::string& error,
                                     const std::string& step_log_json) {
    const auto rows = step_repo_.read_latest(ctx_, boost::uuids::to_string(step.id));
    if (rows.empty())
        return;
    auto updated = rows.front();
    const auto from_state_id = updated.state_id;
    updated.state_id = state_id;
    if (!response_json.empty())
        updated.response_json = response_json;
    updated.error = error;
    if (!step_log_json.empty())
        updated.step_log_json = step_log_json;
    const auto now = std::chrono::system_clock::now();
    const auto in_progress = step_states_.require("in_progress");
    if (state_id == in_progress) {
        // A step that is running has not finished. The retry that puts a failed
        // step back to work is the case that matters: leaving the failure's
        // completion time on it would have the row say the step both stopped
        // and is running, and the run's own completion would then be read off
        // the wrong attempt. Its start stays, because the step's life is what
        // that names.
        updated.completed_at.reset();
        if (!updated.started_at)
            updated.started_at = now;
    } else if (is_step_terminal(state_id) && !updated.completed_at) {
        // The first settlement is the one that counts, so a later answer to a
        // step the engine has already accounted for cannot move it.
        updated.completed_at = now;
    }
    step_repo_.write(ctx_, updated);

    // The claim exists only while the work is in flight, so the moment the step
    // leaves in_progress the claim is taken away. The step's own state is what
    // refuses a replay from here on, so a row left behind would say nothing the
    // step does not already say, and the table would grow by one row for every
    // step ever run. The release names no holder: the engine's write is what
    // closes the window, and the holder is whichever process ran the work.
    if (state_id != step_states_.require("in_progress"))
        claim_repo_.release(ctx_, step.id);

    BOOST_LOG_SEV(lg(), info) << "step " << step.step_index << " (" << step.name << ")"
                              << " workflow=" << boost::uuids::to_string(step.workflow_id)
                              << " step_id=" << boost::uuids::to_string(step.id)
                              << " state=" << state_name(step_states_, from_state_id) << "->"
                              << state_name(step_states_, state_id)
                              << " at=" << ores::platform::time::datetime::to_iso8601_utc(now);
}

void workflow_engine::dispatch_ready_steps(domain::workflow_instance& instance) {
    const auto* def = registry_->find(instance.type);
    if (!def) {
        BOOST_LOG_SEV(lg(), error) << "No workflow definition for type: " << instance.type;
        set_instance_state(instance.id,
                           instance_states_.require("failed"),
                           "",
                           "Unknown workflow type: " + instance.type);
        publish_status_event(instance.id, instance.tenant_id.to_uuid());
        return;
    }

    const auto id_str = boost::uuids::to_string(instance.id);

    // The run's own chain is what it is judged by, not the definition as it
    // stands now, so a definition edited after the run started cannot change
    // what the run was waiting for.
    const auto chain = plan_step_repo_.read_latest_by_workflow_id(ctx_, id_str, 0, 1000);
    if (chain.empty()) {
        const auto reason =
            "The run holds no plan steps, so the chain it was started with cannot be read.";
        BOOST_LOG_SEV(lg(), error) << "Cannot advance workflow " << instance.type << ": " << reason;
        set_instance_state(instance.id, instance_states_.require("failed"), "", reason);
        publish_status_event(instance.id, instance.tenant_id.to_uuid());
        return;
    }

    const auto edges = plan_dependency_repo_.read_latest_by_workflow_id(ctx_, id_str, 0, 1000);
    const auto graph = workflow_graph(nodes_of_chain(chain, edges));
    if (const auto& incoherent = graph.incoherent()) {
        BOOST_LOG_SEV(lg(), error)
            << "Cannot advance workflow " << instance.type << ": " << *incoherent;
        set_instance_state(instance.id, instance_states_.require("failed"), "", *incoherent);
        publish_status_event(instance.id, instance.tenant_id.to_uuid());
        return;
    }

    const auto all_steps = step_repo_.read_latest_by_workflow_id(ctx_, id_str, 0, 1000);

    // Only forward steps count. A compensation step answers a different
    // question -- whether the work was undone -- and its negative position is
    // how it says so.
    std::set<std::string> satisfied;
    std::set<std::string> dispatched;
    for (const auto& s : all_steps) {
        if (s.step_index < 0)
            continue;
        dispatched.insert(s.name);
        if (s.state_id == step_states_.require("completed") ||
            s.state_id == step_states_.require("completed_with_warnings"))
            satisfied.insert(s.name);
    }

    // Every step of the chain has answered, so the run is done. The chain is
    // the authority rather than a count of steps taken, because the steps now
    // run in the order the graph allows and several of them at once. The run's
    // result is the answer of the step the graph puts last.
    const bool all_answered = std::ranges::all_of(
        graph.order(), [&](const std::string& name) { return satisfied.contains(name); });
    if (all_answered) {
        std::string last_result;
        const auto& last = graph.order().back();
        for (const auto& s : all_steps) {
            if (s.step_index >= 0 && s.name == last)
                last_result = s.response_json;
        }
        BOOST_LOG_SEV(lg(), info) << "Workflow COMPLETED:" << " type=" << instance.type
                                  << " workflow=" << id_str << " steps=" << graph.size();
        set_instance_state(instance.id, instance_states_.require("completed"), last_result, "");
        publish_status_event(instance.id, instance.tenant_id.to_uuid());
        return;
    }

    const auto ready = graph.ready(satisfied, dispatched);
    if (ready.empty()) {
        // Nothing may run. If nothing is in flight either, the chain is waiting
        // on a step that will never answer: a run in that state is a run the
        // caller must be told about, not one left in progress for ever.
        if (dispatched.size() == satisfied.size()) {
            std::string waiting;
            for (const auto& name : graph.order()) {
                if (satisfied.contains(name))
                    continue;
                if (!waiting.empty())
                    waiting += ", ";
                waiting += "'" + name + "'";
            }
            const auto reason = "The run has no step left that can run: " + waiting +
                                " have not answered and none of them is in flight.";
            BOOST_LOG_SEV(lg(), error) << "Workflow " << id_str << " is stuck: " << waiting;
            begin_compensation(instance, reason);
        }
        // Otherwise a step is still in flight and its answer brings the next
        // dispatch, so the engine says nothing and waits.
        return;
    }

    // The command builders live on the definition, which is a function of the
    // request the run stored. A definition that no longer builds the steps the
    // run holds is a definition that cannot finish this run.
    std::vector<workflow_step_def> steps;
    try {
        steps = def->build_steps(instance.request_json,
                                 boost::uuids::to_string(instance.tenant_id.to_uuid()),
                                 instance.correlation_id);
    } catch (const std::exception& ex) {
        BOOST_LOG_SEV(lg(), error)
            << "Cannot build the step list for workflow " << id_str << ": " << ex.what();
        begin_compensation(instance, ex.what());
        return;
    }
    if (const auto undeclared = undeclared_deadline(steps)) {
        BOOST_LOG_SEV(lg(), error)
            << "Cannot advance workflow " << instance.type << ": " << *undeclared;
        set_instance_state(instance.id, instance_states_.require("failed"), "", *undeclared);
        publish_status_event(instance.id, instance.tenant_id.to_uuid());
        return;
    }

    std::unordered_map<std::string, const workflow_step_def*> def_by_name;
    for (const auto& s : steps)
        def_by_name.emplace(s.name, &s);
    std::unordered_map<std::string, const domain::workflow_plan_step*> row_by_name;
    for (const auto& p : chain)
        row_by_name.emplace(p.name, &p);

    // Each result is named by the step that produced it, so a step that
    // answered nothing cannot shift what a later step reads.
    workflow_step_results results;
    for (const auto& s : all_steps) {
        if (s.step_index >= 0)
            results.push_back(
                workflow_step_result{.name = s.name, .response_json = s.response_json});
    }

    // The run is touched before its steps go out, so the run's activity time is
    // never later than the start of the step it brought. That ordering also
    // keeps the touch from undoing an answer: the touch reads the run's row and
    // writes it back, so one made after a command was published could restore
    // the state the run was in before a step completed.
    note_instance_activity(instance.id);

    for (const auto& name : ready) {
        const auto definition = def_by_name.find(name);
        const auto row = row_by_name.find(name);
        if (definition == def_by_name.end() || row == row_by_name.end()) {
            const auto reason =
                "The run's chain holds '" + name + "', which the definition no longer builds.";
            BOOST_LOG_SEV(lg(), error)
                << "Cannot advance workflow " << instance.type << ": " << reason;
            begin_compensation(instance, reason);
            return;
        }

        // A builder that cannot produce a command -- a result it reads is
        // missing, or the request no longer parses -- has nothing to publish,
        // so the run is failed here rather than left in progress waiting for a
        // step that was never dispatched.
        std::string cmd_json;
        try {
            cmd_json = definition->second->build_command(instance.request_json, results);
        } catch (const std::exception& ex) {
            BOOST_LOG_SEV(lg(), error) << "Cannot build the command for step " << name
                                       << " of workflow " << id_str << ": " << ex.what();
            begin_compensation(instance, ex.what());
            return;
        }

        const auto new_id = boost::uuids::random_generator()();

        domain::workflow_step step;
        step.id = new_id;
        step.workflow_id = instance.id;
        // The step belongs to the instance's tenant, not to the session's. The
        // insert trigger validates the workflow_id within the step's own
        // tenant, so a step left to its default of "system" is refused for
        // every instance started on behalf of anyone else.
        step.tenant_id = instance.tenant_id;
        // The subjects and the deadline come from the run's own row rather than
        // from the definition, so the step goes to the service the run was
        // started with and is judged by the deadline it was started under.
        step.step_index = row->second->step_index;
        step.name = name;
        step.state_id = step_states_.require("in_progress");
        step.request_json = cmd_json;
        step.command_subject = row->second->command_subject;
        step.command_json = cmd_json;
        step.idempotency_key = boost::uuids::to_string(new_id);
        step.compensation_subject = row->second->compensation_subject;
        // A step that is created is a step that has gone to work, so the start
        // of its life is written with it rather than left for the first answer.
        step.started_at = std::chrono::system_clock::now();
        step.recorded_at = *step.started_at;
        // The instance carries the actor from the start request, so every step
        // of the run is attributed to the same caller.
        step.modified_by = instance.modified_by;

        // Persist before publishing (ensures restart can re-dispatch).
        step_repo_.write(ctx_, step);

        BOOST_LOG_SEV(lg(), info) << "step " << step.step_index << " (" << step.name << ")"
                                  << " workflow=" << id_str
                                  << " step_id=" << boost::uuids::to_string(new_id)
                                  << " state=" << state_name(step_states_, step.state_id) << " at="
                                  << ores::platform::time::datetime::to_iso8601_utc(
                                         *step.started_at)
                                  << " entered=created";

        note_awaiting(step, instance.id, instance.tenant_id.to_uuid());
        publish_command(step, instance.id, instance.tenant_id.to_uuid());
        stamp_command_published(new_id);

        BOOST_LOG_SEV(lg(), info) << "Dispatched step " << row->second->step_index << " (" << name
                                  << ") for workflow=" << id_str << " type=" << instance.type;
    }

    publish_status_event(instance.id, instance.tenant_id.to_uuid());
}

void workflow_engine::begin_compensation(const domain::workflow_instance& instance,
                                         const std::string& failure_msg) {

    BOOST_LOG_SEV(lg(), warn) << "Workflow FAILED — beginning compensation:" << " type="
                              << instance.type
                              << " workflow=" << boost::uuids::to_string(instance.id)
                              << " error=" << failure_msg;

    set_instance_state(instance.id, instance_states_.require("compensating"), "", failure_msg);
    publish_status_event(instance.id, instance.tenant_id.to_uuid());

    // The run has stopped waiting on its forward steps, so none of them is
    // still running. Settled here rather than at the end of the rollback
    // because a sibling that answered in between would otherwise advance a run
    // that is already on its way out.
    settle_abandoned_forward_steps(instance.id, failure_msg);

    const auto* def = registry_->find(instance.type);
    if (!def) {
        BOOST_LOG_SEV(lg(), error) << "Cannot compensate: no definition for type " << instance.type;
        set_instance_state(instance.id, instance_states_.require("compensated"), "", failure_msg);
        return;
    }

    const auto step_defs = def->build_steps(instance.request_json,
                                            boost::uuids::to_string(instance.tenant_id.to_uuid()),
                                            instance.correlation_id);

    // Load completed forward steps in reverse order for compensation.
    auto steps =
        step_repo_.read_latest_by_workflow_id(ctx_, boost::uuids::to_string(instance.id), 0, 1000);
    std::ranges::reverse(steps);

    bool dispatched_any = false;
    for (const auto& s : steps) {
        // Only compensate successfully completed forward steps.
        if (s.step_index < 0)
            continue;
        if (s.state_id != step_states_.require("completed"))
            continue;

        const auto step_index = static_cast<std::size_t>(s.step_index);
        if (step_index >= step_defs.size())
            continue;

        const auto& step_def = step_defs[step_index];
        if (step_def.compensation_subject.empty())
            continue;

        const auto comp_json =
            step_def.build_compensation ?
                step_def.build_compensation(s.command_json, s.response_json, failure_msg) :
                "{}";

        // Persist compensation step (step_index negative to distinguish).
        const auto comp_id = boost::uuids::random_generator()();
        domain::workflow_step comp_step;
        comp_step.id = comp_id;
        comp_step.workflow_id = instance.id;
        comp_step.tenant_id = instance.tenant_id;
        comp_step.step_index = -(s.step_index + 1);
        comp_step.name = step_def.name + "_compensation";
        comp_step.state_id = step_states_.require("in_progress");
        comp_step.request_json = comp_json;
        comp_step.command_subject = step_def.compensation_subject;
        comp_step.command_json = comp_json;
        comp_step.idempotency_key = boost::uuids::to_string(comp_id);
        comp_step.started_at = std::chrono::system_clock::now();
        comp_step.recorded_at = *comp_step.started_at;
        // A compensation step is the same run's work, so it carries the
        // actor the instance was started by.
        comp_step.modified_by = instance.modified_by;
        step_repo_.write(ctx_, comp_step);

        // Publish compensation command with tenant header.
        BOOST_LOG_SEV(lg(), info) << "step " << comp_step.step_index << " (" << comp_step.name
                                  << ")" << " workflow=" << boost::uuids::to_string(instance.id)
                                  << " step_id=" << boost::uuids::to_string(comp_id)
                                  << " state=" << state_name(step_states_, comp_step.state_id)
                                  << " at="
                                  << ores::platform::time::datetime::to_iso8601_utc(
                                         *comp_step.started_at)
                                  << " entered=created";
        BOOST_LOG_SEV(lg(), info) << "Dispatching compensation for step " << s.step_index << " ("
                                  << step_def.name << "_compensation)"
                                  << " workflow=" << boost::uuids::to_string(instance.id);
        publish_command(comp_step, instance.id, instance.tenant_id.to_uuid());
        stamp_command_published(comp_id);
        dispatched_any = true;
    }

    // Rolling work back is work done on the run, so its activity moves too.
    note_instance_activity(instance.id);

    // If no compensation steps were dispatched (e.g. no completed forward
    // steps with compensation subjects), transition directly to compensated.
    if (!dispatched_any) {
        BOOST_LOG_SEV(lg(), info) << "No compensation steps needed for workflow "
                                  << boost::uuids::to_string(instance.id);
        set_instance_state(instance.id, instance_states_.require("compensated"), "", failure_msg);
    }
}

void workflow_engine::stop_on_failure(const domain::workflow_instance& instance,
                                      const std::string& failure_msg) {

    BOOST_LOG_SEV(lg(), warn) << "Workflow STOPPED on its failed step:" << " type=" << instance.type
                              << " workflow=" << boost::uuids::to_string(instance.id)
                              << " error=" << failure_msg;

    // The run stops where it is: the failed step keeps its error and log, the
    // completed steps keep their results, and the run's error stands until a
    // retry clears it.
    set_instance_state(instance.id, instance_states_.require("failed"), "", failure_msg);
    publish_status_event(instance.id, instance.tenant_id.to_uuid());
}

void workflow_engine::check_compensation_complete(const domain::workflow_instance& instance) {

    const auto steps =
        step_repo_.read_latest_by_workflow_id(ctx_, boost::uuids::to_string(instance.id), 0, 1000);

    for (const auto& s : steps) {
        // Compensate the completed steps only; the rest never ran.
        if (s.step_index >= 0)
            continue;
        if (s.state_id == step_states_.require("in_progress")) {
            // At least one compensation step is still running.
            return;
        }
    }

    // All compensation steps have finished (completed or failed).
    BOOST_LOG_SEV(lg(), info) << "Workflow COMPENSATED (all rollback steps complete):" << " type="
                              << instance.type
                              << " workflow=" << boost::uuids::to_string(instance.id);
    set_instance_state(instance.id, instance_states_.require("compensated"), "", "");
    publish_status_event(instance.id, instance.tenant_id.to_uuid());
}

void workflow_engine::on_step_completed(ores::nats::message msg) {
    const std::lock_guard<std::mutex> lock(mutex_);
    // Delivered durably, so it must be answered. Every path below answers
    // explicitly; the guard retries anything that does not, and the transport
    // retries if a call throws before the answer.
    ores::nats::ack_guard ack(msg);
    auto event_result =
        ores::nats::default_wire_codec().decode<messaging::step_completed_event>(msg.data);
    if (!event_result) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to decode step_completed_event: "
                                  << event_result.error().what();
        ack.reject();
        return;
    }
    const auto& event = *event_result;

    BOOST_LOG_SEV(lg(), info) << "Step-completed event received:" << " workflow="
                              << event.workflow_instance_id << " step=" << event.step_id
                              << " outcome=" << ores::workflow::messaging::to_string(event.outcome)
                              << (event.outcome == ores::workflow::messaging::step_outcome::failed ?
                                      " error=" + event.error_message :
                                      "");

    // Load the workflow step.
    boost::uuids::uuid step_id;
    try {
        step_id = boost::lexical_cast<boost::uuids::uuid>(event.step_id);
    } catch (...) {
        // The payload decoded, so the identity is not a parse race but a
        // malformed event; it will not read better on a second delivery.
        BOOST_LOG_SEV(lg(), error) << "Invalid step_id: " << event.step_id;
        ack.reject();
        return;
    }

    const auto found_steps = step_repo_.read_latest(ctx_, boost::uuids::to_string(step_id));
    if (found_steps.empty()) {
        // The step row may not be visible yet, so this is worth another
        // delivery rather than a discard. Waiting the claim hold-off rather
        // than retrying at once is what makes the delivery bound a duration:
        // an immediate NAK spends every attempt in a burst and the window
        // collapses to seconds.
        BOOST_LOG_SEV(lg(), warn) << "Step not found: " << event.step_id;
        ack.retry_after(ores::workflow::messaging::step_claim_hold_off);
        return;
    }
    // A copy rather than a reference: the step's state is written below, and
    // what the code after that needs is the step as it arrived, not as it was
    // left.
    const auto step = found_steps.front();

    /*
     * Guard: an event for a step the engine is no longer waiting on.
     *
     * Two arrivals look alike and mean different things. A duplicate is a
     * service that published its outcome twice, which changes nothing. A late
     * answer is a service that finished after the engine gave up on it -- the
     * deadline pass has already failed the step and stopped the run -- and the
     * work it did is real. Either way the engine keeps the state it recorded,
     * because the run's record is the one a person acted on; what differs is
     * what the log says, and a late answer is worth saying out loud. A retry
     * re-dispatches this step under the same identity, so the same service
     * replays the outcome it already holds instead of doing the work twice.
     */
    if (step.state_id != step_states_.require("in_progress")) {
        const bool late_answer =
            step.state_id == step_states_.require("failed") && !step.error.empty();
        if (late_answer)
            BOOST_LOG_SEV(lg(), warn)
                << "Step " << event.step_id << " (" << step.name
                << ") answered after the engine stopped waiting for it; the run keeps the "
                   "failure it recorded: "
                << step.error;
        else
            BOOST_LOG_SEV(lg(), info) << "Duplicate step-completed event for step " << event.step_id
                                      << " (state is not in_progress); ignoring.";
        // Already accounted for, so the delivery is done with; retrying would
        // only reach this same line again.
        ack.accept();
        return;
    }

    // The engine is no longer waiting on this step, whatever the outcome says.
    awaiting_.erase(std::string(event.step_id));

    // Serialize log entries (empty string when there are none).
    const auto log_json = event.log.empty() ? std::string{} : rfl::json::write(event.log);

    // Update step state.
    using outcome = ores::workflow::messaging::step_outcome;
    switch (event.outcome) {
        case outcome::completed:
            set_step_state(step, step_states_.require("completed"), event.result_json, "", "");
            break;
        case outcome::completed_with_warnings:
            set_step_state(step,
                           step_states_.require("completed_with_warnings"),
                           event.result_json,
                           "",
                           log_json);
            break;
        case outcome::failed:
            set_step_state(step, step_states_.require("failed"), "", event.error_message, log_json);
            break;
    }

    // Load the workflow instance.
    boost::uuids::uuid instance_id;
    try {
        instance_id = boost::lexical_cast<boost::uuids::uuid>(event.workflow_instance_id);
    } catch (...) {
        BOOST_LOG_SEV(lg(), error)
            << "Invalid workflow_instance_id: " << event.workflow_instance_id;
        ack.reject();
        return;
    }

    auto found_instances = instance_repo_.read_latest(ctx_, boost::uuids::to_string(instance_id));
    if (found_instances.empty()) {
        BOOST_LOG_SEV(lg(), error) << "Workflow instance not found: " << event.workflow_instance_id;
        ack.reject();
        return;
    }
    auto* instance = &found_instances.front();

    // Distinguish between forward steps and compensation steps.
    using outcome = ores::workflow::messaging::step_outcome;
    const bool is_success =
        event.outcome == outcome::completed || event.outcome == outcome::completed_with_warnings;

    if (step.step_index < 0) {
        // Compensation step completed — check if all compensations are done.
        if (!is_success) {
            BOOST_LOG_SEV(lg(), error)
                << "Compensation step " << event.step_id << " failed: " << event.error_message;
        }
        check_compensation_complete(*instance);
    } else {
        // Forward step completed.
        if (is_success) {
            dispatch_ready_steps(*instance);
        } else {
            // The policy belongs to the definition. A run that declares stop
            // keeps its completed work and waits for a retry; the default is
            // to roll the work back.
            const auto* def = registry_->find(instance->type);
            if (def != nullptr && def->on_failure == failure_policy::stop)
                stop_on_failure(*instance, event.error_message);
            else
                begin_compensation(*instance, event.error_message);
        }
    }
    // The outcome is recorded and the run is advanced, so the delivery is
    // complete. A throw above leaves this unreached, and the message is
    // redelivered to finish the work.
    ack.accept();
}

void workflow_engine::on_start_workflow(ores::nats::message msg) {
    const std::lock_guard<std::mutex> lock(mutex_);
    // Delivered durably, so it must be answered. Every path below answers
    // explicitly; the guard retries anything that does not, and the transport
    // retries if a call throws before the answer.
    ores::nats::ack_guard ack(msg);

    auto msg_result =
        ores::nats::default_wire_codec().decode<messaging::start_workflow_message>(msg.data);
    if (!msg_result) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to decode start_workflow_message: "
                                  << msg_result.error().what();
        ack.reject();
        return;
    }
    const auto& req = *msg_result;

    BOOST_LOG_SEV(lg(), info) << "Start-workflow request received:" << " type=" << req.type
                              << " instance_id="
                              << (req.instance_id.empty() ? "(auto)" : req.instance_id)
                              << " corr=" << req.correlation_id;

    const auto* def = registry_->find(req.type);
    if (!def) {
        // No definition will appear on a second delivery in this deployment.
        BOOST_LOG_SEV(lg(), error) << "No workflow definition for type: " << req.type;
        ack.reject();
        return;
    }

    // Parse tenant_id.
    boost::uuids::uuid tenant_id;
    try {
        tenant_id = boost::lexical_cast<boost::uuids::uuid>(req.tenant_id);
    } catch (...) {
        BOOST_LOG_SEV(lg(), error) << "Invalid tenant_id: " << req.tenant_id;
        ack.reject();
        return;
    }

    // Parse the target, which is what the run acts on. A caller that names one
    // and cannot be read is refused here: a run stored without the target it was
    // given is a run the caller can no longer find by the thing it acts on.
    boost::uuids::uuid target_id{};
    if (!req.target_id.empty()) {
        if (req.target_kind.empty()) {
            BOOST_LOG_SEV(lg(), error)
                << "A target_id was given with no target_kind: " << req.target_id;
            ack.reject();
            return;
        }
        try {
            target_id = boost::lexical_cast<boost::uuids::uuid>(req.target_id);
        } catch (...) {
            BOOST_LOG_SEV(lg(), error) << "Invalid target_id: " << req.target_id;
            ack.reject();
            return;
        }
    }
    if (!req.target_kind.empty() && req.target_id.empty()) {
        BOOST_LOG_SEV(lg(), error)
            << "A target_kind was given with no target_id: " << req.target_kind;
        ack.reject();
        return;
    }

    // Build the step list for this specific instance. A definition that cannot
    // read the start message, or that does not recognise the chain the message
    // asks for, has no run to create; the refusal is recorded here rather than
    // thrown back at a publisher that has already been answered.
    std::vector<workflow_step_def> steps;
    try {
        steps = def->build_steps(req.request_json, req.tenant_id, req.correlation_id);
        if (const auto undeclared = undeclared_deadline(steps)) {
            BOOST_LOG_SEV(lg(), error)
                << "Cannot run workflow type " << req.type << ": " << *undeclared;
            ack.reject();
            return;
        }
        const auto graph = workflow_graph(nodes_of(steps));
        if (const auto& incoherent = graph.incoherent()) {
            BOOST_LOG_SEV(lg(), error)
                << "Cannot run workflow type " << req.type << ": " << *incoherent;
            ack.reject();
            return;
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error)
            << "Cannot build the step list for workflow type " << req.type << ": " << e.what();
        ack.reject();
        return;
    }
    if (steps.empty()) {
        BOOST_LOG_SEV(lg(), error) << "Workflow definition has no steps: " << req.type;
        ack.reject();
        return;
    }

    // Create the workflow instance (reuse caller-provided UUID if present).
    boost::uuids::uuid instance_id;
    if (!req.instance_id.empty()) {
        try {
            instance_id = boost::lexical_cast<boost::uuids::uuid>(req.instance_id);
        } catch (...) {
            BOOST_LOG_SEV(lg(), error) << "Invalid instance_id: " << req.instance_id;
            ack.reject();
            return;
        }
    } else {
        instance_id = boost::uuids::random_generator()();
    }

    // A caller may supply the id, which is what makes a retried start address
    // the run it already asked for. Recognise the repeat here rather than let
    // the store's create-over-live-row guard refuse it, because a refusal the
    // caller cannot see is worse than the no-op the repeat actually is.
    try {
        const auto existing =
            instance_repo_.read_latest(ctx_, boost::uuids::to_string(instance_id));
        if (!existing.empty()) {
            BOOST_LOG_SEV(lg(), info)
                << "Workflow already started; leaving it alone:" << " type=" << req.type
                << " workflow=" << boost::uuids::to_string(instance_id);
            // The run the caller asked for exists, which is what the repeat
            // means; retrying would reach this same line again.
            ack.accept();
            return;
        }
    } catch (const std::exception& e) {
        // The read is a courtesy and the write is the authority, so a read that
        // fails must not stop a start that would otherwise succeed.
        BOOST_LOG_SEV(lg(), warn) << "Could not check for an existing workflow instance: "
                                  << e.what();
    }

    // Who asked for the run. The publisher forwards the caller's token, so a
    // person-initiated workflow is attributed to the person and the service
    // account is left to the runs that genuinely have no caller.
    const auto actor = actor_from_message(msg, verifier_, ctx_.service_account());

    domain::workflow_instance instance;
    instance.id = instance_id;
    instance.tenant_id = utility::uuid::tenant_id::from_uuid(tenant_id).value();
    instance.type = req.type;
    instance.target_kind = req.target_kind;
    instance.target_id = target_id;
    instance.state_id = instance_states_.require("in_progress");
    instance.request_json = req.request_json;
    instance.correlation_id = req.correlation_id;
    instance.created_by = actor;
    instance.modified_by = actor;
    instance.step_count = static_cast<int>(steps.size());
    instance.last_event_at = std::chrono::system_clock::now();
    instance.recorded_at = *instance.last_event_at;

    bool instance_created = false;
    try {
        instance_repo_.write(ctx_, instance);
        instance_created = true;

        // The run's chain is written with the run, so the run carries the chain
        // it was started with. Every later read of what the run is waiting for
        // is a read of these rows.
        plan_step_repo_.write(ctx_, plan_steps_of(instance, steps, actor));
        plan_dependency_repo_.write(ctx_, plan_dependencies_of(instance, steps, actor));

        // The chain's roots run first. Asking the graph rather than taking the
        // first step is what lets a chain begin with more than one step, which
        // is the fan-out the gathering redesign needs.
        dispatch_ready_steps(instance);

        BOOST_LOG_SEV(lg(), info) << "Workflow STARTED:" << " type=" << req.type
                                  << " workflow=" << boost::uuids::to_string(instance_id)
                                  << " step_count=" << steps.size()
                                  << " corr=" << req.correlation_id;
        publish_status_event(instance_id, tenant_id);
    } catch (const std::exception& e) {
        // A start that fails after the instance row exists (e.g. a dead
        // database connection while dispatching the first steps) must not leave
        // an in_progress instance with no steps behind: waiters would poll it
        // until their timeout with no error surfaced. Record the failure so
        // the query path can report the exact reason.
        const auto id_str = boost::uuids::to_string(instance_id);
        if (instance_created) {
            BOOST_LOG_SEV(lg(), error) << "Failed to start workflow " << id_str << ": " << e.what();
            const auto failure = "Failed to start workflow: " + std::string(e.what());
            set_instance_state(instance_id, instance_states_.require("failed"), "", failure);
            publish_status_event(instance_id, tenant_id);
            // A step persisted before the failure would otherwise stay
            // in_progress for ever: recovery re-dispatches only steps of
            // instances still in_progress, and this instance is now failed.
            for (const auto& started :
                 step_repo_.read_latest_by_workflow_id(ctx_, id_str, 0, 1000)) {
                if (started.step_index >= 0 &&
                    started.state_id == step_states_.require("in_progress"))
                    set_step_state(started, step_states_.require("failed"), "", failure);
            }
        } else {
            BOOST_LOG_SEV(lg(), error)
                << "Failed to create workflow instance " << id_str << ": " << e.what();
        }
    }
    // Either the run started, or the failure is recorded against it; both are
    // terminal, and a redelivery would only record the failure again.
    ack.accept();
}

void workflow_engine::recover_in_progress() {
    const std::lock_guard<std::mutex> lock(mutex_);
    BOOST_LOG_SEV(lg(), info) << "Starting workflow recovery pass.";

    // Recover both in-progress and compensating instances.
    const auto in_progress_id = instance_states_.require("in_progress");
    const auto compensating_id = instance_states_.require("compensating");

    // The generated repository has no find-by-state read, so the recovery pass
    // reads every tenant's instances once and splits them by state in memory.
    // The context is the system tenant, so the run of any tenant is in reach.
    const auto all_instances = instance_repo_.read_latest(ctx_);
    std::vector<domain::workflow_instance> instances;
    std::vector<domain::workflow_instance> compensating;
    std::copy_if(all_instances.begin(),
                 all_instances.end(),
                 std::back_inserter(instances),
                 [&](const auto& i) { return i.state_id == in_progress_id; });
    std::copy_if(all_instances.begin(),
                 all_instances.end(),
                 std::back_inserter(compensating),
                 [&](const auto& i) { return i.state_id == compensating_id; });
    instances.insert(instances.end(),
                     std::make_move_iterator(compensating.begin()),
                     std::make_move_iterator(compensating.end()));

    BOOST_LOG_SEV(lg(), info) << "Found " << instances.size()
                              << " recoverable workflow instance(s).";

    for (const auto& instance : instances) {
        try {
            const auto chain = plan_step_repo_.read_latest_by_workflow_id(
                ctx_, boost::uuids::to_string(instance.id), 0, 1000);
            if (chain.empty()) {
                throw std::logic_error("Instance " + boost::uuids::to_string(instance.id) +
                                       " has no plan steps, so the chain it was started with "
                                       "cannot be read.");
            }

            // Find all in-progress steps for this instance and re-dispatch.
            const auto steps = step_repo_.read_latest_by_workflow_id(
                ctx_, boost::uuids::to_string(instance.id), 0, 1000);
            if (steps.empty()) {
                // A start that died before persisting its first step leaves
                // an in_progress instance with no steps; nothing can ever
                // re-dispatch it. Mark it failed so waiters see a real error.
                BOOST_LOG_SEV(lg(), error) << "Instance " << boost::uuids::to_string(instance.id)
                                           << " has no steps; marking failed";
                set_instance_state(instance.id,
                                   instance_states_.require("failed"),
                                   "",
                                   "instance has no steps after recovery");
                publish_status_event(instance.id, instance.tenant_id.to_uuid());
                continue;
            }
            for (const auto& s : steps) {
                if (s.state_id != step_states_.require("in_progress"))
                    continue;

                BOOST_LOG_SEV(lg(), info)
                    << "Re-dispatching step " << s.step_index << " (" << s.name << ") for instance "
                    << boost::uuids::to_string(instance.id);

                note_awaiting(s, instance.id, instance.tenant_id.to_uuid());
                publish_command(s, instance.id, instance.tenant_id.to_uuid());
                // The deadline measures how long this dispatch has been silent,
                // so the step that was in flight when the fleet went down gets
                // its full budget again rather than the one it was already
                // part-way through.
                stamp_command_published(s.id);
            }
            if (!steps.empty())
                note_instance_activity(instance.id);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Recovery failed for instance "
                                       << boost::uuids::to_string(instance.id) << ": " << e.what();
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Workflow recovery pass complete.";
}

std::optional<std::chrono::system_clock::time_point>
workflow_engine::last_seen_of(const std::string& subject) {
    const auto component = component_of_subject(subject);
    std::optional<std::chrono::system_clock::time_point> last_seen;
    const std::lock_guard<std::mutex> guard(service_last_seen_mutex_);
    for (const auto& [name, seen_at] : service_last_seen_) {
        if (service_owns_component(name, component) && (!last_seen || seen_at > *last_seen))
            last_seen = seen_at;
    }
    return last_seen;
}

std::string workflow_engine::deadline_failure_text(const domain::workflow_step& step,
                                                   std::chrono::seconds budget,
                                                   std::chrono::seconds silent_for,
                                                   bool claimed) {
    std::string reason = "The step did not answer within " + describe_seconds(budget) +
                         " of its command being published (subject " + step.command_subject +
                         "); it has been silent for " + describe_seconds(silent_for) + ".";

    if (!claimed) {
        reason += " No worker ever claimed it, so it waited in the " +
                  component_of_subject(step.command_subject) +
                  " service's queue for its whole budget and longer; the command may have had "
                  "no consumer bound to it.";
        return reason;
    }

    // What the engine knows about the service that owes this step an answer.
    // It states the evidence and stops there: a service that is running but
    // wedged reports for duty as well, so presence is not proof of handling.
    const auto component = component_of_subject(step.command_subject);
    const auto last_seen = last_seen_of(step.command_subject);

    if (!last_seen) {
        reason += " Nothing has reported as the " + component +
                  " service since this engine started, so the command may have had no handler.";
        return reason;
    }
    if (step.command_published_at && *last_seen < *step.command_published_at) {
        reason += " The " + component +
                  " service was not reporting when the command was "
                  "published: it last reported " +
                  describe_seconds(std::chrono::duration_cast<std::chrono::seconds>(
                      *step.command_published_at - *last_seen)) +
                  " before it.";
        return reason;
    }
    reason += " The " + component +
              " service was still reporting when the command was published, so the command "
              "reached a service that did not finish the step.";
    return reason;
}

void workflow_engine::start_deadline_watch() {
    deadline_watch_ =
        ores::platform::concurrency::jthread([this](ores::platform::concurrency::stop_token stop) {
            BOOST_LOG_SEV(lg(), info)
                << "Deadline watch started: a step that outlives the deadline its run states is "
                   "failed every "
                << expiry_pass_interval.count() << "s.";
            while (!stop.stop_requested()) {
                if (ores::platform::concurrency::wait_for(stop, expiry_pass_interval))
                    break;
                try {
                    expire_overdue_steps();
                } catch (const std::exception& e) {
                    // A pass that fails must not take the watch down with it: the
                    // next pass is the one that matters.
                    BOOST_LOG_SEV(lg(), error) << "Deadline watch pass failed: " << e.what();
                }
            }
            BOOST_LOG_SEV(lg(), info) << "Deadline watch stopped.";
        });
}

void workflow_engine::note_service_seen(const std::string& service_name,
                                        std::chrono::system_clock::time_point seen_at) {
    const std::lock_guard<std::mutex> guard(service_last_seen_mutex_);
    auto& last = service_last_seen_[service_name];
    if (seen_at > last)
        last = seen_at;
}

std::size_t workflow_engine::expire_overdue_steps() {
    const std::lock_guard<std::mutex> lock(mutex_);
    const auto now = std::chrono::system_clock::now();
    const auto step_running = step_states_.require("in_progress");
    const auto step_failed = step_states_.require("failed");

    std::size_t expired = 0;
    // The pass walks the steps the engine dispatched, not every run the store
    // holds: what it is looking for is work in flight, and that is what the
    // engine itself handed out. The step row decides whether the step is still
    // running, so an entry that outlived its step is dropped here.
    for (auto it = awaiting_.begin(); it != awaiting_.end();) {
        const auto& waiting = it->second;
        /*
         * One entry the pass cannot judge must not starve the others.
         *
         * A run in a tenant that has gone -- the test tenants a case creates
         * and drops are the everyday example -- cannot be read or failed, and
         * an exception thrown from here would abandon every deadline behind it
         * on this pass and on the next, because the entry stays. The entry is
         * dropped instead: a run the engine cannot reach is not one it can
         * fail, and saying so once is the most it can do.
         */
        try {
            const auto rows = step_repo_.read_latest(ctx_, it->first);
            if (rows.empty() || rows.front().state_id != step_running) {
                it = awaiting_.erase(it);
                continue;
            }

            const auto& step = rows.front();
            // When the step was dispatched is the row's to state, because a
            // restart, a retry and a recovery all rewrite it there.
            if (!step.command_published_at) {
                ++it;
                continue;
            }
            /*
             * A step no worker has taken holds no claim, and a step with no
             * claim has not gone silent: its command is still queued behind
             * whatever that service is running. Counting that queue as silence
             * fails a healthy step for the load of the service it waits on,
             * which is how a provisioning step that had waited two minutes
             * behind a password change was failed without ever starting.
             *
             * The deadline still catches a command whose service has stopped
             * reporting, because then nothing is coming back for it: the
             * engine must not wait for a handler that is not there.
             */
            const auto claimed_at = claim_repo_.claimed_at(ctx_, step.id);
            if (!claimed_at) {
                const auto seen = last_seen_of(step.command_subject);
                const auto queued_for = std::chrono::duration_cast<std::chrono::seconds>(
                    now - *step.command_published_at);
                if (seen && *seen >= *step.command_published_at &&
                    queued_for < waiting.budget * unclaimed_queue_budget_multiple) {
                    ++it;
                    continue;
                }
            }
            /*
             * Silence is measured from the later of the dispatch and the claim:
             * a worker taking the step is the first moment the work could go
             * silent, so a command that waited in a queue still gets the whole
             * budget once someone takes it.
             */
            const auto since = claimed_at && *claimed_at > *step.command_published_at ?
                                   *claimed_at :
                                   *step.command_published_at;
            const auto silent_for = std::chrono::duration_cast<std::chrono::seconds>(now - since);
            if (silent_for < waiting.budget) {
                ++it;
                continue;
            }
            const auto reason =
                deadline_failure_text(step, waiting.budget, silent_for, claimed_at.has_value());
            BOOST_LOG_SEV(lg(), error) << "Step outlived its deadline:" << " workflow="
                                       << boost::uuids::to_string(waiting.instance_id)
                                       << " step=" << step.name << " reason=" << reason;

            set_step_state(step, step_failed, "", reason);
            ++expired;

            const auto instances =
                instance_repo_.read_latest(ctx_, boost::uuids::to_string(waiting.instance_id));
            /*
             * The run stops, whatever failure policy its definition declares.
             *
             * A rollback is the right answer for a step that reported that it
             * failed, because such a step did nothing. A step that went silent is a
             * step whose work is unknown: it may have died before writing anything,
             * or it may be slow and about to finish. Compensating on that guess
             * would undo a run whose work was in fact done, so the engine stops and
             * leaves the decision to a person -- retrying is safe because a step is
             * idempotent, and discarding is a separate action.
             */
            if (!instances.empty())
                stop_on_failure(instances.front(), reason);

            // Erased last: waiting refers into this entry, and the handler below
            // erases it instead if anything above throws.
            it = awaiting_.erase(it);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn)
                << "Dropping a step the deadline pass cannot judge: step=" << waiting.name
                << " workflow=" << boost::uuids::to_string(waiting.instance_id)
                << " reason=" << e.what();
            it = awaiting_.erase(it);
        }
    }

    if (expired > 0)
        BOOST_LOG_SEV(lg(), warn) << "Expiry pass failed " << expired << " overdue step(s).";
    return expired;
}

workflow_engine::retry_outcome
workflow_engine::retry_instance(const boost::uuids::uuid& instance_id,
                                const std::string& step_name,
                                const utility::uuid::tenant_id& caller_tenant) {
    const std::lock_guard<std::mutex> lock(mutex_);
    const auto instance_id_str = boost::uuids::to_string(instance_id);

    const auto found_instances = instance_repo_.read_latest(ctx_, instance_id_str);
    if (found_instances.empty())
        return {.reason = "Workflow instance not found."};

    const auto instance = found_instances.front();
    // The engine reaches every tenant's runs; the caller reaches only its own,
    // except a caller in the system tenant. A service that sweeps every tenant,
    // such as the scheduled retry of a stopped apply, runs there, and the
    // permission the verb checks is what limits who may call it.
    if (instance.tenant_id != caller_tenant && caller_tenant != utility::uuid::tenant_id::system())
        return {.reason = "Workflow instance not found."};

    if (instance.state_id != instance_states_.require("failed"))
        return {.reason = "The run has not stopped, so there is nothing to resume."};

    // The run's own persisted steps, forward steps only: a retry resumes
    // work, so a compensation step is never a target. One page covers a run,
    // because a definition bounds its own step count. The order is stated
    // here rather than taken from the read, because the target is chosen by
    // position: the first failed step, and every step before it.
    const auto raw_steps = step_repo_.read_latest_by_workflow_id(ctx_, instance_id_str, 0, 1000);
    std::vector<domain::workflow_step> forward;
    for (const auto& s : raw_steps)
        if (s.step_index >= 0)
            forward.push_back(s);
    std::ranges::sort(forward, {}, &domain::workflow_step::step_index);

    const auto failed_id = step_states_.require("failed");
    const auto completed_id = step_states_.require("completed");
    const auto completed_with_warnings_id = step_states_.require("completed_with_warnings");
    const auto in_progress_id = step_states_.require("in_progress");

    const auto has_finished = [&](const domain::workflow_step& s) {
        return s.state_id == completed_id || s.state_id == completed_with_warnings_id;
    };

    const domain::workflow_step* target = nullptr;
    if (step_name.empty()) {
        for (const auto& s : forward)
            if (s.state_id == failed_id) {
                target = &s;
                break;
            }
        if (target == nullptr)
            return {.reason = "The run holds no failed step to resume from."};
    } else {
        for (const auto& s : forward)
            if (s.name == step_name) {
                target = &s;
                break;
            }
        if (target == nullptr)
            return {.reason = "The run holds no step named '" + step_name + "'."};
    }

    // Every step before the target must have finished. A retry resumes the
    // work a run still owes; it never resumes past work that did not land.
    for (const auto& s : forward) {
        if (s.step_index >= target->step_index)
            break;
        if (!has_finished(s))
            return {.reason = "The step '" + s.name +
                              "' has not completed, so the run cannot resume past it."};
    }
    if (has_finished(*target))
        return {.reason = "The step '" + target->name +
                          "' has completed, so there is nothing to resume."};

    // The target goes back to running with its error and its failed
    // attempt's log cleared, the instance loses its error, and the command
    // goes out again under the step id the store already holds: that id is
    // the step's idempotency key, so a service that answers twice is the
    // engine's to catch rather than the retry's to prevent.
    set_step_state(*target, in_progress_id, "", "", "[]");
    set_instance_state(instance.id, instance_states_.require("in_progress"), "", "");
    publish_command(*target, instance.id, instance.tenant_id.to_uuid());
    publish_status_event(instance.id, instance.tenant_id.to_uuid());

    /*
     * The resumed step is watched from here, exactly as a dispatched one is:
     * the sweep walks the steps the engine handed out, and a retry hands one
     * out just as a start does. Its clock is restamped for the same reason a
     * recovery restamps it -- the budget belongs to this attempt, not to the
     * failed one it replaces.
     */
    note_awaiting(*target, instance.id, instance.tenant_id.to_uuid());
    stamp_command_published(target->id);

    BOOST_LOG_SEV(lg(), info) << "Retried step " << target->step_index << " (" << target->name
                              << ") for instance " << instance_id_str;

    return {.resumed = true, .step_index = target->step_index, .step_name = target->name};
}

}
