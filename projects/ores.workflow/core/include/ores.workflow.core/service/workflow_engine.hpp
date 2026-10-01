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
#ifndef ORES_WORKFLOW_CORE_SERVICE_WORKFLOW_ENGINE_HPP
#define ORES_WORKFLOW_CORE_SERVICE_WORKFLOW_ENGINE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.platform/concurrency/stoppable_thread.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.workflow.api/domain/workflow_instance.hpp"
#include "ores.workflow.api/domain/workflow_step.hpp"
#include "ores.workflow.api/service/workflow_registry.hpp"
#include "ores.workflow.core/export.hpp"
#include "ores.workflow.core/repository/workflow_instance_repository.hpp"
#include "ores.workflow.core/repository/workflow_step_repository.hpp"
#include "ores.workflow.core/service/fsm_state_map.hpp"
#include <chrono>
#include <cstddef>
#include <memory>
#include <mutex>
#include <optional>
#include <string>
#include <thread>
#include <unordered_map>

namespace ores::workflow::service {

/**
 * @brief Persistent event-driven workflow engine.
 *
 * Processes step-completed events from domain services and advances or
 * compensates the corresponding workflow instance. All state is persisted
 * to PostgreSQL before and after each NATS publish so that the engine can
 * restart at any time and resume all in-flight workflows.
 *
 * Thread safety: a single engine instance is shared by every subscription, and
 * the callbacks are NOT serialised by the NATS library. The start and
 * step-completed handlers have been measured running on different threads, with
 * the completion being processed inside the start that caused it. Every entry
 * point therefore takes mutex_, because the handlers write the same rows with a
 * read-modify-write under optimistic locking and would otherwise lose each
 * other's updates: the identity fixture had set_step_state lose to
 * stamp_command_published, which left the step in_progress for good.
 *
 * The lock is not recursive, and what makes that safe is the client's dispatch
 * model rather than anything the library promises: client::publish is
 * natsConnection_PublishMsg, which returns without waiting, and a callback is
 * delivered on the connection's read loop rather than on the thread that
 * published. A client that ever delivered a callback synchronously on the
 * publishing thread would deadlock here instead of failing, so re-check this
 * before changing the NATS client or the way it dispatches.
 *
 * The lock is engine-wide rather than per-instance, so unrelated instances
 * serialise behind each other. That is a throughput ceiling, not a correctness
 * problem, and it is the honest cost of the fix: a lock keyed by instance id
 * would remove it, but every writer of one instance would still have to be in
 * the same critical section.
 */
class ORES_WORKFLOW_CORE_EXPORT workflow_engine {
private:
    inline static std::string_view logger_name = "ores.workflow.service.workflow_engine";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Constructs the engine.
     *
     * @param nats            Raw NATS client used for fire-and-forget publishes.
     * @param ctx             Service-account database context. It reads and
     *                        writes runs of every tenant, so it must be the
     *                        system tenant: the workflow tables' row-level
     *                        security policy is what admits the engine to
     *                        another tenant's rows, and a context scoped to one
     *                        tenant would confine the engine to that tenant.
     * @param registry        Registry of all known workflow definitions.
     * @param instance_states Pre-loaded FSM state map for workflow_instance.
     * @param step_states     Pre-loaded FSM state map for workflow_step.
     * @param verifier        JWT verifier used to read the caller's username
     *                        from the token a start request forwards. Empty
     *                        when the service has no key, in which case every
     *                        record is attributed to the service account.
     */
    workflow_engine(ores::nats::service::client& nats,
                    ores::database::context ctx,
                    std::shared_ptr<const workflow_registry> registry,
                    fsm_state_map instance_states,
                    fsm_state_map step_states,
                    std::optional<ores::security::jwt::jwt_authenticator> verifier);

    /**
     * @brief Handles a step-completed event from a domain service.
     *
     * Fire-and-forget: no reply is sent. Advances the workflow to the next
     * step, or begins compensation if the step failed.
     */
    void on_step_completed(ores::nats::message msg);

    /**
     * @brief Handles a start-workflow message.
     *
     * Creates a new workflow_instance record, persists and dispatches step 0.
     * Fire-and-forget: no reply is sent.
     */
    void on_start_workflow(ores::nats::message msg);

    /**
     * @brief Recovers all in-progress workflows on service startup.
     *
     * Queries for workflow_instance rows with state=in_progress in every
     * tenant, loads the current step for each, and re-dispatches the step
     * command using the same step_id (idempotency key). Domain services
     * deduplicate via the step_id and re-publish their completion events.
     */
    void recover_in_progress();

    /**
     * @brief Fails every step that has outlived the deadline its run states.
     *
     * A step command is published and the engine waits for the owning service
     * to report the outcome. Nothing in that path notices a service that has
     * died, a command that was dropped, or a handler that wedged: the step
     * stays =in_progress= for good, and every waiter eventually gives up on a
     * run that never failed. This pass is what ends that: a step whose command
     * was published more than the run's deadline ago is failed.
     *
     * The run stops, whatever failure policy its definition declares. A
     * rollback answers a step that reported that it failed, because such a step
     * did nothing; a step nobody heard from is a step whose work is unknown, so
     * the engine leaves the decision to a person. What it fails is retryable,
     * which is what makes a service that comes back one retry away.
     *
     * The failure names the step, its command subject and how long it has been
     * silent, together with what the engine knows about the component that owns
     * that subject -- see =note_service_seen=.
     *
     * @return how many steps it failed, so a caller can log a pass that did
     *         something without logging every pass that did nothing.
     */
    std::size_t expire_overdue_steps();

    /**
     * @brief Starts the clock that enforces the deadlines.
     *
     * A deployed engine runs this for the life of the process; a test drives
     * =expire_overdue_steps= itself, so the pass is a method the clock calls
     * rather than something the engine starts by existing. The watch stops and
     * joins when the engine is destroyed.
     */
    void start_deadline_watch();

    /**
     * @brief Records that a service reported for duty.
     *
     * A component's liveness is the one thing the engine cannot observe from a
     * step: a command published to a subject whose service is gone is dropped
     * by the bus in silence. The heartbeats a service publishes are what say
     * otherwise, and the engine keeps the last time it heard from each so a
     * deadline failure can state whether the component that owns the step's
     * subject was alive when the step was dispatched.
     *
     * A report is evidence and not proof: a service that is running but wedged
     * still reports, and the engine says what it saw rather than concluding.
     *
     * @param service_name the canonical service name a heartbeat carries, for
     *                     example @c ores.marketdata.service.
     * @param seen_at      when the heartbeat arrived.
     */
    void note_service_seen(const std::string& service_name,
                           std::chrono::system_clock::time_point seen_at);

    /**
     * @brief How long the engine waits between passes over the steps it
     * waits on.
     *
     * A deadline is honoured to within one pass, so this is how late a failure
     * may be. It is well below the shortest budget any definition states, so a
     * deadline is late by a fraction of itself.
     */
    static constexpr std::chrono::seconds expiry_pass_interval{30};

    /**
     * @brief What a retry did, so its caller can answer without reading again.
     */
    struct retry_outcome {
        /// Whether the run was resumed.
        bool resumed = false;
        /// Why it was not, naming what was refused. Empty when it was.
        std::string reason;
        /// The step the engine re-dispatched, or -1 when it re-dispatched none.
        int step_index = -1;
        std::string step_name;
    };

    /**
     * @brief Resumes a stopped run from the step that failed.
     *
     * The run must have stopped, the target step must be one the run has not
     * completed, and every step before it must have completed, so a retry
     * never resumes past work that did not finish and never repeats work that
     * did. The target is re-dispatched under the step id it already holds —
     * its idempotency key — with its command as the store persisted it, and
     * the target's error and the instance's error are cleared.
     *
     * @param instance_id   The run to resume.
     * @param step_name     The step to resume from, or empty for the step that
     *                      failed.
     * @param caller_tenant The caller's tenant. The engine reads runs of every
     *                      tenant, so this is what confines a retry to the
     *                      caller's own.
     */
    [[nodiscard]] retry_outcome retry_instance(const boost::uuids::uuid& instance_id,
                                               const std::string& step_name,
                                               const utility::uuid::tenant_id& caller_tenant);

private:
    /**
     * @brief Moves an instance to a state, recording an optional result and error.
     *
     * Reads the instance, applies the change and writes it back, because the
     * generated repository exposes no state-transition method of its own.
     */
    void set_instance_state(const boost::uuids::uuid& instance_id,
                            const boost::uuids::uuid& state_id,
                            const std::string& result_json,
                            const std::string& error);

    /** @brief Advances an instance's step index, leaving everything else alone. */
    void set_step_progress(const boost::uuids::uuid& instance_id, int step_index);

    /**
     * @brief Moves a step to a state, recording its response, error and log.
     *
     * Reads the step, applies the change and writes it back, because the
     * generated repository exposes no state-transition method of its own.
     */
    void set_step_state(const boost::uuids::uuid& step_id,
                        const boost::uuids::uuid& state_id,
                        const std::string& response_json,
                        const std::string& error,
                        const std::string& step_log_json = {});

    /** @brief Stamps a step as having published its command. */
    void stamp_command_published(const boost::uuids::uuid& step_id);

    /**
     * @brief One step the engine is waiting on an answer for.
     *
     * Enough to find the step and to judge it. When the step was dispatched is
     * not here: the step row states that, and the row is the record a restart,
     * a retry and a recovery all agree on. The list says which steps to look
     * at; the store says what is true of them.
     */
    struct awaiting_step {
        boost::uuids::uuid instance_id;
        boost::uuids::uuid tenant_id;
        std::string name;
        std::chrono::seconds budget;
    };

    /** @brief Records a step as dispatched. */
    void note_awaiting(const domain::workflow_step& step,
                       const boost::uuids::uuid& instance_id,
                       const boost::uuids::uuid& tenant_id);

    /**
     * @brief What to record against a step that has outlived its deadline.
     *
     * The reason names the step's command subject and how long it has been
     * silent, and states what the engine knows about the component that owns
     * that subject: whether it was reporting when the command was published,
     * and when it last reported. The engine states the evidence it holds and
     * draws no conclusion from it, because a service that is running but
     * wedged reports for duty too.
     */
    std::string deadline_failure_text(const domain::workflow_step& step,
                                      std::chrono::seconds budget,
                                      std::chrono::seconds silent_for);

    /**
     * @brief Publishes a step command to the domain service.
     *
     * Includes X-Workflow-Instance-Id, X-Workflow-Step-Id, and X-Tenant-Id
     * NATS headers so domain services can extract the idempotency key and
     * build the correct tenant-scoped database context.
     */
    void publish_command(const domain::workflow_step& step,
                         const boost::uuids::uuid& instance_id,
                         const boost::uuids::uuid& tenant_id);

    /**
     * @brief Publishes a workflow_instance_changed event to the NATS event bus.
     *
     * Called at every instance state transition so a subscriber can refresh its
     * view of the instance.
     */
    void publish_status_event(const boost::uuids::uuid& instance_id,
                              const boost::uuids::uuid& tenant_id);

    /**
     * @brief Dispatches the next step in the workflow after a success.
     *
     * Builds the next step's command, persists the step record, publishes
     * the command, and advances the instance's current_step_index.
     */
    void dispatch_next_step(domain::workflow_instance& instance,
                            const std::string& last_result_json);

    /**
     * @brief Begins saga compensation for a failed workflow instance.
     *
     * Iterates completed forward steps in reverse order, builds and
     * publishes compensation commands (with X-Tenant-Id header), and
     * transitions the instance to compensating. Does NOT mark as
     * compensated — that happens when all compensation step-completed
     * events are received.
     */
    void begin_compensation(const domain::workflow_instance& instance,
                            const std::string& failure_msg);

    /**
     * @brief Stops a run whose definition declares the stop-and-retry policy.
     *
     * Leaves the run in failed with the failure recorded and every completed
     * step untouched, so a person can retry the step that failed. The failed
     * step keeps its own error and log.
     */
    void stop_on_failure(const domain::workflow_instance& instance, const std::string& failure_msg);

    /**
     * @brief Checks whether all compensation steps have finished.
     *
     * Called after each compensation step-completed event. When no
     * in-progress compensation steps remain, transitions the instance
     * to the compensated state.
     */
    void check_compensation_complete(const domain::workflow_instance& instance);

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::shared_ptr<const workflow_registry> registry_;
    fsm_state_map instance_states_;
    fsm_state_map step_states_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
    repository::workflow_instance_repository instance_repo_;
    repository::workflow_step_repository step_repo_;

    /**
     * @brief When each service last reported for duty, by canonical name.
     *
     * Written by the heartbeat subscription and read by the expiry pass, which
     * run on different threads, so it carries its own lock rather than the
     * engine's: a heartbeat has nothing to do with the read-modify-write of a
     * run and must not queue behind one.
     */
    std::unordered_map<std::string, std::chrono::system_clock::time_point> service_last_seen_;
    std::mutex service_last_seen_mutex_;

    /// The clock that calls the expiry pass, and nothing until it is started.
    ores::platform::concurrency::stoppable_thread deadline_watch_;

    /**
     * @brief The steps the engine is waiting on, by step id.
     *
     * The pass walks this rather than scanning the store. The engine dispatched
     * each entry, so it knows the step exists, and a pass that read every run of
     * every tenant would grow with the deployment's history rather than with
     * the work in flight. The map decides nothing: the pass reads each entry's
     * step row, which is the authority on whether the step is still running and
     * on when it was dispatched, so the worst a lost entry costs is a deadline
     * that fires late -- and a start re-dispatches whatever a restart
     * interrupted, which fills the map again.
     *
     * It is guarded by the engine's own lock, because every writer of it is
     * already inside one: a dispatch, a completion, a retry and the pass are all
     * engine entry points.
     */
    std::unordered_map<std::string, awaiting_step> awaiting_;

    /**
     * @brief Serialises the entry points against each other.
     *
     * Held for the whole of a handler, so a handler's read-modify-write of a
     * step or instance row cannot interleave with another's. A plain mutex is
     * enough because the handlers run on different threads: a publish is
     * asynchronous, so the handler it provokes never re-enters this one on the
     * same thread and the lock cannot deadlock.
     */
    std::mutex mutex_;
};

}

#endif
