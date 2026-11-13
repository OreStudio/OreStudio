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
#ifndef ORES_SERVICE_MESSAGING_STEP_COMMAND_STREAM_HPP
#define ORES_SERVICE_MESSAGING_STEP_COMMAND_STREAM_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/delivery.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.platform/concurrency/stoppable_thread.hpp"
#include "ores.service/messaging/step_workers.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include <chrono>
#include <cstddef>
#include <exception>
#include <memory>
#include <mutex>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::service::messaging {

/**
 * @brief How many times a step command may be delivered before the server stops.
 *
 * A command that always fails must stop being delivered. The bound is high
 * enough that a transient fault is retried well past a restart, and low enough
 * that a poison command cannot cycle for ever.
 *
 * It is a retry budget for a consumer that is bound and failing, not a
 * downtime window: a push consumer with no subscriber receives nothing, so it
 * spends no deliveries while its service is down. That service reads the
 * command when it returns, however long it was away, for as long as the stream
 * keeps it.
 *
 * Ten is also the number the claim takeover needs; see
 * @c ores::workflow::messaging::step_claim_lease. Each attempt is spaced by
 * @c step_ack_wait, so the budget is about five minutes of a handler that keeps
 * failing, not five attempts in a burst.
 */
inline constexpr int step_max_deliver = 10;

/**
 * @brief How long the server waits for an answer on a step-protocol delivery.
 *
 * It serves the step commands and the workflow engine's two control consumers,
 * which share this delivery bound. The value is set explicitly on the consumer
 * rather than inherited from the server's default, because the claim takeover
 * is written against it: a delivery that finds the step held comes back after
 * this wait, and the lease is sized from the number of deliveries at this
 * spacing. With @c step_max_deliver it also makes the retry budget a real
 * window: ten deliveries at thirty seconds is about five minutes.
 */
inline constexpr std::chrono::milliseconds step_ack_wait{std::chrono::seconds{30}};

/**
 * @brief How long the server keeps a step command that nobody acknowledged.
 *
 * A command only has to outlive the service being restarted or redeployed, so
 * this is a day rather than the seven a stream keeps by default. It is a
 * backstop, not the main rule: under @c step_stream_retention a command is
 * removed as soon as the consumer has it, and this bound only decides how long
 * a command survives when no consumer can take it.
 */
inline constexpr int step_stream_max_age_days = 1;

/**
 * @brief What removes a step command once a consumer has it.
 *
 * A step command is work rather than data. Once the service has served it and
 * acknowledged it there is nothing left to keep, so @c interest says exactly
 * that, and the stream does not accumulate a day of every step ever dispatched
 * - which for a gather-trades step is a trade population. The cost is that a
 * command published while no consumer filters its subject is dropped instead of
 * held, so every step subject has to be declared before anything publishes to
 * it.
 */
inline constexpr ores::nats::service::stream_retention step_stream_retention =
    ores::nats::service::stream_retention::interest;

namespace detail {

inline std::string_view logger_name = "ores.service.messaging.step_command_stream";

inline auto& lg() {
    static auto instance = ores::logging::make_logger(logger_name);
    return instance;
}

}

/**
 * @brief The name of the stream that holds one family of a service's step
 *        commands.
 *
 * Derived from the service's own name, so it cannot drift from the registrar
 * that serves those subjects: @c ores.refdata.service becomes
 * @c <prefix>_refdata_steps.
 *
 * @param group Names the family when one service serves two of them. Leave it
 *        empty for the service's only family.
 */
[[nodiscard]] inline std::string step_stream_name(const ores::nats::service::client& nats,
                                                  std::string_view service_name,
                                                  std::string_view group) {
    constexpr std::string_view ores_prefix = "ores.";
    constexpr std::string_view service_suffix = ".service";
    if (service_name.starts_with(ores_prefix))
        service_name.remove_prefix(ores_prefix.size());
    if (service_name.ends_with(service_suffix))
        service_name.remove_suffix(service_suffix.size());
    std::string suffix(service_name);
    if (!group.empty()) {
        suffix += '_';
        suffix += group;
    }
    suffix += "_steps";
    return nats.make_stream_name(suffix);
}

/**
 * @brief The name of the one consumer that reads a step-command stream.
 *
 * A stream holds one family of step commands, and one consumer reads the whole
 * family, so the stream names the consumer. The service and group already
 * appear in the stream name, and a second derivation of the same key could only
 * drift from the first.
 */
[[nodiscard]] inline std::string step_durable_name(std::string_view stream) {
    return std::string(stream) + "_consumer";
}

/**
 * @brief The step commands one service serves: each subject and the call that
 *        handles it.
 *
 * One table holds the whole family, and three things derive from it: the stream
 * that covers the subjects, the consumer that filters them, and the dispatch
 * from a delivered subject to its handler. They cannot disagree, because there
 * is one list.
 *
 * The group names the family. A service that serves two independent families -
 * say its data-quality steps and its workflow-apply steps - declares two
 * tables, and each gets its own stream and consumer under that service's name.
 */
class step_routes {
public:
    explicit step_routes(std::string group = {})
        : group_(std::move(group)) {}

    step_routes(const step_routes&) = delete;
    step_routes& operator=(const step_routes&) = delete;
    step_routes(step_routes&&) = default;
    step_routes& operator=(step_routes&&) = default;

    /// Adds one subject and the call that serves it.
    step_routes& on(std::string_view subject, ores::nats::delivery_handler handler) {
        subjects_.emplace_back(subject);
        handlers_.push_back(std::move(handler));
        return *this;
    }

    [[nodiscard]] const std::string& group() const noexcept {
        return group_;
    }

    [[nodiscard]] const std::vector<std::string>& subjects() const noexcept {
        return subjects_;
    }

    [[nodiscard]] const std::vector<ores::nats::delivery_handler>& handlers() const noexcept {
        return handlers_;
    }

private:
    std::string group_;
    std::vector<std::string> subjects_;
    std::vector<ores::nats::delivery_handler> handlers_;
};

/**
 * @brief Serves a family of step commands on one durable consumer.
 *
 * Declares the stream that covers the family, then subscribes one durable
 * consumer carrying the whole family as its filter set, then prunes the
 * consumers this service no longer declares. Call it once per family, during
 * startup.
 *
 * A consumer per subject is what this replaces. One consumer per subject means
 * a service owns one server-side object per subject it serves - forty-five of
 * them for a service with forty-five data-quality steps - and every subject
 * that is renamed or dropped leaves a consumer behind holding a filter nothing
 * publishes to, and, when the name is reused, a consumer whose stored filter
 * the subscribe that follows will not attach.
 *
 * The stream removes a command once the consumer has it, so it does not grow
 * into a day-long archive of every step ever dispatched.
 *
 * The delivery bound is a retry budget, not the stream's age. A command
 * published while the service is down is unprocessed work on the consumer, and
 * the consumer spends no delivery on it until a subscriber binds. The stream's
 * day is what bounds that wait. The bound applies once a member is bound and
 * keeps failing: @c step_max_deliver deliveries spaced by @c step_ack_wait,
 * after which the server stops delivering that message to this consumer and the
 * command stays in the stream until the day expires, unreadable by that
 * consumer for ever.
 *
 * Each handler takes the @c ores::nats::delivery and settles it, so a handler
 * that records no outcome drops it unsettled and it is redelivered. A command
 * that does not reach a handler is settled here: one whose step is busy or
 * cannot be claimed comes back later, one whose step is done is committed, and
 * one that names no step is rejected.
 *
 * The steps run on @p workers threads. One, the default, runs each step on the
 * subscription's delivery thread, one at a time. A service whose steps mostly
 * wait on other services, and hold no database connection while they wait, can
 * ask for more, so a long step does not hold back the steps dispatched beside
 * it. A step that holds a connection for long counts against the service's
 * pool, so more workers than connections starves the service's other handlers.
 * A delivery moves to its worker, which settles it; the subscription ends only
 * once its workers have.
 *
 * @throws std::invalid_argument when the service declares no subjects, and
 *         std::runtime_error when the stream cannot be created. For a service
 *         whose steps are its work, either is fatal: no step can then be
 *         delivered.
 */
[[nodiscard]] inline ores::nats::service::subscription
serve_step_commands(ores::nats::service::client& nats,
                    std::string_view service_name,
                    const step_routes& routes,
                    std::size_t workers = 1) {
    using namespace ores::logging;

    const auto& subjects = routes.subjects();
    if (subjects.empty())
        throw std::invalid_argument("A service must serve at least one step-command subject.");

    const std::string stream = step_stream_name(nats, service_name, routes.group());
    /*
     * One stream holds one family. A second family on the same stream replaces
     * the first's consumer filter when it subscribes, so the service would serve
     * the second and silently stop serving the first: a step of the first family
     * then waits for a consumer that no longer exists, and fails for standing
     * still twenty minutes later with nothing in the log to say why.
     */
    {
        static std::mutex declared_mutex;
        static std::set<std::string> declared;
        const std::lock_guard<std::mutex> guard(declared_mutex);
        if (!declared.insert(stream).second)
            throw std::logic_error(
                "A service may serve one family of step commands per stream, and '" + stream +
                "' is already served. Give the second family a group of its own.");
    }
    auto admin = nats.make_admin();
    admin.ensure_stream(
        stream, nats.covering_subjects(subjects), step_stream_max_age_days, step_stream_retention);

    // The consumer filters relative subjects; a delivery names the fully
    // prefixed one, so the dispatch table is built on the same prefixing.
    const auto& handlers = routes.handlers();
    auto by_subject =
        std::make_shared<std::vector<std::pair<std::string, ores::nats::delivery_handler>>>();
    by_subject->reserve(subjects.size());
    for (std::size_t i = 0; i < subjects.size(); ++i)
        by_subject->emplace_back(nats.make_subject(subjects[i]), handlers[i]);

    std::vector<std::string_view> filters;
    filters.reserve(subjects.size());
    for (const auto& subject : subjects)
        filters.push_back(subject);

    const std::string durable = step_durable_name(stream);

    // The holder a claim records is this service, because this service is what
    // runs the step's work. It is computed once, so every claim this process
    // takes names one identity rather than one per delivery.
    const std::string claim_owner = ores::workflow::messaging::step_claim_owner(service_name);

    BOOST_LOG_SEV(detail::lg(), info) << "Serving " << subjects.size() << " step command(s) on '"
                                      << stream << "' as '" << durable << "'";

    // The workers belong to the subscription's callback, so they are joined
    // when the subscription is: no step outlives the service that took it.
    auto pool = std::make_shared<step_workers>(workers);
    auto serve = [by_subject, &nats, claim_owner, pool](ores::nats::delivery delivery) {
        const auto& msg = delivery.msg();
        // A copy, because msg refers into the delivery, which the handler
        // takes below.
        const std::string served = msg.subject;
        for (const auto& [subject, handler] : *by_subject) {
            if (served != subject)
                continue;

            /*
             * A command is served only by the delivery that holds the claim
             * on its step. Delivery is at-least-once, so two deliveries for
             * one step can arrive at once and both would otherwise do the
             * work; the claim is what makes exactly one of them do it.
             *
             * Only a real step command is claimed. A message without the
             * workflow headers names no step, so there is nothing to claim
             * and it goes straight to its handler.
             */
            std::string claimed_step;
            if (const auto wf = workflow_step_context::from_message(nats, msg)) {
                const auto outcome = claim_step(nats, wf->step_id, wf->tenant_id, claim_owner);
                if (outcome == step_claim_outcome::busy) {
                    // Another delivery holds a live lease, so the work is
                    // running elsewhere. Come back rather than do it twice,
                    // and come back after a pause: an immediate redelivery
                    // lands on the same live lease, loses the same race,
                    // and spends the delivery budget in a loop.
                    BOOST_LOG_SEV(detail::lg(), info)
                        << "Step " << wf->step_id << " is held by another delivery; retrying "
                        << msg.subject << " in "
                        << ores::workflow::messaging::step_claim_hold_off.count() << "ms";
                    std::move(delivery).rollback_after(
                        ores::workflow::messaging::step_claim_hold_off);
                    return;
                }
                if (outcome == step_claim_outcome::not_in_progress) {
                    // The step is already done or already failed, so its
                    // state refuses this replay and there is nothing here
                    // for this delivery to do.
                    std::move(delivery).commit();
                    return;
                }
                if (outcome == step_claim_outcome::unknown) {
                    // No such step, and nothing can ever serve it.
                    BOOST_LOG_SEV(detail::lg(), warn)
                        << "Step " << wf->step_id << " is not a workflow step; rejecting "
                        << msg.subject;
                    std::move(delivery).reject();
                    return;
                }
                if (outcome != step_claim_outcome::claimed) {
                    // The engine could not be reached or could not decide.
                    // Failing closed is deliberate: running the work
                    // unclaimed is what the claim exists to prevent.
                    // Come back after a pause, not at once: an immediate
                    // retry burns the whole delivery budget in a burst,
                    // so the bound would be seconds rather than the
                    // minutes it is written to be.
                    BOOST_LOG_SEV(detail::lg(), warn)
                        << "Could not claim step " << wf->step_id << " for " << msg.subject
                        << "; retrying in "
                        << ores::workflow::messaging::step_claim_hold_off.count()
                        << "ms rather than running it unclaimed.";
                    std::move(delivery).rollback_after(
                        ores::workflow::messaging::step_claim_hold_off);
                    return;
                }
                claimed_step = wf->step_id;
                BOOST_LOG_SEV(detail::lg(), info)
                    << "Step " << claimed_step << " claimed by " << claim_owner
                    << "; running its handler for " << msg.subject;

                /*
                 * A handler can outlive the claim's lease. Until the
                 * delivery is settled, the claim is renewed and the server
                 * is told the delivery is still being worked on, so no
                 * redelivery takes the step over from a holder that is
                 * still working. A holder that dies stops renewing, and
                 * its lease ends as before.
                 */
                const auto claimed_at = std::chrono::steady_clock::now();
                delivery.keep_alive(
                    ores::workflow::messaging::step_claim_renew_interval,
                    [&nats,
                     step = wf->step_id,
                     tenant = wf->tenant_id,
                     owner = claim_owner,
                     claimed_at] {
                        using ores::workflow::messaging::step_claim_renew_interval;
                        std::string state;
                        const auto outcome =
                            renew_step_claim(nats,
                                             step,
                                             tenant,
                                             owner,
                                             state,
                                             step_progress_board::instance().latest(step));
                        const auto held = std::chrono::duration<double>(
                                              std::chrono::steady_clock::now() - claimed_at)
                                              .count();
                        if (outcome == step_renewal_outcome::renewed) {
                            BOOST_LOG_SEV(detail::lg(), info)
                                << "Step " << step << " still running after " << held
                                << "s; its claim's lease is renewed";
                            return true;
                        }
                        if (outcome == step_renewal_outcome::lost) {
                            BOOST_LOG_SEV(detail::lg(), error)
                                << "Step " << step << " lost its claim after " << held
                                << "s while its handler still runs (" << state
                                << "); another delivery may now run its work";
                            return false;
                        }
                        BOOST_LOG_SEV(detail::lg(), warn)
                            << "Step " << step << " could not renew its claim after " << held
                            << "s (" << state << "); trying again in "
                            << step_claim_renew_interval.count() << "s";
                        return true;
                    });
            }

            // The handler takes the delivery, and with it the keep-alive,
            // which ends when the handler settles or drops it. The claim and
            // the keep-alive are taken here, on the delivery thread, so a
            // delivery that waits for a free worker keeps its claim and tells
            // the server it is in progress: the wait spends none of its
            // deliveries.
            auto run = [by_subject,
                        &handler,
                        claimed_step,
                        served,
                        delivery = std::move(delivery)]() mutable {
                // The step's report goes with its run, however the handler ends.
                struct forget_progress final {
                    const std::string& step;
                    ~forget_progress() {
                        if (!step.empty())
                            step_progress_board::instance().forget(step);
                    }
                } const forget{claimed_step};
                const auto started = std::chrono::steady_clock::now();
                const auto ran_for = [&started] {
                    return std::chrono::duration<double>(std::chrono::steady_clock::now() - started)
                        .count();
                };
                try {
                    handler(std::move(delivery));
                } catch (const std::exception& e) {
                    if (!claimed_step.empty())
                        BOOST_LOG_SEV(detail::lg(), error)
                            << "Step " << claimed_step << " handler for " << served
                            << " threw after " << ran_for() << "s: " << e.what();
                    throw;
                } catch (...) {
                    if (!claimed_step.empty())
                        BOOST_LOG_SEV(detail::lg(), error)
                            << "Step " << claimed_step << " handler for " << served
                            << " threw a non-standard exception after " << ran_for() << "s";
                    throw;
                }
                if (!claimed_step.empty())
                    BOOST_LOG_SEV(detail::lg(), info)
                        << "Step " << claimed_step << " handler for " << served
                        << " returned after " << ran_for() << "s";
            };
            // One worker runs the step here, without a copy of the handler call.
            if (pool->size() == 1) {
                run();
                return;
            }
            pool->run([run = std::move(run)]() mutable {
                // A worker has no caller to answer an exception, so it ends
                // here. The delivery was dropped unsettled as the exception
                // left the handler, so the server delivers it again.
                try {
                    run();
                } catch (const std::exception& e) {
                    BOOST_LOG_SEV(detail::lg(), error)
                        << "A step handler threw on a worker: " << e.what();
                } catch (...) {
                    BOOST_LOG_SEV(detail::lg(), error)
                        << "A step handler threw a non-standard exception on a worker";
                }
            });
            return;
        }
        // The consumer filters exactly the declared subjects, so a delivery
        // that matches none is a programming error. Reject it, because
        // nothing can ever serve it and the server would otherwise
        // redeliver it until the bound.
        BOOST_LOG_SEV(detail::lg(), error)
            << "No step-command handler for delivered subject " << served;
        std::move(delivery).reject();
    };

    auto sub = nats.js_queue_subscribe_multi(
        filters, durable, service_name, std::move(serve), stream, step_max_deliver, step_ack_wait);

    // The consumer that serves the whole family now exists, so the consumers
    // this service used to declare one subject at a time are dead weight: they
    // filter subjects the new consumer already covers, and under interest
    // retention an unconsumed filter is what pins a message in the stream.
    //
    // Prune after subscribing, never before. Between the two the stream would
    // have no consumer at all, and a command published in that window would be
    // dropped on the spot rather than held for this service.
    admin.prune_consumers(stream, durable);

    return sub;
}

}

#endif
