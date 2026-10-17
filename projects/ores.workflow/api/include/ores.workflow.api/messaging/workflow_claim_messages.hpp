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
#ifndef ORES_WORKFLOW_API_MESSAGING_WORKFLOW_CLAIM_MESSAGES_HPP
#define ORES_WORKFLOW_API_MESSAGING_WORKFLOW_CLAIM_MESSAGES_HPP

#include "ores.platform/process/pid.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <chrono>
#include <string>
#include <string_view>

namespace ores::workflow::messaging {

/**
 * @brief How long one delivery holds the claim on a step before another may
 *        take it over.
 *
 * The claim is built from four numbers: this lease, the hold off a delivery
 * takes before asking again, the number of deliveries a step command gets, and
 * the deadline the engine fails the step at. They are not free of each other,
 * and this constant carries the burden of all three constraints:
 *
 *  - The hold off times the deliveries, less one, must exceed the lease. A
 *    step command is delivered at most ten times, and a delivery that finds the
 *    step held comes back after @c step_claim_hold_off, so a holder that dies
 *    leaves its deliveries arriving at about 30, 60, 90 and 120 seconds. If the
 *    last of them arrived before this lease ended, nothing would be left to
 *    take the claim over and the step would be lost to the engine's deadline.
 *    So hold off x (deliveries - 1) must be greater than the lease: here
 *    30 x 9 = 270 seconds against 120.
 *  - The lease must outlast a healthy step, or a second delivery takes work the
 *    first is still running and both do it. The longest step observed in this
 *    environment runs in under 3 seconds, so 120 seconds is forty times the
 *    slowest healthy step, and four times the ack wait.
 *  - The lease must sit under the shortest step deadline, or the engine fails
 *    the step before a redelivery can take the claim over. The shortest
 *    deadline this deployment declares is 900 seconds, for a reporting step, so
 *    120 seconds is inside it by a factor of seven.
 *
 * Each constraint fails a step in its own way, and raising this number means
 * checking all three rather than only the one that prompted the change.
 */
inline constexpr std::chrono::seconds step_claim_lease{120};

/**
 * @brief How long a delivery that lost the claim waits before asking again.
 *
 * A delivery that finds the step held comes back rather than spinning. The
 * pause matches the consumer's ack wait, 30 seconds, because that is already
 * the rate at which an unanswered delivery is redelivered: asking again sooner
 * only spends the step's remaining deliveries faster.
 *
 * This is the number that decides how many deliveries a takeover costs, so it
 * is bound to the lease -- see @c step_claim_lease for the invariant, and mind
 * it if this changes.
 */
inline constexpr std::chrono::milliseconds step_claim_hold_off{std::chrono::seconds{30}};

/**
 * @brief The identity a claim records as its holder.
 *
 * The holder is the domain service whose transport took the claim, because that
 * is the process running the step's work; the workflow service only grants the
 * row. So the name is the caller's own service name, which
 * @c serve_step_commands already holds, and the column says which process to
 * look at when a step keeps being reported busy. Both parts are needed: the name
 * alone cannot separate two instances of the service, and the process id alone
 * cannot say what it is a process of.
 *
 * Every call in one process returns the same value, so an operator reading the
 * table sees one holder per process rather than one per delivery.
 */
[[nodiscard]] inline std::string step_claim_owner(std::string_view service_name) {
    return std::string(service_name) + "/" + std::to_string(ores::platform::process::current_pid());
}

/**
 * @brief Asks the workflow engine to claim one workflow step for this delivery.
 *
 * A step command is delivered at least once, so two deliveries for one step can
 * arrive at once. Each asks for the claim; exactly one is told it has it, and
 * the other is told to come back.
 *
 * The file is hand-written rather than generated, and its name says so. A
 * `*_protocol.hpp` name marks a generated protocol header, which owes a
 * TypeScript twin that a UI reads the same payload through. This pair is
 * internal to the services and has no UI twin, so it deliberately does not
 * claim that name: the twin gate globs for it, and an honest name is what keeps
 * the gate meaningful rather than something to be silenced.
 */
struct claim_step_request {
    using response_type = struct claim_step_response;
    static constexpr std::string_view nats_subject = "workflow.v1.ops.claim_step";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief UUID of the workflow step the delivery is for.
     */
    std::string step_id;
    /**
     * @brief The tenant the step belongs to.
     *
     * The claim is granted only for a step of that tenant: a request naming a
     * step in another tenant, or no step at all, is answered unknown.
     */
    std::string tenant_id;
    /**
     * @brief Who holds the step, as the claiming service names itself.
     *
     * The caller's own identity rather than one the engine invents: the holder
     * is the process that runs the step's work, and only the caller knows which
     * one that is. The engine records it verbatim, so the column an operator
     * reads names the process that is working the step.
     */
    std::string owner;
};

/**
 * @brief What the engine decided about one delivery's claim on a step.
 */
struct claim_step_response {
    /**
     * @brief Whether the engine could answer at all.
     *
     * A false here means the question was malformed, and nothing about the
     * step is being reported by the rest of the reply.
     */
    bool success = false;
    /**
     * @brief Whether this delivery may do the step's work.
     *
     * False when another delivery holds a live claim, when the step is not
     * in_progress, or when there is no such step.
     */
    bool claimed = false;
    /**
     * @brief The step's state, as its name, when it was not claimed.
     *
     * "unknown" means no such step exists. "in_progress" means the step exists
     * and another delivery holds a live claim. Any other name is the terminal
     * state the step already reached, so this delivery has nothing to do.
     */
    std::string state;
    /**
     * @brief Human-readable detail, for a log.
     */
    std::string message;
    /**
     * @brief How many times the step has been claimed, counting this one.
     *
     * Valid only when claimed is true.
     */
    int attempts = 0;
};

}

#endif
