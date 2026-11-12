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
#include "ores.utility/codec/member_count.hpp"
#include "ores.utility/codec/shaped_wire_traits.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <tuple>

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
 *  - A healthy step may run longer than the lease: provisioning steps run for
 *    one to several minutes. A delivery whose handler is still running renews
 *    the lease every @c step_claim_renew_interval and tells the server it is
 *    still working, so no redelivery takes over a holder that is alive. The
 *    lease only has to outlast the renewal interval, with room for a slow
 *    renewal, which 120 seconds against 30 does four times over.
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
 * @brief How often a delivery whose handler is still running renews its claim.
 *
 * A quarter of the lease, so three renewals can fail in a row, to a slow engine or a
 * lost reply, before the lease can end under a holder that is still working.
 */
inline constexpr std::chrono::seconds step_claim_renew_interval{step_claim_lease / 4};
static_assert(step_claim_lease >= 4 * step_claim_renew_interval,
              "a lease must hold at least three failed renewals before it can end");

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

/**
 * @brief Asks the workflow engine to extend the lease of a claim this delivery holds.
 *
 * A handler can run longer than the lease. While it runs, its delivery renews the
 * lease, so no redelivery can take the step over from a holder that is still working;
 * a holder that has died stops renewing, and its lease ends as it always did.
 */
struct renew_step_claim_request {
    using response_type = struct renew_step_claim_response;
    static constexpr std::string_view nats_subject = "workflow.v1.ops.renew_step_claim";
    static constexpr bool requires_session = true;
    std::string step_id;
    std::string tenant_id;
    /// The holder the claim must still name; a claim another holder took is not renewed.
    std::string owner;
    /// Whether the holder reports its progress with this renewal. A renewal without a
    /// report leaves the stored progress as it was.
    bool reports_progress = false;
    /// What the step is doing, in its own words.
    std::string progress;
    /// How far through it is, and out of how much, when it counts.
    std::optional<std::int64_t> progress_done;
    std::optional<std::int64_t> progress_total;
};

/**
 * @brief Whether the lease was extended, and why not when it was not.
 */
struct renew_step_claim_response {
    /// Whether the engine could answer at all.
    bool success = false;
    /// True when this holder still holds the claim and its lease now runs a full term again.
    bool renewed = false;
    /// The step's state by name when the claim was not renewed: "in_progress" when another
    /// holder took it, "unknown" when there is no such step, otherwise the state it settled in.
    std::string state;
    std::string message;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<claim_step_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"step_id", &claim_step_request::step_id},
        utility::codec::field{"tenant_id", &claim_step_request::tenant_id},
        utility::codec::field{"owner", &claim_step_request::owner}};
}
static_assert(utility::codec::names_every_member<claim_step_request>());

[[nodiscard]] constexpr auto wire_fields(std::type_identity<claim_step_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"success", &claim_step_response::success},
        utility::codec::field{"claimed", &claim_step_response::claimed},
        utility::codec::field{"state", &claim_step_response::state},
        utility::codec::field{"message", &claim_step_response::message},
        utility::codec::field{"attempts", &claim_step_response::attempts}};
}
static_assert(utility::codec::names_every_member<claim_step_response>());

[[nodiscard]] constexpr auto wire_fields(std::type_identity<renew_step_claim_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"step_id", &renew_step_claim_request::step_id},
        utility::codec::field{"tenant_id", &renew_step_claim_request::tenant_id},
        utility::codec::field{"owner", &renew_step_claim_request::owner},
        utility::codec::field{"reports_progress", &renew_step_claim_request::reports_progress},
        utility::codec::field{"progress", &renew_step_claim_request::progress},
        utility::codec::field{"progress_done", &renew_step_claim_request::progress_done},
        utility::codec::field{"progress_total", &renew_step_claim_request::progress_total}};
}
static_assert(utility::codec::names_every_member<renew_step_claim_request>());

[[nodiscard]] constexpr auto wire_fields(std::type_identity<renew_step_claim_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"success", &renew_step_claim_response::success},
        utility::codec::field{"renewed", &renew_step_claim_response::renewed},
        utility::codec::field{"state", &renew_step_claim_response::state},
        utility::codec::field{"message", &renew_step_claim_response::message}};
}
static_assert(utility::codec::names_every_member<renew_step_claim_response>());

}

template <>
struct ores::utility::codec::wire_traits<ores::workflow::messaging::claim_step_request>
    : shaped_wire_traits<ores::workflow::messaging::claim_step_request> {};

template <>
struct ores::utility::codec::wire_traits<ores::workflow::messaging::claim_step_response>
    : shaped_wire_traits<ores::workflow::messaging::claim_step_response> {};

template <>
struct ores::utility::codec::wire_traits<ores::workflow::messaging::renew_step_claim_request>
    : shaped_wire_traits<ores::workflow::messaging::renew_step_claim_request> {};

template <>
struct ores::utility::codec::wire_traits<ores::workflow::messaging::renew_step_claim_response>
    : shaped_wire_traits<ores::workflow::messaging::renew_step_claim_response> {};

#endif
