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

#include "ores.nats/service/client.hpp"
#include <cctype>
#include <stdexcept>
#include <string>
#include <string_view>

namespace ores::service::messaging {

/**
 * @brief How many times a step command may be delivered before the server stops.
 *
 * A command that always fails must stop being delivered. The bound is high
 * enough that a transient fault is retried well past a restart, and low enough
 * that a poison command cannot cycle for ever.
 */
inline constexpr int step_max_deliver = 10;

/**
 * @brief How long the server keeps a step command that nobody acknowledged.
 *
 * A command only has to outlive the service being restarted or redeployed, so
 * this is a day rather than the seven a stream keeps by default. A longer
 * retention would accumulate the payload of every step ever dispatched - which
 * for a gather-trades step is a trade population - long after the run that
 * asked for it had failed or finished.
 */
inline constexpr int step_stream_max_age_days = 1;

/**
 * @brief The name of the stream that holds a service's step commands.
 *
 * Derived from the service's own name, so it cannot drift from the registrar
 * that serves those subjects: @c ores.refdata.service becomes
 * @c <prefix>_refdata_steps.
 */
[[nodiscard]] inline std::string
step_stream_name(const ores::nats::service::client& nats, std::string_view service_name) {
    constexpr std::string_view ores_prefix = "ores.";
    constexpr std::string_view service_suffix = ".service";
    if (service_name.starts_with(ores_prefix))
        service_name.remove_prefix(ores_prefix.size());
    if (service_name.ends_with(service_suffix))
        service_name.remove_suffix(service_suffix.size());
    return nats.make_stream_name(std::string(service_name) + "_steps");
}

/**
 * @brief The durable name for the consumer of one step-command subject.
 *
 * A durable is identified by its name, so the name is derived from the subject
 * it reads: stable across restarts, and unique within the service's stream. A
 * JetStream name may not contain a dot, so every character outside the allowed
 * set becomes a dash.
 */
[[nodiscard]] inline std::string step_durable_name(std::string_view subject) {
    std::string name;
    name.reserve(subject.size());
    for (const char c : subject)
        name += std::isalnum(static_cast<unsigned char>(c)) != 0 ? c : '-';
    return name;
}

/**
 * @brief Creates the stream that holds a service's step commands.
 *
 * The workflow engine dispatches a step command with @c js_publish, so the
 * subject has to be covered by a stream or the dispatch waits out the pub-ack
 * timeout and the run fails with nothing to explain it. The service that serves
 * the subject declares the coverage, from the same list it subscribes, so the
 * two cannot disagree.
 *
 * This is the one place a service should create that stream: it names the
 * stream, builds the filters the subjects need, and sets a retention suited to
 * a command rather than to a data stream. Call it during startup, before the
 * registrar subscribes, because a subject is not durable until its stream is.
 *
 * @param subjects The service's step-command subjects, relative and unprefixed.
 * @return the stream name, to pass to @c js_queue_subscribe.
 * @throws std::runtime_error when the stream cannot be created. The caller
 *         decides whether that is fatal; for a service whose steps are its
 *         work, it is, because no step can then be delivered.
 */
template <typename Range>
[[nodiscard]] inline std::string ensure_step_stream(ores::nats::service::client& nats,
                                                    std::string_view service_name,
                                                    const Range& subjects) {
    const std::string stream = step_stream_name(nats, service_name);
    nats.make_admin().ensure_stream(
        stream, nats.covering_subjects(subjects), step_stream_max_age_days);
    return stream;
}

/**
 * @brief Subscribes one step command durably, on the service's own stream.
 *
 * The durable name, the stream and the delivery bound are the same three
 * arguments at every call site, and they have to agree with each other: a
 * subscribe that names the wrong stream reads nothing, and one that omits the
 * bound redelivers a poison command for ever. This is the one place they are
 * assembled, so a service's registrar says only which subject and which
 * handler.
 *
 * @param stream the name @c ensure_step_stream returned.
 */
template <typename Handler>
[[nodiscard]] inline ores::nats::service::subscription
subscribe_step_command(ores::nats::service::client& nats,
                       std::string_view subject,
                       std::string_view queue_group,
                       std::string_view stream,
                       Handler handler) {
    return nats.js_queue_subscribe(subject,
                                   step_durable_name(subject),
                                   queue_group,
                                   std::move(handler),
                                   stream,
                                   step_max_deliver);
}

}

#endif
