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
#include "ores.telemetry/messaging/registrar.hpp"

#include <span>
#include <string_view>
#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.nats/service/client.hpp"
#include "ores.telemetry/messaging/telemetry_protocol.hpp"
#include "ores.telemetry/messaging/nats_samples_protocol.hpp"

namespace ores::telemetry::messaging {

namespace {

template<typename Resp>
void reply(ores::nats::service::client& nats,
           const ores::nats::message& msg,
           const Resp& resp) {
    if (msg.reply_subject.empty())
        return;
    const auto json = rfl::json::write(resp);
    const auto* p = reinterpret_cast<const std::byte*>(json.data());
    nats.publish(msg.reply_subject, std::span<const std::byte>(p, json.size()));
}

template<typename Req>
std::optional<Req> decode(const ores::nats::message& msg) {
    const std::string_view sv(
        reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto r = rfl::json::read<Req>(sv);
    if (!r)
        return std::nullopt;
    return *r;
}

} // namespace

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
    ores::database::context /*ctx*/) {
    std::vector<ores::nats::service::subscription> subs;

    subs.push_back(nats.queue_subscribe(
        "telemetry.v1.>", "ores.telemetry.service",
        [&nats](ores::nats::message msg) mutable {
            const auto& subj = msg.subject;

            // ----------------------------------------------------------------
            // Telemetry logs — list
            // ----------------------------------------------------------------
            if (subj.ends_with(".logs.list")) {
                // No telemetry query service implemented yet; return empty.
                if (decode<get_telemetry_logs_request>(msg)) {
                    reply(nats, msg, get_telemetry_logs_response{
                        .success = true});
                }
            }
            // ----------------------------------------------------------------
            // NATS server samples — list
            // ----------------------------------------------------------------
            else if (subj.ends_with(".nats.server-samples.list")) {
                if (decode<get_nats_server_samples_request>(msg)) {
                    reply(nats, msg, get_nats_server_samples_response{
                        .success = true});
                }
            }
            // ----------------------------------------------------------------
            // NATS stream samples — list
            // ----------------------------------------------------------------
            else if (subj.ends_with(".nats.stream-samples.list")) {
                if (decode<get_nats_stream_samples_request>(msg)) {
                    reply(nats, msg, get_nats_stream_samples_response{
                        .success = true});
                }
            }
        }));

    return subs;
}

} // namespace ores::telemetry::messaging
