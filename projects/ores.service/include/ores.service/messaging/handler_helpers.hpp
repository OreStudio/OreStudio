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
#ifndef ORES_SERVICE_MESSAGING_HANDLER_HELPERS_HPP
#define ORES_SERVICE_MESSAGING_HANDLER_HELPERS_HPP

#include <optional>
#include <span>
#include <string_view>
#include <rfl/json.hpp>
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.utility/rfl/reflectors.hpp"

namespace ores::service::messaging {

/**
 * @brief Default change reason codes for server-stamped writes.
 */
namespace change_reasons {
    inline constexpr std::string_view new_record = "system.new_record";
    inline constexpr std::string_view update = "system.update";
} // namespace change_reasons

/**
 * @brief Stamps audit fields on a domain object from the request context.
 *
 * Sets modified_by from ctx.actor() (the end-user who initiated the request)
 * and performed_by from ctx.service_account() (the system service executing
 * the write). Sets change_reason_code to the given default if the object has
 * not supplied one. Always overwrites these fields to prevent client spoofing.
 *
 * @param obj           Domain object to stamp (modified in place).
 * @param ctx           Per-request database context carrying actor and service
 *                      account.
 * @param change_reason Default change reason code (applied only if obj has none).
 */
template<typename T>
void stamp(T& obj, const ores::database::context& ctx,
    std::string_view change_reason = change_reasons::new_record) {
    const auto& actor = ctx.actor();
    if (!actor.empty()) {
        if constexpr (requires { obj.modified_by; })
            obj.modified_by = actor;
    }
    const auto& svc = ctx.service_account();
    if (!svc.empty()) {
        if constexpr (requires { obj.performed_by; })
            obj.performed_by = svc;
    }
    if constexpr (requires { obj.change_reason_code; }) {
        if (obj.change_reason_code.empty())
            obj.change_reason_code = std::string(change_reason);
    }
}

// Serialise resp to JSON and publish to the message's reply subject.
// No-op if the message has no reply subject.
template<typename Resp>
void reply(ores::nats::service::client& nats,
    const ores::nats::message& msg,
    const Resp& resp) {
    if (msg.reply_subject.empty()) return;
    const auto json = rfl::json::write(resp);
    const auto* p = reinterpret_cast<const std::byte*>(json.data());
    nats.publish(msg.reply_subject, std::span<const std::byte>(p, json.size()));
}

// Deserialise the message payload from JSON into Req.
// Returns nullopt on parse failure.
template<typename Req>
std::optional<Req> decode(const ores::nats::message& msg) {
    const std::string_view sv(
        reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto r = rfl::json::read<Req>(sv);
    if (!r) return std::nullopt;
    return *r;
}

} // namespace ores::service::messaging

#endif
