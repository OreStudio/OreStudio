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
#include <boost/uuid/uuid.hpp>
#include <rfl/json.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/correlation.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include "ores.service/error_code.hpp"

namespace ores::service::messaging {

/**
 * @brief Extracts (or generates) the correlation ID from an inbound NATS
 * message and logs it at INFO level alongside the message subject.
 *
 * Call this at the entry of every handler method so that all log lines for a
 * single top-level request share the same correlation ID and can be grepped:
 *
 * @code
 *   [[maybe_unused]] const auto cid = log_handler_entry(my_lg(), msg);
 * @endcode
 *
 * The returned string can be threaded into downstream NATS calls via
 * nats_client::with_correlation_id(cid) when the handler makes outbound calls.
 */
template<typename Logger>
inline std::string log_handler_entry(Logger& lg,
    const ores::nats::message& msg) {
    using namespace ores::logging;
    auto cid = ores::nats::extract_or_generate_correlation_id(msg);
    const auto sid_it = msg.headers.find(
        std::string(ores::nats::headers::nats_session_id));
    if (sid_it != msg.headers.end() && !sid_it->second.empty())
        BOOST_LOG_SEV(lg, info) << msg.subject
            << " correlation_id=" << cid
            << " session_id=" << sid_it->second;
    else
        BOOST_LOG_SEV(lg, info) << msg.subject << " correlation_id=" << cid;
    return cid;
}

/**
 * @brief Default change reason codes for server-stamped writes.
 */
namespace change_reasons {
    inline constexpr std::string_view new_record = "system.new_record";
    inline constexpr std::string_view update = "system.update";
} // namespace change_reasons

/**
 * @brief Stamps server-authoritative fields on a domain object from the
 * request context.
 *
 * Fields stamped (where present on the object):
 *  - tenant_id: always overwritten from ctx.tenant_id() — never trusted
 *    from the client, as it is a security boundary enforced by RLS.
 *  - modified_by: overwritten from ctx.actor() (the authenticated user), or
 *    falls back to ctx.service_account() for system-initiated writes where
 *    there is no authenticated actor.
 *  - performed_by: overwritten from ctx.service_account().
 *  - change_reason_code: set to @p change_reason only if the object left
 *    it empty; a client-supplied code is preserved.
 *
 * All field assignments are guarded by if constexpr so the function compiles
 * for any domain type regardless of which fields it declares.
 *
 * @param obj           Domain object to stamp (modified in place).
 * @param ctx           Per-request database context derived from the JWT.
 * @param change_reason Default change reason code (applied only if obj has none).
 */
template<typename T>
void stamp(T& obj, const ores::database::context& ctx,
    std::string_view change_reason = change_reasons::new_record) {
    // tenant_id is a security boundary — always derived from the validated JWT,
    // never from client-supplied data.
    if constexpr (requires { obj.tenant_id; }) {
        if constexpr (std::is_assignable_v<decltype(obj.tenant_id)&, std::string>)
            obj.tenant_id = ctx.tenant_id().to_string();
        else if constexpr (std::is_assignable_v<decltype(obj.tenant_id)&,
                boost::uuids::uuid>)
            obj.tenant_id = ctx.tenant_id().to_uuid();
        else
            obj.tenant_id = ctx.tenant_id();
    }
    const auto& actor = ctx.actor();
    const auto& svc = ctx.service_account();
    if constexpr (requires { obj.modified_by; }) {
        if (!actor.empty())
            obj.modified_by = actor;
        else if (!svc.empty())
            obj.modified_by = svc;
    }
    if (!svc.empty()) {
        if constexpr (requires { obj.performed_by; })
            obj.performed_by = svc;
    }
    if constexpr (requires { obj.change_reason_code; }) {
        if (obj.change_reason_code.empty())
            obj.change_reason_code = std::string(change_reason);
    }
}

/**
 * @brief Returns the acting user from a request context.
 *
 * For user-facing handlers, returns ctx.actor() — the end-user extracted from
 * the validated JWT. For system handlers where no user actor is present, falls
 * back to ctx.service_account().
 *
 * This value is stamped as modified_by in service-to-service requests.
 * The receiving service trusts it as plain payload data; its integrity is
 * guaranteed by the calling service's authentication (service JWT) and
 * least-privilege RBAC on the internal service mesh.
 *
 * @param ctx  Per-request context populated from the validated inbound JWT.
 */
inline const std::string& delegated_actor(const ores::database::context& ctx) {
    return ctx.actor().empty() ? ctx.service_account() : ctx.actor();
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

/**
 * @brief Checks whether the request context carries a required permission.
 *
 * Returns true when:
 * - The context's permission list is empty (token predates RBAC enforcement;
 *   treated as pass-through to preserve backward compatibility with existing
 *   user sessions and un-migrated callers), OR
 * - The list contains @p required_permission exactly, OR
 * - The list contains the component-level wildcard that covers the permission
 *   (e.g. "iam::*" satisfies any "iam::…" permission).
 *
 * Usage in a write handler:
 * @code
 *   if (!has_permission(ctx, "iam::accounts:create")) {
 *       error_reply(nats_, msg, ores::service::error_code::forbidden);
 *       return;
 *   }
 * @endcode
 */
inline bool has_permission(const ores::database::context& ctx,
    std::string_view required_permission) {
    const auto& perms = ctx.roles();
    // Empty list: token predates RBAC — allow to preserve backward compat.
    if (perms.empty()) return true;
    // Exact match or component-level wildcard
    for (const auto& p : perms) {
        if (p == "*" || p == required_permission) return true;
        // Component-level wildcard: "iam::*" satisfies "iam::accounts:create"
        if (p.size() >= 2 && p.ends_with("::*")) {
            const auto prefix = std::string_view(p).substr(0, p.size() - 1);
            if (required_permission.starts_with(prefix)) return true;
        }
    }
    return false;
}

// Publish an error reply with an X-Error header.
// The reply subject is used if present; no-op otherwise.
inline void error_reply(ores::nats::service::client& nats,
    const ores::nats::message& msg,
    ores::service::error_code code) {
    if (msg.reply_subject.empty()) return;
    std::string_view error_str;
    switch (code) {
        case ores::service::error_code::token_expired: error_str = "token_expired"; break;
        case ores::service::error_code::forbidden:     error_str = "forbidden";     break;
        default:                                       error_str = "unauthorized";  break;
    }
    nats.publish(msg.reply_subject, std::span<const std::byte>{},
        {{std::string(ores::nats::headers::x_error), std::string(error_str)}});
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
