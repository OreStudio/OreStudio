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

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/correlation.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/authorization/grants.hpp"
#include "ores.service/error_code.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>

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
template <typename Logger>
inline std::string log_handler_entry(Logger& lg, const ores::nats::message& msg) {
    using namespace ores::logging;
    auto cid = ores::nats::extract_or_generate_correlation_id(msg);
    const auto sid_it = msg.headers.find(std::string(ores::nats::headers::nats_session_id));
    if (sid_it != msg.headers.end() && !sid_it->second.empty())
        BOOST_LOG_SEV(lg, info) << msg.subject << " correlation_id=" << cid
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
 * @brief The bearer token an inbound message carries, or an empty string.
 *
 * A handler that has to know its caller's account validates this token itself.
 * The request context names who the caller is in words -- the actor's name and
 * the tenant and party the session is in -- and an account identifier is not
 * among them, so a handler that needs one reads the token's subject.
 */
inline std::string bearer_token(const ores::nats::message& msg) {
    const auto found = msg.headers.find(std::string(ores::nats::headers::authorization));
    if (found == msg.headers.end())
        return {};
    const auto& value = found->second;
    if (!value.starts_with(ores::nats::headers::bearer_prefix))
        return {};
    return value.substr(ores::nats::headers::bearer_prefix.size());
}

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
 * @warning This is a match-by-field-name reflection helper: it treats any
 * field named tenant_id or party_id as "the scope this row belongs to" and
 * overwrites it from the caller's context. That is right for almost every
 * domain type, and wrong for a type whose similarly named field means
 * something else, such as the target of an operation. Check a new type's
 * fields for a security-boundary name with a different meaning, and write an
 * explicit stamp function for that type instead of reaching for this one;
 * ores.iam.core/messaging/account_party_handler.hpp is the worked case.
 *
 * @param obj           Domain object to stamp (modified in place).
 * @param ctx           Per-request database context derived from the JWT.
 * @param change_reason Default change reason code (applied only if obj has none).
 */
template <typename T>
void stamp(T& obj,
           const ores::database::context& ctx,
           std::string_view change_reason = change_reasons::new_record) {
    // tenant_id and party_id are security boundaries — always derived from the
    // validated JWT context, never from client-supplied data.
    if constexpr (requires { obj.tenant_id; }) {
        if constexpr (std::is_assignable_v<decltype(obj.tenant_id)&, std::string>)
            obj.tenant_id = ctx.tenant_id().to_string();
        else if constexpr (std::is_assignable_v<decltype(obj.tenant_id)&, boost::uuids::uuid>)
            obj.tenant_id = ctx.tenant_id().to_uuid();
        else
            obj.tenant_id = ctx.tenant_id();
    }
    if constexpr (requires { obj.party_id; }) {
        if (const auto pid = ctx.party_id(); pid.has_value()) {
            if constexpr (std::is_assignable_v<decltype(obj.party_id)&, boost::uuids::uuid>)
                obj.party_id = *pid;
            else if constexpr (std::is_assignable_v<decltype(obj.party_id)&, std::string>)
                obj.party_id = boost::uuids::to_string(*pid);
        }
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

template <typename Resp>
void reply(ores::nats::service::client& nats, const ores::nats::message& msg, const Resp& resp) {
    if (msg.reply_subject.empty())
        return;
    const auto bytes = ores::nats::default_wire_codec().encode(resp);
    nats.publish(msg.reply_subject, std::span<const std::byte>(bytes));
}

/**
 * @brief The context a read acts in, for a party the request names.
 *
 * A session that acts for a party reads as that party, whatever the request
 * names. A session that acts for none, as a workflow step's service session
 * does, sees every party's rows, so a request that names a party narrows the
 * read to it. An empty party leaves the context unchanged; one that does not
 * parse throws, rather than quietly reading every party.
 */
inline ores::database::context for_requested_party(const ores::database::context& ctx,
                                                   const std::string& party_id) {
    if (ctx.party_id() || party_id.empty())
        return ctx;
    boost::uuids::uuid party;
    try {
        party = boost::uuids::string_generator()(party_id);
    } catch (const std::exception&) {
        throw std::invalid_argument("Not a party id: " + party_id);
    }
    return ctx.with_party(ctx.tenant_id(), party, {party}, ctx.actor());
}

/**
 * @brief Throws unless the session acts for a party.
 *
 * A write stamps the session's party on what it stores and acts only on that
 * party's rows. A session that acts for no party sees every party, so letting
 * it write would let it change any party's documents.
 */
inline void require_party(const ores::database::context& ctx) {
    if (!ctx.party_id())
        throw std::invalid_argument(
            "A session that acts for no party cannot change configuration documents; act for "
            "the party that owns them.");
}

/**
 * @brief Checks whether the request context carries a required permission.
 *
 * A context built from a token carries the token's permission list, and the
 * check passes only when that list grants the permission; an empty list
 * grants nothing. A context that carries no list is the service's own,
 * derived from its base context, and passes.
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
    const auto& granted = ctx.roles();
    if (!granted)
        return true;
    return ores::security::authorization::grants(*granted, required_permission);
}

/**
 * @brief Publishes an error reply carrying the X-Error header.
 *
 * The reply subject is used if present, and the call is a no-op otherwise.
 */
inline void error_reply(ores::nats::service::client& nats,
                        const ores::nats::message& msg,
                        ores::service::error_code code) {
    if (msg.reply_subject.empty())
        return;
    std::string_view error_str;
    switch (code) {
        case ores::service::error_code::token_expired:
            error_str = "token_expired";
            break;
        case ores::service::error_code::forbidden:
            error_str = "forbidden";
            break;
        case ores::service::error_code::bad_request:
            error_str = "bad_request";
            break;
        default:
            error_str = "unauthorized";
            break;
    }
    nats.publish(msg.reply_subject,
                 std::span<const std::byte>{},
                 {{std::string(ores::nats::headers::x_error), std::string(error_str)}});
}

namespace {
inline auto& decode_lg() {
    static auto instance = ores::logging::make_logger("ores.service.messaging.decode");
    return instance;
}
} // namespace

/**
 * @brief Deserialises the message payload into Req using the process-wide
 * default wire_codec. Returns nullopt on parse failure.
 *
 * The failure is logged with the codec's own reason before it is discarded:
 * the caller can only tell the peer "bad_request", so this line is the only
 * account of what the request got wrong. The payload itself is deliberately
 * not logged — a rejected request can carry a credential — but the reason
 * names the field that did not fit, which is what a mismatch between two
 * ends of the protocol turns on.
 */
template <typename Req>
std::optional<Req> decode(const ores::nats::message& msg) {
    using namespace ores::logging;
    auto r = ores::nats::default_wire_codec().decode<Req>(msg.data);
    if (!r) {
        BOOST_LOG_SEV(decode_lg(), warn) << "Failed to decode " << msg.subject << " ("
                                         << msg.data.size() << " bytes): " << r.error().what();
        return std::nullopt;
    }
    return *r;
}

} // namespace ores::service::messaging

#endif
