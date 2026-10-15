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
#ifndef ORES_NATS_DOMAIN_HEADERS_HPP
#define ORES_NATS_DOMAIN_HEADERS_HPP

#include <string>
#include <string_view>
#include <unordered_map>

namespace ores::nats::headers {

/// Standard HTTP/NATS authorisation header.
inline constexpr std::string_view authorization = "Authorization";

/// Delegation header: the calling service forwards the original end-user JWT
/// verbatim so the receiving service can build the correct DB context
/// (tenant, party, actor, roles) from the user's claims.
inline constexpr std::string_view delegated_authorization = "X-Delegated-Authorization";

/// Prefix for Bearer token values in authorization headers.
inline constexpr std::string_view bearer_prefix = "Bearer ";

/// Server-sent error code header (e.g. "token_expired").
inline constexpr std::string_view x_error = "X-Error";

/// Application-level distributed trace key.
/// Generated once at the entry point of a top-level request and forwarded
/// on every downstream NATS call until the final response is returned.
/// Distinct from the NATS-level Nats-Msg-Id used by JetStream deduplication.
inline constexpr std::string_view nats_correlation_id = "Nats-Correlation-Id";

/// JetStream deduplication identity. Two publishes to the same stream with the
/// same value inside the stream's duplicate window are stored once.
///
/// Carry the identity of the *work*, never one minted per send: a step id, a
/// run id. A value that changes on every attempt — a fresh UUID, a timestamp —
/// defeats the mechanism, because the retry then looks like new work. A message
/// with no identity of its own (a market tick, a liveness notice) carries none,
/// because two such messages are genuinely two messages.
///
/// The window is finite, so this is the first of two lines of defence; the
/// receiver still needs its own guard. See the "Idempotent Message Identity"
/// pattern.
inline constexpr std::string_view nats_msg_id = "Nats-Msg-Id";

/// The headers that deduplicate a durable publish on @p identity.
///
/// Returns an empty map when @p identity is empty, so a message with no
/// identity of its own is published without one rather than under a shared or
/// invented value.
[[nodiscard]] inline std::unordered_map<std::string, std::string>
publish_identity_headers(std::string_view identity) {
    if (identity.empty())
        return {};
    return {{std::string(nats_msg_id), std::string(identity)}};
}

/// The same identity added to headers a caller already assembles.
///
/// A durable publish usually carries more than its identity — the forwarded
/// caller, a trace key — so this merges rather than replaces. An empty
/// @p identity leaves the headers untouched.
[[nodiscard]] inline std::unordered_map<std::string, std::string>
with_publish_identity(std::unordered_map<std::string, std::string> headers,
                      std::string_view identity) {
    if (!identity.empty())
        headers[std::string(nats_msg_id)] = std::string(identity);
    return headers;
}

/// Session-level trace key generated once on Qt client login.
/// Forwarded on every outbound NATS message for the duration of the login
/// session, enabling log queries that group all requests from a single
/// user session (e.g. "show all calls made during session X").
/// Distinct from Nats-Correlation-Id which is per top-level operation.
inline constexpr std::string_view nats_session_id = "Nats-Session-Id";

/// Transparent payload compression marker. When present with value
/// @c content_encoding_gzip, the message body is gzip-compressed on the
/// wire. Set automatically by client::publish()/request_sync()/js_publish()
/// for payloads at or above the compression threshold; extract_message()
/// checks for it and decompresses transparently, so every consumer of
/// ores::nats::message always sees the original, uncompressed bytes
/// regardless of how the sender decided to transmit them. No handler or
/// protocol type needs to know this header exists.
inline constexpr std::string_view x_content_encoding = "X-Content-Encoding";

/// The tenant that owns the row an entity event announces. Always present on
/// an entity event. A subscriber that serves people passes the event on only to
/// a reader of this tenant or of the system tenant.
inline constexpr std::string_view x_tenant_id = "X-Tenant-Id";

/// The party that owns the row an entity event announces. Present only on an
/// event of a party-owned table; an event without it concerns the whole tenant.
inline constexpr std::string_view x_party_id = "X-Party-Id";

/// Value of x_content_encoding when the body is gzip-compressed.
inline constexpr std::string_view content_encoding_gzip = "gzip";

} // namespace ores::nats::headers

#endif
