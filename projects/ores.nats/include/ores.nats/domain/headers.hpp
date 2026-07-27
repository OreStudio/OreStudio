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

#include <string_view>

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

/// Session-level trace key generated once on Qt client login.
/// Forwarded on every outbound NATS message for the duration of the login
/// session, enabling log queries that group all requests from a single
/// user session (e.g. "show all calls made during session X").
/// Distinct from Nats-Correlation-Id which is per top-level operation.
inline constexpr std::string_view nats_session_id = "Nats-Session-Id";

/// Active workspace for this request.
/// Set by the Qt client from WorkspaceContext.id; forwarded on inter-service
/// calls.  Absence means Live workspace (aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa).
inline constexpr std::string_view x_workspace_id = "X-Workspace-Id";

/// Workspace resolution chain for this request (comma-separated UUID list).
/// Set by the Qt client from WorkspaceContext.resolution_order when the
/// selected workspace is not Live.  Enables repository-level workspace
/// inheritance: queries use WHERE workspace_id = ANY(chain) with deduplication
/// so that definitions from ancestor workspaces are included.
/// Absence means single-workspace query (workspace_id exact match only).
inline constexpr std::string_view x_workspace_resolution = "X-Workspace-Resolution";

/// Transparent payload compression marker. When present with value
/// @c content_encoding_gzip, the message body is gzip-compressed on the
/// wire. Set automatically by client::publish()/request_sync()/js_publish()
/// for payloads at or above the compression threshold; extract_message()
/// checks for it and decompresses transparently, so every consumer of
/// ores::nats::message always sees the original, uncompressed bytes
/// regardless of how the sender decided to transmit them. No handler or
/// protocol type needs to know this header exists.
inline constexpr std::string_view x_content_encoding = "X-Content-Encoding";

/// Value of x_content_encoding when the body is gzip-compressed.
inline constexpr std::string_view content_encoding_gzip = "gzip";

} // namespace ores::nats::headers

#endif
