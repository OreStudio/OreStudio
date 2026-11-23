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
#ifndef ORES_HTTP_API_MCP_ENDPOINT_HPP
#define ORES_HTTP_API_MCP_ENDPOINT_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/export.hpp"
#include "ores.http.api/mcp/session_registry.hpp"
#include "ores.http.api/mcp/tools.hpp"
#include "ores.logging/logger.hpp"
#include <boost/asio/awaitable.hpp>
#include <memory>
#include <optional>
#include <string>
#include <vector>

namespace ores::http::net {
class router;
}

namespace ores::http::openapi {
class endpoint_registry;
}

namespace ores::http::mcp {

/**
 * @brief What an MCP endpoint states about itself, and whom it serves.
 */
struct endpoint_options final {
    std::string server_name = "ores.http";
    std::string server_version;
    std::string instructions;
    /**
     * @brief The origins a browser may call from, beside this machine's own.
     * A request that names any other origin is refused, against DNS rebinding;
     * a client that is not a browser names none.
     */
    std::vector<std::string> allowed_origins;
};

/**
 * @brief MCP's Streamable HTTP transport, served at one path.
 *
 * POST carries one JSON-RPC 2.0 message and is answered with JSON. GET is
 * answered with 405, because the server sends nothing unasked. DELETE ends a
 * session. The methods served are initialize, notifications/initialized, ping,
 * tools/list and tools/call; every other method is answered as not found.
 *
 * Only an LLM agent is served: the route authenticates its bearer value, and
 * a token that names no agent is refused with 401.
 */
class ORES_HTTP_API_EXPORT endpoint final {
private:
    inline static std::string_view logger_name = "ores.http.mcp.endpoint";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /// The protocol revisions served, the latest first.
    static constexpr std::string_view latest_version = "2025-11-25";

    endpoint(std::shared_ptr<const tool_provider> tools,
             endpoint_options options,
             std::shared_ptr<session_registry> sessions = std::make_shared<session_registry>());

    [[nodiscard]] boost::asio::awaitable<domain::http_response>
    post(const domain::http_request& req) const;

    [[nodiscard]] domain::http_response get(const domain::http_request& req) const;

    [[nodiscard]] domain::http_response remove(const domain::http_request& req) const;

    /**
     * @brief Whether a protocol revision is one this endpoint serves.
     */
    [[nodiscard]] static bool serves_version(std::string_view version);

private:
    [[nodiscard]] std::optional<domain::http_response>
    refusal(const domain::http_request& req) const;

    std::shared_ptr<const tool_provider> tools_;
    endpoint_options options_;
    std::shared_ptr<session_registry> sessions_;
};

/**
 * @brief Serves an endpoint at /mcp.
 *
 * The routes are MCP's transport: they change nothing themselves, and each
 * call they carry is judged as its own route, so the gateway neither holds
 * nor records a write against /mcp.
 */
ORES_HTTP_API_EXPORT void
register_routes(const std::shared_ptr<const endpoint>& served,
                const std::shared_ptr<net::router>& router,
                const std::shared_ptr<openapi::endpoint_registry>& registry);

}

#endif
