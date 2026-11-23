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
#ifndef ORES_HTTP_API_MCP_TOOLS_HPP
#define ORES_HTTP_API_MCP_TOOLS_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.utility/codec/json_document.hpp"
#include <boost/asio/awaitable.hpp>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::mcp {

/**
 * @brief A tool as MCP's tools/list states it.
 *
 * The input schema is a JSON Schema object, as text, so a tool can take its
 * schema from a route's declared body without a second description of it.
 */
struct tool final {
    std::string name;
    std::string title;
    std::string description;
    std::string input_schema = R"({"type":"object"})";
    bool read_only = true;
};

/**
 * @brief What a tool answers: text for the model, and whether the call failed.
 *
 * A failure the model can act on, such as a refused write, is a result with
 * is_error set, not a protocol error, so the model sees why.
 */
struct tool_result final {
    std::string text;
    bool is_error = false;
};

/**
 * @brief The tools one MCP endpoint serves.
 *
 * Both calls take the request that carries the call, so a provider answers
 * for the person and agent its claims name.
 */
class tool_provider {
public:
    virtual ~tool_provider() = default;

    [[nodiscard]] virtual std::vector<tool> list(const domain::http_request& caller) const = 0;

    /**
     * @brief Calls a tool by name. Answers nothing when no tool has the name.
     */
    virtual boost::asio::awaitable<std::optional<tool_result>>
    call(const domain::http_request& caller,
         std::string_view name,
         const ores::utility::codec::json_value& arguments) const = 0;
};

}

#endif
