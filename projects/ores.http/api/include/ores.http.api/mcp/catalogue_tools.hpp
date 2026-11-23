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
#ifndef ORES_HTTP_API_MCP_CATALOGUE_TOOLS_HPP
#define ORES_HTTP_API_MCP_CATALOGUE_TOOLS_HPP

#include "ores.http.api/domain/bearer_exchange.hpp"
#include "ores.http.api/export.hpp"
#include "ores.http.api/mcp/operation_catalogue.hpp"
#include "ores.http.api/mcp/tools.hpp"
#include "ores.logging/logger.hpp"
#include <memory>

namespace ores::http::mcp {

/**
 * @brief What the catalogue's tools need from the gateway around them.
 *
 * An agent's call through call_operation takes the path its direct call
 * would: a write is offered to the holder, and every call is recorded against
 * the route it reached.
 */
struct catalogue_tools_options final {
    domain::exchanged_write_holder hold;
    domain::exchanged_call_recorder record;
    /// The most lines one list answers.
    std::size_t page_size = 40;
    /// The most characters of a result one answer carries.
    std::size_t answer_limit = 8000;
};

/**
 * @brief The five tools through which an LLM agent walks and calls ORE's
 * operations: list_areas, list_entities, list_operations, find_operations
 * and call_operation.
 *
 * The tools are the same whatever the catalogue holds. Every list shows only
 * what the agent's grant reaches: reads alone when the grant only reads, and
 * an operation whose permission the catalogue knows only when the token
 * grants it.
 */
class ORES_HTTP_API_EXPORT catalogue_tools final : public tool_provider {
private:
    inline static std::string_view logger_name = "ores.http.mcp.catalogue_tools";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    catalogue_tools(std::shared_ptr<const operation_catalogue> catalogue,
                    catalogue_tools_options options);

    [[nodiscard]] std::vector<tool> list(const domain::http_request& caller) const override;

    boost::asio::awaitable<std::optional<tool_result>>
    call(const domain::http_request& caller,
         std::string_view name,
         ores::utility::codec::json_value arguments) const override;

    /**
     * @brief Whether a caller's token may write: it holds a permission that is
     * not a read.
     */
    [[nodiscard]] static bool may_write(const domain::http_request& caller);

    /**
     * @brief Whether a caller may call an operation: a write only when its
     * token may write, and an operation whose permission the catalogue knows
     * only when the token grants it and it is not withheld from agents.
     */
    [[nodiscard]] static bool reaches(const domain::http_request& caller, const operation& op);

private:
    [[nodiscard]] tool_result areas(const domain::http_request& caller) const;
    [[nodiscard]] tool_result entities(const domain::http_request& caller,
                                       const ores::utility::codec::json_value& arguments) const;
    [[nodiscard]] tool_result operations(const domain::http_request& caller,
                                         const ores::utility::codec::json_value& arguments) const;
    [[nodiscard]] tool_result found(const domain::http_request& caller,
                                    const ores::utility::codec::json_value& arguments) const;
    boost::asio::awaitable<tool_result> called(const domain::http_request& caller,
                                               ores::utility::codec::json_value arguments) const;

    std::shared_ptr<const operation_catalogue> catalogue_;
    catalogue_tools_options options_;
};

}

#endif
