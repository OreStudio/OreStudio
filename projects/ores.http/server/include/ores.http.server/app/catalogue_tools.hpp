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
#ifndef ORES_HTTP_SERVER_APP_CATALOGUE_TOOLS_HPP
#define ORES_HTTP_SERVER_APP_CATALOGUE_TOOLS_HPP

#include "ores.http.api/mcp/tools.hpp"

namespace ores::http::server::app {

/**
 * @brief The tools through which an LLM agent walks ORE's operation catalogue.
 *
 * The first step is the published areas.
 */
class catalogue_tools final : public mcp::tool_provider {
public:
    [[nodiscard]] std::vector<mcp::tool> list(const domain::http_request& caller) const override;

    boost::asio::awaitable<std::optional<mcp::tool_result>>
    call(const domain::http_request& caller,
         std::string_view name,
         const ores::utility::codec::json_value& arguments) const override;
};

}

#endif
