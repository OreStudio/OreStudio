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
#include "ores.http.server/app/catalogue_tools.hpp"
#include "ores.http.api/domain/catalogue_rules.hpp"
#include "ores.utility/codec/json_document.hpp"

namespace ores::http::server::app {

namespace {

constexpr std::string_view list_areas = "list_areas";

}

std::vector<mcp::tool> catalogue_tools::list(const domain::http_request&) const {
    return {mcp::tool{
        .name = std::string(list_areas),
        .title = "List areas",
        .description = "Lists the areas of ORE's operations, the first step into its catalogue.",
        .input_schema = R"({"type":"object","properties":{},"additionalProperties":false})",
        .read_only = true}};
}

boost::asio::awaitable<std::optional<mcp::tool_result>>
catalogue_tools::call(const domain::http_request&,
                      std::string_view name,
                      const ores::utility::codec::json_value&) const {
    if (name != list_areas)
        co_return std::nullopt;
    auto doc = ores::utility::codec::json_document::make_object();
    auto areas = doc.root().add_array("areas");
    for (const auto area : domain::published_areas())
        areas.push(area);
    co_return mcp::tool_result{.text = doc.write()};
}

}
