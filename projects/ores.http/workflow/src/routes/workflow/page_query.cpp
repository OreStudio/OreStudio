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
#include "ores.http/routes/workflow/page_query.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include <charconv>
#include <optional>
#include <string>

namespace ores::http::routes::workflow {

namespace {

namespace rr = ores::http::net::record_routes;

/// The whole number a parameter states, or nothing when it states none.
std::optional<std::uint32_t> whole_of(const std::string& raw, std::string_view refused) {
    if (raw.empty())
        return std::nullopt;
    std::uint32_t value = 0;
    const auto* first = raw.data();
    const auto* last = raw.data() + raw.size();
    const auto parsed = std::from_chars(first, last, value);
    if (parsed.ec != std::errc() || parsed.ptr != last)
        rr::invalid(std::string(refused));
    return value;
}

}

page page_of(const ores::http::domain::http_request& req,
             std::uint32_t page_default,
             std::uint32_t page_max,
             std::string_view refused) {
    rr::refuse_unknown_params(req, {"offset", "limit"});
    page out;
    out.limit = page_default;
    if (const auto stated = whole_of(req.get_query_param("offset"), refused))
        out.offset = *stated;
    if (const auto stated = whole_of(req.get_query_param("limit"), refused)) {
        if (*stated < 1 || *stated > page_max)
            rr::invalid(std::string(refused));
        out.limit = *stated;
    }
    return out;
}

}
