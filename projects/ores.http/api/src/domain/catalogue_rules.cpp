/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.http.api/domain/catalogue_rules.hpp"
#include <algorithm>
#include <array>
#include <utility>

namespace ores::http::domain {

namespace {

/// The published areas, as design section 11.2 names them.
constexpr auto areas = std::to_array<std::string_view>({"access",
                                                        "agile",
                                                        "analytics",
                                                        "assets",
                                                        "auth",
                                                        "changes",
                                                        "grid",
                                                        "market-data",
                                                        "operations",
                                                        "qa",
                                                        "reference-data",
                                                        "reporting",
                                                        "simulation",
                                                        "tenancy",
                                                        "trading",
                                                        "workflow"});

/// The segment of a pattern after /api/v1/, or empty.
std::string_view first_api_segment(std::string_view pattern) {
    constexpr std::string_view prefix = "/api/v1/";
    if (!pattern.starts_with(prefix))
        return {};
    pattern.remove_prefix(prefix.size());
    return pattern.substr(0, pattern.find('/'));
}

}

std::span<const std::string_view> published_areas() {
    return areas;
}

bool is_published_area(std::string_view area) {
    return std::ranges::find(areas, area) != areas.end();
}

bool may_declare_read(const route_catalogue& catalogue) {
    return catalogue.tool.starts_with("get_many_") || operation_only_reads(catalogue.tool);
}

std::string area_of_component(std::string_view component) {
    static constexpr auto table = std::to_array<std::pair<std::string_view, std::string_view>>({
        {"iam", "access"},
        {"refdata", "reference-data"},
        {"dq", "reference-data"},
        {"marketdata", "market-data"},
        {"trading", "trading"},
        {"reporting", "reporting"},
        {"ore", "reporting"},
        {"synthetic", "simulation"},
        {"compute", "grid"},
        {"workflow", "workflow"},
        {"inbox", "workflow"},
        {"assets", "assets"},
        {"analytics", "analytics"},
        {"variability", "operations"},
        {"telemetry", "operations"},
        {"scheduler", "operations"},
        {"storage", "operations"},
        {"http", "operations"},
    });
    for (const auto& [name, area] : table)
        if (name == component)
            return std::string(area);
    return std::string(component);
}

bool operation_only_reads(std::string_view name) {
    if (name.find("_or_") != std::string_view::npos)
        return false;
    for (const std::string_view verb : {"list_", "get_", "read_", "find_", "search_", "lookup_"})
        if (name.starts_with(verb))
            return true;
    return false;
}

route_catalogue entity_route_catalogue(http_method method,
                                       std::string_view pattern,
                                       std::string_view component,
                                       std::string_view entity,
                                       std::string_view identifier) {
    route_catalogue c;
    const auto segment = first_api_segment(pattern);
    c.area = is_published_area(segment) ? std::string(segment) : area_of_component(component);
    c.entity = std::string(entity);
    c.tool = std::string(identifier) + "_" + std::string(entity);
    c.writes = method != http_method::get && method != http_method::head &&
               identifier != "get_many" && !operation_only_reads(identifier);
    return c;
}

route_catalogue operation_route_catalogue(std::string_view component,
                                          std::string_view entity,
                                          std::string_view name) {
    route_catalogue c;
    c.area = area_of_component(component);
    c.entity = std::string(entity);
    c.tool = std::string(name);
    c.writes = !operation_only_reads(name);
    return c;
}

}
