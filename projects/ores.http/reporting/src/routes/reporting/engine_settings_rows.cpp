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
#include "ores.http/routes/reporting/engine_settings_rows.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.reporting.api/messaging/engine_settings_group_protocol.hpp"
#include "ores.reporting.api/messaging/engine_settings_group_setting_protocol.hpp"
#include <algorithm>
#include <array>
#include <map>
#include <utility>

namespace ores::http::routes::reporting {

namespace {

namespace forwarding = ores::http::forwarding;
namespace reporting_messaging = ores::reporting::messaging;
using ores::http::domain::http_request;
using ores::nats::service::nats_client;

constexpr std::array<std::string_view, 3> wireable_groups{"building", "execution", "fixings"};

}

namespace engine_settings_rows {

std::span<const std::string_view> wired_groups() {
    return wireable_groups;
}

std::vector<engine_group_row> read_groups(const http_request& req, nats_client& session) {
    auto groups =
        forwarding::forward_all(req,
                                session,
                                reporting_messaging::list_engine_settings_groups_request{},
                                &reporting_messaging::list_engine_settings_groups_response::groups);
    auto settings = forwarding::forward_all(
        req,
        session,
        reporting_messaging::list_engine_settings_group_settings_request{},
        &reporting_messaging::list_engine_settings_group_settings_response::settings);

    std::ranges::stable_sort(
        settings, {}, &ores::reporting::domain::engine_settings_group_setting::position);
    std::map<std::string, std::vector<engine_setting_row>> of_group;
    for (auto& row : settings)
        of_group[row.group_code].push_back(engine_setting_row{
            std::move(row.column_name), std::move(row.ore_name), row.changes_numbers});

    std::ranges::stable_sort(groups, {}, &ores::reporting::domain::engine_settings_group::position);
    std::vector<engine_group_row> rows;
    rows.reserve(groups.size());
    for (auto& group : groups) {
        if (std::ranges::find(wireable_groups, group.code) == wireable_groups.end())
            continue;
        engine_group_row row;
        row.code = std::move(group.code);
        const auto found = of_group.find(row.code);
        if (found != of_group.end())
            row.settings = std::move(found->second);
        row.changes_numbers =
            std::ranges::any_of(row.settings, [](const engine_setting_row& setting) {
                return setting.changes_numbers;
            });
        rows.push_back(std::move(row));
    }
    return rows;
}

}

}
