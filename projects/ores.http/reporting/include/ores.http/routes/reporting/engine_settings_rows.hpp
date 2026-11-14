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
#ifndef ORES_HTTP_ROUTES_REPORTING_ENGINE_SETTINGS_ROWS_HPP
#define ORES_HTTP_ROUTES_REPORTING_ENGINE_SETTINGS_ROWS_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <span>
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::routes::reporting {

/**
 * @brief One setting of one group: the column its value travels under and the
 * ORE parameter that documents it.
 */
struct engine_setting_row final {
    std::string column;
    std::string ore;

    /**
     * @brief Whether the setting moves the numbers a run computes, and is not
     * only operational.
     */
    bool changes_numbers = false;
};

/**
 * @brief One group of settings, with the settings the data places in it.
 */
struct engine_group_row final {
    /// The code of the group, which is the word its part travels under.
    std::string code;

    /**
     * @brief Whether a write to this group moves the numbers a run computes,
     * which is the declaration its settings carry.
     */
    bool changes_numbers = false;

    std::vector<engine_setting_row> settings;
};

/**
 * @brief The groups and the settings of the engine settings, read from the
 * service.
 *
 * A group and its settings are rows, never a change: the model states them, so
 * the screens and the writes both read them here rather than restating them.
 * The part a group's values live in is the service's wiring and not the model,
 * so a group the data holds and this gateway does not name is not offered: its
 * rows could not be read or written.
 */
namespace engine_settings_rows {

/**
 * @brief The groups whose part the gateway reads and writes, in the words a part
 * travels under.
 */
[[nodiscard]] std::span<const std::string_view> wired_groups();

/**
 * @brief The groups the data states, in the order it places them, with the
 * settings of each in the order it places those.
 */
[[nodiscard]] std::vector<engine_group_row> read_groups(const ores::http::domain::http_request& req,
                                                        ores::nats::service::nats_client& session);

}

}

#endif
