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
#ifndef ORES_VARIABILITY_MESSAGING_SYSTEM_SETTINGS_PROTOCOL_HPP
#define ORES_VARIABILITY_MESSAGING_SYSTEM_SETTINGS_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>
#include "ores.variability/domain/system_setting.hpp"

namespace ores::variability::messaging {

// ---------------------------------------------------------------------------
// List
// ---------------------------------------------------------------------------

struct list_settings_request {
    using response_type = struct list_settings_response;
    static constexpr std::string_view nats_subject =
        "variability.v1.settings.list";
};

struct list_settings_response {
    std::vector<ores::variability::domain::system_setting> settings;
};

// ---------------------------------------------------------------------------
// Save
// ---------------------------------------------------------------------------

struct save_setting_request {
    using response_type = struct save_setting_response;
    static constexpr std::string_view nats_subject =
        "variability.v1.settings.save";
    ores::variability::domain::system_setting data;

    static save_setting_request from(ores::variability::domain::system_setting s) {
        return { .data = std::move(s) };
    }
};

struct save_setting_response {
    bool success = false;
    std::string message;
};

// ---------------------------------------------------------------------------
// Delete
// ---------------------------------------------------------------------------

struct delete_setting_request {
    using response_type = struct delete_setting_response;
    static constexpr std::string_view nats_subject =
        "variability.v1.settings.delete";
    std::string name;
};

struct delete_setting_response {
    bool success = false;
    std::string error_message;
};

// ---------------------------------------------------------------------------
// History
// ---------------------------------------------------------------------------

struct get_setting_history_request {
    using response_type = struct get_setting_history_response;
    static constexpr std::string_view nats_subject =
        "variability.v1.settings.history";
    std::string name;
};

struct get_setting_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::variability::domain::system_setting> history;
};

}

#endif
