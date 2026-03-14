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
#ifndef ORES_VARIABILITY_MESSAGING_FEATURE_FLAGS_PROTOCOL_HPP
#define ORES_VARIABILITY_MESSAGING_FEATURE_FLAGS_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>
#include "ores.variability/domain/feature_flags.hpp"

namespace ores::variability::messaging {

struct get_feature_flags_request {
    using response_type = struct get_feature_flags_response;
    static constexpr std::string_view nats_subject = "ores.variability.v1.feature-flags.list";
};

struct get_feature_flags_response {
    std::vector<ores::variability::domain::feature_flags> feature_flags;
};

struct save_feature_flag_request {
    using response_type = struct save_feature_flag_response;
    static constexpr std::string_view nats_subject = "ores.variability.v1.feature-flags.save";
    ores::variability::domain::feature_flags data;

    static save_feature_flag_request from(ores::variability::domain::feature_flags ff) {
        return { .data = std::move(ff) };
    }
};

struct save_feature_flag_response {
    bool success = false;
    std::string message;
};

struct delete_feature_flag_request {
    using response_type = struct delete_feature_flag_response;
    static constexpr std::string_view nats_subject = "ores.variability.v1.feature-flags.delete";
    std::string name;
};

struct delete_feature_flag_response {
    bool success = false;
    std::string error_message;
};

struct get_feature_flag_history_request {
    using response_type = struct get_feature_flag_history_response;
    static constexpr std::string_view nats_subject = "ores.variability.v1.feature-flags.history";
    std::string name;
};

struct get_feature_flag_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::variability::domain::feature_flags> history;
};

}

#endif
