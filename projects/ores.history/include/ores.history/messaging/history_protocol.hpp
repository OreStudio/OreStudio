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
#ifndef ORES_HISTORY_MESSAGING_HISTORY_PROTOCOL_HPP
#define ORES_HISTORY_MESSAGING_HISTORY_PROTOCOL_HPP

#include "ores.diff/domain/diff_result.hpp"
#include "ores.diff/domain/field_value.hpp"
#include <chrono>
#include <string>
#include <string_view>
#include <vector>

namespace ores::history::messaging {

/**
 * @brief One rendered, diffed version of an entity's history.
 *
 * fields is the full render of this version, in mapper order — the
 * detail panel's need for complete values. changes is the field-level
 * diff (with intra-value spans) against the previous version; empty
 * for the oldest version.
 */
struct entity_history_version final {
    int version{};
    std::string modified_by;
    std::chrono::system_clock::time_point recorded_at;
    std::vector<ores::diff::domain::field_value> fields;
    ores::diff::domain::diff_result changes;

    friend bool operator==(const entity_history_version&, const entity_history_version&) = default;
};

struct get_entity_history_response;

/**
 * @brief The one generic history request every entity shares.
 *
 * entity_type is the entity_type_of() dispatch key (e.g.
 * "ores.refdata.currency"); entity_id is the entity's own primary key
 * rendered as a string, since key shapes vary across entities. No
 * typed domain payload crosses the wire for history.
 */
struct get_entity_history_request final {
    using response_type = struct get_entity_history_response;
    static constexpr std::string_view nats_subject = "history.v1.get";
    std::string entity_type;
    std::string entity_id;
};

struct get_entity_history_response final {
    std::vector<entity_history_version> versions;
    bool success = false;
    std::string message;
};

}

#endif
