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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
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
 * fields is the full render of this version, in mapper order -- the
 * detail panel's need for complete values. changes is the field-level
 * diff, with intra-value spans, against the previous version; empty for
 * the oldest version.
 */
struct entity_history_version {
    int version = 0;
    std::string modified_by;
    std::chrono::system_clock::time_point recorded_at;
    std::vector<ores::diff::domain::field_value> fields;
    ores::diff::domain::diff_result changes;
};

/**
 * @brief The one generic history request every entity shares.
 *
 * entity_type is the dispatch key (e.g. "ores.refdata.currency"); entity_id
 * is the entity's own primary key rendered as a string, since key shapes vary
 * across entities.
 *
 * The subject names the component that OWNS the entity, which this model
 * cannot know: the same request reaches iam.v1.history.get for an IAM entity
 * and refdata.v1.history.get for a refdata one. So the segment is left open
 * and history_subject_for() derives it -- one rule, read by the service that
 * subscribes and by every client that sends.
 */
struct get_entity_history_request {
    using response_type = struct get_entity_history_response;
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string entity_type;
    std::string entity_id;
};

struct get_entity_history_response {
    std::vector<entity_history_version> versions;
    bool success = false;
    std::string message;
};


/**
 * @brief The subject these messages are addressed at, for one component.
 *
 * The pattern is stated once, in the model, and this is its only derivation: a
 * service composes the subject it listens on with it, and every client
 * composes the subject it sends to with it, so no two callers can disagree
 * about a subject that is not a constant.
 */
[[nodiscard]] inline std::string history_subject_by_component(std::string_view component) {
    return std::string(component) + std::string(".v1.history.get");
}

/**
 * @brief The subject these messages are addressed at, for the resource a
 * dispatch key names.
 *
 * The same rule as history_subject_by_component(), reached from the key a
 * client holds rather than from the component a service knows. The hole is the
 * component segment of the key, which is always
 * "<product>.<component>.<entity>" -- so "ores.iam.tenant_type" is addressed
 * at iam.v1.history.get.
 */
[[nodiscard]] inline std::string history_subject_for(std::string_view entity_type) {
    const auto first_dot = entity_type.find('.');
    const auto second_dot = first_dot == std::string_view::npos ?
                                std::string_view::npos :
                                entity_type.find('.', first_dot + 1);
    const std::string component =
        first_dot != std::string_view::npos && second_dot != std::string_view::npos ?
            std::string(entity_type.substr(first_dot + 1, second_dot - first_dot - 1)) :
            std::string("unknown");
    return history_subject_by_component(component);
}
}

#endif
