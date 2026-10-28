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
#ifndef ORES_REPORTING_API_MESSAGING_ENGINE_SETTINGS_PROPOSAL_OPERATIONS_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_ENGINE_SETTINGS_PROPOSAL_OPERATIONS_PROTOCOL_HPP

#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>
#include <vector>

namespace ores::reporting::messaging {

/**
 * @brief One setting of a group beside the value in force.
 *
 * The values are text whatever the setting's kind, because a screen draws them
 * and does not compute with them. An empty before or after is the engine
 * default for that setting.
 */
struct engine_settings_value_change {
    std::string column_name;
    std::string ore_name;
    std::string before;
    std::string after;
    bool changed = false;
};

/**
 * @brief One proposed write to a group, as a decider reads it: the group, the
 * configuration it belongs to, the version the maker read, and each setting
 * beside the value in force.
 */
struct engine_settings_change_line {
    int line_no = 0;
    std::string group_code;
    boost::uuids::uuid engine_settings_id;
    int base_version = 0;
    bool applied = false;
    std::vector<engine_settings_value_change> fields;
};

/**
 * @brief One value a person proposes for one setting of a group.
 *
 * The value is the setting's own words: true or false for a flag, its digits
 * for a whole, and one of the words its setting names for a choice. An empty
 * value unsets the setting, which is the engine default.
 */
struct engine_settings_value {
    std::string setting_code;
    std::string text_value = "";
};

/**
 * @brief Holds a proposed change to a group in an approval request.
 *
 * A configuration that no report binds is a draft, and its groups change in
 * place, so a proposal here is refused and the caller writes the group
 * directly. A group that changes no numbers is refused the same way, because
 * the execution group is operational and lands directly.
 *
 * The proposal names the parts its group needs: the kind names one part for
 * each number-changing group, and the request takes the part of the group the
 * proposal writes.
 */
struct raise_engine_settings_change_request {
    using response_type = struct raise_engine_settings_change_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.ops.raise_engine_settings_change";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid engine_settings_id;
    std::string group_code;
    /**
     * @brief The version of the group's row the person read, or zero for a group
     * the configuration had not set.
     */
    int base_version = 0;
    /**
     * @brief The values proposed for the group, one per setting it holds.
     *
     * Every setting of the group travels, so the proposal states the whole group
     * and the apply writes it whole. A setting the person left alone travels with
     * the value in force, which is what makes the proposal a proposal of the group
     * rather than of the settings that changed.
     */
    std::vector<engine_settings_value> values;
    /**
     * @brief Why the person asks, in their words.
     */
    std::string reason;
};

struct raise_engine_settings_change_response {
    ores::utility::domain::result result;
    /**
     * @brief The request the proposal was raised as, empty when none was.
     */
    std::string request_id;
};

/**
 * @brief Reads the groups a request proposes to change, each beside the value
 * in force.
 *
 * Answered to the person who raised the request, to whoever may decide its
 * kind, and to whoever may read the tenant's requests. Answered as not found
 * otherwise, as opening the request is.
 */
struct get_engine_settings_changes_request {
    using response_type = struct get_engine_settings_changes_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ops.get_engine_settings_changes";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string request_id;
};

struct get_engine_settings_changes_response {
    ores::utility::domain::result result;
    /**
     * @brief The proposed groups, in the order the proposal holds them.
     */
    std::vector<engine_settings_change_line> changes;
};

/**
 * @brief Reads the proposal against the version in force and writes nothing.
 *
 * The group may have moved since the proposal was raised, so the checks a
 * decider reads are read now. Answered to the same callers as the lines are.
 */
struct recheck_engine_settings_changes_request {
    using response_type = struct recheck_engine_settings_changes_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.ops.recheck_engine_settings_changes";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string request_id;
};

struct recheck_engine_settings_changes_response {
    ores::utility::domain::result result;
    std::vector<engine_settings_change_line> changes;
    /**
     * @brief One refusal for each group, in the same order as the changes, empty
     * when the apply would stand.
     */
    std::vector<std::string> refusals;
    std::vector<std::string> part_codes;
};

}

#endif
