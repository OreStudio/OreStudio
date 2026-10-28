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
#ifndef ORES_REPORTING_SERVICE_SERVICE_ENGINE_SETTINGS_PROPOSAL_HPP
#define ORES_REPORTING_SERVICE_SERVICE_ENGINE_SETTINGS_PROPOSAL_HPP

#include "ores.database/domain/context.hpp"
#include "ores.reporting.api/domain/engine_settings_group_setting.hpp"
#include "ores.reporting.api/messaging/engine_settings_proposal_operations_protocol.hpp"
#include "ores.reporting.service/export.hpp"
#include "ores.logging/make_logger.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief One group of engine run settings, with the settings it holds.
 *
 * A group is a section of the screen, and the settings are the rows a write to
 * it accepts. The group changes the numbers a run computes when any of its
 * settings does, which is the declaration the sections carry.
 */
struct settings_group {
    std::string code;
    std::vector<domain::engine_settings_group_setting> settings;
    bool changes_numbers = false;
};

/**
 * @brief What applying an approved proposal came to.
 *
 * A refusal does not change on a retry; a transient failure might, so the run
 * that applies it may be resumed.
 */
struct engine_settings_apply {
    bool applied = false;
    int groups_applied = 0;
    std::string refusal;
    bool transient = false;
};

/**
 * @brief A change to a number-changing group of engine run settings, proposed
 * and then applied once its approval request is decided.
 *
 * The values in force never move while a proposal waits: the proposal holds its
 * values in its own rows, and the apply writes the part through the part's own
 * service, so the checks a direct write meets are the checks the apply meets.
 *
 * A configuration that no report binds is a draft, and its groups change in
 * place. This class refuses to propose one, so the caller writes the group
 * directly and the standard's rule holds at the server and not only on the
 * screen.
 */
class ORES_REPORTING_SERVICE_EXPORT engine_settings_proposal_service {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.reporting.service.engine_settings_proposal");
        return instance;
    }

public:
    /** The kind of approval request an engine run settings change is raised as. */
    static constexpr std::string_view kind_code = "reporting.engine_settings_change";

    explicit engine_settings_proposal_service(ores::database::context ctx);

    /**
     * @brief The groups the data states, in the order it places them.
     */
    std::vector<settings_group> groups();

    /**
     * @brief One group, or nothing when the data names none with that code.
     */
    std::optional<settings_group> group(const std::string& code);

    /**
     * @brief Whether a write to this group of this configuration is a proposal.
     *
     * True when the group changes the numbers a run computes and at least one
     * report binds the configuration. A draft, which no report binds, changes
     * in place whatever the group.
     */
    bool needs_approval(const boost::uuids::uuid& engine_settings_id, const std::string& group);

    /**
     * @brief Holds a proposed group in an approval request and writes no part.
     */
    ores::reporting::messaging::raise_engine_settings_change_response
    raise(const ores::reporting::messaging::raise_engine_settings_change_request& request);

    /**
     * @brief The groups a request proposes, each beside the value in force.
     */
    std::vector<ores::reporting::messaging::engine_settings_change_line> lines(const std::string& request_id);

    /**
     * @brief Runs the proposal against the version in force and writes nothing.
     *
     * One refusal for each proposed group, in the order the lines answer, and
     * empty where the apply would stand.
     */
    std::vector<std::string> refusals(const std::string& request_id);

    /**
     * @brief The part codes a request of this kind needs for its groups.
     */
    std::vector<std::string> part_codes(const std::string& group_code);

    /**
     * @brief Writes the groups an approved request holds and marks each applied.
     */
    engine_settings_apply apply(const std::string& request_id);

private:
    ores::database::context ctx_;
};

}

#endif
