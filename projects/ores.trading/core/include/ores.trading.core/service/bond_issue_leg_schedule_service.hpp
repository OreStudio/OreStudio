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
 * Template: cpp_service.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_TRADING_CORE_SERVICE_BOND_ISSUE_LEG_SCHEDULE_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_BOND_ISSUE_LEG_SCHEDULE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/bond_issue_leg_schedule.hpp"
#include "ores.trading.api/messaging/bond_issue_leg_schedule_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/bond_issue_leg_schedule_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing bond issue leg schedules.
 *
 * Provides a higher-level interface for bond issue leg schedule operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT bond_issue_leg_schedule_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.bond_issue_leg_schedule_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a bond_issue_leg_schedule_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit bond_issue_leg_schedule_service(context ctx);

    /**
     * @brief The protocol operations, one method per subject.
     *
     * A method takes the canonical request and answers its response, so the
     * handler that serves the subject decodes, calls and replies without
     * deciding anything. The result a caller reads -- missing, conflicting,
     * denied -- is filled here, where the storage call that decided it is
     * made, rather than being inferred from an exception.
     */
    /**@{*/
    messaging::list_bond_issue_leg_schedules_response
    list_bond_issue_leg_schedules(const messaging::list_bond_issue_leg_schedules_request& request);
    messaging::get_bond_issue_leg_schedule_response
    get_bond_issue_leg_schedule(const messaging::get_bond_issue_leg_schedule_request& request);
    messaging::get_many_bond_issue_leg_schedules_response get_many_bond_issue_leg_schedules(
        const messaging::get_many_bond_issue_leg_schedules_request& request);
    messaging::put_bond_issue_leg_schedule_response
    put_bond_issue_leg_schedule(const messaging::put_bond_issue_leg_schedule_request& request);
    messaging::put_many_bond_issue_leg_schedules_response put_many_bond_issue_leg_schedules(
        const messaging::put_many_bond_issue_leg_schedules_request& request);
    messaging::delete_bond_issue_leg_schedule_response delete_bond_issue_leg_schedule(
        const messaging::delete_bond_issue_leg_schedule_request& request);
    messaging::delete_many_bond_issue_leg_schedules_response delete_many_bond_issue_leg_schedules(
        const messaging::delete_many_bond_issue_leg_schedules_request& request);
    messaging::list_bond_issue_leg_schedule_versions_response list_bond_issue_leg_schedule_versions(
        const messaging::list_bond_issue_leg_schedule_versions_request& request);
    messaging::get_bond_issue_leg_schedule_version_response get_bond_issue_leg_schedule_version(
        const messaging::get_bond_issue_leg_schedule_version_request& request);
    /**@}*/

    /**
     * @brief Lists bond issue leg schedules with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of bond issue leg schedules for the requested page.
     */
    std::vector<domain::bond_issue_leg_schedule> list_bond_issue_leg_schedules(std::uint32_t offset,
                                                                               std::uint32_t limit);

    /**
     * @brief Gets the total count of active bond issue leg schedules.
     *
     * @return Total number of active bond issue leg schedules.
     */
    std::uint32_t count_bond_issue_leg_schedules();


    /**
     * @brief Retrieves a single bond issue leg schedule as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The bond issue leg schedule at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_issue_leg_schedule>
    get_bond_issue_leg_schedule_at_version(const std::string& issue_id,
                                           const std::string& leg_number,
                                           const std::string& schedule_role,
                                           const std::string& sequence_number,
                                           std::uint32_t version);

    /**
     * @brief Retrieves a single bond issue leg schedule by its primary key.
     *
     * @return The bond issue leg schedule if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_issue_leg_schedule>
    get_bond_issue_leg_schedule(const std::string& issue_id,
                                const std::string& leg_number,
                                const std::string& schedule_role,
                                const std::string& sequence_number);

    /**
     * @brief Retrieves a batch of bond issue leg schedules by primary key.
     */
    std::vector<domain::bond_issue_leg_schedule>
    get_bond_issue_leg_schedules(const std::vector<std::string>& issue_ids,
                                 const std::vector<std::string>& leg_numbers,
                                 const std::vector<std::string>& schedule_roles,
                                 const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Saves a bond issue leg schedule (creates or updates).
     *
     * @param bond_issue_leg_schedule The bond issue leg schedule to save.
     * @throws std::exception on failure.
     */
    void
    save_bond_issue_leg_schedule(const domain::bond_issue_leg_schedule& bond_issue_leg_schedule);

    /**
     * @brief Saves a batch of bond issue leg schedules.
     *
     * @param bond_issue_leg_schedules The bond issue leg schedules to save.
     * @throws std::exception on failure.
     */
    void save_bond_issue_leg_schedules(
        const std::vector<domain::bond_issue_leg_schedule>& bond_issue_leg_schedules);

    /**
     * @brief Deletes a bond issue leg schedule by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_bond_issue_leg_schedule(const std::string& issue_id,
                                        const std::string& leg_number,
                                        const std::string& schedule_role,
                                        const std::string& sequence_number);

    /**
     * @brief Deletes bond issue leg schedules by their primary keys.
     */
    void delete_bond_issue_leg_schedules(const std::vector<std::string>& issue_ids,
                                         const std::vector<std::string>& leg_numbers,
                                         const std::vector<std::string>& schedule_roles,
                                         const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Retrieves all historical versions of a bond issue leg schedule.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::bond_issue_leg_schedule>
    get_bond_issue_leg_schedule_history(const std::string& issue_id,
                                        const std::string& leg_number,
                                        const std::string& schedule_role,
                                        const std::string& sequence_number);

private:
    context ctx_;
    repository::bond_issue_leg_schedule_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result
    prepare_change(const messaging::bond_issue_leg_schedule_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::bond_issue_leg_schedule& out);
};

}

#endif
