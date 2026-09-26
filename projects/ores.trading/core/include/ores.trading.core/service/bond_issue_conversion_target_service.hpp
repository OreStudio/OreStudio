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
#ifndef ORES_TRADING_CORE_SERVICE_BOND_ISSUE_CONVERSION_TARGET_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_BOND_ISSUE_CONVERSION_TARGET_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/bond_issue_conversion_target.hpp"
#include "ores.trading.api/messaging/bond_issue_conversion_target_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/bond_issue_conversion_target_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing bond issue conversion targets.
 *
 * Provides a higher-level interface for bond issue conversion target operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT bond_issue_conversion_target_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.bond_issue_conversion_target_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a bond_issue_conversion_target_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit bond_issue_conversion_target_service(context ctx);

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
    messaging::list_bond_issue_conversion_targets_response list_bond_issue_conversion_targets(
        const messaging::list_bond_issue_conversion_targets_request& request);
    messaging::get_bond_issue_conversion_target_response get_bond_issue_conversion_target(
        const messaging::get_bond_issue_conversion_target_request& request);
    messaging::get_many_bond_issue_conversion_targets_response
    get_many_bond_issue_conversion_targets(
        const messaging::get_many_bond_issue_conversion_targets_request& request);
    messaging::put_bond_issue_conversion_target_response put_bond_issue_conversion_target(
        const messaging::put_bond_issue_conversion_target_request& request);
    messaging::put_many_bond_issue_conversion_targets_response
    put_many_bond_issue_conversion_targets(
        const messaging::put_many_bond_issue_conversion_targets_request& request);
    messaging::delete_bond_issue_conversion_target_response delete_bond_issue_conversion_target(
        const messaging::delete_bond_issue_conversion_target_request& request);
    messaging::delete_many_bond_issue_conversion_targets_response
    delete_many_bond_issue_conversion_targets(
        const messaging::delete_many_bond_issue_conversion_targets_request& request);
    messaging::list_bond_issue_conversion_target_versions_response
    list_bond_issue_conversion_target_versions(
        const messaging::list_bond_issue_conversion_target_versions_request& request);
    messaging::get_bond_issue_conversion_target_version_response
    get_bond_issue_conversion_target_version(
        const messaging::get_bond_issue_conversion_target_version_request& request);
    /**@}*/

    /**
     * @brief Lists bond issue conversion targets with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of bond issue conversion targets for the requested page.
     */
    std::vector<domain::bond_issue_conversion_target> list_conversion_targets(std::uint32_t offset,
                                                                              std::uint32_t limit);

    /**
     * @brief Gets the total count of active bond issue conversion targets.
     *
     * @return Total number of active bond issue conversion targets.
     */
    std::uint32_t count_conversion_targets();


    /**
     * @brief Retrieves a single bond issue conversion target as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The bond issue conversion target at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_issue_conversion_target> get_conversion_target_at_version(
        const std::string& issue_id, const std::string& sequence_number, std::uint32_t version);

    /**
     * @brief Retrieves a single bond issue conversion target by its primary key.
     *
     * @return The bond issue conversion target if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_issue_conversion_target>
    get_conversion_target(const std::string& issue_id, const std::string& sequence_number);

    /**
     * @brief Retrieves a batch of bond issue conversion targets by primary key.
     */
    std::vector<domain::bond_issue_conversion_target>
    get_conversion_targets(const std::vector<std::string>& issue_ids,
                           const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Saves a bond issue conversion target (creates or updates).
     *
     * @param conversion_target The bond issue conversion target to save.
     * @throws std::exception on failure.
     */
    void save_conversion_target(const domain::bond_issue_conversion_target& conversion_target);

    /**
     * @brief Saves a batch of bond issue conversion targets.
     *
     * @param conversion_targets The bond issue conversion targets to save.
     * @throws std::exception on failure.
     */
    void save_conversion_targets(
        const std::vector<domain::bond_issue_conversion_target>& conversion_targets);

    /**
     * @brief Deletes a bond issue conversion target by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_conversion_target(const std::string& issue_id, const std::string& sequence_number);

    /**
     * @brief Deletes bond issue conversion targets by their primary keys.
     */
    void delete_conversion_targets(const std::vector<std::string>& issue_ids,
                                   const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Retrieves all historical versions of a bond issue conversion target.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::bond_issue_conversion_target>
    get_conversion_target_history(const std::string& issue_id, const std::string& sequence_number);

private:
    context ctx_;
    repository::bond_issue_conversion_target_repository repo_;

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
    prepare_change(const messaging::bond_issue_conversion_target_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::bond_issue_conversion_target& out);
};

}

#endif
