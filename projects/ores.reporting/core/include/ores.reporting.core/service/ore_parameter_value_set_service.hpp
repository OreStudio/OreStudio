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
#ifndef ORES_REPORTING_CORE_SERVICE_ORE_PARAMETER_VALUE_SET_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_ORE_PARAMETER_VALUE_SET_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/ore_parameter_value_set.hpp"
#include "ores.reporting.api/messaging/ore_parameter_value_set_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/ore_parameter_value_set_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Service for managing ore parameter value sets.
 *
 * Provides a higher-level interface for ore parameter value set operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT ore_parameter_value_set_service {
private:
    inline static std::string_view logger_name =
        "ores.reporting.service.ore_parameter_value_set_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a ore_parameter_value_set_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit ore_parameter_value_set_service(context ctx);

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
    messaging::list_ore_parameter_value_sets_response
    list_ore_parameter_value_sets(const messaging::list_ore_parameter_value_sets_request& request);
    messaging::get_ore_parameter_value_set_response
    get_ore_parameter_value_set(const messaging::get_ore_parameter_value_set_request& request);
    messaging::get_many_ore_parameter_value_sets_response get_many_ore_parameter_value_sets(
        const messaging::get_many_ore_parameter_value_sets_request& request);
    messaging::put_ore_parameter_value_set_response
    put_ore_parameter_value_set(const messaging::put_ore_parameter_value_set_request& request);
    messaging::put_many_ore_parameter_value_sets_response put_many_ore_parameter_value_sets(
        const messaging::put_many_ore_parameter_value_sets_request& request);
    messaging::delete_ore_parameter_value_set_response delete_ore_parameter_value_set(
        const messaging::delete_ore_parameter_value_set_request& request);
    messaging::delete_many_ore_parameter_value_sets_response delete_many_ore_parameter_value_sets(
        const messaging::delete_many_ore_parameter_value_sets_request& request);
    messaging::list_ore_parameter_value_set_versions_response list_ore_parameter_value_set_versions(
        const messaging::list_ore_parameter_value_set_versions_request& request);
    messaging::get_ore_parameter_value_set_version_response get_ore_parameter_value_set_version(
        const messaging::get_ore_parameter_value_set_version_request& request);
    /**@}*/

    /**
     * @brief Lists ore parameter value sets with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of ore parameter value sets for the requested page.
     */
    std::vector<domain::ore_parameter_value_set> list_sets(std::uint32_t offset,
                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active ore parameter value sets.
     *
     * @return Total number of active ore parameter value sets.
     */
    std::uint32_t count_sets();


    /**
     * @brief Retrieves a single ore parameter value set as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The ore parameter value set at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::ore_parameter_value_set> get_set_at_version(const std::string& code,
                                                                      std::uint32_t version);

    /**
     * @brief Retrieves a single ore parameter value set by its primary key.
     *
     * @return The ore parameter value set if found, std::nullopt otherwise.
     */
    std::optional<domain::ore_parameter_value_set> find_set(const std::string& code);

    /**
     * @brief Retrieves a batch of ore parameter value sets by primary key.
     */
    std::vector<domain::ore_parameter_value_set> get_sets(const std::vector<std::string>& codes);

    /**
     * @brief Saves a ore parameter value set (creates or updates).
     *
     * @param set The ore parameter value set to save.
     * @throws std::exception on failure.
     */
    void save_set(const domain::ore_parameter_value_set& set);

    /**
     * @brief Saves a batch of ore parameter value sets.
     *
     * @param sets The ore parameter value sets to save.
     * @throws std::exception on failure.
     */
    void save_sets(const std::vector<domain::ore_parameter_value_set>& sets);

    /**
     * @brief Deletes a ore parameter value set by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_set(const std::string& code);

    /**
     * @brief Deletes ore parameter value sets by their primary keys.
     */
    void delete_sets(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a ore parameter value set.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::ore_parameter_value_set> get_set_history(const std::string& code);

private:
    context ctx_;
    repository::ore_parameter_value_set_repository repo_;

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
    prepare_change(const messaging::ore_parameter_value_set_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::ore_parameter_value_set& out);
};

}

#endif
