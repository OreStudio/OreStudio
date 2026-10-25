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
#ifndef ORES_REPORTING_CORE_SERVICE_ORE_PARAMETER_VALUE_SET_VALUE_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_ORE_PARAMETER_VALUE_SET_VALUE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/ore_parameter_value_set_value.hpp"
#include "ores.reporting.api/messaging/ore_parameter_value_set_value_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/ore_parameter_value_set_value_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Service for managing ore parameter value set values.
 *
 * Provides a higher-level interface for ore parameter value set value operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT ore_parameter_value_set_value_service {
private:
    inline static std::string_view logger_name =
        "ores.reporting.service.ore_parameter_value_set_value_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a ore_parameter_value_set_value_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit ore_parameter_value_set_value_service(context ctx);

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
    messaging::list_ore_parameter_value_set_values_response list_ore_parameter_value_set_values(
        const messaging::list_ore_parameter_value_set_values_request& request);
    messaging::get_ore_parameter_value_set_value_response get_ore_parameter_value_set_value(
        const messaging::get_ore_parameter_value_set_value_request& request);
    messaging::get_many_ore_parameter_value_set_values_response
    get_many_ore_parameter_value_set_values(
        const messaging::get_many_ore_parameter_value_set_values_request& request);
    messaging::put_ore_parameter_value_set_value_response put_ore_parameter_value_set_value(
        const messaging::put_ore_parameter_value_set_value_request& request);
    messaging::put_many_ore_parameter_value_set_values_response
    put_many_ore_parameter_value_set_values(
        const messaging::put_many_ore_parameter_value_set_values_request& request);
    messaging::delete_ore_parameter_value_set_value_response delete_ore_parameter_value_set_value(
        const messaging::delete_ore_parameter_value_set_value_request& request);
    messaging::delete_many_ore_parameter_value_set_values_response
    delete_many_ore_parameter_value_set_values(
        const messaging::delete_many_ore_parameter_value_set_values_request& request);
    messaging::list_ore_parameter_value_set_value_versions_response
    list_ore_parameter_value_set_value_versions(
        const messaging::list_ore_parameter_value_set_value_versions_request& request);
    messaging::get_ore_parameter_value_set_value_version_response
    get_ore_parameter_value_set_value_version(
        const messaging::get_ore_parameter_value_set_value_version_request& request);
    /**@}*/

    /**
     * @brief Lists ore parameter value set values with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of ore parameter value set values for the requested page.
     */
    std::vector<domain::ore_parameter_value_set_value> list_values(std::uint32_t offset,
                                                                   std::uint32_t limit);

    /**
     * @brief Gets the total count of active ore parameter value set values.
     *
     * @return Total number of active ore parameter value set values.
     */
    std::uint32_t count_values();


    /**
     * @brief Retrieves a single ore parameter value set value as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The ore parameter value set value at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::ore_parameter_value_set_value>
    get_value_at_version(const std::string& code, std::uint32_t version);

    /**
     * @brief Retrieves a single ore parameter value set value by its primary key.
     *
     * @return The ore parameter value set value if found, std::nullopt otherwise.
     */
    std::optional<domain::ore_parameter_value_set_value> find_value(const std::string& code);

    /**
     * @brief Retrieves a batch of ore parameter value set values by primary key.
     */
    std::vector<domain::ore_parameter_value_set_value>
    get_values(const std::vector<std::string>& codes);

    /**
     * @brief Saves a ore parameter value set value (creates or updates).
     *
     * @param value The ore parameter value set value to save.
     * @throws std::exception on failure.
     */
    void save_value(const domain::ore_parameter_value_set_value& value);

    /**
     * @brief Saves a batch of ore parameter value set values.
     *
     * @param values The ore parameter value set values to save.
     * @throws std::exception on failure.
     */
    void save_values(const std::vector<domain::ore_parameter_value_set_value>& values);

    /**
     * @brief Deletes a ore parameter value set value by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_value(const std::string& code);

    /**
     * @brief Deletes ore parameter value set values by their primary keys.
     */
    void delete_values(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a ore parameter value set value.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::ore_parameter_value_set_value> get_value_history(const std::string& code);

private:
    context ctx_;
    repository::ore_parameter_value_set_value_repository repo_;

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
    prepare_change(const messaging::ore_parameter_value_set_value_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::ore_parameter_value_set_value& out);
};

}

#endif
