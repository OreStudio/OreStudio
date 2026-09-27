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
#ifndef ORES_ANALYTICS_CORE_SERVICE_CREDIT_SIMULATION_MATRIX_STATE_CONFIG_SERVICE_HPP
#define ORES_ANALYTICS_CORE_SERVICE_CREDIT_SIMULATION_MATRIX_STATE_CONFIG_SERVICE_HPP

#include "ores.analytics.api/domain/credit_simulation_matrix_state_config.hpp"
#include "ores.analytics.api/messaging/credit_simulation_matrix_state_config_protocol.hpp"
#include "ores.analytics.core/export.hpp"
#include "ores.analytics.core/repository/credit_simulation_matrix_state_config_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::analytics::service {

/**
 * @brief Service for managing matrix states.
 *
 * Provides a higher-level interface for matrix state operations,
 * wrapping the underlying repository.
 */
class ORES_ANALYTICS_CORE_EXPORT credit_simulation_matrix_state_config_service {
private:
    inline static std::string_view logger_name =
        "ores.analytics.service.credit_simulation_matrix_state_config_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a credit_simulation_matrix_state_config_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit credit_simulation_matrix_state_config_service(context ctx);

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
    messaging::list_credit_simulation_matrix_state_configs_response
    list_credit_simulation_matrix_state_configs(
        const messaging::list_credit_simulation_matrix_state_configs_request& request);
    messaging::get_credit_simulation_matrix_state_config_response
    get_credit_simulation_matrix_state_config(
        const messaging::get_credit_simulation_matrix_state_config_request& request);
    messaging::get_many_credit_simulation_matrix_state_configs_response
    get_many_credit_simulation_matrix_state_configs(
        const messaging::get_many_credit_simulation_matrix_state_configs_request& request);
    messaging::put_credit_simulation_matrix_state_config_response
    put_credit_simulation_matrix_state_config(
        const messaging::put_credit_simulation_matrix_state_config_request& request);
    messaging::put_many_credit_simulation_matrix_state_configs_response
    put_many_credit_simulation_matrix_state_configs(
        const messaging::put_many_credit_simulation_matrix_state_configs_request& request);
    messaging::delete_credit_simulation_matrix_state_config_response
    delete_credit_simulation_matrix_state_config(
        const messaging::delete_credit_simulation_matrix_state_config_request& request);
    messaging::delete_many_credit_simulation_matrix_state_configs_response
    delete_many_credit_simulation_matrix_state_configs(
        const messaging::delete_many_credit_simulation_matrix_state_configs_request& request);
    messaging::list_by_transition_matrix_id_credit_simulation_matrix_state_configs_response
    list_by_transition_matrix_id_credit_simulation_matrix_state_configs(
        const messaging::
            list_by_transition_matrix_id_credit_simulation_matrix_state_configs_request& request);
    messaging::list_by_credit_rating_code_credit_simulation_matrix_state_configs_response
    list_by_credit_rating_code_credit_simulation_matrix_state_configs(
        const messaging::list_by_credit_rating_code_credit_simulation_matrix_state_configs_request&
            request);
    messaging::list_credit_simulation_matrix_state_config_versions_response
    list_credit_simulation_matrix_state_config_versions(
        const messaging::list_credit_simulation_matrix_state_config_versions_request& request);
    messaging::get_credit_simulation_matrix_state_config_version_response
    get_credit_simulation_matrix_state_config_version(
        const messaging::get_credit_simulation_matrix_state_config_version_request& request);
    /**@}*/

    /**
     * @brief Lists matrix states with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matrix states for the requested page.
     */
    std::vector<domain::credit_simulation_matrix_state_config> list_states(std::uint32_t offset,
                                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active matrix states.
     *
     * @return Total number of active matrix states.
     */
    std::uint32_t count_states();


    /**
     * @brief Lists matrix states filtered by transition_matrix_id, with pagination.
     *
     * @param transition_matrix_id The transition_matrix_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching matrix states for the requested page.
     */
    std::vector<domain::credit_simulation_matrix_state_config> list_states_by_transition_matrix_id(
        const std::string& transition_matrix_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active matrix states filtered by transition_matrix_id.
     *
     * @param transition_matrix_id The transition_matrix_id to filter by.
     * @return Total number of matching matrix states.
     */
    std::uint32_t count_states_by_transition_matrix_id(const std::string& transition_matrix_id);


    /**
     * @brief Lists matrix states filtered by credit_rating_code, with pagination.
     *
     * @param credit_rating_code The credit_rating_code to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching matrix states for the requested page.
     */
    std::vector<domain::credit_simulation_matrix_state_config> list_states_by_credit_rating_code(
        const std::string& credit_rating_code, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active matrix states filtered by credit_rating_code.
     *
     * @param credit_rating_code The credit_rating_code to filter by.
     * @return Total number of matching matrix states.
     */
    std::uint32_t count_states_by_credit_rating_code(const std::string& credit_rating_code);


    /**
     * @brief Retrieves a single matrix state as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The matrix state at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::credit_simulation_matrix_state_config>
    get_state_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single matrix state by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The matrix state if found, std::nullopt otherwise.
     */
    std::optional<domain::credit_simulation_matrix_state_config>
    get_state(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single matrix state by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The matrix state if found, std::nullopt otherwise.
     */
    std::optional<domain::credit_simulation_matrix_state_config>
    get_state_by_credit_rating_code(const std::string& credit_rating_code);

    /**
     * @brief Retrieves a batch of matrix states by primary key.
     */
    std::vector<domain::credit_simulation_matrix_state_config>
    get_states(const std::vector<std::string>& ids);

    /**
     * @brief Saves a matrix state (creates or updates).
     *
     * @param state The matrix state to save.
     * @throws std::exception on failure.
     */
    void save_state(const domain::credit_simulation_matrix_state_config& state);

    /**
     * @brief Saves a batch of matrix states.
     *
     * @param states The matrix states to save.
     * @throws std::exception on failure.
     */
    void save_states(const std::vector<domain::credit_simulation_matrix_state_config>& states);

    /**
     * @brief Deletes a matrix state by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_state(const boost::uuids::uuid& id);

    /**
     * @brief Deletes matrix states by their primary keys.
     */
    void delete_states(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a matrix state.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::credit_simulation_matrix_state_config>
    get_state_history(const std::string& key);

private:
    context ctx_;
    repository::credit_simulation_matrix_state_config_repository repo_;

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
    prepare_change(const messaging::credit_simulation_matrix_state_config_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::credit_simulation_matrix_state_config& out);
};

}

#endif
