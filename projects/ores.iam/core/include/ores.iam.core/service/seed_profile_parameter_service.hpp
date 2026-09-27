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
#ifndef ORES_IAM_CORE_SERVICE_SEED_PROFILE_PARAMETER_SERVICE_HPP
#define ORES_IAM_CORE_SERVICE_SEED_PROFILE_PARAMETER_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/seed_profile_parameter.hpp"
#include "ores.iam.api/messaging/seed_profile_parameter_protocol.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.iam.core/repository/seed_profile_parameter_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::service {

/**
 * @brief Service for managing seed profile parameters.
 *
 * Provides a higher-level interface for seed profile parameter operations,
 * wrapping the underlying repository.
 */
class ORES_IAM_CORE_EXPORT seed_profile_parameter_service {
private:
    inline static std::string_view logger_name = "ores.iam.service.seed_profile_parameter_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a seed_profile_parameter_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit seed_profile_parameter_service(context ctx);

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
    messaging::list_seed_profile_parameters_response
    list_seed_profile_parameters(const messaging::list_seed_profile_parameters_request& request);
    messaging::get_seed_profile_parameter_response
    get_seed_profile_parameter(const messaging::get_seed_profile_parameter_request& request);
    messaging::get_many_seed_profile_parameters_response get_many_seed_profile_parameters(
        const messaging::get_many_seed_profile_parameters_request& request);
    messaging::put_seed_profile_parameter_response
    put_seed_profile_parameter(const messaging::put_seed_profile_parameter_request& request);
    messaging::put_many_seed_profile_parameters_response put_many_seed_profile_parameters(
        const messaging::put_many_seed_profile_parameters_request& request);
    messaging::delete_seed_profile_parameter_response
    delete_seed_profile_parameter(const messaging::delete_seed_profile_parameter_request& request);
    messaging::delete_many_seed_profile_parameters_response delete_many_seed_profile_parameters(
        const messaging::delete_many_seed_profile_parameters_request& request);
    messaging::list_by_seed_profile_id_seed_profile_parameters_response
    list_by_seed_profile_id_seed_profile_parameters(
        const messaging::list_by_seed_profile_id_seed_profile_parameters_request& request);
    messaging::list_seed_profile_parameter_versions_response list_seed_profile_parameter_versions(
        const messaging::list_seed_profile_parameter_versions_request& request);
    messaging::get_seed_profile_parameter_version_response get_seed_profile_parameter_version(
        const messaging::get_seed_profile_parameter_version_request& request);
    /**@}*/

    /**
     * @brief Lists seed profile parameters with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of seed profile parameters for the requested page.
     */
    std::vector<domain::seed_profile_parameter> list_seed_profile_parameters(std::uint32_t offset,
                                                                             std::uint32_t limit);

    /**
     * @brief Gets the total count of active seed profile parameters.
     *
     * @return Total number of active seed profile parameters.
     */
    std::uint32_t count_seed_profile_parameters();


    /**
     * @brief Lists seed profile parameters filtered by seed_profile_id, with pagination.
     *
     * @param seed_profile_id The seed_profile_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching seed profile parameters for the requested page.
     */
    std::vector<domain::seed_profile_parameter> list_seed_profile_parameters_by_seed_profile_id(
        const std::string& seed_profile_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active seed profile parameters filtered by seed_profile_id.
     *
     * @param seed_profile_id The seed_profile_id to filter by.
     * @return Total number of matching seed profile parameters.
     */
    std::uint32_t
    count_seed_profile_parameters_by_seed_profile_id(const std::string& seed_profile_id);


    /**
     * @brief Retrieves a single seed profile parameter as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The seed profile parameter at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::seed_profile_parameter>
    get_seed_profile_parameter_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single seed profile parameter by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The seed profile parameter if found, std::nullopt otherwise.
     */
    std::optional<domain::seed_profile_parameter>
    get_seed_profile_parameter(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of seed profile parameters by primary key.
     */
    std::vector<domain::seed_profile_parameter>
    get_seed_profile_parameters(const std::vector<std::string>& ids);

    /**
     * @brief Saves a seed profile parameter (creates or updates).
     *
     * @param seed_profile_parameter The seed profile parameter to save.
     * @throws std::exception on failure.
     */
    void save_seed_profile_parameter(const domain::seed_profile_parameter& seed_profile_parameter);

    /**
     * @brief Saves a batch of seed profile parameters.
     *
     * @param seed_profile_parameters The seed profile parameters to save.
     * @throws std::exception on failure.
     */
    void save_seed_profile_parameters(
        const std::vector<domain::seed_profile_parameter>& seed_profile_parameters);

    /**
     * @brief Deletes a seed profile parameter by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_seed_profile_parameter(const boost::uuids::uuid& id);

    /**
     * @brief Deletes seed profile parameters by their primary keys.
     */
    void delete_seed_profile_parameters(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a seed profile parameter.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::seed_profile_parameter>
    get_seed_profile_parameter_history(const std::string& id);

private:
    context ctx_;
    repository::seed_profile_parameter_repository repo_;

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
    prepare_change(const messaging::seed_profile_parameter_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::seed_profile_parameter& out);
};

}

#endif
