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
#ifndef ORES_ANALYTICS_CORE_SERVICE_TODAYS_MARKET_CONFIG_SERVICE_HPP
#define ORES_ANALYTICS_CORE_SERVICE_TODAYS_MARKET_CONFIG_SERVICE_HPP

#include "ores.analytics.api/domain/todays_market_config.hpp"
#include "ores.analytics.api/messaging/todays_market_config_protocol.hpp"
#include "ores.analytics.core/export.hpp"
#include "ores.analytics.core/repository/todays_market_config_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::analytics::service {

/**
 * @brief Service for managing today's market configurations.
 *
 * Provides a higher-level interface for today's market configuration operations,
 * wrapping the underlying repository.
 */
class ORES_ANALYTICS_CORE_EXPORT todays_market_config_service {
private:
    inline static std::string_view logger_name =
        "ores.analytics.service.todays_market_config_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a todays_market_config_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit todays_market_config_service(context ctx);

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
    messaging::list_todays_market_configs_response
    list_todays_market_configs(const messaging::list_todays_market_configs_request& request);
    messaging::get_todays_market_config_response
    get_todays_market_config(const messaging::get_todays_market_config_request& request);
    messaging::get_many_todays_market_configs_response get_many_todays_market_configs(
        const messaging::get_many_todays_market_configs_request& request);
    messaging::put_todays_market_config_response
    put_todays_market_config(const messaging::put_todays_market_config_request& request);
    messaging::put_many_todays_market_configs_response put_many_todays_market_configs(
        const messaging::put_many_todays_market_configs_request& request);
    messaging::delete_todays_market_config_response
    delete_todays_market_config(const messaging::delete_todays_market_config_request& request);
    messaging::delete_many_todays_market_configs_response delete_many_todays_market_configs(
        const messaging::delete_many_todays_market_configs_request& request);
    messaging::list_by_configuration_id_todays_market_configs_response
    list_by_configuration_id_todays_market_configs(
        const messaging::list_by_configuration_id_todays_market_configs_request& request);
    messaging::list_todays_market_config_versions_response list_todays_market_config_versions(
        const messaging::list_todays_market_config_versions_request& request);
    messaging::get_todays_market_config_version_response get_todays_market_config_version(
        const messaging::get_todays_market_config_version_request& request);
    /**@}*/

    /**
     * @brief Lists today's market configurations with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of today's market configurations for the requested page.
     */
    std::vector<domain::todays_market_config> list_configs(std::uint32_t offset,
                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active today's market configurations.
     *
     * @return Total number of active today's market configurations.
     */
    std::uint32_t count_configs();


    /**
     * @brief Lists today's market configurations filtered by configuration_id, with pagination.
     *
     * @param configuration_id The configuration_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching today's market configurations for the requested page.
     */
    std::vector<domain::todays_market_config> list_configs_by_configuration_id(
        const std::string& configuration_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active today's market configurations filtered by
     * configuration_id.
     *
     * @param configuration_id The configuration_id to filter by.
     * @return Total number of matching today's market configurations.
     */
    std::uint32_t count_configs_by_configuration_id(const std::string& configuration_id);


    /**
     * @brief Retrieves a single today's market configuration as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The today's market configuration at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::todays_market_config> get_config_at_version(const boost::uuids::uuid& id,
                                                                      std::uint32_t version);

    /**
     * @brief Retrieves a single today's market configuration by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The today's market configuration if found, std::nullopt otherwise.
     */
    std::optional<domain::todays_market_config> get_config(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single today's market configuration by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The today's market configuration if found, std::nullopt otherwise.
     */
    std::optional<domain::todays_market_config> get_config_by_name(const std::string& name);

    /**
     * @brief Retrieves a single today's market configuration by its uuid primary key.
     *
     * @return The today's market configuration if found, std::nullopt otherwise.
     */
    std::optional<domain::todays_market_config> find_config(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single today's market configuration by its name.
     *
     * @return The today's market configuration if found, std::nullopt otherwise.
     */
    std::optional<domain::todays_market_config> find_config_by_code(const std::string& name);

    /**
     * @brief Retrieves a batch of today's market configurations by primary key.
     */
    std::vector<domain::todays_market_config> get_configs(const std::vector<std::string>& ids);

    /**
     * @brief Saves a today's market configuration (creates or updates).
     *
     * @param config The today's market configuration to save.
     * @throws std::exception on failure.
     */
    void save_config(const domain::todays_market_config& config);

    /**
     * @brief Saves a batch of today's market configurations.
     *
     * @param configs The today's market configurations to save.
     * @throws std::exception on failure.
     */
    void save_configs(const std::vector<domain::todays_market_config>& configs);

    /**
     * @brief Deletes a today's market configuration by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_config(const boost::uuids::uuid& id);

    /**
     * @brief Removes a today's market configuration by its uuid primary key.
     *
     * @throws std::exception on failure.
     */
    void remove_config(const boost::uuids::uuid& id);

    /**
     * @brief Deletes today's market configurations by their primary keys.
     */
    void delete_configs(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a today's market configuration.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::todays_market_config> get_config_history(const std::string& key);

    /**
     * @brief Retrieves all historical versions of a today's market configuration
     * by its uuid primary key.
     */
    std::vector<domain::todays_market_config> get_config_history(const boost::uuids::uuid& id);

private:
    context ctx_;
    repository::todays_market_config_repository repo_;

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
    prepare_change(const messaging::todays_market_config_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::todays_market_config& out);
};

}

#endif
