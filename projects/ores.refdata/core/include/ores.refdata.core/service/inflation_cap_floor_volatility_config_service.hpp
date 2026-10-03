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
#ifndef ORES_REFDATA_CORE_SERVICE_INFLATION_CAP_FLOOR_VOLATILITY_CONFIG_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_INFLATION_CAP_FLOOR_VOLATILITY_CONFIG_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/inflation_cap_floor_volatility_config.hpp"
#include "ores.refdata.api/messaging/inflation_cap_floor_volatility_config_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/inflation_cap_floor_volatility_config_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing inflation cap floor volatility configs.
 *
 * Provides a higher-level interface for inflation cap floor volatility config operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT inflation_cap_floor_volatility_config_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.inflation_cap_floor_volatility_config_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a inflation_cap_floor_volatility_config_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit inflation_cap_floor_volatility_config_service(context ctx);

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
    messaging::list_inflation_cap_floor_volatility_configs_response
    list_inflation_cap_floor_volatility_configs(
        const messaging::list_inflation_cap_floor_volatility_configs_request& request);
    messaging::get_inflation_cap_floor_volatility_config_response
    get_inflation_cap_floor_volatility_config(
        const messaging::get_inflation_cap_floor_volatility_config_request& request);
    messaging::get_many_inflation_cap_floor_volatility_configs_response
    get_many_inflation_cap_floor_volatility_configs(
        const messaging::get_many_inflation_cap_floor_volatility_configs_request& request);
    messaging::put_inflation_cap_floor_volatility_config_response
    put_inflation_cap_floor_volatility_config(
        const messaging::put_inflation_cap_floor_volatility_config_request& request);
    messaging::put_many_inflation_cap_floor_volatility_configs_response
    put_many_inflation_cap_floor_volatility_configs(
        const messaging::put_many_inflation_cap_floor_volatility_configs_request& request);
    messaging::delete_inflation_cap_floor_volatility_config_response
    delete_inflation_cap_floor_volatility_config(
        const messaging::delete_inflation_cap_floor_volatility_config_request& request);
    messaging::delete_many_inflation_cap_floor_volatility_configs_response
    delete_many_inflation_cap_floor_volatility_configs(
        const messaging::delete_many_inflation_cap_floor_volatility_configs_request& request);
    messaging::list_inflation_cap_floor_volatility_config_versions_response
    list_inflation_cap_floor_volatility_config_versions(
        const messaging::list_inflation_cap_floor_volatility_config_versions_request& request);
    messaging::get_inflation_cap_floor_volatility_config_version_response
    get_inflation_cap_floor_volatility_config_version(
        const messaging::get_inflation_cap_floor_volatility_config_version_request& request);
    /**@}*/

    /**
     * @brief Lists inflation cap floor volatility configs with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of inflation cap floor volatility configs for the requested page.
     */
    std::vector<domain::inflation_cap_floor_volatility_config>
    list_inflation_cap_floor_volatility_configs(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active inflation cap floor volatility configs.
     *
     * @return Total number of active inflation cap floor volatility configs.
     */
    std::uint32_t count_inflation_cap_floor_volatility_configs();


    /**
     * @brief Retrieves a single inflation cap floor volatility config as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The inflation cap floor volatility config at that version if found, std::nullopt
     * otherwise.
     */
    std::optional<domain::inflation_cap_floor_volatility_config>
    get_inflation_cap_floor_volatility_config_at_version(const boost::uuids::uuid& id,
                                                         std::uint32_t version);

    /**
     * @brief Retrieves a single inflation cap floor volatility config by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The inflation cap floor volatility config if found, std::nullopt otherwise.
     */
    std::optional<domain::inflation_cap_floor_volatility_config>
    get_inflation_cap_floor_volatility_config(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of inflation cap floor volatility configs by primary key.
     */
    std::vector<domain::inflation_cap_floor_volatility_config>
    get_inflation_cap_floor_volatility_configs(const std::vector<std::string>& ids);

    /**
     * @brief Saves a inflation cap floor volatility config (creates or updates).
     *
     * @param inflation_cap_floor_volatility_config The inflation cap floor volatility config to
     * save.
     * @throws std::exception on failure.
     */
    void save_inflation_cap_floor_volatility_config(
        const domain::inflation_cap_floor_volatility_config& inflation_cap_floor_volatility_config);

    /**
     * @brief Saves a batch of inflation cap floor volatility configs.
     *
     * @param inflation_cap_floor_volatility_configs The inflation cap floor volatility configs to
     * save.
     * @throws std::exception on failure.
     */
    void save_inflation_cap_floor_volatility_configs(
        const std::vector<domain::inflation_cap_floor_volatility_config>&
            inflation_cap_floor_volatility_configs);

    /**
     * @brief Deletes a inflation cap floor volatility config by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_inflation_cap_floor_volatility_config(const boost::uuids::uuid& id);

    /**
     * @brief Deletes inflation cap floor volatility configs by their primary keys.
     */
    void delete_inflation_cap_floor_volatility_configs(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a inflation cap floor volatility config.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::inflation_cap_floor_volatility_config>
    get_inflation_cap_floor_volatility_config_history(const std::string& id);

private:
    context ctx_;
    repository::inflation_cap_floor_volatility_config_repository repo_;

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
    prepare_change(const messaging::inflation_cap_floor_volatility_config_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::inflation_cap_floor_volatility_config& out);
};

}

#endif
