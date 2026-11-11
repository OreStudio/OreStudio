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
#ifndef ORES_MARKETDATA_CORE_SERVICE_FX_VOL_SURFACE_PUBLISH_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_FX_VOL_SURFACE_PUBLISH_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/fx_vol_surface_publish.hpp"
#include "ores.marketdata.api/messaging/fx_vol_surface_publish_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/repository/fx_vol_surface_publish_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Service for managing FX volatility surface publishes.
 *
 * Provides a higher-level interface for FX volatility surface publish operations,
 * wrapping the underlying repository.
 */
class ORES_MARKETDATA_CORE_EXPORT fx_vol_surface_publish_service {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.service.fx_vol_surface_publish_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a fx_vol_surface_publish_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit fx_vol_surface_publish_service(context ctx);

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
    ::ores::marketdata::messaging::list_fx_vol_surface_publishes_response
    list_fx_vol_surface_publishes(
        const ::ores::marketdata::messaging::list_fx_vol_surface_publishes_request& request);
    ::ores::marketdata::messaging::get_fx_vol_surface_publish_response get_fx_vol_surface_publish(
        const ::ores::marketdata::messaging::get_fx_vol_surface_publish_request& request);
    ::ores::marketdata::messaging::get_many_fx_vol_surface_publishes_response
    get_many_fx_vol_surface_publishes(
        const ::ores::marketdata::messaging::get_many_fx_vol_surface_publishes_request& request);
    ::ores::marketdata::messaging::list_by_request_id_fx_vol_surface_publishes_response
    list_by_request_id_fx_vol_surface_publishes(
        const ::ores::marketdata::messaging::list_by_request_id_fx_vol_surface_publishes_request&
            request);
    ::ores::marketdata::messaging::list_fx_vol_surface_publish_versions_response
    list_fx_vol_surface_publish_versions(
        const ::ores::marketdata::messaging::list_fx_vol_surface_publish_versions_request& request);
    ::ores::marketdata::messaging::get_fx_vol_surface_publish_version_response
    get_fx_vol_surface_publish_version(
        const ::ores::marketdata::messaging::get_fx_vol_surface_publish_version_request& request);
    /**@}*/

    /**
     * @brief Lists FX volatility surface publishes with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of FX volatility surface publishes for the requested page.
     */
    std::vector<domain::fx_vol_surface_publish> list_publishes(std::uint32_t offset,
                                                               std::uint32_t limit);

    /**
     * @brief Gets the total count of active FX volatility surface publishes.
     *
     * @return Total number of active FX volatility surface publishes.
     */
    std::uint32_t count_publishes();


    /**
     * @brief Lists FX volatility surface publishes filtered by request_id, with pagination.
     *
     * @param request_id The request_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching FX volatility surface publishes for the requested page.
     */
    std::vector<domain::fx_vol_surface_publish> list_publishes_by_request_id(
        const std::string& request_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active FX volatility surface publishes filtered by request_id.
     *
     * @param request_id The request_id to filter by.
     * @return Total number of matching FX volatility surface publishes.
     */
    std::uint32_t count_publishes_by_request_id(const std::string& request_id);


    /**
     * @brief Retrieves a single FX volatility surface publish as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The FX volatility surface publish at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::fx_vol_surface_publish>
    get_publish_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single FX volatility surface publish by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The FX volatility surface publish if found, std::nullopt otherwise.
     */
    std::optional<domain::fx_vol_surface_publish> get_publish(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of FX volatility surface publishes by primary key.
     */
    std::vector<domain::fx_vol_surface_publish> get_publishes(const std::vector<std::string>& ids);

    /**
     * @brief Saves a FX volatility surface publish (creates or updates).
     *
     * @param publish The FX volatility surface publish to save.
     * @throws std::exception on failure.
     */
    void save_publish(const domain::fx_vol_surface_publish& publish);

    /**
     * @brief Saves a batch of FX volatility surface publishes.
     *
     * @param publishes The FX volatility surface publishes to save.
     * @throws std::exception on failure.
     */
    void save_publishes(const std::vector<domain::fx_vol_surface_publish>& publishes);

    /**
     * @brief Deletes a FX volatility surface publish by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_publish(const boost::uuids::uuid& id);

    /**
     * @brief Deletes FX volatility surface publishes by their primary keys.
     */
    void delete_publishes(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a FX volatility surface publish.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::fx_vol_surface_publish> get_publish_history(const std::string& id);

private:
    context ctx_;
    repository::fx_vol_surface_publish_repository repo_;
};

}

#endif
