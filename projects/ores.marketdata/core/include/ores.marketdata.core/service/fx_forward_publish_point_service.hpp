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
#ifndef ORES_MARKETDATA_CORE_SERVICE_FX_FORWARD_PUBLISH_POINT_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_FX_FORWARD_PUBLISH_POINT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/fx_forward_publish_point.hpp"
#include "ores.marketdata.api/messaging/fx_forward_publish_point_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/repository/fx_forward_publish_point_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Service for managing FX forward publish points.
 *
 * Provides a higher-level interface for FX forward publish point operations,
 * wrapping the underlying repository.
 */
class ORES_MARKETDATA_CORE_EXPORT fx_forward_publish_point_service {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.service.fx_forward_publish_point_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a fx_forward_publish_point_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit fx_forward_publish_point_service(const context& ctx);

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
    ::ores::marketdata::messaging::list_fx_forward_publish_points_response
    list_fx_forward_publish_points(
        const ::ores::marketdata::messaging::list_fx_forward_publish_points_request& request);
    ::ores::marketdata::messaging::get_fx_forward_publish_point_response
    get_fx_forward_publish_point(
        const ::ores::marketdata::messaging::get_fx_forward_publish_point_request& request);
    ::ores::marketdata::messaging::get_many_fx_forward_publish_points_response
    get_many_fx_forward_publish_points(
        const ::ores::marketdata::messaging::get_many_fx_forward_publish_points_request& request);
    ::ores::marketdata::messaging::list_fx_forward_publish_point_versions_response
    list_fx_forward_publish_point_versions(
        const ::ores::marketdata::messaging::list_fx_forward_publish_point_versions_request&
            request);
    ::ores::marketdata::messaging::get_fx_forward_publish_point_version_response
    get_fx_forward_publish_point_version(
        const ::ores::marketdata::messaging::get_fx_forward_publish_point_version_request& request);
    /**@}*/

    /**
     * @brief Lists FX forward publish points with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of FX forward publish points for the requested page.
     */
    std::vector<domain::fx_forward_publish_point> list_points(std::uint32_t offset,
                                                              std::uint32_t limit);

    /**
     * @brief Gets the total count of active FX forward publish points.
     *
     * @return Total number of active FX forward publish points.
     */
    std::uint32_t count_points();


    /**
     * @brief Retrieves a single FX forward publish point as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The FX forward publish point at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::fx_forward_publish_point>
    get_point_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single FX forward publish point by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The FX forward publish point if found, std::nullopt otherwise.
     */
    std::optional<domain::fx_forward_publish_point> get_point(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of FX forward publish points by primary key.
     */
    std::vector<domain::fx_forward_publish_point> get_points(const std::vector<std::string>& ids);

    /**
     * @brief Saves a FX forward publish point (creates or updates).
     *
     * @param point The FX forward publish point to save.
     * @throws std::exception on failure.
     */
    void save_point(const domain::fx_forward_publish_point& point);

    /**
     * @brief Saves a batch of FX forward publish points.
     *
     * @param points The FX forward publish points to save.
     * @throws std::exception on failure.
     */
    void save_points(const std::vector<domain::fx_forward_publish_point>& points);

    /**
     * @brief Deletes a FX forward publish point by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_point(const boost::uuids::uuid& id);

    /**
     * @brief Deletes FX forward publish points by their primary keys.
     */
    void delete_points(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a FX forward publish point.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::fx_forward_publish_point> get_point_history(const std::string& id);

private:
    context ctx_;
    repository::fx_forward_publish_point_repository repo_;
};

}

#endif
