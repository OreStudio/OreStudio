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
#ifndef ORES_SYNTHETIC_CORE_SERVICE_VARIANCE_CALIBRATION_INPUT_POINT_SERVICE_HPP
#define ORES_SYNTHETIC_CORE_SERVICE_VARIANCE_CALIBRATION_INPUT_POINT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.synthetic.api/domain/variance_calibration_input_point.hpp"
#include "ores.synthetic.api/messaging/variance_calibration_input_point_protocol.hpp"
#include "ores.synthetic.core/export.hpp"
#include "ores.synthetic.core/repository/variance_calibration_input_point_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Service for managing variance calibration input points.
 *
 * Provides a higher-level interface for variance calibration input point operations,
 * wrapping the underlying repository.
 */
class ORES_SYNTHETIC_CORE_EXPORT variance_calibration_input_point_service {
private:
    inline static std::string_view logger_name =
        "ores.synthetic.service.variance_calibration_input_point_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a variance_calibration_input_point_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit variance_calibration_input_point_service(const context& ctx);

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
    ::ores::synthetic::messaging::list_variance_calibration_input_points_response
    list_variance_calibration_input_points(
        const ::ores::synthetic::messaging::list_variance_calibration_input_points_request&
            request);
    ::ores::synthetic::messaging::get_variance_calibration_input_point_response
    get_variance_calibration_input_point(
        const ::ores::synthetic::messaging::get_variance_calibration_input_point_request& request);
    ::ores::synthetic::messaging::get_many_variance_calibration_input_points_response
    get_many_variance_calibration_input_points(
        const ::ores::synthetic::messaging::get_many_variance_calibration_input_points_request&
            request);
    ::ores::synthetic::messaging::put_variance_calibration_input_point_response
    put_variance_calibration_input_point(
        const ::ores::synthetic::messaging::put_variance_calibration_input_point_request& request);
    ::ores::synthetic::messaging::put_many_variance_calibration_input_points_response
    put_many_variance_calibration_input_points(
        const ::ores::synthetic::messaging::put_many_variance_calibration_input_points_request&
            request);
    ::ores::synthetic::messaging::delete_variance_calibration_input_point_response
    delete_variance_calibration_input_point(
        const ::ores::synthetic::messaging::delete_variance_calibration_input_point_request&
            request);
    ::ores::synthetic::messaging::delete_many_variance_calibration_input_points_response
    delete_many_variance_calibration_input_points(
        const ::ores::synthetic::messaging::delete_many_variance_calibration_input_points_request&
            request);
    ::ores::synthetic::messaging::list_variance_calibration_input_point_versions_response
    list_variance_calibration_input_point_versions(
        const ::ores::synthetic::messaging::list_variance_calibration_input_point_versions_request&
            request);
    ::ores::synthetic::messaging::get_variance_calibration_input_point_version_response
    get_variance_calibration_input_point_version(
        const ::ores::synthetic::messaging::get_variance_calibration_input_point_version_request&
            request);
    /**@}*/

    /**
     * @brief Lists variance calibration input points with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of variance calibration input points for the requested page.
     */
    std::vector<domain::variance_calibration_input_point>
    list_variance_calibration_input_points(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active variance calibration input points.
     *
     * @return Total number of active variance calibration input points.
     */
    std::uint32_t count_variance_calibration_input_points();


    /**
     * @brief Retrieves a single variance calibration input point as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The variance calibration input point at that version if found, std::nullopt
     * otherwise.
     */
    std::optional<domain::variance_calibration_input_point>
    get_variance_calibration_input_point_at_version(const boost::uuids::uuid& id,
                                                    std::uint32_t version);

    /**
     * @brief Retrieves a single variance calibration input point by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The variance calibration input point if found, std::nullopt otherwise.
     */
    std::optional<domain::variance_calibration_input_point>
    get_variance_calibration_input_point(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of variance calibration input points by primary key.
     */
    std::vector<domain::variance_calibration_input_point>
    get_variance_calibration_input_points(const std::vector<std::string>& ids);

    /**
     * @brief Saves a variance calibration input point (creates or updates).
     *
     * @param variance_calibration_input_point The variance calibration input point to save.
     * @throws std::exception on failure.
     */
    void save_variance_calibration_input_point(
        const domain::variance_calibration_input_point& variance_calibration_input_point);

    /**
     * @brief Saves a batch of variance calibration input points.
     *
     * @param variance_calibration_input_points The variance calibration input points to save.
     * @throws std::exception on failure.
     */
    void save_variance_calibration_input_points(
        const std::vector<domain::variance_calibration_input_point>&
            variance_calibration_input_points);

    /**
     * @brief Deletes a variance calibration input point by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_variance_calibration_input_point(const boost::uuids::uuid& id);

    /**
     * @brief Deletes variance calibration input points by their primary keys.
     */
    void delete_variance_calibration_input_points(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a variance calibration input point.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::variance_calibration_input_point>
    get_variance_calibration_input_point_history(const std::string& id);

private:
    context ctx_;
    repository::variance_calibration_input_point_repository repo_;

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
    ores::utility::domain::result prepare_change(
        const ::ores::synthetic::messaging::variance_calibration_input_point_change& change,
        const ores::utility::domain::change_intent& intent,
        domain::variance_calibration_input_point& out);
};

}

#endif
