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
#ifndef ORES_REFDATA_CORE_SERVICE_YIELD_CURVE_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_YIELD_CURVE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/yield_curve.hpp"
#include "ores.refdata.api/messaging/yield_curve_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/yield_curve_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing yield curves.
 *
 * Provides a higher-level interface for yield curve operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT yield_curve_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.yield_curve_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a yield_curve_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit yield_curve_service(context ctx);

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
    messaging::list_yield_curves_response
    list_yield_curves(const messaging::list_yield_curves_request& request);
    messaging::get_yield_curve_response
    get_yield_curve(const messaging::get_yield_curve_request& request);
    messaging::get_many_yield_curves_response
    get_many_yield_curves(const messaging::get_many_yield_curves_request& request);
    messaging::put_yield_curve_response
    put_yield_curve(const messaging::put_yield_curve_request& request);
    messaging::put_many_yield_curves_response
    put_many_yield_curves(const messaging::put_many_yield_curves_request& request);
    messaging::delete_yield_curve_response
    delete_yield_curve(const messaging::delete_yield_curve_request& request);
    messaging::delete_many_yield_curves_response
    delete_many_yield_curves(const messaging::delete_many_yield_curves_request& request);
    messaging::list_yield_curve_versions_response
    list_yield_curve_versions(const messaging::list_yield_curve_versions_request& request);
    messaging::get_yield_curve_version_response
    get_yield_curve_version(const messaging::get_yield_curve_version_request& request);
    /**@}*/

    /**
     * @brief Lists yield curves with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of yield curves for the requested page.
     */
    std::vector<domain::yield_curve> list_yield_curves(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active yield curves.
     *
     * @return Total number of active yield curves.
     */
    std::uint32_t count_yield_curves();


    /**
     * @brief Retrieves a single yield curve as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The yield curve at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::yield_curve> get_yield_curve_at_version(const boost::uuids::uuid& id,
                                                                  std::uint32_t version);

    /**
     * @brief Retrieves a single yield curve by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The yield curve if found, std::nullopt otherwise.
     */
    std::optional<domain::yield_curve> get_yield_curve(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of yield curves by primary key.
     */
    std::vector<domain::yield_curve> get_yield_curves(const std::vector<std::string>& ids);

    /**
     * @brief Saves a yield curve (creates or updates).
     *
     * @param yield_curve The yield curve to save.
     * @throws std::exception on failure.
     */
    void save_yield_curve(const domain::yield_curve& yield_curve);

    /**
     * @brief Saves a batch of yield curves.
     *
     * @param yield_curves The yield curves to save.
     * @throws std::exception on failure.
     */
    void save_yield_curves(const std::vector<domain::yield_curve>& yield_curves);

    /**
     * @brief Deletes a yield curve by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_yield_curve(const boost::uuids::uuid& id);

    /**
     * @brief Deletes yield curves by their primary keys.
     */
    void delete_yield_curves(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a yield curve.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::yield_curve> get_yield_curve_history(const std::string& id);

private:
    context ctx_;
    repository::yield_curve_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::yield_curve_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::yield_curve& out);
};

}

#endif
