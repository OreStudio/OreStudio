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
#ifndef ORES_REFDATA_CORE_SERVICE_COMMODITY_CURVE_INTERPOLATION_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_COMMODITY_CURVE_INTERPOLATION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/commodity_curve_interpolation.hpp"
#include "ores.refdata.api/messaging/commodity_curve_interpolation_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/commodity_curve_interpolation_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing commodity curve interpolations.
 *
 * Provides a higher-level interface for commodity curve interpolation operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT commodity_curve_interpolation_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.commodity_curve_interpolation_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a commodity_curve_interpolation_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit commodity_curve_interpolation_service(context ctx);

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
    ::ores::refdata::messaging::list_commodity_curve_interpolations_response
    list_commodity_curve_interpolations(
        const ::ores::refdata::messaging::list_commodity_curve_interpolations_request& request);
    ::ores::refdata::messaging::get_commodity_curve_interpolation_response
    get_commodity_curve_interpolation(
        const ::ores::refdata::messaging::get_commodity_curve_interpolation_request& request);
    ::ores::refdata::messaging::get_many_commodity_curve_interpolations_response
    get_many_commodity_curve_interpolations(
        const ::ores::refdata::messaging::get_many_commodity_curve_interpolations_request& request);
    ::ores::refdata::messaging::put_commodity_curve_interpolation_response
    put_commodity_curve_interpolation(
        const ::ores::refdata::messaging::put_commodity_curve_interpolation_request& request);
    ::ores::refdata::messaging::put_many_commodity_curve_interpolations_response
    put_many_commodity_curve_interpolations(
        const ::ores::refdata::messaging::put_many_commodity_curve_interpolations_request& request);
    ::ores::refdata::messaging::delete_commodity_curve_interpolation_response
    delete_commodity_curve_interpolation(
        const ::ores::refdata::messaging::delete_commodity_curve_interpolation_request& request);
    ::ores::refdata::messaging::delete_many_commodity_curve_interpolations_response
    delete_many_commodity_curve_interpolations(
        const ::ores::refdata::messaging::delete_many_commodity_curve_interpolations_request&
            request);
    ::ores::refdata::messaging::list_commodity_curve_interpolation_versions_response
    list_commodity_curve_interpolation_versions(
        const ::ores::refdata::messaging::list_commodity_curve_interpolation_versions_request&
            request);
    ::ores::refdata::messaging::get_commodity_curve_interpolation_version_response
    get_commodity_curve_interpolation_version(
        const ::ores::refdata::messaging::get_commodity_curve_interpolation_version_request&
            request);
    /**@}*/

    /**
     * @brief Lists commodity curve interpolations with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of commodity curve interpolations for the requested page.
     */
    std::vector<domain::commodity_curve_interpolation> list_interpolations(std::uint32_t offset,
                                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active commodity curve interpolations.
     *
     * @return Total number of active commodity curve interpolations.
     */
    std::uint32_t count_interpolations();


    /**
     * @brief Retrieves a single commodity curve interpolation as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The commodity curve interpolation at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::commodity_curve_interpolation>
    get_interpolation_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single commodity curve interpolation by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The commodity curve interpolation if found, std::nullopt otherwise.
     */
    std::optional<domain::commodity_curve_interpolation>
    get_interpolation(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of commodity curve interpolations by primary key.
     */
    std::vector<domain::commodity_curve_interpolation>
    get_interpolations(const std::vector<std::string>& ids);

    /**
     * @brief Saves a commodity curve interpolation (creates or updates).
     *
     * @param interpolation The commodity curve interpolation to save.
     * @throws std::exception on failure.
     */
    void save_interpolation(const domain::commodity_curve_interpolation& interpolation);

    /**
     * @brief Saves a batch of commodity curve interpolations.
     *
     * @param interpolations The commodity curve interpolations to save.
     * @throws std::exception on failure.
     */
    void
    save_interpolations(const std::vector<domain::commodity_curve_interpolation>& interpolations);

    /**
     * @brief Deletes a commodity curve interpolation by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_interpolation(const boost::uuids::uuid& id);

    /**
     * @brief Deletes commodity curve interpolations by their primary keys.
     */
    void delete_interpolations(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a commodity curve interpolation.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::commodity_curve_interpolation>
    get_interpolation_history(const std::string& id);

private:
    context ctx_;
    repository::commodity_curve_interpolation_repository repo_;

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
    prepare_change(const ::ores::refdata::messaging::commodity_curve_interpolation_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::commodity_curve_interpolation& out);
};

}

#endif
