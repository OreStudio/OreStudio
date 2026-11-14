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
#ifndef ORES_REFDATA_CORE_SERVICE_CURVE_SEGMENT_DISCOUNT_RATIO_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CURVE_SEGMENT_DISCOUNT_RATIO_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/curve_segment_discount_ratio.hpp"
#include "ores.refdata.api/messaging/curve_segment_discount_ratio_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/curve_segment_discount_ratio_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing curve segment discount ratios.
 *
 * Provides a higher-level interface for curve segment discount ratio operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT curve_segment_discount_ratio_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.curve_segment_discount_ratio_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a curve_segment_discount_ratio_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit curve_segment_discount_ratio_service(const context& ctx);

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
    ::ores::refdata::messaging::list_curve_segment_discount_ratios_response
    list_curve_segment_discount_ratios(
        const ::ores::refdata::messaging::list_curve_segment_discount_ratios_request& request);
    ::ores::refdata::messaging::get_curve_segment_discount_ratio_response
    get_curve_segment_discount_ratio(
        const ::ores::refdata::messaging::get_curve_segment_discount_ratio_request& request);
    ::ores::refdata::messaging::get_many_curve_segment_discount_ratios_response
    get_many_curve_segment_discount_ratios(
        const ::ores::refdata::messaging::get_many_curve_segment_discount_ratios_request& request);
    ::ores::refdata::messaging::put_curve_segment_discount_ratio_response
    put_curve_segment_discount_ratio(
        const ::ores::refdata::messaging::put_curve_segment_discount_ratio_request& request);
    ::ores::refdata::messaging::put_many_curve_segment_discount_ratios_response
    put_many_curve_segment_discount_ratios(
        const ::ores::refdata::messaging::put_many_curve_segment_discount_ratios_request& request);
    ::ores::refdata::messaging::delete_curve_segment_discount_ratio_response
    delete_curve_segment_discount_ratio(
        const ::ores::refdata::messaging::delete_curve_segment_discount_ratio_request& request);
    ::ores::refdata::messaging::delete_many_curve_segment_discount_ratios_response
    delete_many_curve_segment_discount_ratios(
        const ::ores::refdata::messaging::delete_many_curve_segment_discount_ratios_request&
            request);
    ::ores::refdata::messaging::list_curve_segment_discount_ratio_versions_response
    list_curve_segment_discount_ratio_versions(
        const ::ores::refdata::messaging::list_curve_segment_discount_ratio_versions_request&
            request);
    ::ores::refdata::messaging::get_curve_segment_discount_ratio_version_response
    get_curve_segment_discount_ratio_version(
        const ::ores::refdata::messaging::get_curve_segment_discount_ratio_version_request&
            request);
    /**@}*/

    /**
     * @brief Lists curve segment discount ratios with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of curve segment discount ratios for the requested page.
     */
    std::vector<domain::curve_segment_discount_ratio>
    list_segment_discount_ratios(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active curve segment discount ratios.
     *
     * @return Total number of active curve segment discount ratios.
     */
    std::uint32_t count_segment_discount_ratios();


    /**
     * @brief Retrieves a single curve segment discount ratio as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The curve segment discount ratio at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::curve_segment_discount_ratio>
    get_segment_discount_ratio_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single curve segment discount ratio by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The curve segment discount ratio if found, std::nullopt otherwise.
     */
    std::optional<domain::curve_segment_discount_ratio>
    get_segment_discount_ratio(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of curve segment discount ratios by primary key.
     */
    std::vector<domain::curve_segment_discount_ratio>
    get_segment_discount_ratios(const std::vector<std::string>& ids);

    /**
     * @brief Saves a curve segment discount ratio (creates or updates).
     *
     * @param segment_discount_ratio The curve segment discount ratio to save.
     * @throws std::exception on failure.
     */
    void
    save_segment_discount_ratio(const domain::curve_segment_discount_ratio& segment_discount_ratio);

    /**
     * @brief Saves a batch of curve segment discount ratios.
     *
     * @param segment_discount_ratios The curve segment discount ratios to save.
     * @throws std::exception on failure.
     */
    void save_segment_discount_ratios(
        const std::vector<domain::curve_segment_discount_ratio>& segment_discount_ratios);

    /**
     * @brief Deletes a curve segment discount ratio by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_segment_discount_ratio(const boost::uuids::uuid& id);

    /**
     * @brief Deletes curve segment discount ratios by their primary keys.
     */
    void delete_segment_discount_ratios(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a curve segment discount ratio.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::curve_segment_discount_ratio>
    get_segment_discount_ratio_history(const std::string& id);

private:
    context ctx_;
    repository::curve_segment_discount_ratio_repository repo_;

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
    prepare_change(const ::ores::refdata::messaging::curve_segment_discount_ratio_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::curve_segment_discount_ratio& out);
};

}

#endif
