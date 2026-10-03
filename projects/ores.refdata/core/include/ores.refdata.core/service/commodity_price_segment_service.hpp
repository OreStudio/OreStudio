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
#ifndef ORES_REFDATA_CORE_SERVICE_COMMODITY_PRICE_SEGMENT_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_COMMODITY_PRICE_SEGMENT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/commodity_price_segment.hpp"
#include "ores.refdata.api/messaging/commodity_price_segment_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/commodity_price_segment_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing commodity price segments.
 *
 * Provides a higher-level interface for commodity price segment operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT commodity_price_segment_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.commodity_price_segment_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a commodity_price_segment_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit commodity_price_segment_service(context ctx);

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
    messaging::list_commodity_price_segments_response
    list_commodity_price_segments(const messaging::list_commodity_price_segments_request& request);
    messaging::get_commodity_price_segment_response
    get_commodity_price_segment(const messaging::get_commodity_price_segment_request& request);
    messaging::get_many_commodity_price_segments_response get_many_commodity_price_segments(
        const messaging::get_many_commodity_price_segments_request& request);
    messaging::put_commodity_price_segment_response
    put_commodity_price_segment(const messaging::put_commodity_price_segment_request& request);
    messaging::put_many_commodity_price_segments_response put_many_commodity_price_segments(
        const messaging::put_many_commodity_price_segments_request& request);
    messaging::delete_commodity_price_segment_response delete_commodity_price_segment(
        const messaging::delete_commodity_price_segment_request& request);
    messaging::delete_many_commodity_price_segments_response delete_many_commodity_price_segments(
        const messaging::delete_many_commodity_price_segments_request& request);
    messaging::list_commodity_price_segment_versions_response list_commodity_price_segment_versions(
        const messaging::list_commodity_price_segment_versions_request& request);
    messaging::get_commodity_price_segment_version_response get_commodity_price_segment_version(
        const messaging::get_commodity_price_segment_version_request& request);
    /**@}*/

    /**
     * @brief Lists commodity price segments with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of commodity price segments for the requested page.
     */
    std::vector<domain::commodity_price_segment> list_price_segments(std::uint32_t offset,
                                                                     std::uint32_t limit);

    /**
     * @brief Gets the total count of active commodity price segments.
     *
     * @return Total number of active commodity price segments.
     */
    std::uint32_t count_price_segments();


    /**
     * @brief Retrieves a single commodity price segment as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The commodity price segment at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::commodity_price_segment>
    get_price_segment_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single commodity price segment by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The commodity price segment if found, std::nullopt otherwise.
     */
    std::optional<domain::commodity_price_segment> get_price_segment(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of commodity price segments by primary key.
     */
    std::vector<domain::commodity_price_segment>
    get_price_segments(const std::vector<std::string>& ids);

    /**
     * @brief Saves a commodity price segment (creates or updates).
     *
     * @param price_segment The commodity price segment to save.
     * @throws std::exception on failure.
     */
    void save_price_segment(const domain::commodity_price_segment& price_segment);

    /**
     * @brief Saves a batch of commodity price segments.
     *
     * @param price_segments The commodity price segments to save.
     * @throws std::exception on failure.
     */
    void save_price_segments(const std::vector<domain::commodity_price_segment>& price_segments);

    /**
     * @brief Deletes a commodity price segment by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_price_segment(const boost::uuids::uuid& id);

    /**
     * @brief Deletes commodity price segments by their primary keys.
     */
    void delete_price_segments(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a commodity price segment.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::commodity_price_segment> get_price_segment_history(const std::string& id);

private:
    context ctx_;
    repository::commodity_price_segment_repository repo_;

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
    prepare_change(const messaging::commodity_price_segment_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::commodity_price_segment& out);
};

}

#endif
