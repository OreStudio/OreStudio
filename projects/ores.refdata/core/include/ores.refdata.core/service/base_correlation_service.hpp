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
#ifndef ORES_REFDATA_CORE_SERVICE_BASE_CORRELATION_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_BASE_CORRELATION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/base_correlation.hpp"
#include "ores.refdata.api/messaging/base_correlation_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/base_correlation_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing base correlations.
 *
 * Provides a higher-level interface for base correlation operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT base_correlation_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.base_correlation_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a base_correlation_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit base_correlation_service(context ctx);

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
    messaging::list_base_correlations_response
    list_base_correlations(const messaging::list_base_correlations_request& request);
    messaging::get_base_correlation_response
    get_base_correlation(const messaging::get_base_correlation_request& request);
    messaging::get_many_base_correlations_response
    get_many_base_correlations(const messaging::get_many_base_correlations_request& request);
    messaging::put_base_correlation_response
    put_base_correlation(const messaging::put_base_correlation_request& request);
    messaging::put_many_base_correlations_response
    put_many_base_correlations(const messaging::put_many_base_correlations_request& request);
    messaging::delete_base_correlation_response
    delete_base_correlation(const messaging::delete_base_correlation_request& request);
    messaging::delete_many_base_correlations_response
    delete_many_base_correlations(const messaging::delete_many_base_correlations_request& request);
    messaging::list_base_correlation_versions_response list_base_correlation_versions(
        const messaging::list_base_correlation_versions_request& request);
    messaging::get_base_correlation_version_response
    get_base_correlation_version(const messaging::get_base_correlation_version_request& request);
    /**@}*/

    /**
     * @brief Lists base correlations with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of base correlations for the requested page.
     */
    std::vector<domain::base_correlation> list_base_correlations(std::uint32_t offset,
                                                                 std::uint32_t limit);

    /**
     * @brief Gets the total count of active base correlations.
     *
     * @return Total number of active base correlations.
     */
    std::uint32_t count_base_correlations();


    /**
     * @brief Retrieves a single base correlation as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The base correlation at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::base_correlation>
    get_base_correlation_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single base correlation by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The base correlation if found, std::nullopt otherwise.
     */
    std::optional<domain::base_correlation> get_base_correlation(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of base correlations by primary key.
     */
    std::vector<domain::base_correlation>
    get_base_correlations(const std::vector<std::string>& ids);

    /**
     * @brief Saves a base correlation (creates or updates).
     *
     * @param base_correlation The base correlation to save.
     * @throws std::exception on failure.
     */
    void save_base_correlation(const domain::base_correlation& base_correlation);

    /**
     * @brief Saves a batch of base correlations.
     *
     * @param base_correlations The base correlations to save.
     * @throws std::exception on failure.
     */
    void save_base_correlations(const std::vector<domain::base_correlation>& base_correlations);

    /**
     * @brief Deletes a base correlation by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_base_correlation(const boost::uuids::uuid& id);

    /**
     * @brief Deletes base correlations by their primary keys.
     */
    void delete_base_correlations(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a base correlation.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::base_correlation> get_base_correlation_history(const std::string& id);

private:
    context ctx_;
    repository::base_correlation_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::base_correlation_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::base_correlation& out);
};

}

#endif
