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
#ifndef ORES_DQ_CORE_SERVICE_BADGE_SEVERITY_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_BADGE_SEVERITY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/badge_severity.hpp"
#include "ores.dq.api/messaging/badge_severity_protocol.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/badge_severity_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing badge severities.
 *
 * Provides a higher-level interface for badge severity operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT badge_severity_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.badge_severity_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a badge_severity_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit badge_severity_service(context ctx);

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
    messaging::list_badge_severities_response
    list_badge_severities(const messaging::list_badge_severities_request& request);
    messaging::get_badge_severity_response
    get_badge_severity(const messaging::get_badge_severity_request& request);
    messaging::get_many_badge_severities_response
    get_many_badge_severities(const messaging::get_many_badge_severities_request& request);
    messaging::put_badge_severity_response
    put_badge_severity(const messaging::put_badge_severity_request& request);
    messaging::put_many_badge_severities_response
    put_many_badge_severities(const messaging::put_many_badge_severities_request& request);
    messaging::delete_badge_severity_response
    delete_badge_severity(const messaging::delete_badge_severity_request& request);
    messaging::delete_many_badge_severities_response
    delete_many_badge_severities(const messaging::delete_many_badge_severities_request& request);
    messaging::list_badge_severity_versions_response
    list_badge_severity_versions(const messaging::list_badge_severity_versions_request& request);
    messaging::get_badge_severity_version_response
    get_badge_severity_version(const messaging::get_badge_severity_version_request& request);
    /**@}*/

    /**
     * @brief Lists badge severities with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of badge severities for the requested page.
     */
    std::vector<domain::badge_severity> list_severities(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active badge severities.
     *
     * @return Total number of active badge severities.
     */
    std::uint32_t count_severities();


    /**
     * @brief Retrieves a single badge severity as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The badge severity at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::badge_severity> get_severity_at_version(const std::string& code,
                                                                  std::uint32_t version);

    /**
     * @brief Retrieves a single badge severity by its primary key.
     *
     * @return The badge severity if found, std::nullopt otherwise.
     */
    std::optional<domain::badge_severity> get_severity(const std::string& code);

    /**
     * @brief Retrieves a batch of badge severities by primary key.
     */
    std::vector<domain::badge_severity> get_severities(const std::vector<std::string>& codes);

    /**
     * @brief Saves a badge severity (creates or updates).
     *
     * @param severity The badge severity to save.
     * @throws std::exception on failure.
     */
    void save_severity(const domain::badge_severity& severity);

    /**
     * @brief Saves a batch of badge severities.
     *
     * @param severities The badge severities to save.
     * @throws std::exception on failure.
     */
    void save_severities(const std::vector<domain::badge_severity>& severities);

    /**
     * @brief Deletes a badge severity by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_severity(const std::string& code);

    /**
     * @brief Deletes badge severities by their primary keys.
     */
    void delete_severities(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a badge severity.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::badge_severity> get_severity_history(const std::string& code);

private:
    context ctx_;
    repository::badge_severity_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::badge_severity_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::badge_severity& out);
};

}

#endif
