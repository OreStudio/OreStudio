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
#ifndef ORES_DQ_CORE_SERVICE_SUBJECT_AREA_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_SUBJECT_AREA_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/subject_area.hpp"
#include "ores.dq.api/messaging/subject_area_protocol.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/subject_area_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing subject areas.
 *
 * Provides a higher-level interface for subject area operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT subject_area_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.subject_area_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a subject_area_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit subject_area_service(context ctx);

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
    messaging::list_subject_areas_response
    list_subject_areas(const messaging::list_subject_areas_request& request);
    messaging::get_subject_area_response
    get_subject_area(const messaging::get_subject_area_request& request);
    messaging::get_many_subject_areas_response
    get_many_subject_areas(const messaging::get_many_subject_areas_request& request);
    messaging::put_subject_area_response
    put_subject_area(const messaging::put_subject_area_request& request);
    messaging::put_many_subject_areas_response
    put_many_subject_areas(const messaging::put_many_subject_areas_request& request);
    messaging::delete_subject_area_response
    delete_subject_area(const messaging::delete_subject_area_request& request);
    messaging::delete_many_subject_areas_response
    delete_many_subject_areas(const messaging::delete_many_subject_areas_request& request);
    messaging::list_subject_area_versions_response
    list_subject_area_versions(const messaging::list_subject_area_versions_request& request);
    messaging::get_subject_area_version_response
    get_subject_area_version(const messaging::get_subject_area_version_request& request);
    /**@}*/

    /**
     * @brief Lists subject areas with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of subject areas for the requested page.
     */
    std::vector<domain::subject_area> list_areas(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active subject areas.
     *
     * @return Total number of active subject areas.
     */
    std::uint32_t count_areas();


    /**
     * @brief Retrieves a single subject area as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The subject area at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::subject_area> get_area_at_version(const std::string& name,
                                                            const std::string& domain_name,
                                                            std::uint32_t version);

    /**
     * @brief Retrieves a single subject area by its primary key.
     *
     * @return The subject area if found, std::nullopt otherwise.
     */
    std::optional<domain::subject_area> get_area(const std::string& name,
                                                 const std::string& domain_name);

    /**
     * @brief Retrieves a batch of subject areas by primary key.
     */
    std::vector<domain::subject_area> get_areas(const std::vector<std::string>& names,
                                                const std::vector<std::string>& domain_names);

    /**
     * @brief Saves a subject area (creates or updates).
     *
     * @param area The subject area to save.
     * @throws std::exception on failure.
     */
    void save_area(const domain::subject_area& area);

    /**
     * @brief Saves a batch of subject areas.
     *
     * @param areas The subject areas to save.
     * @throws std::exception on failure.
     */
    void save_areas(const std::vector<domain::subject_area>& areas);

    /**
     * @brief Deletes a subject area by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_area(const std::string& name, const std::string& domain_name);

    /**
     * @brief Deletes subject areas by their primary keys.
     */
    void delete_areas(const std::vector<std::string>& names,
                      const std::vector<std::string>& domain_names);

    /**
     * @brief Retrieves all historical versions of a subject area.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::subject_area> get_area_history(const std::string& name,
                                                       const std::string& domain_name);

private:
    context ctx_;
    repository::subject_area_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::subject_area_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::subject_area& out);
};

}

#endif
