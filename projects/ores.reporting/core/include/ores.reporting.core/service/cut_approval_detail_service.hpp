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
#ifndef ORES_REPORTING_CORE_SERVICE_CUT_APPROVAL_DETAIL_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_CUT_APPROVAL_DETAIL_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/cut_approval_detail.hpp"
#include "ores.reporting.api/messaging/cut_approval_detail_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/cut_approval_detail_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Service for managing cut approval details.
 *
 * Provides a higher-level interface for cut approval detail operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT cut_approval_detail_service {
private:
    inline static std::string_view logger_name =
        "ores.reporting.service.cut_approval_detail_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a cut_approval_detail_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit cut_approval_detail_service(context ctx);

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
    ::ores::reporting::messaging::list_cut_approval_details_response list_cut_approval_details(
        const ::ores::reporting::messaging::list_cut_approval_details_request& request);
    ::ores::reporting::messaging::get_cut_approval_detail_response get_cut_approval_detail(
        const ::ores::reporting::messaging::get_cut_approval_detail_request& request);
    ::ores::reporting::messaging::get_many_cut_approval_details_response
    get_many_cut_approval_details(
        const ::ores::reporting::messaging::get_many_cut_approval_details_request& request);
    ::ores::reporting::messaging::list_by_request_id_cut_approval_details_response
    list_by_request_id_cut_approval_details(
        const ::ores::reporting::messaging::list_by_request_id_cut_approval_details_request&
            request);
    ::ores::reporting::messaging::list_by_cut_id_cut_approval_details_response
    list_by_cut_id_cut_approval_details(
        const ::ores::reporting::messaging::list_by_cut_id_cut_approval_details_request& request);
    ::ores::reporting::messaging::list_cut_approval_detail_versions_response
    list_cut_approval_detail_versions(
        const ::ores::reporting::messaging::list_cut_approval_detail_versions_request& request);
    ::ores::reporting::messaging::get_cut_approval_detail_version_response
    get_cut_approval_detail_version(
        const ::ores::reporting::messaging::get_cut_approval_detail_version_request& request);
    /**@}*/

    /**
     * @brief Lists cut approval details with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of cut approval details for the requested page.
     */
    std::vector<domain::cut_approval_detail> list_details(std::uint32_t offset,
                                                          std::uint32_t limit);

    /**
     * @brief Gets the total count of active cut approval details.
     *
     * @return Total number of active cut approval details.
     */
    std::uint32_t count_details();


    /**
     * @brief Lists cut approval details filtered by request_id, with pagination.
     *
     * @param request_id The request_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching cut approval details for the requested page.
     */
    std::vector<domain::cut_approval_detail> list_details_by_request_id(
        const std::string& request_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active cut approval details filtered by request_id.
     *
     * @param request_id The request_id to filter by.
     * @return Total number of matching cut approval details.
     */
    std::uint32_t count_details_by_request_id(const std::string& request_id);


    /**
     * @brief Lists cut approval details filtered by cut_id, with pagination.
     *
     * @param cut_id The cut_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching cut approval details for the requested page.
     */
    std::vector<domain::cut_approval_detail>
    list_details_by_cut_id(const std::string& cut_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active cut approval details filtered by cut_id.
     *
     * @param cut_id The cut_id to filter by.
     * @return Total number of matching cut approval details.
     */
    std::uint32_t count_details_by_cut_id(const std::string& cut_id);


    /**
     * @brief Retrieves a single cut approval detail as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The cut approval detail at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::cut_approval_detail> get_detail_at_version(const boost::uuids::uuid& id,
                                                                     std::uint32_t version);

    /**
     * @brief Retrieves a single cut approval detail by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The cut approval detail if found, std::nullopt otherwise.
     */
    std::optional<domain::cut_approval_detail> get_detail(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of cut approval details by primary key.
     */
    std::vector<domain::cut_approval_detail> get_details(const std::vector<std::string>& ids);

    /**
     * @brief Saves a cut approval detail (creates or updates).
     *
     * @param detail The cut approval detail to save.
     * @throws std::exception on failure.
     */
    void save_detail(const domain::cut_approval_detail& detail);

    /**
     * @brief Saves a batch of cut approval details.
     *
     * @param details The cut approval details to save.
     * @throws std::exception on failure.
     */
    void save_details(const std::vector<domain::cut_approval_detail>& details);

    /**
     * @brief Deletes a cut approval detail by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_detail(const boost::uuids::uuid& id);

    /**
     * @brief Deletes cut approval details by their primary keys.
     */
    void delete_details(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a cut approval detail.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::cut_approval_detail> get_detail_history(const std::string& id);

private:
    context ctx_;
    repository::cut_approval_detail_repository repo_;
};

}

#endif
