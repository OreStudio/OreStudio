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
#ifndef ORES_REFDATA_CORE_SERVICE_REGULATORY_CORPORATE_SECTOR_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_REGULATORY_CORPORATE_SECTOR_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/regulatory_corporate_sector.hpp"
#include "ores.refdata.api/messaging/regulatory_corporate_sector_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/regulatory_corporate_sector_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing regulatory corporate sectors.
 *
 * Provides a higher-level interface for regulatory corporate sector operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT regulatory_corporate_sector_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.regulatory_corporate_sector_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a regulatory_corporate_sector_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit regulatory_corporate_sector_service(context ctx);

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
    ::ores::refdata::messaging::list_regulatory_corporate_sectors_response
    list_regulatory_corporate_sectors(
        const ::ores::refdata::messaging::list_regulatory_corporate_sectors_request& request);
    ::ores::refdata::messaging::get_regulatory_corporate_sector_response
    get_regulatory_corporate_sector(
        const ::ores::refdata::messaging::get_regulatory_corporate_sector_request& request);
    ::ores::refdata::messaging::get_many_regulatory_corporate_sectors_response
    get_many_regulatory_corporate_sectors(
        const ::ores::refdata::messaging::get_many_regulatory_corporate_sectors_request& request);
    ::ores::refdata::messaging::put_regulatory_corporate_sector_response
    put_regulatory_corporate_sector(
        const ::ores::refdata::messaging::put_regulatory_corporate_sector_request& request);
    ::ores::refdata::messaging::put_many_regulatory_corporate_sectors_response
    put_many_regulatory_corporate_sectors(
        const ::ores::refdata::messaging::put_many_regulatory_corporate_sectors_request& request);
    ::ores::refdata::messaging::delete_regulatory_corporate_sector_response
    delete_regulatory_corporate_sector(
        const ::ores::refdata::messaging::delete_regulatory_corporate_sector_request& request);
    ::ores::refdata::messaging::delete_many_regulatory_corporate_sectors_response
    delete_many_regulatory_corporate_sectors(
        const ::ores::refdata::messaging::delete_many_regulatory_corporate_sectors_request&
            request);
    ::ores::refdata::messaging::list_regulatory_corporate_sector_versions_response
    list_regulatory_corporate_sector_versions(
        const ::ores::refdata::messaging::list_regulatory_corporate_sector_versions_request&
            request);
    ::ores::refdata::messaging::get_regulatory_corporate_sector_version_response
    get_regulatory_corporate_sector_version(
        const ::ores::refdata::messaging::get_regulatory_corporate_sector_version_request& request);
    /**@}*/

    /**
     * @brief Lists regulatory corporate sectors with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of regulatory corporate sectors for the requested page.
     */
    std::vector<domain::regulatory_corporate_sector> list_sectors(std::uint32_t offset,
                                                                  std::uint32_t limit);

    /**
     * @brief Gets the total count of active regulatory corporate sectors.
     *
     * @return Total number of active regulatory corporate sectors.
     */
    std::uint32_t count_sectors();


    /**
     * @brief Retrieves a single regulatory corporate sector as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The regulatory corporate sector at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::regulatory_corporate_sector> get_sector_at_version(
        const std::string& code, const std::string& coding_scheme_code, std::uint32_t version);

    /**
     * @brief Retrieves a single regulatory corporate sector by its primary key.
     *
     * @return The regulatory corporate sector if found, std::nullopt otherwise.
     */
    std::optional<domain::regulatory_corporate_sector>
    get_sector(const std::string& code, const std::string& coding_scheme_code);

    /**
     * @brief Retrieves a batch of regulatory corporate sectors by primary key.
     */
    std::vector<domain::regulatory_corporate_sector>
    get_sectors(const std::vector<std::string>& codes,
                const std::vector<std::string>& coding_scheme_codes);

    /**
     * @brief Saves a regulatory corporate sector (creates or updates).
     *
     * @param sector The regulatory corporate sector to save.
     * @throws std::exception on failure.
     */
    void save_sector(const domain::regulatory_corporate_sector& sector);

    /**
     * @brief Saves a batch of regulatory corporate sectors.
     *
     * @param sectors The regulatory corporate sectors to save.
     * @throws std::exception on failure.
     */
    void save_sectors(const std::vector<domain::regulatory_corporate_sector>& sectors);

    /**
     * @brief Deletes a regulatory corporate sector by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_sector(const std::string& code, const std::string& coding_scheme_code);

    /**
     * @brief Deletes regulatory corporate sectors by their primary keys.
     */
    void delete_sectors(const std::vector<std::string>& codes,
                        const std::vector<std::string>& coding_scheme_codes);

    /**
     * @brief Retrieves all historical versions of a regulatory corporate sector.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::regulatory_corporate_sector>
    get_sector_history(const std::string& code, const std::string& coding_scheme_code);

private:
    context ctx_;
    repository::regulatory_corporate_sector_repository repo_;

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
    prepare_change(const ::ores::refdata::messaging::regulatory_corporate_sector_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::regulatory_corporate_sector& out);
};

}

#endif
