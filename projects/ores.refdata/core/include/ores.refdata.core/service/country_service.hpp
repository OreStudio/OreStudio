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
#ifndef ORES_REFDATA_CORE_SERVICE_COUNTRY_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_COUNTRY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/country.hpp"
#include "ores.refdata.api/messaging/country_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/country_repository.hpp"
#include "ores.refdata.core/repository/party_country_repository.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing countries.
 *
 * Provides a higher-level interface for country operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT country_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.country_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a country_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit country_service(context ctx);

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
    messaging::list_countries_response
    list_countries(const messaging::list_countries_request& request);
    messaging::get_country_response get_country(const messaging::get_country_request& request);
    messaging::get_many_countries_response
    get_many_countries(const messaging::get_many_countries_request& request);
    messaging::put_country_response put_country(const messaging::put_country_request& request);
    messaging::put_many_countries_response
    put_many_countries(const messaging::put_many_countries_request& request);
    messaging::delete_country_response
    delete_country(const messaging::delete_country_request& request);
    messaging::delete_many_countries_response
    delete_many_countries(const messaging::delete_many_countries_request& request);
    messaging::list_country_versions_response
    list_country_versions(const messaging::list_country_versions_request& request);
    messaging::get_country_version_response
    get_country_version(const messaging::get_country_version_request& request);
    /**@}*/

    /**
     * @brief Lists countries with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of countries for the requested page.
     */
    std::vector<domain::country> list_countries(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active countries.
     *
     * @return Total number of active countries.
     */
    std::uint32_t count_countries();


    /**
     * @brief Retrieves a single country as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The country at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::country> get_country_at_version(const std::string& alpha2_code,
                                                          std::uint32_t version);

    /**
     * @brief Retrieves a single country by its primary key.
     *
     * @return The country if found, std::nullopt otherwise.
     */
    std::optional<domain::country> get_country(const std::string& alpha2_code);

    /**
     * @brief Retrieves a batch of countries by primary key.
     */
    std::vector<domain::country> get_countries(const std::vector<std::string>& alpha2_codes);

    /**
     * @brief Saves a country (creates or updates).
     *
     * @param country The country to save.
     * @throws std::exception on failure.
     */
    void save_country(const domain::country& country);

    /**
     * @brief Saves a batch of countries.
     *
     * @param countries The countries to save.
     * @throws std::exception on failure.
     */
    void save_countries(const std::vector<domain::country>& countries);

    /**
     * @brief Deletes a country by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_country(const std::string& alpha2_code);

    /**
     * @brief Deletes countries by their primary keys.
     */
    void delete_countries(const std::vector<std::string>& alpha2_codes);

    /**
     * @brief Retrieves all historical versions of a country.
     */
    std::vector<domain::country> get_country_history(const std::string& alpha2_code);

    /**
     * @brief Lists countries visible to a specific party, with pagination.
     *
     * @param party_id The UUID of the party.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of visible countries for the requested page.
     */
    std::vector<domain::country> list_countries_for_party(const boost::uuids::uuid& party_id,
                                                          std::uint32_t offset,
                                                          std::uint32_t limit);

    /**
     * @brief Gets the total count of countries visible to a specific party.
     *
     * @param party_id The UUID of the party.
     * @return Number of countries the party is permitted to see.
     */
    std::uint32_t count_countries_for_party(const boost::uuids::uuid& party_id);

private:
    context ctx_;
    repository::country_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::country_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::country& out);
    repository::party_country_repository junction_repo_;
};

}

#endif
