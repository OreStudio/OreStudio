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
#ifndef ORES_REFDATA_CORE_SERVICE_COUNTERPARTY_IDENTIFIER_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_COUNTERPARTY_IDENTIFIER_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/counterparty_identifier.hpp"
#include "ores.refdata.api/messaging/counterparty_identifier_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/counterparty_identifier_repository.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing counterparty identifiers.
 *
 * Provides a higher-level interface for counterparty identifier operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT counterparty_identifier_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.counterparty_identifier_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a counterparty_identifier_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit counterparty_identifier_service(context ctx);

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
    messaging::list_counterparty_identifiers_response
    list_counterparty_identifiers(const messaging::list_counterparty_identifiers_request& request);
    messaging::get_counterparty_identifier_response
    get_counterparty_identifier(const messaging::get_counterparty_identifier_request& request);
    messaging::get_many_counterparty_identifiers_response get_many_counterparty_identifiers(
        const messaging::get_many_counterparty_identifiers_request& request);
    messaging::put_counterparty_identifier_response
    put_counterparty_identifier(const messaging::put_counterparty_identifier_request& request);
    messaging::put_many_counterparty_identifiers_response put_many_counterparty_identifiers(
        const messaging::put_many_counterparty_identifiers_request& request);
    messaging::delete_counterparty_identifier_response delete_counterparty_identifier(
        const messaging::delete_counterparty_identifier_request& request);
    messaging::delete_many_counterparty_identifiers_response delete_many_counterparty_identifiers(
        const messaging::delete_many_counterparty_identifiers_request& request);
    messaging::list_by_counterparty_id_counterparty_identifiers_response
    list_by_counterparty_id_counterparty_identifiers(
        const messaging::list_by_counterparty_id_counterparty_identifiers_request& request);
    messaging::list_counterparty_identifier_versions_response list_counterparty_identifier_versions(
        const messaging::list_counterparty_identifier_versions_request& request);
    messaging::get_counterparty_identifier_version_response get_counterparty_identifier_version(
        const messaging::get_counterparty_identifier_version_request& request);
    /**@}*/

    /**
     * @brief Lists counterparty identifiers with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of counterparty identifiers for the requested page.
     */
    std::vector<domain::counterparty_identifier> list_counterparty_identifiers(std::uint32_t offset,
                                                                               std::uint32_t limit);

    /**
     * @brief Gets the total count of active counterparty identifiers.
     *
     * @return Total number of active counterparty identifiers.
     */
    std::uint32_t count_counterparty_identifiers();


    /**
     * @brief Lists counterparty identifiers filtered by counterparty_id, with pagination.
     *
     * @param counterparty_id The counterparty_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching counterparty identifiers for the requested page.
     */
    std::vector<domain::counterparty_identifier> list_counterparty_identifiers_by_counterparty_id(
        const std::string& counterparty_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active counterparty identifiers filtered by counterparty_id.
     *
     * @param counterparty_id The counterparty_id to filter by.
     * @return Total number of matching counterparty identifiers.
     */
    std::uint32_t
    count_counterparty_identifiers_by_counterparty_id(const std::string& counterparty_id);

    /**
     * @brief Lists counterparty identifiers filtered by counterparty_id, with pagination.
     *
     * @param counterparty_id The counterparty_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching counterparty identifiers for the requested page.
     */
    std::vector<domain::counterparty_identifier> list_counterparty_identifiers_by_counterparty_id(
        const boost::uuids::uuid& counterparty_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active counterparty identifiers filtered by counterparty_id.
     *
     * @param counterparty_id The counterparty_id to filter by.
     * @return Total number of matching counterparty identifiers.
     */
    std::uint32_t
    count_counterparty_identifiers_by_counterparty_id(const boost::uuids::uuid& counterparty_id);


    /**
     * @brief Lists counterparty identifiers filtered by counterparty_id that were live at
     * any point during a parent version's own [valid_from, valid_to) window.
     * See the "Temporal composite entity versioning" architecture doc.
     *
     * @param counterparty_id The counterparty_id to filter by.
     * @param valid_from_bound The parent version's own valid_from.
     * @param valid_to_bound The parent version's own valid_to.
     * @return Vector of matching counterparty identifiers.
     */
    std::vector<domain::counterparty_identifier>
    list_counterparty_identifiers_by_counterparty_id_as_of(
        const std::string& counterparty_id,
        std::chrono::system_clock::time_point valid_from_bound,
        std::chrono::system_clock::time_point valid_to_bound);

    /**
     * @brief Retrieves a single counterparty identifier as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The counterparty identifier at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::counterparty_identifier>
    get_counterparty_identifier_at_version(const std::string& id, std::uint32_t version);

    /**
     * @brief Retrieves a single counterparty identifier by its primary key.
     *
     * @return The counterparty identifier if found, std::nullopt otherwise.
     */
    std::optional<domain::counterparty_identifier>
    get_counterparty_identifier(const std::string& id);

    /**
     * @brief Retrieves a single counterparty identifier by its uuid primary key.
     *
     * @return The counterparty identifier if found, std::nullopt otherwise.
     */
    std::optional<domain::counterparty_identifier>
    find_counterparty_identifier(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single counterparty identifier by its
     * counterparty_id and id_scheme (this entity's natural key is the
     * pair, not id_scheme alone).
     *
     * @return The counterparty identifier if found, std::nullopt otherwise.
     */
    std::optional<domain::counterparty_identifier>
    find_counterparty_identifier_by_code(const boost::uuids::uuid& counterparty_id,
                                         const std::string& id_scheme);

    /**
     * @brief Retrieves a batch of counterparty identifiers by primary key.
     */
    std::vector<domain::counterparty_identifier>
    get_counterparty_identifiers(const std::vector<std::string>& ids);

    /**
     * @brief Saves a counterparty identifier (creates or updates).
     *
     * @param counterparty_identifier The counterparty identifier to save.
     * @throws std::exception on failure.
     */
    void
    save_counterparty_identifier(const domain::counterparty_identifier& counterparty_identifier);

    /**
     * @brief Saves a batch of counterparty identifiers.
     *
     * @param counterparty_identifiers The counterparty identifiers to save.
     * @throws std::exception on failure.
     */
    void save_counterparty_identifiers(
        const std::vector<domain::counterparty_identifier>& counterparty_identifiers);

    /**
     * @brief Deletes a counterparty identifier by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_counterparty_identifier(const std::string& id);

    /**
     * @brief Removes a counterparty identifier by its uuid primary key.
     *
     * @throws std::exception on failure.
     */
    void remove_counterparty_identifier(const boost::uuids::uuid& id);

    /**
     * @brief Deletes counterparty identifiers by their primary keys.
     */
    void delete_counterparty_identifiers(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a counterparty identifier.
     */
    std::vector<domain::counterparty_identifier>
    get_counterparty_identifier_history(const std::string& id);

    /**
     * @brief Retrieves all historical versions of a counterparty identifier
     * by its uuid primary key.
     */
    std::vector<domain::counterparty_identifier>
    get_counterparty_identifier_history(const boost::uuids::uuid& id);

private:
    context ctx_;
    repository::counterparty_identifier_repository repo_;

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
    prepare_change(const messaging::counterparty_identifier_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::counterparty_identifier& out);
};

}

#endif
