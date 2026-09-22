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
#ifndef ORES_REFDATA_CORE_SERVICE_PARTY_CONTACT_INFORMATION_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_PARTY_CONTACT_INFORMATION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/party_contact_information.hpp"
#include "ores.refdata.api/messaging/party_contact_information_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/party_contact_information_repository.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing party contact informations.
 *
 * Provides a higher-level interface for party contact information operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT party_contact_information_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.party_contact_information_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a party_contact_information_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit party_contact_information_service(context ctx);

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
    messaging::list_party_contact_informations_response list_party_contact_informations(
        const messaging::list_party_contact_informations_request& request);
    messaging::get_party_contact_information_response
    get_party_contact_information(const messaging::get_party_contact_information_request& request);
    messaging::get_many_party_contact_informations_response get_many_party_contact_informations(
        const messaging::get_many_party_contact_informations_request& request);
    messaging::put_party_contact_information_response
    put_party_contact_information(const messaging::put_party_contact_information_request& request);
    messaging::put_many_party_contact_informations_response put_many_party_contact_informations(
        const messaging::put_many_party_contact_informations_request& request);
    messaging::delete_party_contact_information_response delete_party_contact_information(
        const messaging::delete_party_contact_information_request& request);
    messaging::delete_many_party_contact_informations_response
    delete_many_party_contact_informations(
        const messaging::delete_many_party_contact_informations_request& request);
    messaging::list_by_party_id_party_contact_informations_response
    list_by_party_id_party_contact_informations(
        const messaging::list_by_party_id_party_contact_informations_request& request);
    messaging::list_party_contact_information_versions_response
    list_party_contact_information_versions(
        const messaging::list_party_contact_information_versions_request& request);
    messaging::get_party_contact_information_version_response get_party_contact_information_version(
        const messaging::get_party_contact_information_version_request& request);
    /**@}*/

    /**
     * @brief Lists party contact informations with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of party contact informations for the requested page.
     */
    std::vector<domain::party_contact_information>
    list_party_contact_informations(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active party contact informations.
     *
     * @return Total number of active party contact informations.
     */
    std::uint32_t count_party_contact_informations();


    /**
     * @brief Lists party contact informations filtered by party_id, with pagination.
     *
     * @param party_id The party_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching party contact informations for the requested page.
     */
    std::vector<domain::party_contact_information> list_party_contact_informations_by_party_id(
        const std::string& party_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active party contact informations filtered by party_id.
     *
     * @param party_id The party_id to filter by.
     * @return Total number of matching party contact informations.
     */
    std::uint32_t count_party_contact_informations_by_party_id(const std::string& party_id);

    /**
     * @brief Lists party contact informations filtered by party_id, with pagination.
     *
     * @param party_id The party_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching party contact informations for the requested page.
     */
    std::vector<domain::party_contact_information> list_party_contact_informations_by_party_id(
        const boost::uuids::uuid& party_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active party contact informations filtered by party_id.
     *
     * @param party_id The party_id to filter by.
     * @return Total number of matching party contact informations.
     */
    std::uint32_t count_party_contact_informations_by_party_id(const boost::uuids::uuid& party_id);


    /**
     * @brief Lists party contact informations filtered by party_id that were live at
     * any point during a parent version's own [valid_from, valid_to) window.
     * See the "Temporal composite entity versioning" architecture doc.
     *
     * @param party_id The party_id to filter by.
     * @param valid_from_bound The parent version's own valid_from.
     * @param valid_to_bound The parent version's own valid_to.
     * @return Vector of matching party contact informations.
     */
    std::vector<domain::party_contact_information>
    list_party_contact_informations_by_party_id_as_of(
        const std::string& party_id,
        std::chrono::system_clock::time_point valid_from_bound,
        std::chrono::system_clock::time_point valid_to_bound);

    /**
     * @brief Retrieves a single party contact information as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The party contact information at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::party_contact_information>
    get_party_contact_information_at_version(const std::string& id, std::uint32_t version);

    /**
     * @brief Retrieves a single party contact information by its primary key.
     *
     * @return The party contact information if found, std::nullopt otherwise.
     */
    std::optional<domain::party_contact_information>
    get_party_contact_information(const std::string& id);

    /**
     * @brief Retrieves a single party contact information by its uuid primary key.
     *
     * @return The party contact information if found, std::nullopt otherwise.
     */
    std::optional<domain::party_contact_information>
    find_party_contact_information(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single party contact information by its
     * party_id and contact_type (this entity's natural key is the
     * pair, not contact_type alone).
     *
     * @return The party contact information if found, std::nullopt otherwise.
     */
    std::optional<domain::party_contact_information>
    find_party_contact_information_by_code(const boost::uuids::uuid& party_id,
                                           const std::string& contact_type);

    /**
     * @brief Retrieves a batch of party contact informations by primary key.
     */
    std::vector<domain::party_contact_information>
    get_party_contact_informations(const std::vector<std::string>& ids);

    /**
     * @brief Saves a party contact information (creates or updates).
     *
     * @param party_contact_information The party contact information to save.
     * @throws std::exception on failure.
     */
    void save_party_contact_information(
        const domain::party_contact_information& party_contact_information);

    /**
     * @brief Saves a batch of party contact informations.
     *
     * @param party_contact_informations The party contact informations to save.
     * @throws std::exception on failure.
     */
    void save_party_contact_informations(
        const std::vector<domain::party_contact_information>& party_contact_informations);

    /**
     * @brief Deletes a party contact information by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_party_contact_information(const std::string& id);

    /**
     * @brief Removes a party contact information by its uuid primary key.
     *
     * @throws std::exception on failure.
     */
    void remove_party_contact_information(const boost::uuids::uuid& id);

    /**
     * @brief Deletes party contact informations by their primary keys.
     */
    void delete_party_contact_informations(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a party contact information.
     */
    std::vector<domain::party_contact_information>
    get_party_contact_information_history(const std::string& id);

    /**
     * @brief Retrieves all historical versions of a party contact information
     * by its uuid primary key.
     */
    std::vector<domain::party_contact_information>
    get_party_contact_information_history(const boost::uuids::uuid& id);

private:
    context ctx_;
    repository::party_contact_information_repository repo_;

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
    prepare_change(const messaging::party_contact_information_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::party_contact_information& out);
};

}

#endif
