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
#ifndef ORES_IAM_CORE_SERVICE_EXTERNAL_IDENTITY_SERVICE_HPP
#define ORES_IAM_CORE_SERVICE_EXTERNAL_IDENTITY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/external_identity.hpp"
#include "ores.iam.api/messaging/external_identity_protocol.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.iam.core/repository/external_identity_repository.hpp"
#include "ores.logging/logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::service {

/**
 * @brief Service for managing external identities.
 *
 * Provides a higher-level interface for external identity operations,
 * wrapping the underlying repository.
 */
class ORES_IAM_CORE_EXPORT external_identity_service {
private:
    inline static std::string_view logger_name = "ores.iam.service.external_identity_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a external_identity_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit external_identity_service(const context& ctx);

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
    ::ores::iam::messaging::list_external_identities_response list_external_identities(
        const ::ores::iam::messaging::list_external_identities_request& request);
    ::ores::iam::messaging::get_external_identity_response
    get_external_identity(const ::ores::iam::messaging::get_external_identity_request& request);
    ::ores::iam::messaging::get_many_external_identities_response get_many_external_identities(
        const ::ores::iam::messaging::get_many_external_identities_request& request);
    ::ores::iam::messaging::put_external_identity_response
    put_external_identity(const ::ores::iam::messaging::put_external_identity_request& request);
    ::ores::iam::messaging::put_many_external_identities_response put_many_external_identities(
        const ::ores::iam::messaging::put_many_external_identities_request& request);
    ::ores::iam::messaging::delete_external_identity_response delete_external_identity(
        const ::ores::iam::messaging::delete_external_identity_request& request);
    ::ores::iam::messaging::delete_many_external_identities_response
    delete_many_external_identities(
        const ::ores::iam::messaging::delete_many_external_identities_request& request);
    ::ores::iam::messaging::list_by_account_id_external_identities_response
    list_by_account_id_external_identities(
        const ::ores::iam::messaging::list_by_account_id_external_identities_request& request);
    ::ores::iam::messaging::list_by_identity_provider_id_external_identities_response
    list_by_identity_provider_id_external_identities(
        const ::ores::iam::messaging::list_by_identity_provider_id_external_identities_request&
            request);
    ::ores::iam::messaging::list_external_identity_versions_response
    list_external_identity_versions(
        const ::ores::iam::messaging::list_external_identity_versions_request& request);
    ::ores::iam::messaging::get_external_identity_version_response get_external_identity_version(
        const ::ores::iam::messaging::get_external_identity_version_request& request);
    /**@}*/

    /**
     * @brief Lists external identities with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of external identities for the requested page.
     */
    std::vector<domain::external_identity> list_links(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active external identities.
     *
     * @return Total number of active external identities.
     */
    std::uint32_t count_links();


    /**
     * @brief Lists external identities filtered by account_id, with pagination.
     *
     * @param account_id The account_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching external identities for the requested page.
     */
    std::vector<domain::external_identity> list_links_by_account_id(const std::string& account_id,
                                                                    std::uint32_t offset,
                                                                    std::uint32_t limit);

    /**
     * @brief Gets the total count of active external identities filtered by account_id.
     *
     * @param account_id The account_id to filter by.
     * @return Total number of matching external identities.
     */
    std::uint32_t count_links_by_account_id(const std::string& account_id);


    /**
     * @brief Lists external identities filtered by identity_provider_id, with pagination.
     *
     * @param identity_provider_id The identity_provider_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching external identities for the requested page.
     */
    std::vector<domain::external_identity> list_links_by_identity_provider_id(
        const std::string& identity_provider_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active external identities filtered by identity_provider_id.
     *
     * @param identity_provider_id The identity_provider_id to filter by.
     * @return Total number of matching external identities.
     */
    std::uint32_t count_links_by_identity_provider_id(const std::string& identity_provider_id);


    /**
     * @brief Retrieves a single external identity as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The external identity at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::external_identity> get_link_at_version(const boost::uuids::uuid& id,
                                                                 std::uint32_t version);

    /**
     * @brief Retrieves a single external identity by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The external identity if found, std::nullopt otherwise.
     */
    std::optional<domain::external_identity> get_link(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single external identity by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The external identity if found, std::nullopt otherwise.
     */
    std::optional<domain::external_identity> get_link_by_email(const std::string& email);

    /**
     * @brief Retrieves a batch of external identities by primary key.
     */
    std::vector<domain::external_identity> get_links(const std::vector<std::string>& ids);

    /**
     * @brief Saves a external identity (creates or updates).
     *
     * @param link The external identity to save.
     * @throws std::exception on failure.
     */
    void save_link(const domain::external_identity& link);

    /**
     * @brief Saves a batch of external identities.
     *
     * @param links The external identities to save.
     * @throws std::exception on failure.
     */
    void save_links(const std::vector<domain::external_identity>& links);

    /**
     * @brief Deletes a external identity by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_link(const boost::uuids::uuid& id);

    /**
     * @brief Deletes external identities by their primary keys.
     */
    void delete_links(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a external identity.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::external_identity> get_link_history(const std::string& key);

private:
    context ctx_;
    repository::external_identity_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * The caller reads the current row, so a batch reads every row it names
     * in one query rather than one query per change.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param current The row the change names as it is now, or empty.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result
    prepare_change(const ::ores::iam::messaging::external_identity_change& change,
                   const ores::utility::domain::change_intent& intent,
                   const std::vector<domain::external_identity>& current,
                   domain::external_identity& out);
};

}

#endif
