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
#ifndef ORES_REFDATA_CORE_SERVICE_SANDBOX_MEMBER_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_SANDBOX_MEMBER_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/sandbox_member.hpp"
#include "ores.refdata.api/messaging/sandbox_member_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/sandbox_member_repository.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing sandbox members.
 *
 * Provides a higher-level interface for sandbox member operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT sandbox_member_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.sandbox_member_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a sandbox_member_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit sandbox_member_service(context ctx);

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
    messaging::list_sandbox_members_response
    list_sandbox_members(const messaging::list_sandbox_members_request& request);
    messaging::get_sandbox_member_response
    get_sandbox_member(const messaging::get_sandbox_member_request& request);
    messaging::get_many_sandbox_members_response
    get_many_sandbox_members(const messaging::get_many_sandbox_members_request& request);
    messaging::put_sandbox_member_response
    put_sandbox_member(const messaging::put_sandbox_member_request& request);
    messaging::put_many_sandbox_members_response
    put_many_sandbox_members(const messaging::put_many_sandbox_members_request& request);
    messaging::delete_sandbox_member_response
    delete_sandbox_member(const messaging::delete_sandbox_member_request& request);
    messaging::delete_many_sandbox_members_response
    delete_many_sandbox_members(const messaging::delete_many_sandbox_members_request& request);
    messaging::list_by_sandbox_id_sandbox_members_response list_by_sandbox_id_sandbox_members(
        const messaging::list_by_sandbox_id_sandbox_members_request& request);
    messaging::list_by_account_id_sandbox_members_response list_by_account_id_sandbox_members(
        const messaging::list_by_account_id_sandbox_members_request& request);
    messaging::list_sandbox_member_versions_response
    list_sandbox_member_versions(const messaging::list_sandbox_member_versions_request& request);
    messaging::get_sandbox_member_version_response
    get_sandbox_member_version(const messaging::get_sandbox_member_version_request& request);
    /**@}*/

    /**
     * @brief Lists sandbox members with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of sandbox members for the requested page.
     */
    std::vector<domain::sandbox_member> list_sandbox_members(std::uint32_t offset,
                                                             std::uint32_t limit);

    /**
     * @brief Gets the total count of active sandbox members.
     *
     * @return Total number of active sandbox members.
     */
    std::uint32_t count_sandbox_members();


    /**
     * @brief Lists sandbox members filtered by sandbox_id, with pagination.
     *
     * @param sandbox_id The sandbox_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching sandbox members for the requested page.
     */
    std::vector<domain::sandbox_member> list_sandbox_members_by_sandbox_id(
        const std::string& sandbox_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active sandbox members filtered by sandbox_id.
     *
     * @param sandbox_id The sandbox_id to filter by.
     * @return Total number of matching sandbox members.
     */
    std::uint32_t count_sandbox_members_by_sandbox_id(const std::string& sandbox_id);

    /**
     * @brief Lists sandbox members filtered by sandbox_id, with pagination.
     *
     * @param sandbox_id The sandbox_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching sandbox members for the requested page.
     */
    std::vector<domain::sandbox_member> list_sandbox_members_by_sandbox_id(
        const boost::uuids::uuid& sandbox_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active sandbox members filtered by sandbox_id.
     *
     * @param sandbox_id The sandbox_id to filter by.
     * @return Total number of matching sandbox members.
     */
    std::uint32_t count_sandbox_members_by_sandbox_id(const boost::uuids::uuid& sandbox_id);


    /**
     * @brief Lists sandbox members filtered by account_id, with pagination.
     *
     * @param account_id The account_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching sandbox members for the requested page.
     */
    std::vector<domain::sandbox_member> list_sandbox_members_by_account_id(
        const std::string& account_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active sandbox members filtered by account_id.
     *
     * @param account_id The account_id to filter by.
     * @return Total number of matching sandbox members.
     */
    std::uint32_t count_sandbox_members_by_account_id(const std::string& account_id);

    /**
     * @brief Lists sandbox members filtered by account_id, with pagination.
     *
     * @param account_id The account_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching sandbox members for the requested page.
     */
    std::vector<domain::sandbox_member> list_sandbox_members_by_account_id(
        const boost::uuids::uuid& account_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active sandbox members filtered by account_id.
     *
     * @param account_id The account_id to filter by.
     * @return Total number of matching sandbox members.
     */
    std::uint32_t count_sandbox_members_by_account_id(const boost::uuids::uuid& account_id);


    /**
     * @brief Retrieves a single sandbox member as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The sandbox member at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::sandbox_member>
    get_sandbox_member_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single sandbox member by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The sandbox member if found, std::nullopt otherwise.
     */
    std::optional<domain::sandbox_member> get_sandbox_member(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single sandbox member by its uuid primary key.
     *
     * @return The sandbox member if found, std::nullopt otherwise.
     */
    std::optional<domain::sandbox_member> find_sandbox_member(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of sandbox members by primary key.
     */
    std::vector<domain::sandbox_member> get_sandbox_members(const std::vector<std::string>& ids);

    /**
     * @brief Saves a sandbox member (creates or updates).
     *
     * @param sandbox_member The sandbox member to save.
     * @throws std::exception on failure.
     */
    void save_sandbox_member(const domain::sandbox_member& sandbox_member);

    /**
     * @brief Saves a batch of sandbox members.
     *
     * @param sandbox_members The sandbox members to save.
     * @throws std::exception on failure.
     */
    void save_sandbox_members(const std::vector<domain::sandbox_member>& sandbox_members);

    /**
     * @brief Deletes a sandbox member by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_sandbox_member(const boost::uuids::uuid& id);

    /**
     * @brief Removes a sandbox member by its uuid primary key.
     *
     * @throws std::exception on failure.
     */
    void remove_sandbox_member(const boost::uuids::uuid& id);

    /**
     * @brief Deletes sandbox members by their primary keys.
     */
    void delete_sandbox_members(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a sandbox member.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::sandbox_member> get_sandbox_member_history(const std::string& id);

    /**
     * @brief Retrieves all historical versions of a sandbox member
     * by its uuid primary key.
     */
    std::vector<domain::sandbox_member> get_sandbox_member_history(const boost::uuids::uuid& id);

private:
    context ctx_;
    repository::sandbox_member_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::sandbox_member_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::sandbox_member& out);
};

}

#endif
