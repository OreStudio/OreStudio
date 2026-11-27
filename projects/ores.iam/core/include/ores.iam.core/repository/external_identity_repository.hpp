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
 * Template: cpp_domain_type_repository.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_IAM_CORE_REPOSITORY_EXTERNAL_IDENTITY_REPOSITORY_HPP
#define ORES_IAM_CORE_REPOSITORY_EXTERNAL_IDENTITY_REPOSITORY_HPP

#include "ores.database/domain/context_fwd.hpp"
#include "ores.iam.api/domain/external_identity.hpp"
#include "ores.iam.api/messaging/external_identity_protocol.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.logging/logger.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::iam::repository {

/**
 * @brief Reads and writes external identities to data storage.
 */
class ORES_IAM_CORE_EXPORT external_identity_repository {
private:
    inline static std::string_view logger_name = "ores.iam.repository.external_identity_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Returns the insert statement the repository writes.
     */
    std::string sql();

    /**
     * @brief Writes external identities to database.
     *
     * The plain form replaces the row the caller last read: it states the
     * version the row carries now, so the store can tell a replace from a
     * create. A row that moved on since that read is a conflict, never a silent
     * overwrite.
     */
    /**@{*/
    void write(const context& ctx, const domain::external_identity& v);
    void write(const context& ctx, const std::vector<domain::external_identity>& v);
    /**@}*/

    /**
     * @brief Writes a external identity, honouring the claim it states.
     *
     * The claim is the version the caller read (@c must_match_version), that no
     * current row exists (@c must_not_exist), or neither (@c any, which
     * replaces the row as it stands). The store decides in the write's own
     * transaction, so a create that collides with a live row and a write over a
     * row that moved on are refused by the store rather than by a check a
     * caller might have forgotten.
     */
    void write(const context& ctx,
               const domain::external_identity& v,
               const ores::utility::domain::precondition& claim);

    /**
     * @brief Writes a set of external identities, each honouring its own
     * claim, as one statement.
     */
    void write(const context& ctx,
               const std::vector<domain::external_identity>& v,
               const std::vector<ores::utility::domain::precondition>& claims);

    /**
     * @brief Reads latest external identities, possibly filtered by primary key.
     */
    /**@{*/
    std::vector<domain::external_identity> read_latest(const context& ctx);
    std::vector<domain::external_identity> read_latest(const context& ctx, const std::string& id);
    std::vector<domain::external_identity> read_latest(const context& ctx,
                                                       const std::vector<std::string>& ids);
    /**@}*/

    /**
     * @brief Reads latest external identities filtered by email.
     */
    std::vector<domain::external_identity> read_latest_by_email(const context& ctx,
                                                                const std::string& email);

    /**
     * @brief Reads latest external identities filtered by a set of email values.
     *
     * The batch form of the read above, with the same rules, so a batch
     * addressed by the declared key costs one read.
     */
    std::vector<domain::external_identity>
    read_latest_by_email(const context& ctx, const std::vector<std::string>& emails);

    /**
     * @brief Reads the newest external identities filtered by email, current or not.
     *
     * History is addressed by the key the model declares and must stay readable
     * after a delete, which closes the transaction-time window rather than
     * removing the row. A latest read cannot resolve a closed row, so this one
     * ignores the window and takes the newest match.
     */
    std::vector<domain::external_identity> read_any_by_email(const context& ctx,
                                                             const std::string& email);


    /**
     * @brief Reads all external identities, possibly filtered by primary key.
     */
    std::vector<domain::external_identity> read_all(const context& ctx, const std::string& id);

    /**
     * @brief Reads a single external identity as it stood at a specific
     * version — the version's own [valid_from, valid_to) window is returned
     * verbatim, so the caller can compose child entities "as of" the same
     * window. See the "Temporal composite entity versioning" architecture
     * doc.
     * @param ctx Repository context with database connection
     * @param version The version to fetch
     */
    std::optional<domain::external_identity>
    read_at_version(const context& ctx, const std::string& id, std::uint32_t version);

    /**
     * @brief Reads latest external identities filtered by account_id, with pagination.
     * @param ctx Repository context with database connection
     * @param account_id The account_id to filter by
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     * @param order The stated order; an empty field is the default order
     */
    std::vector<domain::external_identity> read_latest_by_account_id(
        const context& ctx,
        const std::string& account_id,
        std::uint32_t offset,
        std::uint32_t limit,
        const ores::utility::domain::order& order = {},
        const std::optional<messaging::external_identities_filter>& filter = std::nullopt);

    /**
     * @brief Gets the total count of active external identities filtered by account_id.
     */
    std::uint32_t get_total_link_count_by_account_id(
        const context& ctx,
        const std::string& account_id,
        const std::optional<messaging::external_identities_filter>& filter = std::nullopt);


    /**
     * @brief Reads latest external identities filtered by identity_provider_id, with pagination.
     * @param ctx Repository context with database connection
     * @param identity_provider_id The identity_provider_id to filter by
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     * @param order The stated order; an empty field is the default order
     */
    std::vector<domain::external_identity> read_latest_by_identity_provider_id(
        const context& ctx,
        const std::string& identity_provider_id,
        std::uint32_t offset,
        std::uint32_t limit,
        const ores::utility::domain::order& order = {},
        const std::optional<messaging::external_identities_filter>& filter = std::nullopt);

    /**
     * @brief Gets the total count of active external identities filtered by identity_provider_id.
     */
    std::uint32_t get_total_link_count_by_identity_provider_id(
        const context& ctx,
        const std::string& identity_provider_id,
        const std::optional<messaging::external_identities_filter>& filter = std::nullopt);


    /**
     * @brief Whether a list of external identities can be ordered by a field.
     *
     * The model's :sortable: columns, and nothing else.
     */
    static bool is_sortable(std::string_view field);

    /**
     * @brief Reads latest external identities with pagination support.
     * @param ctx Repository context with database connection
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     * @param order The stated order; an empty field is the default order
     * @param filter The filter record; the members it sets must all hold
     * @throws std::invalid_argument if the field is not sortable
     */
    std::vector<domain::external_identity>
    read_latest(const context& ctx,
                std::uint32_t offset,
                std::uint32_t limit,
                const ores::utility::domain::order& order = {},
                const std::optional<messaging::external_identities_filter>& filter = std::nullopt,
                const std::optional<std::string>& as_of = std::nullopt);

    /**
     * @brief Gets the total count of active external identities.
     * @param ctx Repository context with database connection
     * @return Total number of active external identities
     */
    std::uint32_t get_total_link_count(
        const context& ctx,
        const std::optional<messaging::external_identities_filter>& filter = std::nullopt,
        const std::optional<std::string>& as_of = std::nullopt);

    /**
     * @brief Deletes a external identity by closing its temporal validity.
     */
    void remove(const context& ctx, const std::string& id);

    /**
     * @brief What a removal did, so a caller reports a conflict as an outcome
     * rather than catching an exception.
     *
     * @c missing means there was no current row to remove, and @c unsupported
     * means the store cannot answer the version at all -- a current-state
     * table has no version column, so a versioned removal has no meaning
     * there.
     */
    enum class remove_status { removed, conflicting, missing, unsupported };

    /**
     * @brief Removes a external identity, refusing a row that moved on.
     *
     * A stated version is the version the caller read. The removal is refused
     * with @c conflicting when the current row carries another, so a caller
     * that decided on stale state cannot remove a change it never saw. A null
     * version removes whatever is current, which is what a caller that stated
     * no version asked for.
     */
    remove_status
    remove(const context& ctx, const std::string& id, std::optional<std::uint32_t> version);

    /**
     * @brief Deletes external identities by closing their temporal validity.
     */
    void remove(const context& ctx, const std::vector<std::string>& ids);
};

}

#endif
