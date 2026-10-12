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
#ifndef ORES_IAM_CORE_REPOSITORY_ROLE_PERMISSION_REPOSITORY_HPP
#define ORES_IAM_CORE_REPOSITORY_ROLE_PERMISSION_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/role_permission.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::iam::repository {

/**
 * @brief One version of a role permission, with the end of its validity.
 *
 * A link is closed by ending its validity, not by deleting the row, so a closed
 * link is a row whose end is in the past. @c closed_at is set on a version that
 * ended because the link was closed, and empty on a version that a later
 * version replaced and on a link that is open. The domain type does not carry
 * it, and it is final, so the history row holds the link beside it. The actor
 * and the time stand flat on the row as well, which is the shape the history
 * builder reads.
 */
struct role_permission_history_row {
    domain::role_permission link;
    std::string modified_by;
    std::chrono::system_clock::time_point recorded_at;
    std::optional<std::chrono::system_clock::time_point> closed_at;
};

/**
 * @brief Reads and writes role permissions to data storage.
 */
class ORES_IAM_CORE_EXPORT role_permission_repository {
private:
    inline static std::string_view logger_name = "ores.iam.repository.role_permission_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit role_permission_repository(context ctx);

    std::string sql();

    /**
     * @brief Writes role permissions to database.
     *
     * The plain form replaces the link the caller last read: it states the
     * version the row carries now, so the store can tell a replace from a
     * create. A row that moved on since that read is a conflict, never a
     * silent overwrite.
     */
    /**@{*/
    void write(const domain::role_permission& v);
    void write(const std::vector<domain::role_permission>& v);
    /**@}*/

    /**
     * @brief Writes a role permission, honouring the claim it states.
     *
     * The claim is the version the caller read (@c must_match_version), that no
     * current row exists (@c must_not_exist), or neither (@c any, which
     * replaces the row as it stands). The store decides in the write's own
     * transaction, so a create that collides with a live row and a write over a
     * row that moved on are refused by the store rather than by a check a
     * caller might have forgotten.
     */
    void write(const domain::role_permission& v, const ores::utility::domain::precondition& claim);

    /**
     * @brief Writes a set of role permissions, each honouring its own claim, as
     * one statement.
     */
    void write(const std::vector<domain::role_permission>& v,
               const std::vector<ores::utility::domain::precondition>& claims);

    std::vector<domain::role_permission> read_latest();
    std::vector<domain::role_permission> read_latest(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Reads the role permission rows for the given pair of keys.
     *
     * A junction key is the whole pair the link names, so a read that states
     * only one half addresses a set and not a row.
     */
    std::vector<domain::role_permission> read_latest(const boost::uuids::uuid& role_id,
                                                     const boost::uuids::uuid& permission_id);

    /**
     * @brief Gets the total count of active role permissions.
     */
    std::uint32_t get_total_role_permission_count();
    std::vector<domain::role_permission> read_latest_by_role(const boost::uuids::uuid& role_id);
    /**
     * @brief Reads latest role permissions filtered by role_id, with pagination.
     */
    std::vector<domain::role_permission> read_latest_by_role(const boost::uuids::uuid& role_id,
                                                             std::uint32_t offset,
                                                             std::uint32_t limit);

    /**
     * @brief Gets the total count of active role permissions filtered by role_id.
     */
    std::uint32_t get_total_role_permission_count_by_role(const boost::uuids::uuid& role_id);

    std::vector<domain::role_permission>
    read_latest_by_permission(const boost::uuids::uuid& permission_id);
    /**
     * @brief Reads latest role permissions filtered by permission_id, with pagination.
     */
    std::vector<domain::role_permission> read_latest_by_permission(
        const boost::uuids::uuid& permission_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active role permissions filtered by permission_id.
     */
    std::uint32_t
    get_total_role_permission_count_by_permission(const boost::uuids::uuid& permission_id);

    /**
     * @brief Reads every version of every role permission of one role.
     *
     * Closed links are included. The rows are grouped by permission
     * and run newest first within a group, so a caller numbers and diffs the
     * versions of one link by walking a group.
     */
    std::vector<role_permission_history_row> read_all_by_role(const boost::uuids::uuid& role_id);

    /**
     * @brief Deletes a role permission by its pair of keys.
     */
    void remove(const boost::uuids::uuid& role_id, const boost::uuids::uuid& permission_id);

    /**
     * @brief What a removal did, so a caller reports a conflict as an outcome
     * rather than catching an exception.
     *
     * @c missing means there was no current row to remove, and @c unsupported
     * means the store cannot answer the version at all.
     */
    enum class remove_status { removed, conflicting, missing, unsupported };

    /**
     * @brief Removes a role permission, refusing a row that moved on.
     *
     * A stated version is the version the caller read. The removal is refused
     * with @c conflicting when the current row carries another, so a caller
     * that decided on stale state cannot remove a change it never saw. A null
     * version removes whatever is current, which is what a caller that stated
     * no version asked for.
     */
    remove_status remove(const boost::uuids::uuid& role_id,
                         const boost::uuids::uuid& permission_id,
                         std::optional<std::uint32_t> version);

    /**
     * @brief Deletes role permissions by their pairs of keys.
     */
    void remove(const std::vector<boost::uuids::uuid>& role_ids,
                const std::vector<boost::uuids::uuid>& permission_ids);

    void remove_by_role(const boost::uuids::uuid& role_id);

private:
    context ctx_;

    /**
     * @brief The claim a replace makes: the version the row carries now, or
     * that no row exists yet.
     */
    ores::utility::domain::precondition replace_claim(const domain::role_permission& v);

    /**
     * @brief The object with the claim's version stamped onto it.
     *
     * A claim the store cannot check is refused here rather than ignored.
     */
    domain::role_permission apply_claim(const domain::role_permission& v,
                                        const ores::utility::domain::precondition& claim);
};

}

#endif
