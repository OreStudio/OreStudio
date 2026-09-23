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
#ifndef ORES_COMPUTE_CORE_REPOSITORY_APP_VERSION_PLATFORM_REPOSITORY_HPP
#define ORES_COMPUTE_CORE_REPOSITORY_APP_VERSION_PLATFORM_REPOSITORY_HPP

#include "ores.compute.api/domain/app_version_platform.hpp"
#include "ores.compute.core/export.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::compute::repository {

/**
 * @brief Reads and writes app version platforms to data storage.
 */
class ORES_COMPUTE_CORE_EXPORT app_version_platform_repository {
private:
    inline static std::string_view logger_name =
        "ores.compute.repository.app_version_platform_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit app_version_platform_repository(context ctx);

    std::string sql();

    /**
     * @brief Writes app version platforms to database.
     *
     * The plain form replaces the link the caller last read: it states the
     * version the row carries now, so the store can tell a replace from a
     * create. A row that moved on since that read is a conflict, never a
     * silent overwrite.
     */
    /**@{*/
    void write(const domain::app_version_platform& v);
    void write(const std::vector<domain::app_version_platform>& v);
    /**@}*/

    /**
     * @brief Writes a app version platform, honouring the claim it states.
     *
     * The claim is the version the caller read (@c must_match_version), that no
     * current row exists (@c must_not_exist), or neither (@c any, which
     * replaces the row as it stands). The store decides in the write's own
     * transaction, so a create that collides with a live row and a write over a
     * row that moved on are refused by the store rather than by a check a
     * caller might have forgotten.
     */
    void write(const domain::app_version_platform& v,
               const ores::utility::domain::precondition& claim);

    /**
     * @brief Writes a set of app version platforms, each honouring its own claim, as
     * one statement.
     */
    void write(const std::vector<domain::app_version_platform>& v,
               const std::vector<ores::utility::domain::precondition>& claims);

    std::vector<domain::app_version_platform> read_latest();
    std::vector<domain::app_version_platform> read_latest(std::uint32_t offset,
                                                          std::uint32_t limit);

    /**
     * @brief Reads the app version platform rows for the given pair of keys.
     *
     * A junction key is the whole pair the link names, so a read that states
     * only one half addresses a set and not a row.
     */
    std::vector<domain::app_version_platform> read_latest(const boost::uuids::uuid& app_version_id,
                                                          const boost::uuids::uuid& platform_id);

    /**
     * @brief Gets the total count of active app version platforms.
     */
    std::uint32_t get_total_app_version_platform_count();
    std::vector<domain::app_version_platform>
    read_latest_by_app_version(const boost::uuids::uuid& app_version_id);
    /**
     * @brief Reads latest app version platforms filtered by app_version_id, with pagination.
     */
    std::vector<domain::app_version_platform> read_latest_by_app_version(
        const boost::uuids::uuid& app_version_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active app version platforms filtered by app_version_id.
     */
    std::uint32_t
    get_total_app_version_platform_count_by_app_version(const boost::uuids::uuid& app_version_id);

    std::vector<domain::app_version_platform>
    read_latest_by_platform(const boost::uuids::uuid& platform_id);

    /**
     * @brief Gets the total count of active app version platforms filtered by platform_id.
     */
    std::uint32_t
    get_total_app_version_platform_count_by_platform(const boost::uuids::uuid& platform_id);

    /**
     * @brief Deletes a app version platform by its pair of keys.
     */
    void remove(const boost::uuids::uuid& app_version_id, const boost::uuids::uuid& platform_id);

    /**
     * @brief What a removal did, so a caller reports a conflict as an outcome
     * rather than catching an exception.
     *
     * @c missing means there was no current row to remove, and @c unsupported
     * means the store cannot answer the version at all.
     */
    enum class remove_status { removed, conflicting, missing, unsupported };

    /**
     * @brief Removes a app version platform, refusing a row that moved on.
     *
     * A stated version is the version the caller read. The removal is refused
     * with @c conflicting when the current row carries another, so a caller
     * that decided on stale state cannot remove a change it never saw. A null
     * version removes whatever is current, which is what a caller that stated
     * no version asked for.
     */
    remove_status remove(const boost::uuids::uuid& app_version_id,
                         const boost::uuids::uuid& platform_id,
                         std::optional<std::uint32_t> version);

    /**
     * @brief Deletes app version platforms by their pairs of keys.
     */
    void remove(const std::vector<boost::uuids::uuid>& app_version_ids,
                const std::vector<boost::uuids::uuid>& platform_ids);

    void remove_by_app_version(const boost::uuids::uuid& app_version_id);

private:
    context ctx_;

    /**
     * @brief The claim a replace makes: the version the row carries now, or
     * that no row exists yet.
     */
    ores::utility::domain::precondition replace_claim(const domain::app_version_platform& v);

    /**
     * @brief The object with the claim's version stamped onto it.
     *
     * A claim the store cannot check is refused here rather than ignored.
     */
    domain::app_version_platform apply_claim(const domain::app_version_platform& v,
                                             const ores::utility::domain::precondition& claim);
};

}

#endif
