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
#ifndef ORES_REFDATA_CORE_REPOSITORY_TENOR_CONVENTION_RESOLUTION_REPOSITORY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_TENOR_CONVENTION_RESOLUTION_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/tenor_convention_resolution.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <cstdint>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::refdata::repository {

/**
 * @brief Reads and writes tenor convention resolutions to data storage.
 */
class ORES_REFDATA_CORE_EXPORT tenor_convention_resolution_repository {
private:
    inline static std::string_view logger_name =
        "ores.refdata.repository.tenor_convention_resolution_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit tenor_convention_resolution_repository(context ctx);

    std::string sql();

    /**
     * @brief Writes tenor convention resolutions to database.
     *
     * The plain form replaces the link the caller last read: it states the
     * version the row carries now, so the store can tell a replace from a
     * create. A row that moved on since that read is a conflict, never a
     * silent overwrite.
     */
    /**@{*/
    void write(const domain::tenor_convention_resolution& v);
    void write(const std::vector<domain::tenor_convention_resolution>& v);
    /**@}*/

    /**
     * @brief Writes a tenor convention resolution, honouring the claim it states.
     *
     * The claim is the version the caller read (@c must_match_version), that no
     * current row exists (@c must_not_exist), or neither (@c any, which
     * replaces the row as it stands). The store decides in the write's own
     * transaction, so a create that collides with a live row and a write over a
     * row that moved on are refused by the store rather than by a check a
     * caller might have forgotten.
     */
    void write(const domain::tenor_convention_resolution& v,
               const ores::utility::domain::precondition& claim);

    /**
     * @brief Writes a set of tenor convention resolutions, each honouring its own claim, as
     * one statement.
     */
    void write(const std::vector<domain::tenor_convention_resolution>& v,
               const std::vector<ores::utility::domain::precondition>& claims);

    std::vector<domain::tenor_convention_resolution> read_latest();
    std::vector<domain::tenor_convention_resolution> read_latest(std::uint32_t offset,
                                                                 std::uint32_t limit);

    /**
     * @brief Reads the tenor convention resolution rows for the given pair of keys.
     *
     * A junction key is the whole pair the link names, so a read that states
     * only one half addresses a set and not a row.
     */
    std::vector<domain::tenor_convention_resolution> read_latest(const std::string& convention_code,
                                                                 const std::string& tenor_code);

    /**
     * @brief Gets the total count of active tenor convention resolutions.
     */
    std::uint32_t get_total_resolution_count();
    std::vector<domain::tenor_convention_resolution>
    read_latest_by_convention(const std::string& convention_code);
    /**
     * @brief Reads latest tenor convention resolutions filtered by convention_code, with
     * pagination.
     */
    std::vector<domain::tenor_convention_resolution> read_latest_by_convention(
        const std::string& convention_code, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active tenor convention resolutions filtered by
     * convention_code.
     */
    std::uint32_t get_total_resolution_count_by_convention(const std::string& convention_code);

    std::vector<domain::tenor_convention_resolution>
    read_latest_by_tenor(const std::string& tenor_code);

    /**
     * @brief Gets the total count of active tenor convention resolutions filtered by tenor_code.
     */
    std::uint32_t get_total_resolution_count_by_tenor(const std::string& tenor_code);

    /**
     * @brief Deletes a tenor convention resolution by its pair of keys.
     */
    void remove(const std::string& convention_code, const std::string& tenor_code);

    /**
     * @brief What a removal did, so a caller reports a conflict as an outcome
     * rather than catching an exception.
     *
     * @c missing means there was no current row to remove, and @c unsupported
     * means the store cannot answer the version at all.
     */
    enum class remove_status { removed, conflicting, missing, unsupported };

    /**
     * @brief Removes a tenor convention resolution, refusing a row that moved on.
     *
     * A stated version is the version the caller read. The removal is refused
     * with @c conflicting when the current row carries another, so a caller
     * that decided on stale state cannot remove a change it never saw. A null
     * version removes whatever is current, which is what a caller that stated
     * no version asked for.
     */
    remove_status remove(const std::string& convention_code,
                         const std::string& tenor_code,
                         std::optional<std::uint32_t> version);

    /**
     * @brief Deletes tenor convention resolutions by their pairs of keys.
     */
    void remove(const std::vector<std::string>& convention_codes,
                const std::vector<std::string>& tenor_codes);

    void remove_by_convention(const std::string& convention_code);

private:
    context ctx_;

    /**
     * @brief The claim a replace makes: the version the row carries now, or
     * that no row exists yet.
     */
    ores::utility::domain::precondition replace_claim(const domain::tenor_convention_resolution& v);

    /**
     * @brief The object with the claim's version stamped onto it.
     *
     * A claim the store cannot check is refused here rather than ignored.
     */
    domain::tenor_convention_resolution
    apply_claim(const domain::tenor_convention_resolution& v,
                const ores::utility::domain::precondition& claim);
};

}

#endif
