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
#ifndef ORES_COMPUTE_CORE_REPOSITORY_GRID_SAMPLE_REPOSITORY_HPP
#define ORES_COMPUTE_CORE_REPOSITORY_GRID_SAMPLE_REPOSITORY_HPP

#include "ores.compute.api/domain/grid_sample.hpp"
#include "ores.compute.core/export.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::compute::repository {

/**
 * @brief Reads and writes grid samples to data storage.
 */
class ORES_COMPUTE_CORE_EXPORT grid_sample_repository {
private:
    inline static std::string_view logger_name = "ores.compute.repository.grid_sample_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Returns the SQL created by sqlgen to construct the table.
     */
    std::string sql();

    /**
     * @brief Writes grid samples to database.
     *
     * The plain form replaces the row the caller last read: it states the
     * version the row carries now, so the store can tell a replace from a
     * create. A row that moved on since that read is a conflict, never a silent
     * overwrite.
     */
    /**@{*/
    void write(context ctx, const domain::grid_sample& v);
    void write(context ctx, const std::vector<domain::grid_sample>& v);
    /**@}*/

    /**
     * @brief Writes a sample, honouring the claim it states.
     *
     * The claim is the version the caller read (@c must_match_version), that no
     * current row exists (@c must_not_exist), or neither (@c any, which
     * replaces the row as it stands). The store decides in the write's own
     * transaction, so a create that collides with a live row and a write over a
     * row that moved on are refused by the store rather than by a check a
     * caller might have forgotten.
     *
     * This table carries no version column, so the store cannot check a
     * version. A @c must_match_version claim is refused here, and a
     * @c must_not_exist claim over a live row is refused by the read below
     * rather than by the trigger.
     */
    void write(context ctx,
               const domain::grid_sample& v,
               const ores::utility::domain::precondition& claim);

    /**
     * @brief Writes a set of grid samples, each honouring its own
     * claim, as one statement.
     */
    void write(context ctx,
               const std::vector<domain::grid_sample>& v,
               const std::vector<ores::utility::domain::precondition>& claims);

    /**
     * @brief Reads latest grid samples, possibly filtered by primary key.
     */
    /**@{*/
    std::vector<domain::grid_sample> read_latest(context ctx);
    std::vector<domain::grid_sample>
    read_latest(context ctx, const std::string& id, const std::string& sampled_at);
    std::vector<domain::grid_sample> read_latest(context ctx,
                                                 const std::vector<std::string>& ids,
                                                 const std::vector<std::string>& sampled_ats);
    /**@}*/


    /**
     * @brief Reads the sample rows for the given primary key.
     *
     * A current-state table holds one row per key, so this is the single
     * current row, not a version history.
     */
    std::vector<domain::grid_sample>
    read_all(context ctx, const std::string& id, const std::string& sampled_at);


    /**
     * @brief Reads latest grid samples with pagination support.
     * @param ctx Repository context with database connection
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     */
    std::vector<domain::grid_sample>
    read_latest(context ctx, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Reads the newest sample for the caller.
     *
     * A time-series table is read from its newest end. A paged read walks the
     * key in order, so it returns the oldest row first and cannot answer "the
     * last one" without reading everything before it. The model names the
     * column the newest is measured by.
     */
    std::optional<domain::grid_sample> read_newest(context ctx);

    /**
     * @brief Gets the total count of active grid samples.
     * @param ctx Repository context with database connection
     * @return Total number of active grid samples
     */
    std::uint32_t get_total__count(context ctx);

    /**
     * @brief Deletes a sample permanently.
     *
     * A current-state table has no history, so the row is removed, not
     * soft-closed.
     */
    void remove(context ctx, const std::string& id, const std::string& sampled_at);

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
     * @brief Removes a sample, refusing a row that moved on.
     *
     * A stated version is the version the caller read. The removal is refused
     * with @c conflicting when the current row carries another, so a caller
     * that decided on stale state cannot remove a change it never saw. A null
     * version removes whatever is current, which is what a caller that stated
     * no version asked for.
     */
    remove_status remove(context ctx,
                         const std::string& id,
                         const std::string& sampled_at,
                         std::optional<std::uint32_t> version);

    /**
     * @brief Deletes grid samples permanently.
     */
    void remove(context ctx,
                const std::vector<std::string>& ids,
                const std::vector<std::string>& sampled_ats);


private:
    /**
     * @brief The claim a replace makes: the version the row carries now, or
     * that no row exists yet.
     */
    ores::utility::domain::precondition replace_claim(context ctx, const domain::grid_sample& v);

    /**
     * @brief The object with the claim's version stamped onto it.
     *
     * A claim the store cannot check is refused here rather than ignored.
     */
    domain::grid_sample apply_claim(context ctx,
                                    const domain::grid_sample& v,
                                    const ores::utility::domain::precondition& claim);
};

}

#endif
